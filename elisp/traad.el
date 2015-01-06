;;; traad.el --- emacs interface to the traad refactoring server.
;;
;; Copyright (c) 2012-2015 Austin Bingham
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.9
;; URL: https://github.com/abingham/traad
;; Package-Requires: ((deferred "0.3.2") (popup "0.5.0") (request "0.2.0") (request-deferred "0.2.0") (python-environment "0.0.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; traad is a JSON+HTTP server built around the rope refactoring library. This
;; file provides an API for talking to that server - and thus to rope - from
;; emacs lisp. Or, put another way, it's another way to use rope from emacs.
;;
;; For more details, see the project page at
;; https://github.com/abingham/traad.
;;
;; Installation:
;;
;; traad depends on the following packages:
;;
;;   cl
;;   deferred - https://github.com/kiwanami/emacs-deferred
;;   json
;;   request - https://github.com/tkf/emacs-request
;;   request-deferred - (same as request)
;;
;; Copy traad.el to some location in your emacs load path. Then add
;; "(require 'traad)" to your emacs initialization (.emacs,
;; init.el, or something).
;;
;; Example config:
;;
;;   (require 'traad)
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'cl)
(require 'deferred)
(require 'json)
(require 'popup)
(require 'python-environment)
(require 'request)
(require 'request-deferred)

					; TODO: Call
					; update-history-buffer as
					; needed.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user variables

(defgroup traad nil
  "A Python refactoring tool."
  :group 'tools
  :group 'programming)

(defcustom traad-host "127.0.0.1"
  "The host on which the traad server is running."
  :type '(string)
  :group 'traad)

(defcustom traad-server-program nil
  "The name of the traad server program. 

If this is nil (default) then the server found in the
`traad-environment-name' virtual environment is used.

Note that for python3 projects this commonly needs to be set to `traad3'."
  :type '(repeat string)
  :group 'traad)

(defcustom traad-server-port 0
  "Port on which the traad server will listen. 

0 means any available port."
  :type '(number)
  :group 'traad)

(defcustom traad-server-args (list "-V" "2")
  "Parameters passed to the traad server before the directory name."
  :type '(repeat string)
  :group 'traad)

(defcustom traad-auto-revert nil
  "Whether proximal buffers should be automatically reverted \
after successful refactorings."
  :type '(boolean)
  :group 'traad)

(defcustom traad-debug nil
  "Whether debug info should be generated."
  :type '(boolean)
  :group 'traad)

(defconst traad-required-protocol-version 2
  "The required protocol version.")

(defcustom traad-environment-root "traad"
  "The name of the Python environment containing the traad server to use.

When `traad-install-server' runs, it uses this variable to
determine where to install the server.

When `traad-server-program' is nil, this variable is used to
determine where the traad server program is installed.

This name is used by `python-environment.el' to locate the
virtual environment into which the desired version of traad is
installed.  If you have multiple traads in different virtual
environment (e.g. one for Python 2 and one for Python 3) then you
may need to dynamically alter this variable to select the one you
want to use."
  :group 'traad)

(defcustom traad-environment-virtualenv nil
  "``virtualenv`` command to use.  A list of string.

If nil, `python-environment-virtualenv' is used instead."
  :group 'traad)

(defun traad--server-command ()
  "Get the command to launch the server."
  (or traad-server-program
      (let* ((get-bin (lambda (x) (python-environment-bin x traad-environment-root)))
             (script (or (funcall get-bin "traad")
                         (funcall get-bin "traad3"))))
        (when script (list script)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-close

;;;###autoload
(defun traad-open (directory)
  "Open a traad project on the files in DIRECTORY."
  (interactive
   (list
    (read-directory-name "Directory: ")))
  (traad-close)
  (let ((proc-buff (get-buffer-create "*traad-server*")))
    (set-buffer proc-buff)
    (erase-buffer)
    (let* ((program (traad--server-command))
           (program (if (listp program) program (list program)))
	   (args (append traad-server-args
			 (list "-p" (number-to-string traad-server-port))
			 (list directory)))
	   (program+args (append program args))
	   (default-directory "~/")
	   (proc (apply #'start-process "traad-server" proc-buff program+args))
	   (cont 1))
      (while cont
	(set-process-query-on-exit-flag proc nil)
	(accept-process-output proc 0 100 t)
	(let ((proc-output (with-current-buffer proc-buff
			     (buffer-string))))
	  (cond
	   ((string-match "^Listening on http://.*:\\\([0-9]+\\\)/$" proc-output)
	    (setq traad-server-port-actual (string-to-number (match-string 1 proc-output))
		  cont nil))
	   (t
	    (incf cont)
	    (when (< 30 cont) ; timeout after 3 seconds
	      (error "Server timeout."))))))
      (traad-check-protocol-version))))

(defun traad-check-protocol-version ()
  (deferred:$

    (traad-deferred-request "/protocol_version")

    (deferred:nextc it
      (lambda (req)
	(let ((protocol-version (assoc-default
				 'protocol-version
				 (request-response-data req))))
	  (if (eq protocol-version traad-required-protocol-version)
	      (message "Supported protocol version detected: %s" protocol-version)
	    (error "Server protocol version is %s, but we require version %s"
		   protocol-version
		   traad-required-protocol-version)))))))

					; TODO
;; (defun traad-add-cross-project (directory)
;;   "Add a cross-project to the traad instance."
;;   (interactive
;;    (list
;;     (read-directory-name "Directory:")))
;;   (traad-call 'add_cross_project directory))

					; TODO
;; (defun traad-remove-cross-project (directory)
;;   "Remove a cross-project from the traad instance."
;;   (interactive
;;    (list
;;     (completing-read
;;      "Directory: "
;;      (traad-call 'cross_project_directories))))
;;   (traad-call 'remove_cross_project directory))

					; TODO
;; (defun traad-get-cross-project-directories ()
;;   "Get a list of root directories for cross projects."
;;   (interactive)
;;   (traad-call 'cross_project_directories))

;;;###autoload
(defun traad-close ()
  "Close the current traad project, if any."
  (interactive)
  (if (traad-running?)
      (delete-process "traad-server")))

;;;###autoload
(defun traad-running? ()
  "Determine if a traad server is running."
  (interactive)
  (if (get-process "traad-server") 't nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; status of tasks running on the server.

(defun traad-task-status (task-id)
  "Get the status of a traad task. Returns a deferred request."
  (traad-deferred-request
   "/task/" (number-to-string task-id)))

(defun traad-full-task-status ()
  "Get the status of all traad tasks. Returns a deferred request."
  (traad-deferred-request
   "/tasks"))

;;;###autoload
(defun traad-display-task-status (task-id)
  "Get the status of a traad task."
  (interactive
   (list
    (read-number "ID: ")))
  (deferred:$
    (traad-task-status task-id)
    (deferred:nextc it
      (lambda (response)
	(message "Task status: %s"
		 (request-response-data response))))))

;;;###autoload
(defun traad-display-full-task-status ()
					; TODO: Improve the display of
					; this data.
  (interactive)
  (deferred:$
    (traad-full-task-status)
    (deferred:nextc it
      (lambda (response)
	(let ((buff (get-buffer-create "*traad-task-status*")))
	  (switch-to-buffer buff)
	  (erase-buffer)
	  (insert (format "%s"
			  (request-response-data response))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; server info stuff.

(defun traad-get-root ()
  "Get the project root.

  Returns a deferred request object. The root information is in
  the 'data' key of the JSON data.
  "
					; TODO: Can we cache this
					; value? That works if we
					; assume that the server
					; doesn't change roots or get
					; restarted on a different
					; root. Hmm...
  (traad-deferred-request
   "/root"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; history

;;;###autoload
(defun traad-undo (idx)
  "Undo the IDXth change from the history. \
IDX is the position of an entry in the undo list (see: \
traad-history). This change and all that depend on it will be \
undone."
  (interactive
   (list
    (read-number "Index: " 0)))
  (lexical-let ((data (list (cons "index" idx))))
    
    (deferred:$
      
      (traad-deferred-request
       "/history/undo"
       :data data
       :type "POST")

      (deferred:nextc it
	(lambda (rsp) (message "Undo"))))))


;;;###autoload
(defun traad-redo (idx)
  "Redo the IDXth change from the history. \
IDX is the position of an entry in the redo list (see: \
traad-history). This change and all that depend on it will be \
redone."
  (interactive
   (list
    (read-number "Index: " 0)))
  (lexical-let ((data (list (cons "index" idx))))
    
    (deferred:$
      
      (traad-deferred-request
       "/history/redo"
       :data data
       :type "POST")
      
      (deferred:nextc it
	(lambda (rsp) (message "Redo"))))))

(defun traad-update-history-buffer ()
  "Update the contents of the history buffer, creating it if \
necessary. Return the history buffer."
  (deferred:$

    (deferred:parallel
      (traad-deferred-request
       "/history/view_undo")
      (traad-deferred-request
       "/history/view_redo"))

    (deferred:nextc it
      (lambda (inputs)
	(let* ((undo (assoc-default 'history (request-response-data (elt inputs 0))))
	       (redo (assoc-default 'history (request-response-data (elt inputs 1))))
	       (buff (get-buffer-create "*traad-history*")))
	  (set-buffer buff)
	  (erase-buffer)
	  (insert "== UNDO HISTORY ==\n")
	  (if undo (insert (pp-to-string (traad-enumerate undo))))
	  (insert "\n")
	  (insert "== REDO HISTORY ==\n")
	  (if redo (insert (pp-to-string (traad-enumerate redo))))
	  buff)))))

;;;###autoload
(defun traad-display-history ()
  "Display undo and redo history."
  (interactive)
  (deferred:$
    (traad-update-history-buffer)
    (deferred:nextc it
      (lambda (buffer)
	(switch-to-buffer buffer)))))

(defun traad-history-info-core (location)
  "Display information on a single undo/redo operation."

  (deferred:$

    (traad-deferred-request
     location)

    (deferred:nextc it
      (lambda (rsp)
	(let ((buff (get-buffer-create "*traad-change*"))
	      (info (assoc-default 'info (request-response-data rsp))))
	  (switch-to-buffer buff)
	  (diff-mode)
	  (erase-buffer)
	  (insert "Description: " (cdr (assoc 'description info)) "\n"
		  "Time: " (number-to-string (cdr (assoc 'time info))) "\n"
		  "Change:\n"
		  (cdr (assoc 'full_change info))))))))

;;;###autoload
(defun traad-undo-info (i)
  "Get info on the I'th undo history."
  (interactive
   (list
    (read-number "Undo index: " 0)))
  (traad-history-info-core
   (concat "/history/undo_info/" (number-to-string i))))

;;;###autoload
(defun traad-redo-info (i)
  "Get info on the I'th redo history."
  (interactive
   (list
    (read-number "Redo index: " 0)))
  (traad-history-info-core
   (concat "/history/redo_info/" (number-to-string i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; renaming support

;;;###autoload
(defun traad-rename-current-file (new-name)
  "Rename the current file/module."
  (interactive
   (list
    (read-string "New file name: ")))
  (lexical-let ((new-name new-name)
		(data (list (cons "name" new-name)
			    (cons "path" (buffer-file-name))))
		(old-buff (current-buffer))
		(dirname (file-name-directory buffer-file-name))
		(extension (file-name-extension buffer-file-name)))
    (deferred:$

      (traad-deferred-request
       "/refactor/rename"
       :data data)

					; TODO: This should actually
					; poll until the operation is
					; complete. But then so should
					; other stuff, I suppose...
      (deferred:nextc it
	(lambda (_)
	  (switch-to-buffer
	   (find-file
	    (expand-file-name
	     (concat new-name "." extension)
	     dirname)))
	  (kill-buffer old-buff)
	  (traad-update-history-buffer))))))

;;;###autoload
(defun traad-rename (new-name)
  "Rename the object at the current location."
  (interactive
   (list
    (read-string "New name: ")))
  (traad-typical-deferred-post
   "Rename"
   "/refactor/rename"
   (list (cons "name" new-name)
	 (cons "path" (buffer-file-name))
	 (cons "offset" (traad-adjust-point (point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Change signature support

;;;###autoload
(defun traad-normalize-arguments ()
  "Normalize the arguments for the method at point."
  (interactive)
  (traad-typical-deferred-post
   "Normalize-arguments"
   "/refactor/normalize_arguments"
   (list (cons "path" (buffer-file-name))
	 (cons "offset" (traad-adjust-point (point))))))

;;;###autoload
(defun traad-remove-argument (index)
  "Remove the INDEXth argument from the signature at point."
  (interactive
   (list
    (read-number "Index: ")))
					; TODO: Surely there's a
					; better way to construct
					; these lists...
  (traad-typical-deferred-post
   "Remove-argument"
   "/refactor/remove_argument"
   (list (cons "arg_index" index)
	 (cons "path" (buffer-file-name))
	 (cons "offset" (traad-adjust-point (point))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extraction support

(defun traad-extract-core (location name begin end)
  (traad-typical-deferred-post
   name
   location
   (list (cons "path" (buffer-file-name))
	 (cons "start-offset" (traad-adjust-point begin))
	 (cons "end-offset" (traad-adjust-point end))
	 (cons "name" name))))

;;;###autoload
(defun traad-extract-method (name begin end)
  "Extract the currently selected region to a new method."
  (interactive "sMethod name: \nr")
  (traad-extract-core "/refactor/extract_method" name begin end))

;;;###autoload
(defun traad-extract-variable (name begin end)
  "Extract the currently selected region to a new variable."
  (interactive "sVariable name: \nr")
  (traad-extract-core "/refactor/extract_variable" name begin end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; importutils support

;;;###autoload
(defun traad-organize-imports (filename)
  "Organize the import statements in FILENAME."
  (interactive
   (list
    (read-file-name "Filename: " (buffer-file-name))))
  (traad-typical-deferred-post
   "Organize-imports"
   "/imports/organize"
   (list (cons "path" filename))))

;;;###autoload
(defun traad-expand-star-imports (filename)
  "Expand * import statements in FILENAME."
  (interactive
   (list
    (read-file-name "Filename: " (buffer-file-name))))
  (traad-typical-deferred-post
   "Expand-star-imports"
   "/imports/expand_star"
   (list (cons "path" filename))))

;;;###autoload
(defun traad-froms-to-imports (filename)
  "Convert 'from' imports to normal imports in FILENAME."
  (interactive
   (list
    (read-file-name "Filename: " (buffer-file-name))))
  (traad-typical-deferred-post
   "Froms-to-imports"
   "/imports/froms_to_imports"
   (list (cons "path" filename))))

;;;###autoload
(defun traad-relatives-to-absolutes (filename)
  "Convert relative imports to absolute in FILENAME."
  (interactive
   (list
    (read-file-name "Filename: " (buffer-file-name))))
  (traad-typical-deferred-post
   "Relatives-to-absolutes"
   "/imports/relatives_to_absolutes"
   (list (cons "path" filename))))

;;;###autoload
(defun traad-handle-long-imports (filename)
  "Clean up long import statements in FILENAME."
  (interactive
   (list
    (read-file-name "Filename: " (buffer-file-name))))
  (traad-typical-deferred-post
   "Handle-long-imports"
   "/imports/handle_long_imports"
   (list (cons "path" filename))))

;;;###autoload
(defun traad-imports-super-smackdown (filename)
  (interactive
   (list
    (read-file-name "Filename: " (buffer-file-name))))
  (mapcar (lambda (f) (funcall f filename))
	  (list
	   'traad-expand-star-imports
	   'traad-relatives-to-absolutes
	   'traad-froms-to-imports
	   'traad-handle-long-imports
	   'traad-organize-imports)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; findit

(defun traad-find-occurrences (pos)
  "Get all occurences the use of the symbol at POS in the
current buffer.

  Returns a deferred request. The 'data' key in the JSON hold the
  location data in the form:

	[[path, [region-start, region-stop], offset, unsure, lineno], . . .]
  "
  (lexical-let ((data (list (cons "offset" (traad-adjust-point pos))
			    (cons "path" (buffer-file-name)))))
    (traad-deferred-request
     "/findit/occurrences"
     :data data
     :type "POST")))

(defun traad-find-implementations (pos)
  "Get the implementations of the symbol at POS in the current buffer.

  Returns a deferred request. The 'data' key in the JSON hold the
  location data in the form:

	[[path, [region-start, region-stop], offset, unsure, lineno], . . .]
  "
  (lexical-let ((data (list (cons "offset" (traad-adjust-point pos))
			    (cons "path" (buffer-file-name)))))
    (traad-deferred-request
     "/findit/implementations"
     :data data
     :type "POST")))

(defun traad-find-definition (pos)
  "Get location of a function definition.

  Returns a deferred request. The 'data' key in the JSON hold the location in
  the form:

	[path, [region-start, region-stop], offset, unsure, lineno]
  "
  (lexical-let ((data (list (cons "offset" (traad-adjust-point pos))
			    (cons "path" (buffer-file-name)))))
    (traad-deferred-request
     "/findit/definition"
     :data data
     :type "POST")))

(defun traad-display-findit (pos func buff-name)
  "Common display routine for occurrences and implementations.

  Call FUNC with POS and fill up the buffer BUFF-NAME with the results."
  (lexical-let ((buff-name buff-name))
    (deferred:$
					; Fetch in parallel...
      (deferred:parallel
	
					; ...the occurrence data...
	(deferred:$
	  (apply func (list pos))
	  (deferred:nextc it
	    'request-response-data)
	  (deferred:nextc it
	    (lambda (x) (assoc-default 'data x))))
	
					; ...and the project root.
	(deferred:$
	  (traad-get-root)
	  (deferred:nextc it
	    'request-response-data)
	  (deferred:nextc it
	    (lambda (x) (assoc-default 'root x)))))
      
      (deferred:nextc it
	(lambda (input)
	  (let ((locs (elt input 0)) ; the location vector
		(root (elt input 1)) ; the project root
		(buff (get-buffer-create buff-name))
		(inhibit-read-only 't))
	    (pop-to-buffer buff)
	    (erase-buffer)
	    
					; For each location, add a
					; line to the buffer.  TODO:
					; Is there a "dovector" we can
					; use? This is a bit fugly.
	    (mapcar
	     (lambda (loc)
	       (lexical-let* ((path (elt loc 0))
			      (abspath (concat root "/" path))
			      (lineno (elt loc 4))
			      (code (nth (- lineno 1) (traad-read-lines abspath))))
		 (insert-button
		  (format "%s:%s: %s\n"
			  path
			  lineno
			  code)
		  'action (lambda (x)
			    (goto-line
			     lineno
			     (find-file-other-window abspath))))))
	     locs)))))))


;;;###autoload
(defun traad-display-occurrences (pos)
  "Display all occurences the use of the symbol at POS in the
current buffer."
  (interactive "d")
  (traad-display-findit pos 'traad-find-occurrences "*traad-occurrences*"))

;;;###autoload
(defun traad-display-implementations (pos)
  "Display all occurences the use of the symbol as POS in the
current buffer."
  (interactive "d")
  (traad-display-findit pos 'traad-find-implementations "*traad-implementations*"))

;;;###autoload
(defun traad-goto-definition (pos)
  "Go to the definition of the function as POS."
  (interactive "d")
  (deferred:$
    (deferred:parallel
      (deferred:$
	(traad-find-definition pos)
	(deferred:nextc it
	  'request-response-data)
	(deferred:nextc it
	  (lambda (x) (assoc-default 'data x))))
      (deferred:$
	(traad-get-root)
	(deferred:nextc it
	  'request-response-data)
	(deferred:nextc it
	  (lambda (x) (assoc-default 'root x)))))
    
    (deferred:nextc it
      (lambda (input)
	(let ((loc (elt input 0)))
	  (if (not loc)
	      (message "No definition found.")
	    (letrec ((path (elt loc 0))
		     (root (elt input 1))
		     (abspath (if (file-name-absolute-p path) path (concat root "/" path)))
		     (lineno (elt loc 4)))
	      (goto-line
	       lineno
	       (find-file-other-window abspath)))))))))

;;;###autoload
(defun traad-findit (type)
  "Run a findit function at the current point."
  (interactive
   (list
    (completing-read
     "Type: "
     (list "occurrences" "implementations" "definition"))))
  (cond
   ((equal type "occurrences")
    (traad-display-occurrences (point)))
   ((equal type "implementations")
    (traad-display-implementations (point)))
   ((equal type "definition")
    (traad-goto-definition (point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code assist

;;;###autoload
(defun traad-code-assist (pos)
  "Get possible completions at POS in current buffer.

This returns an alist like ((completions . [[name documentation scope type]]) (result . \"success\"))"
  (interactive "d")
  (let ((data (list (cons "offset" (traad-adjust-point pos))
		    (cons "path" (buffer-file-name))))
	(request-backend 'url-retrieve))
    (request-response-data
     (request
      (concat "http://" traad-host ":" (number-to-string traad-server-port-actual)
	      "/code_assist/completions")
      :headers '(("Content-Type" . "application/json"))
      :data (json-encode data)
      :sync t
      :parser 'json-read
      :data (json-encode data)
      :type "POST"))))

(defun traad-display-in-buffer (msg buffer)
  (let ((cbuff (current-buffer))
	(buff (get-buffer-create buffer))
	(inhibit-read-only 't))
    (pop-to-buffer buff)
    (erase-buffer)
    (insert msg)
    (pop-to-buffer cbuff)))

(defun traad-get-calltip (pos)
  "Get the calltip for an object.

  Returns a deferred which produces the calltip string.
  "
  (lexical-let ((data (list (cons "offset" (traad-adjust-point pos))
			    (cons "path" (buffer-file-name)))))
    (deferred:$
      (traad-deferred-request
       "/code_assist/calltip"
       :data data
       :type "POST")
      (deferred:nextc it
	(lambda (req)
	  (assoc-default
	   'calltip
	   (request-response-data req)))))))

;;;###autoload
(defun traad-display-calltip (pos)
  "Display calltip for an object."
  (interactive "d")
  (deferred:$
    (traad-get-calltip pos)
    (deferred:nextc it
      (lambda (calltip)
	(if calltip
	    (traad-display-in-buffer
	     calltip
	     "*traad-calltip*")
	  (message "No calltip available."))))))

;;;###autoload
(defun traad-popup-calltip (pos)
  (interactive "d")
  (lexical-let ((pos pos))
    (deferred:$
      (traad-get-calltip pos)
      (deferred:nextc it
	(lambda (calltip)
	  (if calltip
	      (popup-tip
	       calltip
	       :point pos)))))))

(defun traad-get-doc (pos)
  "Get docstring for an object.

  Returns a deferred which produces the doc string. If there is
  not docstring, the deferred produces nil.
  "
  (lexical-let ((data (list (cons "offset" (traad-adjust-point pos))
			    (cons "path" (buffer-file-name)))))
    (deferred:$

      (traad-deferred-request
       "/code_assist/doc"
       :data data
       :type "POST")

      (deferred:nextc it
	(lambda (req)
	  (assoc-default
	   'doc
	   (request-response-data req)))))))

;;;###autoload
(defun traad-display-doc (pos)
  "Display docstring for an object."
  (interactive "d")
  (deferred:$
    (traad-get-doc pos)
    (deferred:nextc it
      (lambda (doc)
	(if doc
	    (traad-display-in-buffer
	     doc
	     "*traad-doc*")
	  (message "No docstring available."))))))

;;;###autoload
(defun traad-popup-doc (pos)
  (interactive "d")
  (lexical-let ((pos pos))
    (deferred:$
      (traad-get-doc pos)
      (deferred:nextc it
	(lambda (doc)
	  (if doc
	      (popup-tip
	       doc
	       :point pos)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; low-level support

(defun traad-construct-url (location)
  "Construct a URL to a specific location on the traad server.

  In short: http://server_host:server_port<location>
  "
  (concat
   "http://" traad-host
   ":" (number-to-string traad-server-port-actual)
   location))

(defun* traad-deferred-request (location &key (type "GET") (data '()))
  (let ((request-backend 'url-retrieve))
    (request-deferred
     (traad-construct-url location)
     :type type
     :parser 'json-read
     :headers '(("Content-Type" . "application/json"))
     :data (json-encode data))))

(defun traad-typical-deferred-post (name location data)
  "Posts DATA to LOCATION and afterwards prints NAME and the new
task-id. Should only be used with URLs that respond with a
task_id field in the response."
  (lexical-let ((data data)
		(name name))
    (deferred:$
      
      (traad-deferred-request
       location
       :type "POST"
       :data data)

      (deferred:nextc it
	(lambda (rsp)
	  (message
	   "%s started with task-id %s"
	   name
	   (assoc-default 'task_id (request-response-data rsp))))))))

(defun traad-range (upto)
  (defun range_ (x)
    (if (> x 0)
	(cons x (range_ (- x 1)))
      (list 0)))
  (nreverse (range_ upto)))

(defun traad-enumerate (l)
  (map 'list 'cons (traad-range (length l)) l))

(defun traad-adjust-point (p)
  "Rope uses 0-based indexing, but Emacs points are 1-based."
  (- p 1))

(defun traad-read-lines (path)
  "Return a list of lines of a file at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" nil)))

					; TODO: invalidation support?

(defconst traad-install-server--command
  `("pip" "install" "--upgrade" "traad"))

(defun traad-install-server ()
  "Install traad. 

This installs the server into the `traad-environment-root'
virtual environment using `traad-environment-virtualenv' to
create the virtualenv if necessary.  Manipulate these two variable
to create multiple installations of traad if needed.

By default, then, it installs traad into
`PYTHON-ENVIRONMENT_DIRECTORY/traad`."
  (interactive)
  (deferred:$
    (python-environment-run traad-install-server--command
                            traad-environment-root
                            traad-environment-virtualenv)))

(provide 'traad)

;;; traad.el ends here
