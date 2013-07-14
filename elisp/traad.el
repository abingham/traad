;;; traad.el --- emacs interface to the traad xmlrpc refactoring server.
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.1
;; URL: https://github.com/abingham/traad
;;
;; This file is not part of GNU Emacs.
;;
;; Copyright (c) 2012 Austin Bingham
;;
;;; Commentary:
;;
;; Description:
;;
;; traad is an xmlrpc server built around the rope refactoring library. This
;; file provides an API for talking to that server - and thus to rope - from
;; emacs lisp. Or, put another way, it's another way to use rope from emacs.
;;
;; For more details, see the project page at
;; https://github.com/abingham/traad.
;;
;; Installation:
;;
;; Make sure xml-rpc.el is in your load path. Check the emacswiki for more info:
;;
;;    http://www.emacswiki.org/emacs/XmlRpc
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
(require 'request)
(require 'request-deferred)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user variables

(defcustom traad-host "127.0.0.1"
  "The host on which the traad server is running."
  :type '(string)
  :group 'traad)

(defcustom traad-server-program "traad"
  "The name of the traad server program. This may be a string or a list. For python3 projects this commonly needs to be set to 'traad3'."
  :type '(string)
  :group 'traad)

(defcustom traad-server-args (list "-V" "2")
  "Parameters passed to the traad server before the directory name."
  :type '(list)
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

; TODO: get rid of this when possible.
(defcustom traad-use-async t
  "Whether traad should use asynchrounous XMLRPC calls when possible."
  :type '(boolean)
  :group 'traad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-close 

(defun traad-open (directory)
  "Open a traad project on the files in DIRECTORY."
  (interactive
   (list
    (read-directory-name "Directory: ")))
  (traad-close)
  (let ((proc-buff (get-buffer-create "*traad-server*")))
    (set-buffer proc-buff)
    (erase-buffer)
    (let* ((program (if (listp traad-server-program) 
			traad-server-program 
		      (list traad-server-program)))
	   (args (append traad-server-args (list directory)))
	   (program+args (append program args))
	   (default-directory "~/")
	   (proc (apply #'start-process "traad-server" proc-buff program+args))
	   (cont 1))
      (while cont
    (set-process-query-on-exit-flag proc nil)
	(accept-process-output proc 0 100 t)
	(let ((port-str (with-current-buffer proc-buff
			  (buffer-string))))
	  (cond
	   ((string-match "^[0-9]+$" port-str)
	    (setq traad-port (string-to-number port-str)
		  cont nil))
	   (t
	    (incf cont)
	    (when (< 30 cont) ; timeout after 3 seconds
	      (error "Server timeout.")))))))))

; TODO
(defun traad-add-cross-project (directory)
  "Add a cross-project to the traad instance."
  (interactive
   (list
    (read-directory-name "Directory:")))
  (traad-call 'add_cross_project directory))

; TODO
(defun traad-remove-cross-project (directory)
  "Remove a cross-project from the traad instance."
  (interactive
   (list
    (completing-read
     "Directory: "
     (traad-call 'cross_project_directories))))
  (traad-call 'remove_cross_project directory))

; TODO
(defun traad-get-cross-project-directories ()
  "Get a list of root directories for cross projects."
  (interactive)
  (traad-call 'cross_project_directories))

(defun traad-close ()
  "Close the current traad project, if any."
  (interactive)
  (if (traad-running?)
      (delete-process "traad-server")))

(defun traad-running? ()
  "Determine if a traad server is running."
  (interactive)
  (if (get-process "traad-server") 't nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; status of tasks running on the server.

(defun traad-task-status (task-id)
  "Get the status of a traad task. Returns a deferred request."
  (request-deferred
   (concat
    "http://" traad-host ":" (number-to-string traad-port)
    "/task/" (number-to-string task-id))
   :type "GET"
   :parser 'json-read))

(defun traad-full-task-status ()
  "Get the status of all traad tasks. Returns a deferred request."
  (request-deferred
   (concat
    "http://" traad-host ":" (number-to-string traad-port)
    "/tasks")
   :type "GET"
   :parser 'json-read))

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

(defun traad-display-full-task-status ()
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
;; resource access

; TODO
(defun traad-get-all-resources ()
  "Get all resources in a project."
  (traad-call 'get_all_resources))

; TODO
(defun traad-get-children (path)
  "Get all child resources for PATH. PATH may be absolute or relative to
the project root."
  (traad-call 'get_children path))

; TODO
(defun traad-get-root ()
  "Get the project root."
  (traad-call 'get_root))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; history

(defun traad-undo (idx)
  "Undo the IDXth change from the history. \
IDX is the position of an entry in the undo list (see: \
traad-history). This change and all that depend on it will be \
undone."
  (interactive
   (list
    (read-number "Index: " 0)))
  (let ((data (list (cons "index" idx))))
    (traad-request
     "/history/undo"
     data
     (function*
      (lambda (&key data &allow-other-keys)
       (message "Undo"))))))


; TODO
(defun traad-redo (idx)
  "Redo the IDXth change from the history. \
IDX is the position of an entry in the redo list (see: \
traad-history). This change and all that depend on it will be \
redone."
  (interactive
   (list
    (read-number "Index: " 0)))
  (traad-call-async-standard
   'redo (list idx)))

; TODO
(defun traad-update-history-buffer ()
  "Update the contents of the history buffer, creating it if \
necessary. Return the history buffer."
  (save-excursion
    (let ((undo (traad-call 'undo_history))
	  (redo (traad-call 'redo_history))
	  (buff (get-buffer-create "*traad-history*")))
      (set-buffer buff)
      (erase-buffer)
      (insert "== UNDO HISTORY ==\n")
      (if undo (insert (pp-to-string (traad-enumerate undo))))
      (insert "\n")
      (insert "== REDO HISTORY ==\n")
      (if redo (insert (pp-to-string (traad-enumerate redo))))
      buff)
    ))

; TODO
(defun traad-history ()
  "Display undo and redo history."
  (interactive)
  (switch-to-buffer (traad-update-history-buffer)))

; TODO
(defun traad-history-info-core (info)
  "Display information on a single undo/redo operation."
  (let ((buff (get-buffer-create "*traad-change*")))
    (switch-to-buffer buff)
    (diff-mode)
    (erase-buffer)
    (insert "Description: " (cdr (assoc "description" info)) "\n"
	    "Time: " (number-to-string (cdr (assoc "time" info))) "\n"
	    "Change:\n"
	    (cdr (assoc "full_change" info))
	    )))

; TODO
(defun traad-undo-info (i)
  "Get info on the I'th undo history."
  (interactive
   (list
    (read-number "Undo index: " 0)))
  (traad-history-info-core (traad-call 'undo_info i)))

; TODO
(defun traad-redo-info (i)
  "Get info on the I'th redo history."
  (interactive
   (list
    (read-number "Redo index: " 0)))
  (traad-history-info-core (traad-call 'redo_info i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; renaming support

; TODO
(defun traad-rename-current-file (new-name)
  "Rename the current file/module."
  (interactive
   (list
    (read-string "New file name: ")))
  (traad-call-async
   'rename (list new-name buffer-file-name)
   (lambda (_ new-name dirname extension old-buff)
     (switch-to-buffer 
      (find-file
       (expand-file-name 
	(concat new-name "." extension) 
	dirname)))
     (kill-buffer old-buff)
     (traad-update-history-buffer))
   (list new-name
	 (file-name-directory buffer-file-name)
	 (file-name-extension buffer-file-name)
	 (current-buffer))))

(defun traad-rename (new-name)
  "Rename the object at the current location."
  (interactive
   (list
    (read-string "New name: ")))
  (let ((data (list (cons "name" new-name)
                    (cons "path" (buffer-file-name))
                    (cons "offset" (traad-adjust-point (point))))))
    (traad-request
     "/refactor/rename"
     data
     (function*
      (lambda (&key data &allow-other-keys)
        (let* ((task-id (assoc-default 'task_id data)))
          (message "Rename started with task-id %s" task-id)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Change signature support

(defun traad-normalize-arguments ()
  "Normalize the arguments for the method at point."
  (interactive)
  (let ((data (list (cons "path" (buffer-file-name))
                    (cons "offset" (traad-adjust-point (point))))))
    (traad-request
     "/refactor/normalize_arguments"
     data
     (function* (lambda (&key data &allow-other-keys)
                  (let* ((task-id (assoc-default 'task_id data)))
                    (message "Normalize-arguments started with task-id %s" task-id)))))))

(defun traad-remove-argument (index)
  "Remove the INDEXth argument from the signature at point."
  (interactive
   (list
    (read-number "Index: ")))
  ; TODO: Surely there's a better way to construct these lists...
  (let ((data (list (cons "arg_index" index)
                    (cons "path" (buffer-file-name))
                    (cons "offset" (traad-adjust-point (point))))))
    (traad-request
     "/refactor/remove_argument"
     data
     (function* (lambda (&key data &allow-other-keys)
                  (let* ((task-id (assoc-default 'task_id data)))
                    (message "Remove-argument started with task-id %s" task-id)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extraction support

(defun traad-extract-core (type name begin end)
  (traad-call-async-standard
   type (list name (buffer-file-name) 
	      (traad-adjust-point begin)
	      (traad-adjust-point end))))

(defun traad-extract-method (name begin end)
  "Extract the currently selected region to a new method."
  (interactive "sMethod name: \nr")
  (let ((data (list (cons "path" (buffer-file-name))
                    (cons "start-offset" (traad-adjust-point begin))
                    (cons "end-offset" (traad-adjust-point end))
                    (cons "name" name))))
    (traad-request
     "/refactor/extract_method"
     data
     (function* (lambda (&key data &allow-other-keys)
                  (let* ((task-id (assoc-default 'task_id data)))
                    (message "Extract-method started with task-id %s" task-id)))))))

; TODO
(defun traad-extract-variable (name begin end)
  "Extract the currently selected region to a new variable."
  (interactive "sVariable name: \nr")
  (traad-extract-core 'extract_variable name begin end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; importutils support

;; TODO: refactor these importutils using a macro?

; TODO
(defun traad-organize-imports (filename)
  "Organize the import statements in FILENAME."
  (interactive
   (list
    (read-file-name "Filename: ")))
  (traad-call-async-standard
   'organize_imports (list filename)))

; TODO
(defun traad-expand-star-imports (filename)
  "Expand * import statements in FILENAME."
  (interactive
   (list
    (read-file-name "Filename: ")))
  (traad-call-async-standard
   'expand_star_imports (list filename)))

; TODO
(defun traad-froms-to-imports (filename)
  "Convert 'from' imports to normal imports in FILENAME."
  (interactive
   (list
    (read-file-name "Filename: ")))
  (traad-call-async-standard
   'froms_to_imports (list filename)))

; TODO
(defun traad-relatives-to-absolutes (filename)
  "Convert relative imports to absolute in FILENAME."
  (interactive
   (list
    (read-file-name "Filename: ")))
  (traad-call-async-standard
   'relatives_to_absolutes (list filename)))

; TODO
(defun traad-handle-long-imports (filename)
  "Clean up long import statements in FILENAME."
  (interactive
   (list
    (read-file-name "Filename: ")))
  (traad-call-async-standard
   'handle_long_imports (list filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; findit

; TODO
(defun traad-find-occurrences (pos)
  "Get all occurences the use of the symbol as POS in the
current buffer."
  (traad-call 'find_occurrences
	      (traad-adjust-point pos)
	      (buffer-file-name)))

; TODO
(defun traad-find-implementations (pos)
  "Find all places a given method is overridden."
  (traad-call 'find_implementations
	      (traad-adjust-point pos)
	      (buffer-file-name)))

; TODO
(defun traad-find-definition (pos)
  "Get location of a function definition."
  (traad-call 'find_definition
	      (buffer-substring-no-properties
	       (point-min)
	       (point-max))
	      (traad-adjust-point pos)
	      (buffer-file-name)))

; TODO
(defun traad-display-findit (pos func buff-name)
  "Common display routine for occurrences and implementations."
  (let ((locs (apply func (list pos)))
	(buff (get-buffer-create buff-name))
	(inhibit-read-only 't))
    (pop-to-buffer buff)
    (erase-buffer)
    (dolist (loc locs)
      (lexical-let* ((path (car loc))
		     (abspath (concat (traad-get-root) "/" path))
		     (lineno (nth 4 loc))
		     (code (nth (- lineno 1) (traad-read-lines abspath))))
	(insert-button
	 (format "%s:%s: %s\n" 
		 path
		 lineno
		 code)
	 'action (lambda (x) 
		   (goto-line 
		    lineno 
		    (find-file-other-window abspath))))))))

; TODO
(defun traad-display-occurrences (pos)
  "Display all occurences the use of the symbol as POS in the
current buffer."
  (interactive "d")
  (traad-display-findit pos 'traad-find-occurrences "*traad-occurrences*"))

; TODO
(defun traad-display-implementations (pos)
  "Display all occurences the use of the symbol as POS in the
current buffer."
  (interactive "d")
  (traad-display-findit pos 'traad-find-implementations "*traad-implementations*"))

; TODO
(defun traad-goto-definition (pos)
  "Go to the definition of the function as POS."
  (interactive "d")
  (let* ((loc (traad-find-definition pos))
	 (path (car loc))
	 (abspath (if (file-name-absolute-p path) path (concat (traad-get-root) "/" path)))
	 (lineno (nth 4 loc)))
    (goto-line 
     lineno
     (find-file-other-window abspath))))

; TODO
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

(defun traad-code-assist (pos)
  "Get possible completions at POS in current buffer.

This returns an alist like ((completions . [[name documentation scope type]]) (result . \"success\"))"
  (interactive "d")
  (let ((data (list (cons "code" (buffer-substring-no-properties 
                                  (point-min) 
                                  (point-max)))
                    (cons "offset" (traad-adjust-point pos))
                    (cons "path" (buffer-file-name)))))
    (request-response-data
     (request
      (concat "http://" traad-host ":" (number-to-string traad-port)
              "/code_assist/completions")
      :type "GET"
      :headers '(("Content-Type" . "application/json"))
      :data (json-encode data)
      :sync t
      :parser 'json-read
      :data (json-encode data)))))

; TODO
(defun traad-display-in-buffer (msg buffer)
  (let ((cbuff (current-buffer))
	(buff (get-buffer-create buffer))
	(inhibit-read-only 't))
    (pop-to-buffer buff)
    (erase-buffer)
    (insert msg)
    (pop-to-buffer cbuff)))

; TODO
(defun traad-get-calltip (pos)
  "Get the calltip for an object."
  ; TODO: Why do I have this "or" here? Get rid of it when you have a
  ; chance.
  (or (traad-call 'get_calltip
		  (buffer-substring-no-properties
		   (point-min)
		   (point-max))
		  (traad-adjust-point pos)
		  (buffer-file-name))))

; TODO
(defun traad-display-calltip (pos)
  "Display calltip for an object."
  (interactive "d")
  (traad-display-in-buffer
   (traad-get-calltip pos)
   "*traad-calltip*"))

; TODO
(defun traad-get-doc (pos)
  "Get docstring for an object."
  (or (traad-call 'get_doc
		  (buffer-substring-no-properties 
		   (point-min)
		   (point-max))
		  (traad-adjust-point pos)
		  (buffer-file-name))
      "<no docs available>"))

; TODO
(defun traad-display-doc (pos)
  "Display docstring for an object."
  (interactive "d")
  (traad-display-in-buffer 
   (traad-get-doc pos)
   "*traad-doc*"))

; TODO
(defun traad-get-definition (pos)
  "Go to definition of the object at POS."
  (interactive "d")
  (let* ((loc (traad-call 'get_definition_location
			  (buffer-substring-no-properties 
			   (point-min)
			   (point-max))
			  (traad-adjust-point pos)
			  (buffer-file-name)))
	 (path (elt loc 0))
	 (lineno (elt loc 1)))
    (when path
      (find-file path)
      (goto-line lineno))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; low-level support

(defun* traad-request (location data callback &key (type "POST"))
  "Post `data` as JSON to `location` on the server, calling `callback` with the response."
  (request
   (concat
    "http://" traad-host ":" (number-to-string traad-port)
    location)
   :type type
   :data (json-encode data)
   :headers '(("Content-Type" . "application/json"))
   :parser 'json-read
   :success callback
   ; :complete (lambda (&rest _) (message "Finished!"))
   :error (function*
           (lambda (&key error-thrown &allow-other-keys&rest _)
             (message "Error: %S" error-thrown)))))

(defun traad-call (func &rest args)
  "Make an XMLRPC call to FUNC with ARGS on the traad server."
  (let* ((tbegin (time-to-seconds))
	 (rslt 
	  (condition-case err
	      (apply
	       #'xml-rpc-method-call
	       (concat
		"http://" traad-host ":"
		(number-to-string traad-port))
	       func args)
	    (error 
	     (error (error-message-string err)))))
	 (_ (traad-trace tbegin func args)))
    rslt))

(defun traad-async-handler (rslt callback cbargs)
  "Called with result of asynchronous calls made with traad-call-async."

  ; Print out ay error messages
  (if rslt
					; TODO: This feels wrong, but
					; I don't know the "proper"
					; way to deconstruct the
					; result object.
					; Look at "destructuring-bind".
      (let ((type (car rslt)))
	(if (eq type ':error) 
	    (let* ((info (cadr rslt))
		   (reason (cadr info)))
	      (if (eq reason 'connection-failed)
		  (message "Unable to contact traad server. Is it running?")
		(message (pp-to-string reason)))))))

  ; Call the user callback
  (apply callback (append (list rslt) cbargs)))

(defun traad-call-async (fun funargs callback &optional cbargs)
  "Make an asynchronous XMLRPC call to FUN with FUNARGS on the traad server."
  (if traad-use-async
      
      ; If async is enabled, use it
      (apply
       #'xml-rpc-method-call-async
       (lexical-let ((callback callback)
		     (cbargs cbargs))
	 (lambda (result) (traad-async-handler result callback cbargs)))
       (concat
	"http://" traad-host ":"
	(number-to-string traad-port))
       fun funargs)
    
    ; otherwise, use a synchronous call
    (let ((rslt (apply 'traad-call fun funargs)))
      (apply callback rslt cbargs))))

(defun traad-call-async-standard (fun funargs)
  "A version of traad-call-async which calls a standard callback function."
  (traad-call-async
   fun funargs
   (lambda (_ buff)
     (progn
       (traad-maybe-revert buff)
       (traad-update-history-buffer)))
   (list (current-buffer))))

(defun traad-shorten-string (x)
  (let* ((s (if (stringp x) 
		x 
	      (pp-to-string x)))
	 (l (length s)))
    (subseq s 0 (min l 10))))

(defun traad-trace (start-time func args)
  "Trace output for a function."
  (if traad-debug
      (message 
       (concat
	"[traad-call] "
	(pp-to-string func) " "
	(mapconcat 'traad-shorten-string args " ") " "
	(number-to-string (- (time-to-seconds) start-time))
	"s"
	))))

(defun traad-maybe-revert (buff)
  "If traad-auto-revert is true, revert BUFF without asking."
  (let ((fname (buffer-file-name buff)))
    (if (and traad-auto-revert 
	     fname)
      (if (file-exists-p fname)
	  (save-excursion
	    (switch-to-buffer buff)
	    (revert-buffer nil 't))
	(message 
	 (format 
	  "File %s no longer exists. Possibly removed by an un/redo." 
	  fname))))))

(defun traad-range (upto)
  (defun range_ (x)
    (if (> x 0)
	(cons x (range_ (- x 1)))
      (list 0)))
  (nreverse (range_ upto)))

(defun traad-enumerate (l)
  (map 'list 'cons (traad-range (length l)) l))

(defun traad-adjust-point (p)
  "rope uses 0-based indexing, but emacs points are 1-based. This adjusts."
  (- p 1))

(defun traad-read-lines (path)
  "Return a list of lines of a file at PATH."
  (with-temp-buffer
    (insert-file-contents path)
    (split-string (buffer-string) "\n" nil)))

; TODO: invalidation support?

(provide 'traad)
