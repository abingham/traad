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

(require 'xml-rpc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user variables

(defcustom traad-host "127.0.0.1"
  "The host on which the traad server is running."
  :type '(string)
  :group 'traad)

(defcustom traad-port 6942
  "The port on which the traad server is listening."
  :type '(integer)
  :group 'traad)

(defcustom traad-server-program "traad"
  "The name of the traad server program. This may be a string or a list. For python3 projects this commonly needs to be set to 'traad3'."
  :type '(string)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; open-close 

(defun traad-open (directory)
  "Open a traad project on the files in DIRECTORY."
  (interactive
   (list
    (read-directory-name "Directory: ")))
  (traad-close)
  (let ((program+args
         (append (if (listp traad-server-program)
                     traad-server-program
                   (list traad-server-program))
                 (list "-V" "1" directory)))
        (default-directory "~/"))
    (apply #'start-process "traad-server" "*traad-server*" program+args)))

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
;; resource access

(defun traad-get-all-resources ()
  "Get all resources in a project."
  (traad-call 'get_all_resources))

(defun traad-get-children (path)
  "Get all child resources for PATH. PATH may be absolute or relative to
the project root."
  (traad-call 'get_children path))

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
  (traad-call-async 'undo idx)
  (traad-maybe-revert))

(defun traad-redo (idx)
  "Redo the IDXth change from the history. \
IDX is the position of an entry in the redo list (see: \
traad-history). This change and all that depend on it will be \
redone."
  (interactive
   (list
    (read-number "Index: " 0)))
  (traad-call-async 'redo idx)
  (traad-maybe-revert))

(defun traad-history ()
  "Display undo and redo history."
  (interactive)
  (let ((undo (traad-call 'undo_history))
	(redo (traad-call 'redo_history))
	(buff (get-buffer-create "*traad-history*")))
    (switch-to-buffer buff)
    (erase-buffer)
    (insert "== UNDO HISTORY ==\n")
    (if undo (insert (pp-to-string (traad-enumerate undo))))
    (insert "\n")
    (insert "== REDO HISTORY ==\n")
    (if redo (insert (pp-to-string (traad-enumerate redo))))))

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

(defun traad-undo-info (i)
  "Get info on the I'th undo history."
  (interactive
   (list
    (read-number "Undo index: " 0)))
  (traad-history-info-core (traad-call 'undo_info i)))

(defun traad-redo-info (i)
  "Get info on the I'th redo history."
  (interactive
   (list
    (read-number "Redo index: " 0)))
  (traad-history-info-core (traad-call 'redo_info i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; renaming support

(defun traad-rename-core (new-name path &optional offset)
  "Rename PATH (or the subelement at OFFSET) to NEW-NAME."
  (if offset
      (traad-call-async 'rename new-name path offset)
      (traad-call-async 'rename new-name path))
  (traad-maybe-revert))

(defun traad-rename-current-file (new-name)
  "Rename the current file/module."
  (interactive
   (list
    (read-string "New file name: ")))
  (traad-rename-core new-name buffer-file-name)
  (let ((dirname (file-name-directory buffer-file-name))
	(extension (file-name-extension buffer-file-name))
	(old-buff (current-buffer)))
    (switch-to-buffer 
     (find-file
      (expand-file-name 
       (concat new-name "." extension) 
       dirname)))
    (kill-buffer old-buff)))

(defun traad-rename (new-name)
  "Rename the object at the current location."
  (interactive
   (list
    (read-string "New name: ")))
  (traad-rename-core new-name buffer-file-name (point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extraction support

(defun traad-extract-core (type name begin end)
  (traad-call-async type 
		    name 
		    (buffer-file-name)
		    begin
		    end)
  (traad-maybe-revert))

(defun traad-extract-method (name begin end)
  "Extract the currently selected region to a new method."
  (interactive "sMethod name: \nr")
  (traad-extract-core 'extract_method name begin end))

(defun traad-extract-variable (name begin end)
  "Extract the currently selected region to a new variable."
  (interactive "sVariable name: \nr")
  (traad-extract-core 'extract_variable name begin end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code assist

(defun traad-code-assist (pos)
  "Get possible completions at POS in current buffer. This returns a list of \
lists: ((name, documentation, scope, type), . . .)."
  (interactive "d")
  (traad-call 'code_assist
	      (buffer-substring-no-properties (point-min) (point-max))
	      pos
	      (buffer-file-name)))
  
(defun traad-get-doc (pos)
  "Display docstring for an object."
  (interactive "d")
  (let ((cbuff (current-buffer))
	(doc (or (traad-call 'get_doc
			     (buffer-substring-no-properties (point-min) (point-max))
			     pos
			     (buffer-file-name))
		 "<no docs available>"))
	(buff (get-buffer-create "*traad-doc*"))
	(inhibit-read-only 't))
    (pop-to-buffer buff)
    (erase-buffer)
    (insert doc))
    (pop-to-buffer cbuff))

(defun traad-get-definition (pos)
  "Go to definition of the object at POS."
  (interactive "d")
  (let* ((loc (traad-call 'get_definition_location
			  (buffer-substring-no-properties (point-min) (point-max))
			  pos
			  (buffer-file-name)))
	 (path (elt loc 0))
	 (lineno (elt loc 1)))
    (when path
      (find-file path)
      (goto-line lineno))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; low-level support

(defun traad-call (func &rest args)
  "Make an XMLRPC call to FUNC with ARGS on the traad server."
  (let* ((tbegin (time-to-seconds))
	 (rslt 
	  (condition-case nil
	      (apply
	       #'xml-rpc-method-call
	       (concat
		"http://" traad-host ":"
		(number-to-string traad-port))
	       func args)
	    (error 
	     (error "Unable to contact traad server. Is it running?"))))
	 (_ (traad-trace tbegin func args)))
    rslt))

(defun traad-async-handler (rslt)
  "Called with result of asynchronous calls made with traad-call-async."
  (cond 
   ((not rslt) nil)
   (t
    ; TODO: This feels wrong, but I don't know the "proper" way to
    ; deconstruct the result object.
    (let ((type (car rslt)))
      (if (eq type ':error) 
	  (let* ((info (cadr rslt))
		 (reason (cadr info)))
	    (if (eq reason 'connection-failed)
		(message "Unable to contact traad server. Is it running?")
	      (message (pp-to-string reason)))))))))

(defun traad-call-async (func &rest args)
  "Make an asynchronous XMLRPC call to FUNC with ARGS on the traad server."
  (apply
   #'xml-rpc-method-call-async
   'traad-async-handler
   (concat
    "http://" traad-host ":"
    (number-to-string traad-port))
   func args))

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

(defun traad-maybe-revert ()
  "If configured, revert the current buffer without asking."
  (if traad-auto-revert (revert-buffer nil 't)))

(defun traad-range (upto)
  (defun range_ (x)
    (if (> x 0)
	(cons x (range_ (- x 1)))
      (list 0)))
  (nreverse (range_ upto)))

(defun traad-enumerate (l)
  (map 'list 'cons (traad-range (length l)) l))

; TODO: invalidation support?

(provide 'traad)
