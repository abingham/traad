;; This is an example of how to use traad with autocomplete to get
;; completions for python stuff.

(require 'auto-complete)

(defvar ac-traad-cache nil
  "Hold the results of the last traad-code-assist.")

(defun ac-traad-candidates ()
  "Get the list of completions at point."
  (if (traad-running?)
      (progn
	(setq ac-traad-cache (assoc-default 'completions (traad-code-assist (point))))
        (message "traad-cache: %S" ac-traad-cache)
	(mapcar 'car ac-traad-cache))
    (setq ac-traad-cache nil)))

(defun ac-traad-documentation (sym)
  "Look up symbol documentation in the cache."
    (let ((entry (assoc sym ac-traad-cache)))
      (if entry (cadr entry))))

;; The autocomplete source for traad
(ac-define-source traad
  '((depends traad)
    (candidates . ac-traad-candidates)
    (cache)
    (document . ac-traad-documentation)
    (symbol . "s")
    (requires . 0)))

(defun ac-traad-setup ()
  "Add ac-source-traad to autocomplete list."
  (setq ac-sources (append '(ac-source-traad) ac-sources)))

;; Insert the traad source in python mode.
(add-hook 'python-mode-hook 'ac-traad-setup)

