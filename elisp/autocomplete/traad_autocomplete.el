;; This is an example of how to use traad with autocomplete to get
;; completions for python stuff.

(require 'auto-complete)

(defun ac-traad-candidates ()
  "Get the list of completions at point."
  (mapcar 'car (traad-code-assist (point))))

(defun ac-traad-documentation (sym)
  "Look up symbol documentation in the cache."
  (traad-get-doc (point)))

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
