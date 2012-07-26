;; This is an example of how to use traad with autocomplete to get
;; completions for python stuff.

(require 'auto-complete)

(defun ac-traad-find ()
  "Find the starting point of the 'thing' at point."
  (require 'thingatpt)
  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
    (if (null symbol)
        (if (string= "." (buffer-substring (- (point) 1) (point)))
            (point)
          nil)
      symbol)))

(defvar ac-traad-cache nil
  "Holds the results of the last traad-code-assist.")

(defun ac-traad-candidates ()
  "Get the list of completions at point."
  (let* ((real-point (ac-traad-find))
         (pt (if real-point real-point (point))))
    (setq ac-traad-cache (traad-code-assist pt))
    (mapcar 'car ac-traad-cache)))

(defun ac-traad-documentation (sym)
  "Look up symbol documentation in the cache."
  (let ((entry (assoc sym ac-traad-cache)))
    (if entry (cadr entry))))

;; The autocomplete source for traad
(ac-define-source traad
  '((depends traad)
    (candidates . ac-traad-candidates)
    (document . ac-traad-documentation)
    (symbol . "s")
    (requires . 0)))

(defun ac-traad-setup ()
  "Add ac-source-traad to autocomplete list."
  ;(setq ac-sources (append '(ac-source-traad) ac-sources)))
  (setq ac-sources '(ac-source-traad)))

;; Insert the traad source in python mode.
(add-hook 'python-mode-hook 'ac-traad-setup)
