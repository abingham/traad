;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

(require 'auto-complete)

;(defvar traad-source-cache nil)

;(ac-clear-variable-after-save 'traad-source-cache)

(defun traad-source-candidates ()
  ;(or traad-source-cache
  ;    (setq traad-source-cache (mapcar car (traad-code-assist (point))))))
  (error "llama!")
  (mapcar car (traad-code-assist (point))))

(defvar traad-ac-source
  '((candidates . traad-source-candidates)))

(setq ac-sources '(traad-ac-source))