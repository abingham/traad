;;; traad-autocomplete.el --- auto-complete sources for traad

;; Copyright (c) 2012-2014 Austin Bingham

;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.1
;; URL: https://github.com/abingham/traad
;; Package-Requires: ((traad "0.1") (auto-complete "1.4.0"))
;;

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Insert the traad source in python mode.
;; (add-hook 'python-mode-hook 'ac-traad-setup)

;;; Code:

(require 'traad)
(require 'auto-complete)

(defvar ac-traad-cache nil
  "Hold the results of the last traad-code-assist.")

(defun ac-traad-candidates ()
  "Get the list of completions at point."
  (if (traad-running?)
      (progn
    (setq ac-traad-cache (assoc-default 'completions (traad-code-assist (point))))
    (mapcar (lambda (v) (elt v 0)) ac-traad-cache))
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

;;;###autoload
(defun ac-traad-setup ()
  "Add ac-source-traad to autocomplete list."
  (setq ac-sources (append '(ac-source-traad) ac-sources)))

(provide 'traad-autocomplete)

;;; traad-autocomplete.el ends here
