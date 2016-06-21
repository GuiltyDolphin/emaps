;;; emaps.el --- utilities for working with keymaps.
;;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Provide utilities for working with keymaps in Emacs.
;;;
;;; Code:

(defmacro emaps--with-modify-help-buffer (body)
  "Execute BODY with the current help buffer active; allow modifications."
  `(with-current-buffer (get-buffer "*Help*")
     (let ((buffer-read-only nil))
       ,body
       (set-buffer-modified-p nil))))

(defun emaps--completing-read-variable (prompt &optional pred)
  "Prompt the user with PROMPT for a variable that satisfied PRED (if supplied)."
  (let ((v (variable-at-point))
        (enable-recursive-minibuffers t)
        (check
         (lambda (it)
           (and (symbolp it)
                (boundp it)
                (if pred (funcall pred (symbol-value it)) t))))
        vars
        val)
    (mapatoms (lambda (atom) (when (funcall check atom) (push atom vars))))
    (setq val (completing-read
               (if (funcall check v)
                   (format
                    (concat prompt " (default %s): ") v)
                 (concat prompt ": "))
               vars
               check
               t nil nil
               (if (symbolp v) (symbol-name v))))
    (list (if (equal val "") v (intern val)))))

;;;###autoload
(defun emaps-describe-keymap (keymap)
  "Display the full documentation of KEYMAP (a symbol).

Unlike `describe-variable', this will display characters as strings rather than integers."
  (interactive (emaps--completing-read-variable "Describe keymap" 'keymapp))
  (describe-variable keymap)
  (emaps--with-modify-help-buffer
   (save-excursion (while (search-forward-regexp "(\\([0-9]+\\) ." nil t)
                     (replace-match (char-to-string (string-to-number (match-string 1))) nil nil nil 1)))))

(provide 'emaps)
;;; emaps.el ends here
