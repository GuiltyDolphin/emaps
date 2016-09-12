;;; emaps-tests.el
;;; Code:

(require 'dash)
(require 'ert)
(require 'emaps)

(ert-deftest emaps-test-kbd ()
  "Tests for `emaps--kbd'."
  (let ((valid-forms '("a" "C-a" "C-M-a" "<foo>")))
    (dolist (vform valid-forms)
      (should (equal (emaps--kbd vform) (kbd vform))))
  (should (eq (emaps--kbd "C-M-") nil))))

(ert-deftest emaps-test-looks-like-key ()
  "Tests for `emaps--looks-like-key'."
  (let ((valid-forms '("a" "C-a" "C-M-a" "<foo>"))
        (invalid-forms '("" " " "foo" "C-M-" "C-x y")))
    (dolist (vform valid-forms)
      (should (emaps--looks-like-key vform)))
    (dolist (iform invalid-forms)
      (should-not (emaps--looks-like-key iform)))))

(ert-deftest emaps-test-key-at-point ()
  "Tests for `emaps--key-at-point'."
  (let ((forms `(("C-x" 1 ,(kbd "C-x"))
                 ("C-x y" 1 ,(kbd "C-x"))
                 ("C-x y" 4 ,(kbd "C-x"))
                 ("C-x  y" 5 nil)
                 ("C-x y" 5 ,(kbd "y"))
                 ("foo" 1 nil))))
    (dolist (form forms)
      (-let (((buf-string point expected) form))
        (with-temp-buffer
          (insert buf-string)
          (should (equal expected (emaps--key-at-point point))))))))

(provide 'emaps-tests)
;;; emaps-tests.el ends here
