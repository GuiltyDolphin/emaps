;;; emaps-tests.el
;;; Code:

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

(provide 'emaps-tests)
;;; emaps-tests.el ends here
