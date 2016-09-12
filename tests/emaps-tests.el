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

(provide 'emaps-tests)
;;; emaps-tests.el ends here
