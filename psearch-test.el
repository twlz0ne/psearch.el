;;; psearch-test.el --- Test psearch -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

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

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'psearch)

(when noninteractive
  (transient-mark-mode))


;;; utils

(defmacro psearch-test-with-buffer (init &rest body)
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,init)
     (let ((noninteractive nil)
           (jit-lock-functions '(font-lock-fontify-region)))
       (font-lock-mode 1)
       (font-lock-set-defaults)
       (jit-lock-fontify-now))
     (goto-char (point-min))
     ,@body))


;;; tests

(ert-deftest psearch-test-point-at-list-p ()
  (psearch-test-with-buffer
   "`',@(["
   (let ((string (buffer-substring-no-properties (point-min) (point-max)))
         chars)
     (while (psearch-point-at-list-p)
       (push (char-after (point)) chars)
       (forward-char))
     (should
      (string= string (mapconcat #'char-to-string (reverse chars) ""))))))

(ert-deftest psearch-test-print-to-string ()
  (let ((psearch-pp-print-p nil))
    (should (string= "nil" (psearch--print-to-string nil)))
    (should (string= "(nil)" (psearch--print-to-string '(nil))))
    (should (string= "(foo)" (psearch--print-to-string '(foo))))
    (should (string= "(foo bar)" (psearch--print-to-string '(foo bar))))
    (should (string= "(foo 'bar)" (psearch--print-to-string '(foo 'bar))))
    (should (string= "(foo #'bar)" (psearch--print-to-string '(foo #'bar))))
    (should (string= "`(foo ,bar)" (psearch--print-to-string '`(foo ,bar))))
    (should (string= "`(foo ,(bar 1 2))" (psearch--print-to-string '`(foo ,(bar 1 2)))))
    (should (string= "`(foo (,bar 1 2))" (psearch--print-to-string '`(foo (,bar 1 2)))))
    (should (string= "`(foo ,@(bar 1 2))" (psearch--print-to-string '`(foo ,@(bar 1 2)))))
    (should (string= "`(foo `,@(,bar 1 2))" (psearch--print-to-string '`(foo `,@(,bar 1 2)))))
    ;; incomplete backquote expressions
    (should (string= "(foo ,(bar 1 2))" (psearch--print-to-string '(foo ,(bar 1 2)))))
    (should (string= "(foo (,bar 1 2))" (psearch--print-to-string '(foo (,bar 1 2)))))
    (should (string= "(foo ,@(bar 1 2))" (psearch--print-to-string '(foo ,@(bar 1 2)))))
    (should (string= "(foo `,@(,bar 1 2))" (psearch--print-to-string '(foo `,@(,bar 1 2)))))
    ))

(ert-deftest psearch-test-matcher ()
  (let ((matcher (psearch-make-matcher '`(foo . ,_))))
    (should (funcall matcher '(foo)))
    (should (funcall matcher '(foo a)))
    (should (funcall matcher '(foo a b)))
    (should (funcall matcher '(foo a b ,(c 1 2))))))

(ert-deftest psearch-test-beginning-of-prev-sexp ()
  (psearch-test-with-buffer
   "\
(foo a b (c 1 2))
((()))"
   (goto-char (point-max))
   (psearch--beginning-of-prev-sexp)
   (should (equal (sexp-at-point) nil))
   (psearch--beginning-of-prev-sexp 4)
   (should (equal (sexp-at-point) 2))))

(ert-deftest psearch-test-beginning-of-next-sexp ()
  (psearch-test-with-buffer
   "\
((()))
(foo a b (c 1 2))"
   (goto-char (point-min))
   (psearch--beginning-of-next-sexp)
   (should (equal (sexp-at-point) '(())))
   (psearch--beginning-of-next-sexp 4)
   (should (equal (sexp-at-point) 'foo))))

(ert-deftest psearch-test-backward ()
  (psearch-test-with-buffer
   "\
(foo)
(foo a)
(foo a b)
(unless nil
  `(foo a b ,(c 1 2)))"
   (goto-char (point-max))
   (let ((pattern '`(foo a b . ,_)))
     (psearch-backward pattern)
     (should (equal (sexp-at-point) '`(foo a b ,(c 1 2))))
     (psearch-backward pattern)
     (should (equal (sexp-at-point) '(foo a b))))))

(ert-deftest psearch-test-forward ()
  (psearch-test-with-buffer
   "\
(foo)
(foo a)
(foo a b)
(unless nil
  `(foo a b ,(c 1 2)))"
   (let ((pattern '`(foo a b . ,_))
         (assert-func
          (lambda (expected-sexp)
            (lambda (_result bounds)
              (let ((sexp (read (buffer-substring (car bounds) (cdr bounds)))))
                (should (equal sexp expected-sexp)))))))
     (psearch-forward pattern t (funcall assert-func '(foo a b)))
     (psearch-forward pattern t (funcall assert-func '(foo a b ,(c 1 2)))))))

(ert-deftest psearch-test-replace ()
  (psearch-test-with-buffer
   "\
(foo)
(foo a)
(foo a b)
(unless nil
  (foo a b (c 1 2))
  `(foo a b ,(c 1 2)))"
   (let ((psearch-pp-print-p nil))
     (psearch-replace '`(foo . ,rest) '`(bar ,@rest)))
   (should
    (string= (buffer-substring-no-properties (point-min) (point-max))
             "\
(bar)
(bar a)
(bar a b)
(unless nil
  (bar a b (c 1 2))
  `(bar a b ,(c 1 2)))"))
   (should (= (point) (1- (point-max))))))

(provide 'psearch-test)

;;; psearch-test.el ends here
