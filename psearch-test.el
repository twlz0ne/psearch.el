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
(require 'seq)
(require 'ert)
(require 'psearch)

(when noninteractive
  (transient-mark-mode))


;;; polyfill

;; Simulate ‘(-flatten-n 1 list)’
(defun -flatten-1 (list)
  (apply #'append list))


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
  (mapc (pcase-lambda (`(,input ,expected))
          (with-temp-buffer
            (insert input)
            (goto-char (point-min))
            (should (equal expected (psearch-point-at-list-p)))))
        '(("'foo"        nil)
          ("'(foo)"      t)
          ("',(foo)"     t)
          ("',@(foo)"    t)
          ("'`,@(foo)"   t)
          ("`',(foo)"    t)
          ("`',@(foo)"   t)
          ("``',,@(foo)" t))))

(ert-deftest psearch-test-print-to-string ()
  (let ((psearch-pp-print-p nil))
    (should (string= "nil" (psearch--print-to-string nil)))
    (should (string= "(nil)" (psearch--print-to-string '(nil))))
    (should (string= "(foo)" (psearch--print-to-string '(foo))))
    (should (string= "(foo bar)" (psearch--print-to-string '(foo bar))))
    (should (string= "(foo 'bar)" (psearch--print-to-string '(foo 'bar))))
    (should (string= "(foo #'bar)" (psearch--print-to-string '(foo #'bar))))
    (should (string= "(foo \"bar\")" (psearch--print-to-string '(foo "bar"))))
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

(ert-deftest psearch-test-pp-region1 ()
  (mapc (pcase-lambda (`(,init ,expected))
          (with-temp-buffer
            (insert init)
            (emacs-lisp-mode)
            (psearch--pp-region1 (point-min) (+ (point-min) 13))
            (should
             (string= expected (substring-no-properties (buffer-string))))))
        '(("((1) (2) (3))"      "((1)\n (2)\n (3))")
          ("((1) (2) (3))(4)"   "((1)\n (2)\n (3))(4)")
          ("((1) (2) (3)) (4)"  "((1)\n (2)\n (3)) (4)")
          ("((1) (2) (3))\n(4)" "((1)\n (2)\n (3))\n(4)"))))

(ert-deftest psearch-test-matcher ()
  (let ((matcher (psearch-make-matcher '`(foo . ,_))))
    (should (funcall matcher '(foo)))
    (should (funcall matcher '(foo a)))
    (should (funcall matcher '(foo a b)))
    (should (funcall matcher '(foo a b ,(c 1 2))))))

(ert-deftest psearch-test-beginning-of-prev-sexp-1 ()
  (psearch-test-with-buffer
   "(foo bar)"
   (goto-char (point-max))
   (psearch--beginning-of-prev-sexp)
   (should (string= "bar)" (buffer-substring (point) (point-max))))
   (psearch--beginning-of-prev-sexp)
   (should (string= "foo bar)" (buffer-substring (point) (point-max))))
   (psearch--beginning-of-prev-sexp)
   (should (string= "(foo bar)" (buffer-substring (point) (point-max))))))

(ert-deftest psearch-test-beginning-of-prev-sexp-2 ()
  (psearch-test-with-buffer
   "(foo 'bar)\n"
   (goto-char (point-max))
   (psearch--beginning-of-prev-sexp)
   (should (string= "'bar)\n" (buffer-substring (point) (point-max))))
   (psearch--beginning-of-prev-sexp)
   (should (string= "foo 'bar)\n" (buffer-substring (point) (point-max))))
   (psearch--beginning-of-prev-sexp)
   (should (string= "(foo 'bar)\n" (buffer-substring (point) (point-max))))))

(ert-deftest psearch-test-beginning-of-prev-sexp-3 ()
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

(ert-deftest psearch-test-replace-collect ()
  (psearch-test-with-buffer
   "\
(setq aaa 1)
(setq bbb '(2 3))"
   (goto-char (point-min))
   (let ((psearch-pp-print-p nil))
     (psearch-replace '`(setq ,sym ,val)
                      '(`(,sym ,val) `(setq ,@(-flatten-1 its))))
     (should (= (point) (point-max)))
     (should (string= "(setq aaa 1 bbb '(2 3))"
                      (string-trim (substring-no-properties (buffer-string)))))))
  (psearch-test-with-buffer
   "\
(setq ccc 1)
(keep-this)
(setq ddd '(2 3))"
   (goto-char (point-min))
   (let ((psearch-pp-print-p nil)
         (psearch-delete-collect-replace-region-p nil))
     (psearch-replace '`(setq ,sym ,val)
                      '(`(,sym ,val) `(setq ,@(-flatten-1 its))))
     (should (= (point) (point-max)))
     (should (string= "(keep-this)\n(setq ccc 1 ddd '(2 3))"
                      (string-trim (substring-no-properties (buffer-string)))))))
  (psearch-test-with-buffer
   "\
(setq eee 1)
(delete-this)
(setq fff '(2 3))"
   (goto-char (point-min))
   (let ((psearch-pp-print-p nil)
         (psearch-delete-collect-replace-region-p t))
     (psearch-replace '`(setq ,sym ,val)
                      '(`(,sym ,val) `(setq ,@(-flatten-1 its)))
                      (point-min) (point-max))
     (should (= (point) (point-max)))
     (should (string= "(setq eee 1 fff '(2 3))"
                      (string-trim (substring-no-properties (buffer-string))))))))

(ert-deftest psearch-test-replace+splice ()
  (mapc
   (pcase-lambda (`(,expected ,splice-p ,pp-print-p))
     (psearch-test-with-buffer
      "(setq a 1 b 2)\n(setq c 3 d 4)"
      (let ((psearch-pp-print-p pp-print-p))
        (psearch-replace '`(setq . ,(and r (guard (> (length r) 2))))
                         '(mapcar (lambda (pair)
                                    (cons 'setq pair))
                           (seq-partition r 2))
                         nil nil
                         :splice splice-p)
        (should (= (point) (point-max)))
        (should (string= expected (substring-no-properties (buffer-string)))))))
   '(("(setq a 1)\n(setq b 2)\n(setq c 3)\n(setq d 4)" t t)
     ("(setq a 1)(setq b 2)\n(setq c 3)(setq d 4)"     t nil)
     ("((setq a 1)(setq b 2))\n((setq c 3)(setq d 4))" nil nil))))

(ert-deftest psearch-test-replace-at-point+splice ()
  (mapc
   (pcase-lambda (`(,expected ,splice-p ,pp-print-p))
     (psearch-test-with-buffer
      "(setq a 1 b 2)\n(setq c 3 d 4)"
      (let ((psearch-pp-print-p pp-print-p))
        (psearch-replace-at-point '`(setq . ,(and r (guard (> (length r) 2))))
                                  '(mapcar (lambda (pair)
                                             (cons 'setq pair))
                                    (seq-partition r 2))
                                  :splice splice-p)
        (should (= (point) (- (point-max) (length "\n(setq c 3 d 4)"))))
        (should (string= expected (substring-no-properties (buffer-string)))))))
   '(("(setq a 1)\n(setq b 2)\n(setq c 3 d 4)" t t)
     ("(setq a 1)(setq b 2)\n(setq c 3 d 4)"   t nil)
     ("((setq a 1)(setq b 2))\n(setq c 3 d 4)" nil nil))))

(ert-deftest psearch-test-function-patch ()
  (defun test-patch ()
    (list '(1 2 3)
          (if nil '(4 5 6))
          '(7 8 9)))
  (should (equal (test-patch) '((1 2 3) nil (7 8 9))))
  (psearch-with-function-patch test-patch
    (psearch-replace '`(if nil ,body)
                     '`(if t ,body)))
  (should (equal (test-patch) '((1 2 3) (4 5 6) (7 8 9)))))

(provide 'psearch-test)

;;; psearch-test.el ends here
