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

;; (setq ert-batch-backtrace-right-margin nil)
;; (setq ert-batch-print-level 20)
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

(ert-deftest psearch-test-replace-with-nil ()
  (psearch-test-with-buffer
   "\
(foo)
(foo a)
(foo a b)
(unless nil
  (foo a b (c 1 2))
  `(foo a b ,(c 1 2)))"
   (let ((psearch-pp-print-p nil))
     (psearch-replace '`(foo . ,rest) nil))
   (should
    (string= (buffer-substring-no-properties (point-min) (point-max))
             "\



(unless nil
\s\s
  `)"))
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

;;; psearch-patch

(ert-deftest psearch-test-ensure-symbol-function-and-library ()
  "Ensure symbol function/library the patch based on."
  (defun syml-lib-test-1 () "Docstring." 1 2 3)
  (should (equal (symbol-function #'syml-lib-test-1)
                 '(closure (t) nil "Docstring." 1 2 3)))

  (eval '(defun syml-lib-test-2 () "Docstring." 1 2 3))
  (should (equal (symbol-function #'syml-lib-test-2)
                 '(lambda nil "Docstring." 1 2 3)))

  (byte-compile (defun syml-lib-test-3 () "Docstring." 1 2 3))
  (should (byte-code-function-p
           (symbol-function #'syml-lib-test-3))))

(ert-deftest psearch-test-ensure-generic-symbol-function-and-library ()
  "Ensure generic symbol function/library the patch based on."
  (require 'cl-generic)

  ;; Define directly
  (cl-defgeneric sym-lib-test-1 (tag) "Interface." nil)
  (cl-defmethod sym-lib-test-1 ((tag (eql foo))) "Implement." 1 2 3)
  (cl-defmethod sym-lib-test-1 :before ((tag (eql foo))) "Before Implement." 4)
  (should
   (equal (mapcar #'psearch-test--cl-method-to-list (cl--generic-method-table (cl--generic #'sym-lib-test-1)))
          '((cl--generic-method ((eql foo)) (:before) (closure (t) (tag) "Before Implement." (progn 4)))
            (cl--generic-method ((eql foo)) nil (closure (t) (tag) "Implement." (progn (progn 1 2 3))))
            (cl--generic-method (t) nil (closure (t) (tag) (progn nil))))))

  ;; Eval
  (eval '(cl-defgeneric sym-lib-test-2 (tag) "Interface." nil))
  (eval '(cl-defmethod sym-lib-test-2 ((tag (eql foo))) "Implement." 1 2 3))
  (eval '(cl-defmethod sym-lib-test-2 :before ((tag (eql foo))) "Before Implement." 4))
  (should
   (equal (mapcar #'psearch-test--cl-method-to-list (cl--generic-method-table (cl--generic #'sym-lib-test-2)))
          '((cl--generic-method ((eql foo)) (:before) (lambda (tag) "Before Implement." (progn 4)))
            (cl--generic-method ((eql foo)) nil (lambda (tag) "Implement." (progn (progn 1 2 3))))
            (cl--generic-method (t) nil (lambda (tag) (progn nil))))))

  ;; Compiled
  (byte-compile (cl-defgeneric sym-lib-test-3 (tag) "Interface." nil))
  (byte-compile (cl-defmethod sym-lib-test-3 ((tag (eql foo))) "Implement." 1 2 3))
  (byte-compile (cl-defmethod sym-lib-test-3 :before ((tag (eql foo))) "Before implement." 4))
  (should
   (equal (mapcar #'psearch-test--cl-method-to-list (cl--generic-method-table (cl--generic #'sym-lib-test-3)))
          '((cl--generic-method ((eql foo)) (:before) (closure (t) (tag) "Before implement." (progn 4)))
            (cl--generic-method ((eql foo)) nil (closure (t) (tag) "Implement." (progn (progn 1 2 3))))
            (cl--generic-method (t) nil (closure (t) (tag) (progn nil)))))))

(defun psearch-test--find-patched-function (func-spec)
  (let* ((func-spec (if (symbolp func-spec) (list 'defun func-spec) func-spec))
         (def-type (pop func-spec))
         (function (pop func-spec)))
    (pcase-exhaustive def-type
      ('defun
          (let ((func-lib (find-function-library function 'lisp-only t)))
            (psearch-patch--symbol-function-def function)))
      ('cl-defgeneric
          (let ((func-lib (find-function-library function 'lisp-only t)))
            (when-let ((cl-method (psearch-patch--find-cl-generic-method
                                   function (list function nil t))))
              (pcase-let ((`(,extra ,qualifier ,specializer ,spec-rest)
                           (psearch-patch--cl-generic-args func-spec)))
                (psearch-patch--cl-generic-function-def
                 def-type function extra qualifier specializer cl-method)))))
      ('cl-defmethod
        (pcase-let* ((`(,extra ,qualifier ,specializer ,spec-rest)
                      (psearch-patch--cl-generic-args func-spec))
                     (met-name
                      (psearch-patch--met-name function qualifier specializer)))
          (let ((cl-method (psearch-patch--find-cl-generic-method
                            function met-name)))
            (when cl-method
              (psearch-patch--cl-generic-function-def
               def-type function extra qualifier specializer cl-method))))))))

(defun psearch-test--cl-method-to-list (cl-method)
  ;; Convert #s(cl--generic-method SPECIALIZERS QUALIFIERS FUNCTION)
  ;; To       '(cl--generic-method SPECIALIZERS QUALIFIERS FUNCTION)
  (list
   'cl--generic-method
   (cl--generic-method-specializers cl-method)
   (cl--generic-method-qualifiers cl-method)
   (cl--generic-method-function cl-method)))

(ert-deftest psearch-test-function-patch ()
  (defun func ()
    "Docstring."
    (list '(1 2 3)
          (if nil '(4 5 6))
          '(7 8 9)))

  (should
   (equal
    (psearch-patch--find-function 'func)
    '(defun func nil "Docstring." (list '(1 2 3) (if nil '(4 5 6)) '(7 8 9)))))
  (should (equal (func) '((1 2 3) nil (7 8 9))))

  (psearch-patch func
    (psearch-replace '`(if nil ,body)
                     '`(if t ,body)))
  (should
   (equal
    (psearch-test--find-patched-function 'func)
    '(defun func nil "[PATCHED]Docstring."
            (list '(1 2 3) (if t '(4 5 6)) '(7 8 9)))))
  (should (equal (func) '((1 2 3) (4 5 6) (7 8 9))))

  ;;;

  (require 'cl-lib)
  (cl-defun cl-func (arg &key foo bar)
    "Docstring."
    (list arg (if nil foo) bar))
  (should (equal (cl-func '(1 2 3) :foo '(4 5 6) :bar '(7 8 9))
                 '((1 2 3) nil (7 8 9))))
  (psearch-patch cl-func
    (psearch-replace '`(if nil ,body)
                     '`(if t ,body)))
  (should (equal (cl-func '(1 2 3) :foo '(4 5 6) :bar '(7 8 9))
                 '((1 2 3) (4 5 6) (7 8 9)))))

(ert-deftest psearch-test-cl-function-patch-1 ()
  (let ((generic-def '(cl-defgeneric cl-test-1 (tag) "Docstring." nil))
        (method-def '(cl-defmethod cl-test-1 ((tag (eql foo)))
                       "Docstring."
                       (list '(1 2 3)
                             (if nil '(4 5 6))
                             '(7 8 9))))
        (generic-assert '(cl-defgeneric cl-test-1 (tag)
                           "[PATCHED]Docstring." (progn "No implement.")))
        (method-assert '(cl-defmethod cl-test-1 ((tag (eql foo)))
                          "[PATCHED]Docstring."
                          (progn
                            (progn
                              (list '(1 2 3)
                                    (if t '(4 5 6))
                                    '(7 8 9)))))))
    (with-temp-buffer
      (insert "(require 'cl-generic)\n")
      (insert (format "%S\n" generic-def))
      (insert (format "%S\n" method-def))
      (emacs-lisp-mode)
      (eval-buffer))

    ;; Assert unpatched function
    (should (equal '(cl-defgeneric cl-test-1 (tag) "Docstring." (progn nil))
                   (psearch-test--find-patched-function
                    '(cl-defgeneric cl-test-1 (tag)))))
    (should (equal '(cl-defmethod cl-test-1 ((tag (eql foo)))
                      "Docstring."
                      (progn
                        (list '(1 2 3)
                              (if nil '(4 5 6))
                              '(7 8 9))))
                   (psearch-patch--find-function
                    '(cl-defmethod cl-test-1 ((tag (eql foo)))))))
    (should (equal (cl-test-1 'foo) '((1 2 3) nil (7 8 9))))
    (should (equal (cl-test-1 'bar) nil))

    ;; Patch functions
    (psearch-patch
        (cl-defmethod cl-test-1 ((tag (eql foo))))
      (psearch-replace '`(if nil ,body)
                       '`(if t ,body)))
    (psearch-patch
        (cl-defgeneric cl-test-1 (tag))
      (psearch-forward '`(tag))
      (forward-sexp 2)
      (let ((bound (bounds-of-thing-at-point 'sexp)))
        (delete-and-extract-region (car bound) (cdr bound))
        (insert (format "%S" "No implement."))))

    ;; Asset patch functions
    (should (equal generic-assert
                   (psearch-patch--find-function
                    '(cl-defgeneric cl-test-1 (tag)))))
    (should (equal method-assert
                   (psearch-test--find-patched-function
                    '(cl-defmethod cl-test-1 ((tag (eql foo)))))))
    (should (equal (cl-test-1 'foo) '((1 2 3) (4 5 6) (7 8 9))))
    (should (equal (cl-test-1 'bar) "No implement."))))

(ert-deftest psearch-test-cl-function-patch-2 ()
  (require 'cl-generic)

  (cl-defgeneric cl-test-2 (tag) "Docstring." nil)
  (cl-defmethod cl-test-2 ((tag (eql foo)))
    "Docstring."
    (list '(1 2 3)
          (if nil '(4 5 6))
          '(7 8 9)))

  (let ((orignal-generic '(cl-defgeneric cl-test-2 (tag)
                            "Docstring." (progn nil)))
        (orignal-method1 '(cl-defmethod cl-test-2 ((tag (eql foo)))
                            "Docstring."
                            (progn
                              (list '(1 2 3)
                                    (if nil '(4 5 6))
                                    '(7 8 9)))))
        (patched-generic '(cl-defgeneric cl-test-2 (tag)
                            "[PATCHED]Docstring." (progn "No implement.")))
        (patched-method1 '(cl-defmethod cl-test-2 ((tag (eql foo)))
                            "[PATCHED]Docstring."
                            (progn
                              (progn
                                (list '(1 2 3)
                                      (if t '(4 5 6))
                                      '(7 8 9)))))))

    ;; Assert unpatched function
    (should (equal orignal-generic
                   (psearch-patch--find-function
                    '(cl-defgeneric cl-test-2 (tag)))))
    (should (equal orignal-method1
                   (psearch-patch--find-function
                    '(cl-defmethod cl-test-2 ((tag (eql foo)))))))
    (should (equal (cl-test-2 'foo) '((1 2 3) nil (7 8 9))))
    (should (equal (cl-test-2 'bar) nil))

    ;; Patch functions
    (psearch-patch
        (cl-defmethod cl-test-2 ((tag (eql foo))))
      (psearch-replace '`(if nil ,body)
                       '`(if t ,body)))
    (psearch-patch
        (cl-defgeneric cl-test-2 (tag))
      (psearch-forward '`(tag))
      (forward-sexp 2)
      (let ((bound (bounds-of-thing-at-point 'sexp)))
        (delete-and-extract-region (car bound) (cdr bound))
        (insert (format "%S" "No implement."))))

    ;; Asset patch functions
    (should (equal patched-generic
                   (psearch-patch--find-function
                    '(cl-defgeneric cl-test-2 (tag)))))
    (should (equal patched-method1
                   (psearch-test--find-patched-function
                    '(cl-defmethod cl-test-2 ((tag (eql foo)))))))
    (should (equal (cl-test-2 'foo) '((1 2 3) (4 5 6) (7 8 9))))
    (should (equal (cl-test-2 'bar) "No implement."))))

(ert-deftest psearch-test-cl-function-patch-3 ()
  (let ((file (make-temp-file "test-psearch-cl-function-"))
        (generic-def '(cl-defgeneric cl-test-3 (tag) "Docstring." nil))
        (method-def '(cl-defmethod cl-test-3 ((tag (eql foo)))
                       "Docstring."
                       (list '(1 2 3)
                             (if nil '(4 5 6))
                             '(7 8 9))))
        (generic-assert '(cl-defgeneric cl-test-3 (tag)
                           "[PATCHED]Docstring." (progn "No implement.")))
        (method-assert '(cl-defmethod cl-test-3 ((tag (eql foo)))
                          "[PATCHED]Docstring."
                          (progn
                            (list '(1 2 3)
                                  (if t '(4 5 6))
                                  '(7 8 9))))))

    (with-temp-file file
      (insert "(require 'cl-generic)\n")
      (insert (format "%S\n" generic-def))
      (insert (format "%S\n" method-def)))
    (byte-compile-file file)
    (load-file file)

    ;; Assert unpatched function
    (should (equal generic-def
                   (psearch-patch--find-function
                    '(cl-defgeneric cl-test-3 (tag)))))
    (should (equal method-def
                   (psearch-patch--find-function
                    '(cl-defmethod cl-test-3 ((tag (eql foo)))))))
    (should (equal (cl-test-3 'foo) '((1 2 3) nil (7 8 9))))
    (should (equal (cl-test-3 'bar) nil))

    ;; Patch functions
    (psearch-patch
     (cl-defmethod cl-test-3 ((tag (eql foo))))
     (psearch-replace '`(if nil ,body)
                      '`(if t ,body)))
    (psearch-patch
     (cl-defgeneric cl-test-3 (tag))
     (psearch-forward '`(tag))
     (forward-sexp 2)
     (let ((bound (bounds-of-thing-at-point 'sexp)))
       (delete-and-extract-region (car bound) (cdr bound))
       (insert (format "%S" "No implement."))))

    ;; Asset patch functions
    (should (equal generic-assert
                   (psearch-patch--find-function
                    '(cl-defgeneric cl-test-3 (tag)))))
    (should (equal method-assert
                   (psearch-test--find-patched-function
                    '(cl-defmethod cl-test-3 ((tag (eql foo)))))))
    (should (equal (cl-test-3 'foo) '((1 2 3) (4 5 6) (7 8 9))))
    (should (equal (cl-test-3 'bar) "No implement."))))

(provide 'psearch-test)

;;; psearch-test.el ends here
