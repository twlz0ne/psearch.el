;;; pcase-search-test.el --- Test pcase-search -*- lexical-binding: t; -*-

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
(require 'pcase-search)

(when noninteractive
  (transient-mark-mode))


;;; utils

(defmacro pcase-search-test-with-buffer (init &rest body)
  `(with-temp-buffer
     (emacs-lisp-mode)
     (insert ,init)
     (goto-char (point-min))
     ,@body))


;;; tests

(ert-deftest pcase-search-test-point-at-list-p ()
  (pcase-search-test-with-buffer
   "`',@(["
   (let ((string (buffer-substring-no-properties (point-min) (point-max)))
         chars)
     (while (pcase-search-point-at-list-p)
       (push (char-after (point)) chars)
       (forward-char))
     (should
      (string= string (mapconcat #'char-to-string (reverse chars) ""))))))

(ert-deftest pcase-search-test-matcher ()
  (let ((matcher (pcase-search-make-matcher '`(foo . ,_))))
    (should (funcall matcher '(foo)))
    (should (funcall matcher '(foo a)))
    (should (funcall matcher '(foo a b)))
    (should (funcall matcher '(foo a b ,(c 1 2))))))

(ert-deftest pcase-search-test-forward ()
  (pcase-search-test-with-buffer
   "\
(foo)
(foo a)
(foo a b)
(unless nil
  `(foo a b ,(c 1 2)))"
   (let ((pattern '`(foo a b . ,_)))
     (pcase-search-forward pattern)
     (should (equal (sexp-at-point) '(foo a b)))
     (pcase-search-forward pattern)
     (should (equal (sexp-at-point) '`(foo a b ,(c 1 2)))))))

(ert-deftest pcase-search-test-replace ()
  (pcase-search-test-with-buffer
   "\
(foo)
(foo a)
(foo a b)
(unless nil
  (foo a b (c 1 2))
  `(foo a b ,(c 1 2)))"
   (let ((pcase-search-pp-print-p nil))
     (pcase-search-replace '`(foo . ,rest) '`(bar ,@rest)))
   (should
    (string= (buffer-substring-no-properties (point-min) (point-max))
             "\
(bar)
(bar a)
(bar a b)
(unless nil
  (bar a b (c 1 2))
  `(bar a b ,(c 1 2)))"))))

(provide 'pcase-search-test)

;;; pcase-search-test.el ends here
