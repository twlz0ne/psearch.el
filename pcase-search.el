;;; pcase-search.el --- Pcase-based search API -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2020/08/29
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/twlz0ne/pcase-search
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Pcase-based search API

;;; Change Log:

;;  0.1.0  2020/08/29  Initial version.

;;; Code:

(require 'subr-x)
(require 'pcase)

(defun pcase-search-make-matcher (match-pattern &optional output-pattern)
  "Create a function to match the input sexp.

MATCH-PATTERN   to match the input sexp
OUTPUT-PATTERN  to generate the output (default t)

Example:

    ```
    (let ((matcher (pcase-search-make-matcher '`(foo ,val) '`(bar ,val))))
      (funcall matcher '(foo 1)))
    ;; => (bar 1)

    (let ((matcher (pcase-search-make-matcher '`(foo ,val))))
      (funcall matcher '(foo 1)))
    ;; t
    ```"
  `(lambda (sexp)
     (pcase sexp
       (,match-pattern ,(or output-pattern t)))))

(defsubst pcase-search--apply-replacement-at-point (matcher)
  "Apply replacement at point if MATCHER returned no-nil."
  (when-let* ((sexp (let ((bounds (bounds-of-thing-at-point 'sexp)))
                      (save-restriction
                        (narrow-to-region (point) (cdr bounds))
                        (sexp-at-point))))
              (rep (funcall matcher sexp)))
    (mark-sexp)
    (delete-region (region-beginning) (region-end))
    (insert (format "%S" rep))))

(defun pcase-search--beginning-of-next-sexp (&optional arg)
  "Jump to the beginning of next ARG sexp and return the new point.

Unlike normal forward behaviour, if the point not at the beginning of a sexp,
it will find the nearest sexp rather than jumping to the next, for example:

   | (sexp1)  (sepx2)  ;; init point
    |(sexp1)  (sexp2)  ;; after 1st execution
     (|sexp1) (sexp2)  ;; after 2nd execution
     (sexp1) |(sexp2)  ;; after 3rd execution
     (sexp1)  (|sexp2) ;; after 4th execution"
  (let ((point-at-sexp-p (looking-at-p "[^\s\t\r\n]"))
        (old-point (point))
        new-point)
    (save-excursion
      (if (pcase-search-point-at-list-p)
          (forward-char)
        (condition-case err
            (progn
              (forward-sexp (+ (or arg 1) (if point-at-sexp-p 1 0)))
              (backward-sexp))
          (scan-error
           (up-list)
           (unless (pcase-search-point-at-list-p)
             (pcase-search--beginning-of-next-sexp)))))
      (setq new-point (point)))
    (when (> new-point old-point)
      (goto-char new-point))))

(defun pcase-search-point-at-list-p ()
  (looking-at-p "[[`',@(\\[]"))

(defun pcase-search-forward-1 (matcher)
  (let (pos)
    (when (save-excursion
            (catch 'break
              (while (setq pos (pcase-search--beginning-of-next-sexp))
                (let ((sexp
                       (let ((bounds (bounds-of-thing-at-point 'sexp)))
                         (save-restriction
                           (narrow-to-region (point) (cdr bounds))
                           (sexp-at-point)))))
                  (when (funcall matcher sexp)
                    (throw 'break t))))))
      (goto-char pos))))

(defun pcase-search-forward (pattern)
  (pcase-search-forward-1 (pcase-search-make-matcher pattern)))

(defun pcase-search-replace (match-pattern replace-pattern)
  "Replace some matches of pattern MATCH-PATTERN with pattern REPLACE-PATTERN.

MATCH-PATTERN is an pcase pattern to match. REPLACE-PATTERN is an Elisp
expression that is evaluated repeatedly for each match with bindings created
in MATCH-PATTERN.

Example:

    ```
    (pcase-search-replace '`(foo . ,rest)
                          '`(bar ,@rest))
    ;; (foo a b ...) -> (bar a b ...)
    ```"
  (let ((matcher (pcase-search-make-matcher match-pattern replace-pattern))
        pos)
    (when (pcase-search-point-at-list-p)
      (pcase-search--apply-replacement-at-point matcher))
    (save-excursion
      (while (setq pos (pcase-search--beginning-of-next-sexp))
        (pcase-search--apply-replacement-at-point matcher)))
    (when pos
      (goto-char pos))))

(provide 'pcase-search)

;;; pcase-search.el ends here
