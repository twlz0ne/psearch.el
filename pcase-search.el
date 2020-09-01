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

(require 'cl-lib)
(require 'pcase)
(require 'subr-x)
(require 'thingatpt)

(defcustom pcase-search-pp-print-p t
  "Control whether the results of ‘pcase-search--print-to-string’ are pretty-printed."
  :group 'pcase-search
  :type 'boolean)

(defun pcase-search-make-matcher (match-pattern &optional result-pattern)
  "Create a function to match the input sexp.

MATCH-PATTERN   to match the input sexp
RESULT-PATTERN  to generate the result (default t)

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
       (,match-pattern ,(or result-pattern t)))))

(defun pcase-search--prin1 (object &optional stream)
  (if (consp object)
      (progn
        (let* ((car (pop object))
               (quote-p (memq car '(\, quote function \` \,@ \,.))))
          (unless quote-p 
            (princ "(" stream))
          (if (consp car)
              (if (memq (car car) '(\,))
                  (progn
                    (princ (car car) stream)
                    (princ (cadr car) stream)
                    (when object
                      (princ " " stream)))
                (pcase-search--prin1 car stream))
            (princ (cond
                    ((eq car 'quote) '\')
                    ((eq car 'function) "#'")
                    (t car))
                   stream)
            (when (and object (not quote-p))
              (princ " " stream)))
          (when object
            (while (setq car (pop object))
              (pcase-search--prin1 car stream)
              (when object
                (princ " " stream))))
          (unless quote-p
            (princ ")" stream))))
    (princ object stream)))

(defsubst pcase-search--print-to-string (expr)
  "Return a string containing the printed representation of EXPR.
If ‘pcase-search-pp-print-p’ is not nil, return pretty-printted string."
  (with-temp-buffer
    (pcase-search--prin1 expr (current-buffer))
    (when pcase-search-pp-print-p
      (emacs-lisp-mode)
      (pp-buffer))
    (string-trim-right (buffer-string))))

(defsubst pcase-search--apply-replacement-at-point (matcher &optional callback)
  "Apply replacement at point.

MATCHER  validate the match pattern and return replacement
CALLBACK can be nil, t or a callback:
         - t         apply
         - nil       don't apply
         - callback  apply in callback. The callback accept replacemanet and
                     bounds, return t if success"
  (let* ((bounds (let ((bounds (bounds-of-thing-at-point 'sexp)))
                   ;; (bounds-of-thing-at-point)
                   ;; => actual    [`(,foo)]
                   ;;    expected  `[(,foo)]
                   (cons (point) (cdr bounds))))
         (sexp (save-restriction
                 (narrow-to-region (point) (cdr bounds))
                 (sexp-at-point)))
         (rep (funcall matcher sexp)))
    (when (and bounds sexp rep)
      (if callback
          (if (functionp callback)
              (funcall callback rep bounds)
            (save-excursion
              (delete-region (car bounds) (cdr bounds))
              (insert (pcase-search--print-to-string rep))
              t))
        t))))

(defun pcase-search--beginning-of-prev-sexp-1 ()
  "Jump to the beginning of prev sexp."
  (let (new-point)
    (if (catch 'break
          (while (let ((ret (looking-back "[])]" 1)))
                   (unless ret (throw 'break new-point))
                   ret)
            (down-list -1)
            (setq new-point (point))))
        (condition-case _err
            (backward-sexp)
          (scan-error))
      (condition-case _err 
          (progn
            (backward-sexp)
            (when (pcase-search-point-at-list-p)
              (forward-sexp)
              (down-list -1)
              (pcase-search--beginning-of-prev-sexp-1)))
        (scan-error
         (backward-char))))
    (point)))

(defun pcase-search--beginning-of-prev-sexp (&optional arg)
  "Jump to the beginning of prev ARG sexp and return the new point.

Unlike normal backward behaviour, it will try to find every sub element, for example:

     (sexp1)  (sepx2) |;; init point
     (sexp1)  (|sexp2) ;; after 1st execution
     (sexp1) |(sexp2)  ;; after 2nd execution
     (|sexp1) (sexp2)  ;; after 3rd execution
    |(sexp1)  (sexp2)  ;; after 4th execution"
  (let ((old-point (point))
        (new-point (save-excursion
                     (dotimes (_ (or arg 1))
                       (pcase-search--beginning-of-prev-sexp-1))
                     (point))))
    (when (< new-point old-point)
      (goto-char new-point))))

(defun pcase-search--beginning-of-next-sexp-1 ()
  "Jump to the beginning of next regexp."
  (let ((point-at-sexp-p (looking-at-p "[^\s\t\r\n]")))
    (if (pcase-search-point-at-list-p)
        (forward-char)
      (condition-case _err
          (progn
            (forward-sexp (1+ (if point-at-sexp-p 1 0)))
            (backward-sexp))
        (scan-error
         (up-list)
         (unless (pcase-search-point-at-list-p)
           (pcase-search--beginning-of-next-sexp-1)))))
    (point)))

(defun pcase-search--beginning-of-next-sexp (&optional arg)
  "Jump to the beginning of next ARG sexp and return the new point.

Unlike normal forward behaviour, if the point not at the beginning of a sexp,
it will find the nearest sexp rather than jumping to the next, for example:

   | (sexp1)  (sepx2)  ;; init point
    |(sexp1)  (sexp2)  ;; after 1st execution
     (|sexp1) (sexp2)  ;; after 2nd execution
     (sexp1) |(sexp2)  ;; after 3rd execution
     (sexp1)  (|sexp2) ;; after 4th execution"
  (let ((old-point (point))
        (new-point (save-excursion
                     (dotimes (_ (or arg 1))
                       (pcase-search--beginning-of-next-sexp-1))
                     (point))))
    (when (> new-point old-point)
      (goto-char new-point))))

(defun pcase-search-point-at-list-p ()
  (looking-at-p "[`',@(\\[]"))

(defun pcase-search-backward-1 (matcher &optional result-callback)
  "Move backward across one sexp."
  (let (pos)
    (when (save-excursion
            (catch 'break
              (while (setq pos (pcase-search--beginning-of-prev-sexp))
                (when (pcase-search--apply-replacement-at-point
                       matcher
                       result-callback)
                  (throw 'break t)))))
      (when pos
        (goto-char pos))
      (point))))

(defun pcase-search-backward (pattern &optional result-pattern result-callback)
  (pcase-search-backward-1 (pcase-search-make-matcher pattern result-pattern)
                           result-callback))

(defun pcase-search-forward-1 (matcher &optional result-callback)
  (let (pos)
    (when (or
           ;; - point at sexp
           (when (pcase-search-point-at-list-p)
             (pcase-search--apply-replacement-at-point matcher
                                                       result-callback))
           ;; - rest
           (save-excursion
             (catch 'break
               (while (setq pos (pcase-search--beginning-of-next-sexp))
                 (when (pcase-search--apply-replacement-at-point
                        matcher
                        result-callback)
                   (throw 'break t))))))
      (when pos
        (goto-char pos))
      (thing-at-point--end-of-sexp)
      (point))))

(defun pcase-search-forward (pattern &optional result-pattern result-callback)
  (pcase-search-forward-1 (pcase-search-make-matcher pattern result-pattern)
                          result-callback))

;;; minibuffer history

(defvar pcase-search-replace-separator " → ")

(defvar pcase-search-replace-history nil)

(defvar pcase-search-replace-history-variable 'pcase-search-replace-history)

(defvar pcase-search-replace-defaults nil)

(defun pcase-search-replace-args ()
  (let* ((minibuffer-history
          (append
           (mapcar (pcase-lambda (`(,from . ,to))
                     (concat from pcase-search-replace-separator to))
                   pcase-search-replace-defaults)
           (symbol-value pcase-search-replace-history-variable)))
         (default (car pcase-search-replace-defaults))
         (from (read-from-minibuffer
                (format "Query replace %s: "
                        (if default
                            (list "default"
                                  (concat (car default)
                                          pcase-search-replace-separator
                                          (cdr default)))
                          (list "e.g." (concat "`(foo . ,rest"
                                               pcase-search-replace-separator
                                               "`(bar ,@rest)"))))
                nil nil nil nil
                (car search-ring) t))
         (default (if (string-empty-p from)
                      default
                    (let ((arr (split-string from pcase-search-replace-separator)))
                      (cons (car arr) (cadr arr)))))
         (to (or (cdr default)
                 (let ((minibuffer-history
                        (symbol-value pcase-search-replace-history-variable)))
                   (read-from-minibuffer
                    (format "Query replace %s with: " (car default))
                    nil nil nil nil
                    pcase-search-replace-history-variable t)))))
    (let ((from (car default)))
      (add-to-history pcase-search-replace-history-variable to)
      (add-to-history pcase-search-replace-history-variable from)
      (unless (or (string-empty-p from) (string-empty-p to))
        (let ((default (assoc from pcase-search-replace-defaults)))
          (unless (and (eq from (car default)) (eq to (cdr default)))
            (add-to-list 'pcase-search-replace-defaults (cons from to))))
        (list (read from) (read to))))))

;;;###autoload
(defun pcase-search-replace (match-pattern replace-pattern)
  "Replace some matches of pattern MATCH-PATTERN with pattern REPLACE-PATTERN.

MATCH-PATTERN is a pcase pattern to match. REPLACE-PATTERN is an Elisp
expression that is evaluated repeatedly for each match with bindings created
in MATCH-PATTERN.

Example:

    ```
    (pcase-search-replace '`(foo . ,rest)
                          '`(bar ,@rest))
    ;; (foo a b ...) -> (bar a b ...)
    ```"
  (interactive (pcase-search-replace-args))
  (let* ((matcher
          (pcase-search-make-matcher match-pattern replace-pattern))
         (points
          (save-excursion
            (cl-loop with pos
                     while (setq pos (pcase-search-forward-1 matcher t))
                     collect pos))))
    (when points
      (goto-char (car (last points)))
      (if (called-interactively-p 'any)
          (message "Replaced %s occurrences" (length points))
        (point)))))

(provide 'pcase-search)

;;; pcase-search.el ends here
