;;; psearch.el --- Pcase based search for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2020/08/29
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/twlz0ne/psearch.el
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

;; Pcase based search for Emacs Lisp.  Not as powerful as
;; [el-search](https://elpa.gnu.org/packages/el-search.html), but easier to use.

;; See README.md for more information.

;;; Change Log:

;;  0.1.0  2020/08/29  Initial version.

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'subr-x)
(require 'thingatpt)

(defcustom psearch-pp-print-p t
  "Control whether the results of ‘psearch--print-to-string’ are pretty-printed."
  :group 'psearch
  :type 'boolean)

(defun psearch-point-at-list-p ()
  "Determine if point at the beginning of list."
  (looking-at-p "[`',@(\\[]"))

(defun psearch--prin1 (object &optional stream)
  "Print OBJECT on STREAM according to its type."
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
                (psearch--prin1 car stream))
            (princ (cond
                    ((eq car 'quote) '\')
                    ((eq car 'function) "#'")
                    (t car))
                   stream)
            (when (and object (not quote-p))
              (princ " " stream)))
          (when object
            (while (setq car (pop object))
              (psearch--prin1 car stream)
              (when object
                (princ " " stream))))
          (unless quote-p
            (princ ")" stream))))
    (princ object stream)))

(defsubst psearch--print-to-string (expr)
  "Return a string containing the printed representation of EXPR.
If ‘psearch-pp-print-p’ is not nil, return pretty-printted string."
  (with-temp-buffer
    (psearch--prin1 expr (current-buffer))
    (when psearch-pp-print-p
      (emacs-lisp-mode)
      (pp-buffer))
    (string-trim-right (buffer-string))))

(defun psearch-make-matcher (match-pattern &optional result-pattern)
  "Create a function to match the input sexp.

MATCH-PATTERN   to match the input sexp
RESULT-PATTERN  to generate the result (default t)

Example:

    ```
    (let ((matcher (psearch-make-matcher '`(foo ,val) '`(bar ,val))))
      (funcall matcher '(foo 1)))
    ;; => (bar 1)

    (let ((matcher (psearch-make-matcher '`(foo ,val))))
      (funcall matcher '(foo 1)))
    ;; t
    ```"
  `(lambda (sexp)
     (pcase sexp
       (,match-pattern ,(or result-pattern t)))))

(defsubst psearch--apply-replacement-at-point (matcher &optional callback)
  "Apply replacement at point.

MATCHER  validate the match pattern and return replacement
CALLBACK can be nil, t or a callback:
         - t         apply
         - nil       don't apply
         - callback  apply in callback.  The callback accept replacemanet and
                     bounds, return t if success"
  (let* ((bounds (let ((bounds (bounds-of-thing-at-point 'sexp)))
                   ;; (bounds-of-thing-at-point)
                   ;; => actual    [`(,foo)]
                   ;;    expected  `[(,foo)]
                   (when bounds (cons (point) (cdr bounds)))))
         (sexp (when bounds
                 (save-restriction
                   (narrow-to-region (point) (cdr bounds))
                   (sexp-at-point))))
         (rep (when sexp (funcall matcher sexp))))
    (when rep
      (if callback
          (if (functionp callback)
              (funcall callback rep bounds)
            (save-excursion
              (delete-region (car bounds) (cdr bounds))
              (insert (psearch--print-to-string rep))
              t))
        t))))

(defun psearch--beginning-of-prev-sexp-1 ()
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
            (when (psearch-point-at-list-p)
              (forward-sexp)
              (down-list -1)
              (psearch--beginning-of-prev-sexp-1)))
        (scan-error
         (backward-char))))
    (point)))

(defun psearch--beginning-of-prev-sexp (&optional arg)
  "Jump to the beginning of prev ARG sexp and return the new point.

Unlike normal backward behaviour, it will try to find every sub element, for
example:

     (sexp1)  (sepx2) |;; init point
     (sexp1)  (|sexp2) ;; after 1st execution
     (sexp1) |(sexp2)  ;; after 2nd execution
     (|sexp1) (sexp2)  ;; after 3rd execution
    |(sexp1)  (sexp2)  ;; after 4th execution"
  (let ((old-point (point))
        (new-point (save-excursion
                     (dotimes (_ (or arg 1))
                       (psearch--beginning-of-prev-sexp-1))
                     (point))))
    (when (< new-point old-point)
      (goto-char new-point))))

(defun psearch--beginning-of-next-sexp-1 ()
  "Jump to the beginning of next regexp."
  (let ((point-at-sexp-p (looking-at-p "[^\s\t\r\n]")))
    (if (psearch-point-at-list-p)
        (forward-char)
      (condition-case _err
          (progn
            (forward-sexp (1+ (if point-at-sexp-p 1 0)))
            (backward-sexp))
        (scan-error
         (up-list)
         (unless (psearch-point-at-list-p)
           (psearch--beginning-of-next-sexp-1)))))
    (point)))

(defun psearch--beginning-of-next-sexp (&optional arg)
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
                       (psearch--beginning-of-next-sexp-1))
                     (point))))
    (when (> new-point old-point)
      (goto-char new-point))))

(defun psearch-backward-1 (matcher &optional result-callback)
  "Move backward across one sexp matched by PATTERN.
Set point to the beginning of the occurrence found, and return point.

MATCHER          a function generated by ‘psearch-make-matcher’
RESULT-CALLBACK  a function to handle the result, see ‘psearch-forward’ for more"
  (let (pos)
    (when (save-excursion
            (catch 'break
              (while (setq pos (psearch--beginning-of-prev-sexp))
                (when (psearch--apply-replacement-at-point
                       matcher
                       result-callback)
                  (throw 'break t)))))
      (when pos
        (goto-char pos))
      (point))))

(defun psearch-forward-1 (matcher &optional result-callback)
  "Move forward across one sexp matched by PATTERN.
Set point to the end of the occurrence found, and return point.

MATCHER          a function generated by ‘psearch-make-matcher’
RESULT-CALLBACK  a function to handle the result, see ‘psearch-forward’ for more"
  (let (pos)
    (when (or
           ;; - point at sexp
           (when (psearch-point-at-list-p)
             (psearch--apply-replacement-at-point matcher
                                                       result-callback))
           ;; - rest
           (save-excursion
             (catch 'break
               (while (setq pos (psearch--beginning-of-next-sexp))
                 (when (psearch--apply-replacement-at-point
                        matcher
                        result-callback)
                   (throw 'break t))))))
      (when pos
        (goto-char pos))
      (thing-at-point--end-of-sexp)
      (point))))

;;; minibuffer history

(defcustom psearch-replace-separator " → "
  "String that separates FROM and TO in the history of replacement pairs."
  :group 'psearch
  :type '(choice
          (const :tag "Disabled" nil)
          string))

(defvar psearch-replace-history nil
  "Default history list for ‘psearch-replace’ commands.")

(defcustom psearch-replace-history-variable 'psearch-replace-history
  "History list to use for the TO argument of ‘psearch-replace’ commands.
The value of this variable should be a symbol; that symbol
is used as a variable to hold a history list for replacement
strings or patterns."
  :group 'psearch
  :type 'symbol)

(defvar psearch-replace-defaults nil
  "Default values of FROM-STRING and TO-STRING for ‘psearch-replace’.
This is a list of cons cells (FROM-STRING . TO-STRING), or nil
if there are no default values.")

(defun psearch-replace-args (&optional prompt-prefix)
  "Read the match pattern and the replace pattern.
If PROMPT-PREFIX is nil, use \"Query replace\" as default prompt prefix."
  (let* ((separator
          (propertize psearch-replace-separator
                      'display psearch-replace-separator
                      'face 'minibuffer-prompt
                      'separator t))
         (minibuffer-history
          (append
           (mapcar (pcase-lambda (`(,from . ,to))
                     (concat from separator to))
                   psearch-replace-defaults)
           (symbol-value psearch-replace-history-variable)))
         (default (car psearch-replace-defaults))
         (from (read-from-minibuffer
                (format "%s %s: "
                        (or prompt-prefix "Query replace")
                        (if default
                            (list "default"
                                  (concat (car default)
                                          separator
                                          (cdr default)))
                          (list "e.g." (concat "`(foo . ,rest"
                                               separator
                                               "`(bar ,@rest)"))))
                nil nil nil nil
                (car search-ring) t))
         (default (if (string-empty-p from)
                      default
                    (let ((arr (split-string from separator)))
                      (cons (car arr) (cadr arr)))))
         (to (or (cdr default)
                 (let ((minibuffer-history
                        (symbol-value psearch-replace-history-variable)))
                   (read-from-minibuffer
                    (format "Query replace %s with: " (car default))
                    nil nil nil nil
                    psearch-replace-history-variable t)))))
    (let ((from (car default)))
      (add-to-history psearch-replace-history-variable to)
      (add-to-history psearch-replace-history-variable from)
      (unless (or (string-empty-p from) (string-empty-p to))
        (let ((default (assoc from psearch-replace-defaults)))
          (unless (and (eq from (car default)) (eq to (cdr default)))
            (add-to-list 'psearch-replace-defaults (cons from to))))
        (list (read from) (read to))))))

;;;###autoload
(defun psearch-backward (pattern &optional result-pattern result-callback)
  "Move backward across one sexp matched by PATTERN.
Set point to the beginning of the occurrence found, and return point.

PATTERN          pattern to match sexp
RESULT-PATTERN   pattern to generate result
RESULT-CALLBACK  a function to handle the result, it accpet two arguments:
                 - result  the result generated by RESULT-PATTERN
                 - bounds  the bounds of the matched sexp"
  (psearch-backward-1 (psearch-make-matcher pattern result-pattern)
                           result-callback))

;;;###autoload
(defun psearch-forward (pattern &optional result-pattern result-callback)
  "Move forward across one sexp matched by PATTERN.
Set point to the end of the occurrence found, and return point.

PATTERN          pattern to match sexp
RESULT-PATTERN   pattern to generate result
RESULT-CALLBACK  a function to handle the result, it accpet two arguments:
                 - result  the result generated by RESULT-PATTERN
                 - bounds  the bounds of the matched sexp"
  (psearch-forward-1 (psearch-make-matcher pattern result-pattern)
                          result-callback))

;;;###autoload
(defun psearch-replace (match-pattern replace-pattern)
  "Replace some occurences mathcing MATCH-PATTERN with REPLACE-PATTERN.

MATCH-PATTERN is a pcase pattern to match.  REPLACE-PATTERN is an Elisp
expression that is evaluated repeatedly for each match with bindings created
in MATCH-PATTERN.

Example:

    ```
    (psearch-replace '`(foo . ,rest)
                     '`(bar ,@rest))
    ;; (foo a b ...) -> (bar a b ...)
    ```"
  (interactive (psearch-replace-args))
  (let* ((matcher
          (psearch-make-matcher match-pattern replace-pattern))
         (points
          (save-excursion
            (cl-loop with pos
                     while (setq pos (psearch-forward-1 matcher t))
                     collect pos))))
    (when points
      (goto-char (car (last points)))
      (if (called-interactively-p 'any)
          (message "Replaced %s occurrences" (length points))
        (point)))))

(defun psearch-replace-at-point (match-pattern replace-pattern)
  "Replace the sexp matching MATCH-PATTERN at point with REPLACE-PATTERN.

Replace only the sexp at point, that is, the point is at the beginning of it or
inside it.  For example:

        |(match a (b c))    =>     |(replace a (b c))
         (match a (b|c))    =>     |(replace a (b c))"
  (interactive (psearch-replace-args "Query replace at point"))
  (let ((matcher (psearch-make-matcher match-pattern replace-pattern))
        (pos (point)))
    (when (or (psearch--apply-replacement-at-point matcher t)
              (psearch-backward-1 matcher
                                  (lambda (replacement bounds)
                                    (when (< (car bounds) pos (cdr bounds))
                                      (delete-region (car bounds) (cdr bounds))
                                      (insert (psearch--print-to-string
                                               replacement))
                                      t))))
      (if (called-interactively-p 'any)
          (message "Replaced")
        (point)))))

(provide 'psearch)

;;; psearch.el ends here
