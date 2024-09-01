;;; psearch.el --- Pcase based search for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2020/08/29
;; Version: 0.2.3
;; Last-Updated: 2024-08-03 00:32:13 +0800
;;           By: Gong Qijian
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

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'subr-x)
(require 'thingatpt)
(require 'cl-generic)
(require 'find-func)

(define-error 'psearch-error "Psearch error")
(define-error 'psearch-patch-failed "Function patch applied failed" 'psearch-error)

(defcustom psearch-pp-print-p t
  "Control whether the results of ‘psearch--print-to-string’ are pretty-printed."
  :group 'psearch
  :type 'boolean)

(defun psearch-point-at-list-p ()
  "Determine if point at the beginning of list."
  (or (looking-at-p "[(\\[]")
      (looking-at-p "['`,@]*(")))

(defcustom psearch-delete-collect-replace-region-p nil
  "Wheter to delete whole region when search replace with collect pattern.

nil     delete bounds of matched only
t       delete whole search region

Example:

        ;; # delete matched bounds
        [ (matched1) (others)... (matched2) ] => [ (others)... (replacement) ]

        ;; # delete whole region
        [ (matched1) (others)... (matched2) ] => [ (replacement) ]"
  :group 'psearch
  :type 'boolean)

(defvar psearch-patch-function-regexp
  ;; (xr "\\`(defun[[:space:]]+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)")
  (rx (seq bos
           "(" (or "defun" "cl-defgeneric" "cl-defmethod")
           (1+ space)
           (group (1+ (or (syntax word)
                          (syntax symbol)
                          (seq "\\" nonl))))))
  "Regex to match function definition.")

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
            (if (stringp car)
                (prin1 car stream)
              (princ (cond
                      ((eq car 'quote) '\')
                      ((eq car 'function) "#'")
                      (t car))
                     stream))
            (when (and object (not quote-p))
              (princ " " stream)))
          (while object
            (setq car (pop object))
            (psearch--prin1 car stream)
            (when object
              (princ " " stream)))
          (unless quote-p
            (princ ")" stream))))
    (if (stringp object)
        (prin1 object stream)
      (princ object stream))))

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
                     bounds, return non-nil if success"
  (cl-assert (functionp matcher) t "MATCHER must be a function.  Given: %S" matcher)
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

(defsubst psearch--pp-region1 (beg end)
  "Prettify region from BEG to END."
  (interactive "r")
  (let ((newline-p (eq (char-before end) ?\n)))
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (pp-buffer)
        (when (and (not newline-p) (eq (char-before (point-max)) ?\n))
          (delete-region (1- (point-max)) (point-max)))))))

(defsubst psearch--splice-and-insert (object)
  "Splice OBJECT and insert to current point."
  (let ((pp-start-pos (point)))
    (mapc (lambda (it)
            (insert (psearch--print-to-string it)))
          object)
    (when psearch-pp-print-p
      (psearch--pp-region1 pp-start-pos (point)))))

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
Set point to the end of the occurrence found (unless RESULT-CALLBACK returns
the ‘nonmoving’ directive), and return point.

MATCHER          a function generated by ‘psearch-make-matcher’
RESULT-CALLBACK  a function to handle the result, return non-nil if success,
                 see ‘psearch-forward’ for the rest information. "
  (let (pos
        replace-result)
    (when (or
           ;; - point at sexp
           (when (psearch-point-at-list-p)
             (setq replace-result
                   (psearch--apply-replacement-at-point matcher
                                                        result-callback)))
           ;; - rest
           (save-excursion
             (catch 'break
               (while (setq pos (psearch--beginning-of-next-sexp))
                 (when (setq replace-result
                             (psearch--apply-replacement-at-point
                              matcher
                              result-callback))
                   (throw 'break t))))))
      (when pos
        (goto-char pos))
      (when (and replace-result (not (eq replace-result 'nonmoving)))
        (thing-at-point--end-of-sexp))
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

(defvar psearch-count-current nil
  "The current number of psearch matches (start from 1).
It's only available in result-callback of `psearch-forward'/`psearch-backward'.")

(defvar psearch-patch-function-definition-docpos nil
  "Specifying the docstring pos in function definition.

Some macro-generated functions have different struct with `defun', e.g.:

  (psearch-patch--find-function 'doom-modeline-segment--foobar)
  ;; => (doom-modeline-def-segment foobar \"Docstring\" body...)
  ;;                    |             |         |        |
  ;;                    |             |         |        `--- [3]body
  ;;                    |             |         `------------ [2]docstring
  ;;                    |             `---------------------- [1]name
  ;;                    `------------------------------------ [0]declare

  (psearch-patch--find-function 'foobar)
  ;; => (defun foobar nil \"Docstring\" body...)
  ;;       |     |     |       |        |
  ;;       |     |     |       |        `--- [4]body
  ;;       |     |     |       `------------ [3]docstring
  ;;       |     |     `-------------------- [2]arguments
  ;;       |     `-------------------------- [1]name
  ;;       `-------------------------------- [0]declare

Use a variable with `let' to tell the patch function how to locate the element
in function definition:

  (let ((psearch-patch-function-definition-docpos 2))
    (psearch-patch doom-modeline-segment--matches
      (psearch-replace '`match-pattern
                       '`replacement)))")

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
(defun psearch-backward (pattern &optional result-pattern result-callback count)
  "Move backward across one sexp matched by PATTERN.
Set point to the beginning of the occurrence found, and return point.

PATTERN          pattern to match sexp.
RESULT-PATTERN   pattern to generate result.
RESULT-CALLBACK  a function to handle the result, return non-nil if success.
                 The function accpet two arguments:
                 - result  the result generated by RESULT-PATTERN.
                 - bounds  the bounds of the matched sexp.
COUNT            a number (default 1) that indicates the number of occurrences
                 to search for.  The current number will be stored in
                 `psearch-count-current' which avariable in RESULT-CALLBACK."
  (let (point last-point)
    (catch 'break
      (dotimes (i (or count 1))
        (let ((psearch-count-current (1+ i)))
          (if (setq point (psearch-backward-1
                           (psearch-make-matcher pattern result-pattern)
                           result-callback))
              (setq last-point point)
            (throw 'break nil)))))
    last-point))

;;;###autoload
(defun psearch-forward (pattern &optional result-pattern result-callback count)
  "Move forward across one sexp matched by PATTERN.
Set point to the end of the occurrence found, and return point.

PATTERN          pattern to match sexp.
RESULT-PATTERN   pattern to generate result.
RESULT-CALLBACK  a function to handle the result, return non-nil if success.
                 The function accpet two arguments:
                 - result  the result generated by RESULT-PATTERN.
                 - bounds  the bounds of the matched sexp.
COUNT            a number (default 1) that indicates the number of occurrences
                 to search for.  The current number will be stored in
                 `psearch-count-current' which avariable in RESULT-CALLBACK."
  (let (point last-point)
    (catch 'break
      (dotimes (i (or count 1))
        (let ((psearch-count-current (1+ i)))
          (if (setq point (psearch-forward-1
                           (psearch-make-matcher pattern result-pattern)
                           result-callback))
              (setq last-point point)
            (throw 'break nil)))))
    last-point))

;;;###autoload
(cl-defun psearch-replace (match-pattern replace-pattern
                                         &optional beg end
                                         &key splice
                                         &allow-other-keys)
  "Replace some occurences mathcing MATCH-PATTERN with REPLACE-PATTERN.

MATCH-PATTERN   a pcase pattern to match.
REPLACE-PATTERN an expression generate replacement for each match with bindings
                created in MATCH-PATTERN and apply them immediately:

                (psearch-replace '`MATCH '`REPLACE)

                or a pair of expressions that the first is used to collect the
                replacement for each matched into local variable ‘its’ instead
                of appling them immediately. The second is used to apply all of
                the collected replacement when the search complete:

                (psearch-replace '`MATCH '(`COLLECT `FINAL))

                or `nil' just to delete contents matched by MATCH-PATTERN.

BEG and END     specify the search region, default are (point) and (point-max).

SPLICE          if non-nil, splice the replacement value.

Examples:

- replace

    ```
    (psearch-replace '`(foo . ,rest)
                     '`(bar ,@rest))
    ;; |(foo a b ...) -> (bar a b ...)|
    ```

- replace (collect -> final)

   ```
   (psearch-replace '`(setq ,sym ,val)
                    '(`(,sym ,val) `(setq ,@(-flattern-n 1 its))))
   ;; [(setq foo 1)  =>  (setq foo 1
   ;;  (setq bar 2)]           bar 2)|
   ```

- replace (splice)

   ```
   (psearch-replace '`(setq . ,(and rest (guard (> (length rest) 2))))
                    '(mapcar (lambda (pair)
                               (cons 'setq pair))
                      (seq-partition rest 2))
                    nil nil
                    :splice t)
   ;; |(setq foo 1    =>   (setq foo 1)
   ;;        bar 2)        (setq bar 2)|
   ```"
  (interactive (psearch-replace-args))
  (let* (its
         last-point
         collected-bounds
         collect-pattern
         (final-pattern (pcase replace-pattern
                          (`(`,_collect `,_final)
                           (setq collect-pattern (car replace-pattern))
                           (cadr replace-pattern))))
         (callback
          (if collect-pattern
              (lambda (result bounds)
                (push result its)
                (push bounds collected-bounds)
                t)
            (if splice
                (lambda (result bounds)
                  (delete-region (car bounds) (cdr bounds))
                  (save-excursion
                    (psearch--splice-and-insert result)
                    (setq last-point (point)))
                  'nonmoving)
              (if (not replace-pattern)
                  (lambda (_ bounds)
                    (delete-region (car bounds) (cdr bounds))
                    'nonmoving)
                t))))
         (matcher
          (psearch-make-matcher match-pattern
                                (or collect-pattern replace-pattern)))
         (points
          (save-excursion
            (save-restriction
              (when (or beg end)
                (narrow-to-region (if beg (goto-char beg) (point))
                                  (or end (point-max))))
              (cl-loop with pos
                       while (setq pos (psearch-forward-1 matcher callback))
                       collect pos)))))
    (when points
      (if last-point
          (goto-char last-point)
        (goto-char (car (last points))))
      (when its
        (let ((newline-p (eq (char-before end) ?\n)))
          (if (and psearch-delete-collect-replace-region-p beg end)
              (delete-region beg end)
            (dolist (bounds collected-bounds)
              (delete-region (car bounds) (cdr bounds))))
          (let ((collected-objects
                 ;; circumvent lexical binding problem
                 (funcall `(lambda (its) ,final-pattern)
                          (reverse its))))
            (if splice
                (psearch--splice-and-insert (apply #'append collected-objects))
              (insert (psearch--print-to-string collected-objects))))
          (when newline-p
            ;; preserve \n for region like following:
            ;; [(matched)
            ;; ](other)
            (insert "\n"))))
      (if (called-interactively-p 'any)
          (message "Replaced %s occurrences" (length points))
        (point)))))

;;;###autoload
(cl-defun psearch-replace-at-point (match-pattern replace-pattern
                                                  &key splice
                                                  &allow-other-keys)
  "Replace the sexp matching MATCH-PATTERN at point with REPLACE-PATTERN.

Replace only the sexp at point, that is, the point is at the beginning of it or
inside it. If SPLICE is non-nil, splice the replacement value.

Unlike ‘psearch-replace’, the REPLACE-PATTERN here does not support (and is not
necessary) the collect->finle operation.

Examples:

- point at beginning of sexp

        ```
        |(match a (b c))    =>     |(replace a (b c))
        ```

- point in sexp

        ```
        (match a (b|c))     =>     |(replace a (b c))
        ```

- splice

        ```
        (psearch-replace-at-point '`(setq . ,(and rest (guard (> (length rest) 2))))
                                  '(mapcar (lambda (pair)
                                             (cons 'setq pair))
                                    (seq-partition rest 2))
                                :splice t)
        ;; |(setq foo 1    =>   (setq foo 1)
        ;;        bar 2)        (setq bar 2)|
        ```"
  (interactive (psearch-replace-args "Query replace at point"))
  (let* ((pos (point))
         (matcher (psearch-make-matcher match-pattern replace-pattern))
         (callback
          (lambda (replacement bounds)
            (when (<= (car bounds) pos (cdr bounds))
              (delete-region (car bounds) (cdr bounds))
              (if splice
                  (psearch--splice-and-insert replacement)
                (if replace-pattern
                    (insert (psearch--print-to-string replacement))))
              t))))
    (when (or (psearch--apply-replacement-at-point matcher callback)
              (psearch-backward-1 matcher callback))
      (if (called-interactively-p 'any)
          (message "Replaced")
        (point)))))

;;; Patch functions

(defun psearch-patch--find-cl-generic-method (function met)
  (let ((generic (cl--generic function)))
    (catch 'found
      (dolist (method (cl--generic-method-table generic))
        (when (equal met
                     (psearch-patch--cl-generic-load-hist-format
                      function
                      (cl--generic-method-qualifiers method)
                      (cl--generic-method-specializers method)))
          (throw 'found method))))))

(defun psearch-patch--cl-generic-args (spec-args)
  "Return (EXTRA QUALIFIER SPECIALIZER REST)."
  (list (when (car (equal :extra (car spec-args)))
                 (pop spec-args)
                 (pop spec-args))
               (when (memq (car spec-args) '(:before :after :around))
                 (pop spec-args))
               (pop spec-args)
               spec-args))

(defun psearch-patch--xref-function-def (xref-args)
  (pcase-let* ((`(,fun ,file ,type) xref-args)
               (`(,buf . ,pos) (find-function-search-for-symbol fun type file)))
    (with-current-buffer buf
      (goto-char pos)
      (let ((bounds (bounds-of-thing-at-point 'sexp)))
        (read (buffer-substring-no-properties (car bounds) (cdr bounds)))))))

(defun psearch-patch--symbol-function-def (function)
  (let ((def (symbol-function function)))
    (if (byte-code-function-p def)
        (signal 'psearch-patch-failed
                (list function "Can't patch a byte-compiled function"))
      ;; For 30+
      (if (and (fboundp 'interpreted-function-p)
               (funcall 'interpreted-function-p def))
          ;; (eval '(defun foobar (arg) "Docstring" 'body))
          ;; => #[(arg) ('body) nil nil "Docstring"]
          ;; <= (defun foobar (arg) "Docstring" 'body)
          `(defun ,function ,(aref def 0) ,(aref def 4) ,@(aref def 1))
        (let ((new (nthcdr (if (eq (car def) 'closure) 2 1) def)))
          (push function new)
          (push 'defun new)
          new)))))

(defun psearch-patch--cl-generic-function-def (def-type function extra qualifier args cl-method)
  "Return a redable generic function definition from CL-METHOD object.

For example:

   #s(...) =>
   (cl-DEF-TYPE EXTRA QUALIFIER FUNCTION ARGS docstring func-body)"
  (let* ((method-def (cl--generic-method-function cl-method))
         (func-body
          ;; For 30+
          (if (and (fboundp 'interpreted-function-p)
                   (funcall 'interpreted-function-p method-def))
            (if (= 5 (length method-def))
                (cons (aref method-def 4) (aref method-def 1))
              (aref method-def 1))
            (nthcdr (if (equal (nth 0 method-def) 'lambda) 2 3)
                    method-def))))
    (when (eq def-type 'cl-defgeneric)
      (let* ((doc-raw (documentation function t))
             (doc (cdr (help-split-fundoc doc-raw function))))
        (when doc
          (push doc func-body))))
    (when args
      (push args func-body))
    (when qualifier
      (push qualifier func-body))
    (when extra
      (push extra func-body)
      (push :extra func-body))
    (push function func-body)
    (push def-type func-body)
    func-body))

(defalias 'psearch-patch--cl-generic-load-hist-format
  (if (functionp 'cl--generic-load-hist-format)
      'cl--generic-load-hist-format
    (lambda (name _ specializers)
      (cons name specializers))))

(defun psearch-patch--cl-generic-met-name (function qualifier specializer)
  (psearch-patch--cl-generic-load-hist-format
   function (and qualifier (list qualifier))
   (pcase-let ((`(,spec-args . ,_) (cl--generic-split-args specializer)))
     (mapcar
      (lambda (spec-arg)
        (if (eq '&context (car-safe (car spec-arg)))
            spec-arg (cdr spec-arg)))
      spec-args))))

(defun psearch-patch--find-function (func-spec)
  (let* ((func-spec (if (symbolp func-spec) (list 'defun func-spec) func-spec))
         (def-type (pop func-spec))
         (function (pop func-spec))
         file)
    (pcase-exhaustive def-type
      ('defun
        (let ((func-lib (find-function-library function 'lisp-only t)))
          (if (setq file (cdr func-lib))
              (psearch-patch--xref-function-def (list function file))
            (psearch-patch--symbol-function-def function))))
      ('cl-defgeneric
        (let ((func-lib (find-function-library function 'lisp-only t)))
          (if (setq file (cdr func-lib))
              (psearch-patch--xref-function-def (list function file))
            (when-let ((cl-method (psearch-patch--find-cl-generic-method
                                   function (psearch-patch--cl-generic-met-name function nil '(t)))))
              (pcase-let ((`(,extra ,qualifier ,specializer ,_spec-rest)
                           (psearch-patch--cl-generic-args func-spec)))
                (psearch-patch--cl-generic-function-def
                 def-type function extra qualifier specializer cl-method))))))
      ('cl-defmethod
        (pcase-let* ((`(,extra ,qualifier ,specializer ,_spec-rest)
                      (psearch-patch--cl-generic-args func-spec))
                     (met-name
                      (psearch-patch--cl-generic-met-name function qualifier specializer)))
          (let ((cl-method (psearch-patch--find-cl-generic-method
                            function met-name)))
            (when cl-method
              (if (setq file (find-lisp-object-file-name met-name 'cl-defmethod))
                  (psearch-patch--xref-function-def (list met-name file def-type))
                (psearch-patch--cl-generic-function-def
                 def-type function extra qualifier specializer cl-method)))))))))

;;;###autoload
(defmacro psearch-patch-define (name orig-func-spec &rest patch-form)
  "Define NAME as a function based on a existing function with patch applied.

See `psearch-patch' for explanation on arguments ORIG-FUNC-SPEC and PATCH-FORM."
  (declare (indent 2))
  (let ((docpos (or psearch-patch-function-definition-docpos
                    (if (symbolp orig-func-spec) 3 (length orig-func-spec)))))
    `(let ((func-def (psearch-patch--find-function ',orig-func-spec)))
       (with-temp-buffer
         ;; Modifiy function name
         (when (and (memq (nth 0 func-def) '(defun defsubst)) (not (eq ',name (nth 1 func-def))))
           (setcdr func-def (cons ',name (nthcdr 2 func-def))))
         (print func-def (current-buffer))
         ;; Modify docstring.
         (goto-char (point-min))
         (down-list)
         (forward-sexp
          (+ ,docpos (if (equal 'lambda (sexp-at-point)) 0
                       (if (memq (sexp-at-point) '(defun defsubst cl-defgeneric cl-defmethod)) 1
                         0))))
         (let ((str "[PATCHED]"))
           (goto-char (car (bounds-of-thing-at-point 'sexp)))
           (if (equal (char-after) ?\")
               (progn
                 (forward-char)
                 (insert str))
             (insert (format "%S\s" str))))
         ;; Apply patch
         (goto-char (point-min))
         (if (progn ,@patch-form)
             (eval-region (point-min) (point-max))
           (signal 'psearch-patch-failed
                   (list ',orig-func-spec "PATCH-FORM not applied")))))))

;;;###autoload
(defmacro psearch-patch (orig-func-spec &rest patch-form)
  "Create a patched function when PATCH-FORM return non-nil.

ORIG-FUNC-SPEC should be a function name or (CL-DEF[GENERIC,METHOD] NAME [EXTRA]
[QUALIFIER]) if it is a generic function.

PATCH-FORM expressions to be executed. The target function will be patched only
if PATCH-FORM returns a non-nil, otherwise raise an error.

Examples:

  (defun test () (list 1 (if nil 2) 3))

  ;; Patch 1
  (psearch-patch test
    (psearch-replace \\='\\=`(if nil ,body)
                     \\='\\=`(if t ,body)))

  ;; Patch 2
  (psearch-patch test
    (when (psearch-forward '`3)
      (insert \" 4\")
      t ;; Manual return t
      ))

  (cl-defgeneric cl-test (_tag) nil)
  (cl-defdefmethod cl-test ((tag (eql foo))) (list 1 (if nil 2) 3))

  ;; Patch 3
  (psearch-patch (cl-defmethod cl-test ((tag (eql tag)))) ;; Match generic method
    (psearch-replace \\='\\=`(if nil ,body)
                     \\='\\=`(if t ,body)))"
  (declare (indent 1))
  (let ((name (if (symbolp orig-func-spec) orig-func-spec (cadr orig-func-spec))))
    `(psearch-patch-define ,name ,orig-func-spec ,@patch-form)))

(provide 'psearch)

;;; psearch.el ends here
