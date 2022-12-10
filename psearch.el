;;; psearch.el --- Pcase based search for Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2020/08/29
;; Version: 0.2.2
;; Last-Updated: 2022-08-09 16:27:18 +0800
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
  (eval-when-compile
    (rx (seq bos "(" (or "defun" "cl-defun" "defmacro" "cl-defmacro")
             (one-or-more space)
             (group (one-or-more (or (syntax word) (syntax symbol) (seq "\\" nonl)))))))
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
          (when object
            (while (setq car (pop object))
              (psearch--prin1 car stream)
              (when object
                (princ " " stream))))
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
                 see ‘psearch-forward’ for the rest information."
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

;; (defun psearch--advice-find-function-library (orig-fn function &rest args)
;;   "Advice around `find-function-library'."
;;   (let ((return (apply orig-fn function args)))
;;     (if (cdr return)
;;         (prog1 return
;;           (put function 'functin-library return))
;;       (or (get function 'functin-library)
;;           return))))

;; (advice-add 'find-function-library :around 'psearch--advice-find-function-library)

;; (defun psearch-unload-function ()
;;   "Unload psearch library."
;;   (advice-remove 'find-function-library 'psearch--advice-find-function-library))

(defun psearch--find-function-library (function)
  "Return the pair (ORIG-FUNCTION . LIBRARY) for FUNCTION."
  (pcase-let* ((`(,_real-function ,def ,aliased ,real-def)
                (help-fns--analyze-function function))
               (file-name
                (find-lisp-object-file-name function (if aliased 'defun def))))
    (when file-name
      (find-function-search-for-symbol
       function nil file-name))))

;;;###autoload
(defmacro psearch-with-function-create (new-fn orig-fn &rest patch-form)
  "Create a patched function when PATCH-FORM return non-nil.

ORIG-FN    Symbol of the original function
NEW-FN     Symbol of the patched function"
  (declare (indent 2) (debug t))
  `(let* ((printer nil)
          (new-name ,(symbol-name new-fn))
          (sexp (condition-case err
                    ;; Find in file
                    (let ((location
                           (pcase-let*
                               ((`(,_real-function ,def ,aliased ,real-def)
                                 (help-fns--analyze-function ',orig-fn))
                                (file-name (find-lisp-object-file-name
                                            ',orig-fn (if aliased 'defun def))))
                             (when file-name
                               (find-function-search-for-symbol
                                ',orig-fn nil file-name)))))
                      (with-current-buffer (car location)
                        (setq printer 'princ)
                        (goto-char (cdr location))
                        (let ((s (thing-at-point 'sexp)))
                          (if (string-match psearch-patch-function-regexp s)
                              (replace-match new-name 'fixedcase 'literal s 1)
                            (signal 'psearch-patch-failed
                                    (list ',orig-fn
                                          "Failt to mutch the function name"))))))
                  (error
                   ;; Find uncompiled function
                   (let ((definition
                          (if (and (stringp (cadr err))
                                   (string-prefix-p "Don’t know where" (cadr err)))
                              (symbol-function ',orig-fn))))
                     ;; NOTE: Some forms will change after evaluating, e.g.:
                     ;; ```
                     ;; (with-emacs
                     ;;   (with-temp-buffer
                     ;;     (insert "(defun test () (when t '(1 2 3)))")
                     ;;     (eval-buffer))
                     ;;   (symbol-function 'test))
                     ;; => (lambda nil (if t (progn '(1 2 3))))
                     ;; ```
                     (if (not definition)
                         (apply 'signal err)
                       (if (byte-code-function-p definition)
                           (signal 'psearch-patch-failed
                                   (list ',orig-fn
                                         "Can't patch a byte-compiled function"))
                         (setq printer 'print)
                         (list 'setf '(symbol-function ',new-fn)
                               `#',definition))))))))
     (with-temp-buffer
       (save-excursion
         (funcall printer sexp (current-buffer)))
       (print (buffer-string))
       (if (progn ,@patch-form)
           (prog1 ',new-fn
             (eval-region (point-min) (point-max))
             (put ',new-fn 'function-documentation
                  (format "This is a patched version of `%s'." ',orig-fn)))
         (signal 'psearch-patch-failed
                 (list ',orig-fn "PATCH-FORM not applied"))))))

;;;###autoload
(defmacro psearch-with-function-patch (function &rest patch-form)
  "Patch the FUNCTION if PATCH-FORM return non-nil.

Example:

   ;; Patch the function `orig-fun':
   (psearch-with-function-patch orig-fun
     (psearch-replace match-pattern
                      replace-pattern))

   ;; Equivalent to:
   (psearch-with-function-create psearch-patched@orig-fun orig-fun
     (psearch-replace match-pattern
                      replace-pattern))
   (advice-add \\='orig-fun :override #\\='psearch-patched@orig-fun)"
  (declare (indent defun) (debug t))
  (let ((adsym (intern (format "psearch-patched@%s" function))))
    `(progn
       (psearch-with-function-create ,adsym ,function ,@patch-form)
       (message "==> [debug] %s\n%S" ',adsym (symbol-function ',adsym))
       (setf (symbol-function ',function) ',adsym))))

(provide 'psearch)

;;; psearch.el ends here
