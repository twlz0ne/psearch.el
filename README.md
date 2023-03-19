[![CI](https://github.com/twlz0ne/psearch.el/workflows/CI/badge.svg)](https://github.com/twlz0ne/psearch.el/actions?query=workflow%3ACI)

# psearch.el

Pcase based search for Emacs Lisp.  Not as powerful as [el-search](https://elpa.gnu.org/packages/el-search.html), but easier to use.

## Installation

``` elisp
(quelpa '(psearch
          :fetcher github
          :repo "twlz0ne/psearch.el"
          :files ("psearch.el")))
```

## Usage

- **psearch-forward** _`(pattern &optional result-pattern result-callback)`_

    Move forward across one sexp matched by PATTERN.
    Set point to the end of the occurrence found, and return point.

    ``` elisp
    (psearch-forward '`(foo . ,rest))
    ;; |(bar a b         =>    (bar a b
    ;;       (foo 1 2))             (foo 1 2)|)
    ```

- **psearch-backward** _`(pattern &optional result-pattern result-callback)`_

    Move backward across one sexp matched by PATTERN.
    Set point to the beginning of the occurrence found, and return point.

    ``` elisp
    (psearch-backward '`(foo . ,rest))
    ;; (foo a b         =>    |(foo a b
    ;;      (bar|1 2))              (bar 1 2))
    ```

- **psearch-replace** _`(match-pattern replace-pattern &splice)`_

    Replace some occurences mathcing MATCH-PATTERN with REPLACE-PATTERN.

    ``` elisp
    (psearch-replace '`(foo . ,rest) '`(bar ,@rest))
    ;; |(foo a b         =>    (bar a b
    ;;       (c 1 2))               (c 1 2))|
    ```

    or <kbd>M-x</kbd> <code>psearch-replace</code> <kbd>RET</kbd> <code>\`(foo . ,rest)</code> <kbd>RET</kbd> <code>\`(bar ,@rest)</code>.
    
    REPLACE-PATTERN also can be a pair:

    ``` elisp
    (psearch-replace '`(setq ,sym ,val)
                     '(`(,sym ,val)                    ;; collect only
                       `(setq ,@(-flattern-n 1 its)))) ;; apply when search complete
    ;; [(setq foo 1)  =>  (setq foo 1
    ;;  (setq bar 2)]           bar 2)|
    ```
    
    Splice the the replacement:
    
    ``` elisp
    (psearch-replace '`(setq . ,(and rest (guard (> (length rest) 2))))
                     '(mapcar (lambda (pair)
                                (cons 'setq pair))
                       (seq-partition rest 2))
                     nil nil
                     :splice t)
    ;; |(setq foo 1    =>   (setq foo 1)
    ;;        bar 2)        (setq bar 2)|
    ```
    
    Without `:splice t` the result will be:
   
    ``` elisp
    ((setq foo 1)
     (setq bar 2))
    ``` 

- **psearch-replace-at-point** _`(match-pattern replace-pattern &key splice)`_

    Similar to `psearch-replace`, but replace only the sexp at point, that is, the point is at the beginning of it or inside it.  For example:
    
    ```
    |(match a (b c))     =>     |(replace a (b c))  ;; point at beginning of sexp
     (match a (b|c))     =>     |(replace a (b c))  ;; point in sexp
    ```

    Unlike ‘psearch-replace’, the `replace-pattern` here does not support (and is not necessary) the `collect->finle` operation. 

- **psearch-patch** _`(orig-func-spec &rest patch-form)`_

    Patch function with PATCH-FORM.
    
    ORIG-FUNC-SPEC could be a symbol if function is defined by `defun`/`cl-defun`:
    
    ``` elisp
    (psearch-patch test
      (psearch-replace '`(if nil ,body)
                       '`(if t ,body)))
    ;; (defun test ()               =>    (defun test ()
    ;;   (list '(1 2 3)                     (list '(1 2 3)
    ;;         (if nil '(4 5 6))                  (if t '(4 5 6))
    ;;         '(7 8 9)))                         '(7 8 9)))
    ```

    or a list if it is a generic method:
    
    ``` elisp
    ;; Definition
    (cl-defgeneric test (_tag) nil)
    (cl-defdefmethod test ((tag (eql foo))) (list 1 (if nil 2) 3))
    ;; Patch
    (psearch-patch (cl-defmethod test ((tag (eql tag))))
      (psearch-replace '`(if nil ,body)
                       '`(if t ,body)))
    ```
    
    The target function will be patched only if PATCH-FORM returns a non-nil, otherwise raise an error. If the `PATCH-FORM` contains multiple statements, make sure each one of them executes successfully:
    
    ``` elisp
    (with-eval-after-load 'corfu-doc-terminal
      (psearch-patch corfu-doc-terminal--preprocess-docstring
        ;; Delete all `(let ...)` form except the first.
        (let ((pattern '`(let . ,rest)))
          (and (psearch-forward pattern)
               (psearch-replace pattern nil)))))
    ```

## Known issue

Since the behaviour of `thing-at-point` has changed since 27 ([`0efb881`](https://emba.gnu.org/emacs/emacs/-/commit/0efb88150df56559e8d649e657902fb51ad43bc1)), the result of the following code will vary depending on the Emacs version:

``` elisp
;; |(foo (bar))
(when (psearch-forward '`(bar))
  (thing-at-point 'sexp))
;; => (bar) [28.0]
;;    (bar) [27.1]
;;     nil  [26.1]
;;     nil  [25.1]
```

The reliable way to get the sexp is:

``` elisp
;; |(foo (bar))
(let (sexp-bounds)
  (when (psearch-forward '`(bar) t (lambda (_ bounds) (setq sexp-bounds bounds)))
    (buffer-substring (car sexp-bounds) (car sexp-bounds))))
```

