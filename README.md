[![Build Status](https://travis-ci.com/twlz0ne/psearch.el.svg?branch=master)](https://travis-ci.com/twlz0ne/psearch.el)

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

- **psearch-replace** _`(match-pattern replace-pattern)`_

    Replace some occurences mathcing MATCH-PATTERN with REPLACE-PATTERN.

    ``` elisp
    (psearch-replace '`(foo . ,rest) '`(bar ,@rest))
    ;; |(foo a b         =>    (bar a b
    ;;       (c 1 2))               (c 1 2))|
    ```

    or <kbd>M-x</kbd> <code>psearch-replace</code> <kbd>RET</kbd> <code>\`(foo . ,rest)</code> <kbd>RET</kbd> <code>\`(bar ,@rest)</code>.


- **psearch-replace-at-point** _`(match-pattern replace-pattern)`_

  Similar to `psearch-replace`, but replace only the sexp at point, that is, the point is at the beginning of it or inside it.  For example:

    ```
    |(match a (b c))     =>     |(replace a (b c))
     (match a (b|c))     =>     |(replace a (b c))
    ```
