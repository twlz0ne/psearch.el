[![Build Status](https://travis-ci.com/twlz0ne/psearch.el.svg?branch=master)](https://travis-ci.com/twlz0ne/psearch.el)

# psearch.el

Pcase based search for Emacs Lisp.  Not as powerful as [el-search](https://elpa.gnu.org/packages/el-search.html), but easier to use.

## Installation

``` elisp
(quelpa '(multi-translate
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
    ```


- **psearch-backward** _`(pattern &optional result-pattern result-callback)`_

    Move backward across one sexp matched by PATTERN.
    Set point to the beginning of the occurrence found, and return point.
    
    ``` elisp
    (psearch-backward '`(foo . ,rest)) 
    ```

- **psearch-replace** _`(match-pattern replace-pattern)`_

    Replace some occurences mathcing MATCH-PATTERN with REPLACE-PATTERN.

    ``` elisp
    (psearch-replace '`(foo . ,rest) '`(bar ,@rest)) 
    ```
    
    or <kbd>M-x</kbd> <code>psearch-replace</code> <kbd>RET</kbd> <code>\`(foo . ,rest)</code> <kbd>RET</kbd> <code>\`(bar ,@rest)</code>.
