;;;rel-mode.el --- major mode for editing Rel. -*- coding: utf-8; lexical-binding: t; -*-

;; See: http://ergoemacs.org/emacs/elisp_syntax_coloring.html

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;; To activate, put this file in ~/.emacs.d/rel/ , say, and then add this to your .emacs.el :
;; (add-to-list 'load-path "~/.emacs.d/rel/")
;; (require 'rel-mode)


;;; Code:

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq rel-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("exists" "forall" "and" "or" "not" "reduce" "with" "implies" "true" "false" "if" "then" "else" "end"))
            (x-types '("Float" "Int" "String" "int64" "int128" "uint64"))
            (x-constants '("def" "redef" "include" "entity" "bound" "constraint" "ic")) ;; Decl keywords
            (x-events '("inline" "function")) ;; not sure how to get the "@" to work here
            ;; from stdlib, mostly:
            (x-functions '("add" "subtract" "divide" "multiply" "power" "max" "min" "abs" "sqrt" "eq" "neq" "gt" "lt" "lt_eq"
                           "gt_eq" "equal" "count" "sum" "product" "Max" "ArgMax" "ArgMin" "Min" "mean" "mse" "rmse" "mae" "sort"
                           "substring" "regex_match"))

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            (x-types-regexp (regexp-opt x-types 'words))
            (x-constants-regexp (regexp-opt x-constants 'words))
            (x-events-regexp (regexp-opt x-events 'words))
            (x-functions-regexp (regexp-opt x-functions 'words)))

        `(
          ("@inline" . font-lock-builtin-face)
          ("@function" . font-lock-builtin-face)
          (,x-types-regexp . font-lock-type-face)
          (,x-constants-regexp . font-lock-constant-face)
          (,x-events-regexp . font-lock-builtin-face)
          (,x-functions-regexp . font-lock-function-name-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          ;; Higlight expressions starting with ":"
          ;; (":[[:word:]]" . font-lock-function-name-face) ;; should work, doesn't ?
          (":[A-Za-z]+[A-Za-z_0-9]*" . font-lock-type-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          ("//.*$" 0 'font-lock-comment-face t)
          )))

;;;###autoload
;;; previously derived from C for // and /* */ comments, but got annoying indentation
(define-derived-mode rel-mode text-mode "rel mode" ;; c-mode "rel mode"
  "Major mode for editing Rel"
  (modify-syntax-entry ?_ "w") ;; make "_" part of words

          (make-local-variable 'comment-start)
          (make-local-variable 'comment-end)
          (make-local-variable 'comment-start-skip)
          (setq comment-start "/* "
                comment-end " */"
                comment-start-skip "/\\*[ \n\t]+")
          (modify-syntax-entry ?* ". 23")
          (modify-syntax-entry ?/ ". 14")


  ;; code for syntax highlighting
  (setq font-lock-defaults '((rel-font-lock-keywords))))

;; add the mode to the `features' list
(provide 'rel-mode)

(add-to-list 'auto-mode-alist '("\\.rel\\'" . rel-mode))

;; do not use tabs when indenting regions:
(setq indent-tabs-mode nil)

;;; rel-mode.el ends here
