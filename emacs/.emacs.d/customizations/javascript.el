(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

;; GitHub syntax highlighting.
(set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "#255FBF")
(set-face-attribute 'web-mode-html-tag-face nil :foreground "#438343")
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#255FBF")
(set-face-attribute 'web-mode-html-attr-equal-face nil :foreground "#255FBF")

(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files

(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(add-hook 'web-mode-hook #'prettier-mode)

(require 'flycheck)

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

;; Enable eslint checker for web-mode
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; Enable flycheck globally
(add-hook 'web-mode-hook #'flycheck-mode)
(add-hook 'flycheck-mode-hook 'add-node-modules-path)

(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))

(add-hook 'web-mode-hook  'web-mode-init-prettier-hook)
(add-hook 'web-mode-hook  'emmet-mode)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(add-hook 'web-mode-hook #'setup-tide-mode)

;; {{ flyspell setup for web-mode
(defun web-mode-flyspell-verify ()
  (let* ((f (get-text-property (- (point) 1) 'face))
         rlt)
    (cond
     ;; Check the words with these font faces, possibly.
     ;; this *blacklist* will be tweaked in next condition
     ((not (memq f '(web-mode-html-attr-value-face
                     web-mode-html-tag-face
                     web-mode-html-attr-name-face
                     web-mode-constant-face
                     web-mode-doctype-face
                     web-mode-keyword-face
                     web-mode-comment-face ;; focus on get html label right
                     web-mode-function-name-face
                     web-mode-variable-name-face
                     web-mode-css-property-name-face
                     web-mode-css-selector-face
                     web-mode-css-color-face
                     web-mode-type-face
                     web-mode-block-control-face)))
      (setq rlt t))
     ;; check attribute value under certain conditions
     ((memq f '(web-mode-html-attr-value-face))
      (save-excursion
        (search-backward-regexp "=['\"]" (line-beginning-position) t)
        (backward-char)
        (setq rlt (string-match "^\\(value\\|class\\|ng[A-Za-z0-9-]*\\)$"
                                (thing-at-point 'symbol)))))
     ;; finalize the blacklist
     (t
      (setq rlt nil)))
    rlt))
(put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspell-verify)
;; }}

(defvar flyspell-check-doublon t
  "Check doublon (double word) when calling `flyspell-highlight-incorrect-region'.")
 (make-variable-buffer-local 'flyspell-check-doublon)

(defadvice flyspell-highlight-incorrect-region (around flyspell-highlight-incorrect-region-hack activate)
  (if (or flyspell-check-doublon (not (eq 'doublon (ad-get-arg 2))))
      ad-do-it))

(defun web-mode-hook-setup ()
  (flyspell-mode 1)
  (setq flyspell-check-doublon nil))

(add-hook 'web-mode-hook 'web-mode-hook-setup)

(defun js-flyspell-verify ()
  (let* ((f (get-text-property (- (point) 1) 'face)))
    ;; *whitelist*
    ;; only words with following font face will be checked
    (memq f '(js2-function-call
              js2-function-param
              js2-object-property
              font-lock-variable-name-face
              font-lock-string-face
              font-lock-function-name-face))))
(put 'js2-mode 'flyspell-mode-predicate 'js-flyspell-verify)
(put 'rjsx-mode 'flyspell-mode-predicate 'js-flyspell-verify)
