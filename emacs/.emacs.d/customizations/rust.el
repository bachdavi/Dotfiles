;; Rust 
;; (require 'rust-mode)

(use-package rustic)

(require 'rustic)

(add-hook 'rustic-mode-hook 'flycheck-mode)

(remove-hook 'rustic-mode-hook 'flymake-mode)
(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))

(evil-leader/set-key-for-mode 'rustic-mode
  "." 'lsp-find-definition
  "k" 'lsp-describe-thing-at-point)

;; I'm running my own rust binaries
(add-to-list 'exec-path "/Users/david/.cargo/bin")

;; Add several hooks
;; (add-hook 'rust-mode-hook 'linum-mode)
;; (add-hook 'rust-mode-hook 'flycheck-mode)
;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)
;;(add-hook 'racer-mode-hook #'company-mode)

;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;; (setq company-tooltip-align-annotations t)

;; (with-eval-after-load 'rust-mode
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; Repl evalutaion

;; Add insert mode to the racer-help window s.t. we can exist via 'q'
;; (add-to-list 'evil-insert-state-modes 'racer-help-mode)

;; (add-hook 'rust-mode-hook 'cargo-minor-mode)
