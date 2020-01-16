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
