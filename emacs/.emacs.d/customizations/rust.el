;; Rust 
(require 'rust-mode)

;; Tell racer where to find its stuff
(setq racer-rust-src-path "/Users/david/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")

;; I'm running my own rust binaries
(add-to-list 'exec-path "/Users/david/.cargo/bin")

;; Add several hooks
(add-hook 'rust-mode-hook 'linum-mode)
(add-hook 'rust-mode-hook 'flycheck-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'rust-mode-hook 'cargo-minor-mode)
