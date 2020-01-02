;; Julia config file

(require 'julia-mode)

;; When in jupyter result mode.
(add-to-list 'evil-insert-state-modes 'special-mode)

;; Autocompletion
(add-hook 'julia-mode-hook 'company-mode)
(add-hook 'jupyter-repl-mode-hook 'company-mode)

(add-hook 'jupyter-repl-mode-hook
          (lambda () (local-set-key (kbd "C-c M-o") 'jupyter-repl-clear-cells)))
