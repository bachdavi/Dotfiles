;; Julia config file

(require 'julia-mode)

;; When in jupyter result mode.
(add-to-list 'evil-insert-state-modes 'special-mode)

;; Inline evaluation
(setq jupyter-eval-use-overlays t)

;; Autocompletion
(add-hook 'julia-mode-hook 'company-mode)
(add-hook 'jupyter-repl-mode-hook 'company-mode)

(add-hook 'jupyter-repl-mode-hook
          (lambda () (local-set-key (kbd "C-c M-o") 'jupyter-repl-clear-cells)))

;; Jupyter Repl customizations
(custom-set-faces
 '(jupyter-repl-input-prompt ((t (:foreground "dark blue"))))
 '(jupyter-repl-output-prompt ((t (:foreground "dark red"))))
 '(jupyter-repl-traceback ((t (:background "firebrick3")))))

