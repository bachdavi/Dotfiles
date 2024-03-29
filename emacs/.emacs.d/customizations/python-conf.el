;; Python configuration
;; UT-8 as default encoding
(set-language-environment "UTF-8")

;; elpy stuff
(setq elpy-rpc-python-command "python3")
(setq elpy-rpc-backend "jedi")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i")

;; (add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'elpy-mode)
;; (add-hook 'elpy-mode-hook 'linum-mode)

;; Remove flymake s.t. we use flycheck
(with-eval-after-load 'elpy (remove-hook 'elpy-modules 'elpy-module-flymake))

;; Set same keys as in cider
(add-hook 'elpy-mode-hook
          (lambda () (local-set-key (kbd "C-c C-q") 'elpy-shell-kill)))

;; I'm not sure if this is necessary
(add-hook 'python-mode-hook
            (lambda ()
              (setq flycheck-python-pylint-executable "/usr/local/bin/pylint")))

;; Flycheck
;; (with-eval-after-load 'flycheck
;;   (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

;; Repl evalutaion
(evil-leader/set-key-for-mode 'python-mode
  "e" 'elpy-shell-send-statement
  "." 'elpy-goto-definition
  "k" 'elpy-doc)

;; Setting virtual env
(use-package pyenv-mode
  :init
  (setenv "WORKON_HOME" "~/Envs/"))

(defalias 'workon 'pyvenv-workon)

;; Jupyter notebook stuff
(use-package ein
  :pin melpa
  :config
  (setq ein:use-auto-complete t)
  (setq ein:complete-on-dot t)
  (setq ein:completion-backend 'ein:use-company-backend)
  (setq ein:use-auto-complete-superpack nil)
  (setq ein:use-smartrep nil)
)

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(setq ein:console-executable "/usr/local/bin/ipython")
