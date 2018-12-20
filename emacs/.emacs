;; Emacs configuration file
;; Author: David Bach
;; 20-12-2018

;; set up package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
(package-initialize)

(add-to-list 'exec-path "/usr/local/bin")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

;; No locking
(setq create-lockfiles nil)
(setq auto-save-default nil) 

;; All my packages
(defvar my-packages '(better-defaults
                      org
											key-chord
                      ivy
                      counsel
                      projectile
                      avy
											rg
                      rainbow-delimiters
                      highlight-symbol
                      flx
                      evil
                      evil-cleverparens
											python-mode
											elpy
                      py-autopep8
                      ein
                      clojure-mode
                      clojure-mode-extra-font-locking
                      cider
                      markdown-mode
                      rainbow-mode
                      rust-mode
											cargo
											racer
											flycheck
											flycheck-inline
											flycheck-rust
                      ))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;;;;
;; EVIL
;;;;
(use-package evil
:ensure t
:init
(setq evil-vsplit-window-right t)
:config
(evil-mode 1)

(use-package evil-leader
:ensure t
:config
(global-evil-leader-mode))

(use-package evil-surround
:ensure t
:config
(global-evil-surround-mode))

(use-package evil-indent-textobject
:ensure t)

(use-package evil-commentary
:ensure t
:config
(evil-commentary-mode)))

(use-package evil-cleverparens
  :ensure t
  :config
  (evil-cleverparens-mode))

(use-package evil-terminal-cursor-changer
:ensure t
:init
(setq evil-motion-state-cursor 'box)  ; █
(setq evil-visual-state-cursor 'box)  ; █
(setq evil-normal-state-cursor 'box)  ; █
(setq evil-insert-state-cursor 'bar)  ; ⎸
(setq evil-emacs-state-cursor  'hbar) ; _
:config
(evil-terminal-cursor-changer-activate))

(defun minibuffer-keyboard-quit ()
(interactive)
(if (and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark  t)
(when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
(abort-recursive-edit)))

(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [tab] 'aggressive-indent-indent-region-and-on)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Dont know anymore what that is supposed to do
(setq evil-want-C-i-jump nil)

(setq evil-want-C-u-scroll t)

;; Use words and symbols
(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))

;;;;
;; AUTO-COMPLETION
;;;;
(global-company-mode)
(setq company-minimum-prefix-length 1)
(eval-after-load 'company
  '(add-to-list 'company-frontends 'company-tng-frontend))
;; Use tab
(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)))

;;;;
;; MARKDOWN
;;;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;;
;; ORG MODE
;;;;
(setq org-startup-indented t)
(setq org-startup-folded "overview")
(setq org-directory "~/Dropbox/org/")
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook 'org-bullets-mode)

(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/gtd.org"
                         "~/gtd/tickler.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets '(("~/gtd/gtd.org" :maxlevel . 3)
                           ("~/gtd/someday.org" :level . 1)
                           ("~/gtd/tickler.org" :maxlevel . 2)))

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-support-shift-select 'always)


;;;;
;; CUSTOMIZATION
;;;;

;; yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't know why but I have to do it here otherwise stuff breaks
(require 'cider)

;; Load all the things
(add-to-list 'load-path "~/.emacs.d/customizations")
(load "ui.el")
(load "editing.el")
(load "navigation.el")
(load "keyboard.el")

;; clojure environment
(setq org-babel-clojure-backend 'cider)
(load "clojure.el")

;; python environment
(load "python-conf.el")

;; rust environment
(load "rust.el")

;;;;
;; FLYCHECK
;;;;
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
	 [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
	 (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#262626"))
 '(compilation-message-face (quote default))
 '(custom-safe-themes
	 (quote
		("28bf1b0a72e3a1e08242d776c5befc44ba67a36ced0e55df27cfc7ae6be6c24d" "12bacee81d067acf07dec4c867be541a04744a6ac6a39636de25a2c77e9b573c" "62c81ae32320ceff5228edceeaa6895c029cc8f43c8c98a023f91b5b339d633f" "a5956ec25b719bf325e847864e16578c61d8af3e8a3d95f60f9040d02497e408" "f27c3fcfb19bf38892bc6e72d0046af7a1ded81f54435f9d4d09b3bff9c52fc1" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
 '(elpy-modules
	 (quote
		(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(fci-rule-color "#3a3a3a")
 '(highlight-changes-colors (quote ("#EF5350" "#7E57C2")))
 '(highlight-tail-colors
	 (quote
		(("#010F1D" . 0)
		 ("#B44322" . 20)
		 ("#34A18C" . 30)
		 ("#3172FC" . 50)
		 ("#B49C34" . 60)
		 ("#B44322" . 70)
		 ("#8C46BC" . 85)
		 ("#010F1D" . 100))))
 '(hl-sexp-background-color "#121212")
 '(magit-diff-use-overlays nil)
 '(org-modules
	 (quote
		(org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-velocity)))
 '(package-selected-packages
	 (quote
		(pyflakes python-pylint flycheck-mypy pylint flycheck-pycheckers flycheck-pyflakes flycheck-inline cargo flymake-rust flycheck-rust key-chord 0blayout dash-functional pretty-symbols pretty-mode-plus pretty-mode oceanic-theme night-owl-theme tango-2-theme tango-plus-theme sublime-themes sublime flatui-dark-theme org-bullets evil-org groovy-mode rg geiser hl-todo racer company-ycmd ycmd jedi ipython aggressive-indent deadgrep github-theme pyenv-mode autopair evil-mc xclip use-package rust-mode rainbow-mode rainbow-delimiters powerline parinfer neotree multiple-cursors monokai-theme material-theme markdown-mode hlinum highlight-symbol helm-projectile helm-ag gruvbox-theme gruber-darker-theme flx flatland-theme evil-visual-mark-mode evil-terminal-cursor-changer evil-surround evil-smartparens evil-paredit evil-magit evil-leader evil-indent-textobject evil-easymotion evil-commentary evil-cleverparens drag-stuff dracula-theme counsel-projectile company color-theme-sanityinc-tomorrow clojure-mode-extra-font-locking cider better-defaults auto-complete ag ace-jump-mode ace-jump-buffer)))
 '(pos-tip-background-color "#FFF9DC")
 '(pos-tip-foreground-color "#011627")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
	 (quote
		((20 . "#f36c60")
		 (40 . "#ff9800")
		 (60 . "#fff59d")
		 (80 . "#8bc34a")
		 (100 . "#81d4fa")
		 (120 . "#4dd0e1")
		 (140 . "#b39ddb")
		 (160 . "#f36c60")
		 (180 . "#ff9800")
		 (200 . "#fff59d")
		 (220 . "#8bc34a")
		 (240 . "#81d4fa")
		 (260 . "#4dd0e1")
		 (280 . "#b39ddb")
		 (300 . "#f36c60")
		 (320 . "#ff9800")
		 (340 . "#fff59d")
		 (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
	 (quote
		(unspecified "#011627" "#010F1D" "#DC2E29" "#EF5350" "#D76443" "#F78C6C" "#D8C15E" "#FFEB95" "#5B8FFF" "#82AAFF" "#AB69D7" "#C792EA" "#AFEFE2" "#7FDBCA" "#D6DEEB" "#FFFFFF"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

