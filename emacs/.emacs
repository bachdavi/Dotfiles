;; set up package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
(package-initialize)

(add-to-list 'exec-path "/usr/local/bin")

(global-set-key "\M-'" 'other-frame)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'evil-mc)
(setq create-lockfiles nil)

(setq use-package-always-ensure t)

(setq auto-save-default nil) 

;; (load-theme 'tango-plus t) ;; load material theme
;; (set-cursor-color "#00ff7f") 

(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Operator Mono Lig-17"))

;; Fontify current frame
(fontify-frame nil)
;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions) 

(setq cider-clojure-cli-global-options "-A:dev")

(menu-bar-mode -1)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(defalias 'yes-or-no-p 'y-or-n-p)

;; (global-linum-mode 1)
(setq linum-format " %4d ")

(setq elpy-rpc-python-command "python3")
(setq elpy-rpc-backend "jedi")
(elpy-enable)
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")

(add-hook 'elpy-mode-hook 'linum-mode)

(use-package hlinum
  :ensure t)
(set-face-foreground 'linum-highlight-face "white")
(set-face-background 'linum-highlight-face nil)
(hlinum-activate)

(line-number-mode 1)
(column-number-mode 1)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(electric-pair-mode 1)

(global-visual-line-mode t)

(global-evil-mc-mode  1) 

(use-package drag-stuff
  :ensure t)
(drag-stuff-global-mode 1)
(global-set-key (kbd "M-k") 'drag-stuff-up)
(global-set-key (kbd "M-j") 'drag-stuff-down)

(define-key key-translation-map [(meta ?5)] [?\[])
(define-key key-translation-map [(meta ?6)] [?\]])
(define-key key-translation-map [(meta ?9)] [?\}])
(define-key key-translation-map [(meta ?8)] [?\{])
(define-key key-translation-map [(meta ?7)] [?\|])
(define-key key-translation-map [(meta ?3)] [?\#])
(define-key key-translation-map [(meta ?n)] [?\~])
(define-key key-translation-map [(meta ?g)] [?\@])
(global-set-key (kbd "s-7") "\\")

(global-hl-line-mode +1)

(setq show-paren-delay 0)
(show-paren-mode 1)

(setq evil-want-C-i-jump nil)

(setq evil-want-C-u-scroll t)

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

(evil-leader/set-leader ",")
(evil-leader/set-key
  "d" 'counsel-projectile
  "f" 'counsel-recentf
  "s" 'ivy-switch-buffer
  "y" 'counsel-find-file
  "p" 'projectile-switch-project
  "a" 'counsel-projectile-rg
  "q" 'evil-quit
  "w" 'save-buffer
  "t" 'neotree-toggle
  ",w" 'avy-goto-word-1
  ",b" 'avy-goto-word-1-above
  "e" 'emojify-insert-emoji
  "g" 'magit)


(setq ein:completion-backend 'ein:use-ac-jedi-backend)

(evil-leader/set-key-for-mode 'clojure-mode   "." 'cider-find-dwim
                                                "e" 'cider-eval-sexp-at-point
                                                "b" 'cider-eval-buffer
                                                "r" 'cider-insert-last-sexp-in-repl
                                                "K" 'cider-doc)

(evil-leader/set-key-for-mode 'clojurec-mode "." 'cider-find-dwim
                                                "e" 'cider-eval-sexp-at-point
                                                "b" 'cider-eval-buffer
                                                "r" 'cider-insert-last-sexp-in-repl
                                                "K" 'cider-doc)

(evil-leader/set-key-for-mode 'clojurescript-mode "." 'cider-find-dwim
                                                "e" 'cider-eval-sexp-at-point
                                                "b" 'cider-eval-buffer
                                                "r" 'cider-insert-last-sexp-in-repl
                                                "K" 'cider-doc)

(evil-leader/set-key-for-mode 'python-mode
  "e" 'elpy-shell-send-statement
  "gd" 'elpy-goto-definition
  "K" 'elpy-doc)

(use-package pyenv-mode
  :init
  (setenv "WORKON_HOME" "~/Envs/"))

(require 'ein)

(defalias 'workon 'pyvenv-workon)

(define-key evil-normal-state-map (kbd "SPC") 'counsel-grep-or-swiper)

;; (use-package helm
;;   :ensure t
;;   :config (helm-mode t))
(use-package projectile
  :ensure projectile
  :config
  (setq projectile-indexing-method 'git)
  (projectile-mode +1))
;; (use-package helm-projectile
;;   :ensure t)
;; (use-package helm-ag
;;   :ensure t)

(use-package neotree :ensure t)

;; (require 'ycmd)
;; (add-hook 'after-init-hook #'global-ycmd-mode)


(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

(use-package rainbow-delimiters
  :init
    (add-hook 'web-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'rust-mode-hook #'rainbow-delimiters-mode))

(add-hook 'rust-mode-hook 'linum-mode)

(defvar my-packages '(better-defaults
                       multiple-cursors
                       counsel
                       ein
                       py-autopep8
                       flx
                       highlight-symbol
                       avy
                       projectile
                       counsel-projectile
                       ag
                       parinfer
                       paredit
                       rainbow-delimiters
                       clojure-mode
                       clojure-mode-extra-font-locking
                      cider
                      org
                      markdown-mode
                      rainbow-mode
                      elpy
                      evil-cleverparens
                      material-theme
                      ))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


(global-set-key (kbd "C-M-i") 'flyspell-auto-correct-word)

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(recentf-mode 1)
(setq-default recent-save-file "~/.emacs.d/recentf") 
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 100)

(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol))

(setq ein:console-executable "/usr/local/bin/ipython3")

;; (add-hook
;;  'rust-mode-hook
;;  (lambda ()
;;    (racer-mode)
;;    (setq-local eldoc-documentation-function #'ignore)))

 ;; Automatically reload files was modified by external program
(global-auto-revert-mode 1)
;; and display "half modal" warning about it
; (require 'w32-msgbox)
; (defun inform-revert-modified-file (&optional p1 p2)
;   "bdimych custom function"
;   (let ((revert-buffer-function nil))
;     (revert-buffer p1 p2)
;     (w32-msgbox (buffer-file-name) "Emacs: Modified file automatically reverted" 'vb-ok-only 'vb-information nil t)))
;
;; (setq revert-buffer-function 'inform-revert-modified-file)

;; set up ivy completion
(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
	("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "%d/%d ")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
	'((t . ivy--regex-plus))))

(counsel-projectile-mode)

;; set up markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; set up org mode
(setq org-startup-indented t)
(setq org-startup-folded "overview")
(setq org-directory "~/Dropbox/org/")
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook 'org-bullets-mode)

;; (setq org-agenda-files '("~/gtd"))
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

;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

;; general
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))
(save-place-mode 1)

(setq ring-bell-function 'ignore)

(define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)

;; clojure environment
(setq org-babel-clojure-backend 'cider)
;; (load "clojure.el")

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; Set the default comment column to 70
(setq-default comment-column 70)

;; (add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'evil-cleverparens-mode)
(add-hook 'clojure-mode-hook 'linum-mode)

(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))))

;; special indents
(define-clojure-indent
  (go-loop-sub '(:defn))
  (async '(:defn))
	(or-join '(:defn))
	(not-join '(:defn))
	(loop '(:defn))
	(try '(:defn))
	(exec! '(:defn)))

;;;;
;; Cider
;;;;

(require 'cider)

;; Enter cider mode when entering the clojure major mode
(add-hook 'clojure-mode-hook 'cider-mode)

(global-set-key [f9] 'cider-jack-in)
(global-set-key [f10] 'cider-jack-in-clojurescript)

;; Turn on auto-completion with Company-Mode
(global-company-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(global-company-mode)

(setq company-minimum-prefix-length 1)

;; (require 'company-ycmd)
;; (company-ycmd-setup)

(eval-after-load 'company
  '(add-to-list 'company-frontends 'company-tng-frontend))


(setq help-window-select t)

(eval-after-load 'company
  '(progn
     (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
     (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)))

;;Exit insert mode by pressing j and then j quickly
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

(key-chord-define evil-insert-state-map "$1" "(")
(key-chord-define evil-insert-state-map "$2" "[")
(key-chord-define evil-insert-state-map "$3" "{")

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook #'eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
;; (setq cider-show-error-buffer t)
;; (setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable parinfer in your REPL
;; (add-hook 'cider-repl-mode-hook #'parinfer-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))


;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))



(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-c C-k") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))

(add-hook 'cider-repl-mode-hook #'eldoc-mode)


;; better figwheel integration
(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")


(evil-cleverparens-mode 1)

(defun paredit-wiggle-back ()
  (paredit-forward)
  (paredit-backward))

(defmacro defparedit-wrapper (name invoked-wrapper)
  `(defun ,name ()
     (interactive)
     (paredit-wiggle-back)
     (,invoked-wrapper)))

(defparedit-wrapper back-then-wrap paredit-wrap-sexp)
(defparedit-wrapper back-then-wrap-square paredit-wrap-square)
(defparedit-wrapper back-then-wrap-curly paredit-wrap-curly)
(defparedit-wrapper back-then-wrap-angled paredit-wrap-angled)
(defparedit-wrapper back-then-wrap-doublequote paredit-meta-doublequote)

(define-key evil-normal-state-map ";W" 'back-then-wrap)
(define-key evil-normal-state-map ";w]" 'back-then-wrap-square)
(define-key evil-normal-state-map ";w}" 'back-then-wrap-curly)
(define-key evil-normal-state-map ";w>" 'back-then-wrap-angled)
(define-key evil-normal-state-map ";w\"" 'back-then-wrap-doublequote)

(define-key evil-normal-state-map ";S" 'paredit-splice-sexp)
(define-key evil-normal-state-map ";A" 'paredit-splice-sexp-killing-backward)
(define-key evil-normal-state-map ";D" 'paredit-splice-sexp-killing-forward)
(define-key evil-normal-state-map ";|" 'paredit-split-sexp)
(define-key evil-normal-state-map ";J" 'paredit-join-sexps)
(define-key evil-normal-state-map ",;<" 'paredit-backward-slurp-sexp)
(define-key evil-normal-state-map ";," 'paredit-backward-barf-sexp) 
(define-key evil-normal-state-map ";>" 'paredit-forward-slurp-sexp)
(define-key evil-normal-state-map ";." 'paredit-forward-barf-sexp) 
(define-key evil-normal-state-map ";~" 'paredit-convolute-sexp)

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
    ("62c81ae32320ceff5228edceeaa6895c029cc8f43c8c98a023f91b5b339d633f" "a5956ec25b719bf325e847864e16578c61d8af3e8a3d95f60f9040d02497e408" "f27c3fcfb19bf38892bc6e72d0046af7a1ded81f54435f9d4d09b3bff9c52fc1" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" default)))
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
    (dash-functional pretty-symbols pretty-mode-plus pretty-mode oceanic-theme night-owl-theme tango-2-theme tango-plus-theme sublime-themes sublime flatui-dark-theme org-bullets evil-org groovy-mode rg geiser hl-todo racer company-ycmd ycmd jedi ipython aggressive-indent deadgrep github-theme pyenv-mode autopair evil-mc xclip use-package rust-mode rainbow-mode rainbow-delimiters powerline parinfer neotree multiple-cursors monokai-theme material-theme markdown-mode hlinum highlight-symbol helm-projectile helm-ag gruvbox-theme gruber-darker-theme flx flatland-theme evil-visual-mark-mode evil-terminal-cursor-changer evil-surround evil-smartparens evil-paredit evil-magit evil-leader evil-indent-textobject evil-easymotion evil-commentary evil-cleverparens drag-stuff dracula-theme counsel-projectile company color-theme-sanityinc-tomorrow clojure-mode-extra-font-locking cider better-defaults auto-complete ag ace-jump-mode ace-jump-buffer)))
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

