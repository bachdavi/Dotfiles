;; Emacs configuration file
;; Author: David Bach
;; 20-12-2018

;; set up package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
(package-initialize)

;; (add-to-list 'exec-path "/opt/local/bin")
;; (add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;; (setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin"))
;; (setenv "FZF_DEFAULT_COMMAND" "rg --files --hidden --follow --glob "!.git/*"")

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
                      smex
											deft
                      org
											org-ref
											org-bullets
											org-journal
											key-chord
                      ivy
                      ivy-bibtex
                      counsel
                      counsel-projectile
                      projectile
                      avy
											rg
                      rainbow-delimiters
                      highlight-symbol
                      flx
                      evil
                      evil-cleverparens
											python-mode
											ipython
											elpy
											jedi
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
											flycheck-pycheckers
											flycheck-mypy
											auctex
											company-auctex
                      pdf-tools
											aggressive-indent
											flyspell
											flyspell-popup))

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
;; (setq evil-auto-indent nil)
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
(setq evil-motion-state-cursor 'box)  
(setq evil-visual-state-cursor 'box)  
(setq evil-normal-state-cursor 'box) 
(setq evil-insert-state-cursor 'bar)
(setq evil-emacs-state-cursor  'hbar)
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
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
;; Use tab
;; (eval-after-load 'company
;;   '(progn
;;      (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
;;      (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)))

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
;; RECOLL
;;;;
(defun counsel-recoll-function (string &rest _unused)
  "Issue recoll for STRING."
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (counsel--async-command
     (format "recoll -t -b '%s'" string))
    nil))

(defun counsel-recoll (&optional initial-input)
  "Search for a string in the recoll database.
You'll be given a list of files that match.
Selecting a file will launch `swiper' for that file.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (ivy-read "recoll: " 'counsel-recoll-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-git-grep-history
            :action (lambda (x)
                      (when (string-match "file://\\(.*\\)\\'" x)
                        (let ((file-name (match-string 1 x)))
                          (find-file file-name)
                          (unless (string-match "pdf$" x)
                            (swiper ivy-text)))))))
;;;;
;; LATEX
;;;;
; (add-to-list 'exec-path "/Library/TeX/texbin/")
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))

;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background  
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

;;;;
;; BIBTEX
;;;;

(autoload 'ivy-bibtex "ivy-bibtex" "" t)
;; ivy-bibtex requires ivy's `ivy--regex-ignore-order` regex builder, which
;; ignores the order of regexp tokens when searching for matching candidates.
;; Add something like this to your init file:
(setq ivy-re-builders-alist
      '((ivy-bibtex . ivy--regex-ignore-order)
        (t . ivy--regex-plus)))

(defun bibtex-completion-pdf (entry)
  (let ((pdf (bibtex-completion-find-pdf entry)))
    (call-process "open" nil 0 nil (car pdf))))

(defun bibtex-completion-skim (entry)
  (let ((pdf (bibtex-completion-find-pdf entry)))
    (call-process "open" nil 0 nil "-a" "skim" (car pdf))))

;; (use-package ivy
;; 	:ensure t
;; 	:config
;; 	(ivy-add-actions
;; 	 'ivy-bibtex
;; 	 '(("P" bibtex-completion-pdf "Open pdf with mac's preview")
;; 		 ("S" bibtex-completion-skim "Open pdf with skim"))))

(use-package org-ref :ensure t
	:config
	(setq org-ref-bibliography-notes "~/Dropbox/org/ref/notes.org"
				org-ref-default-bibliography '("~/Dropbox/org/ref/master.bib")
				org-ref-pdf-directory "~/Dropbox/org/ref/pdfs/")

	(setq bibtex-completion-bibliography "~/Dropbox/org/ref/master.bib"
				bibtex-completion-library-path "~/Dropbox/org/ref/pdfs"
				bibtex-completion-notes-path "~/Dropbox/org/ref/notes.org")

	(setq org-latex-pdf-process
				'("pdflatex -interaction nonstopmode -output-directory %o %f"
					"bibtex %b"
					"pdflatex -interaction nonstopmode -output-directory %o %f"
					"pdflatex -interaction nonstopmode -output-directory %o %f")))

(defun bibtex-completion-format-citation-org (keys)
  "Formatter for ebib references."
  (s-join ", "
          (--map (format "cite:%s" it) keys)))

(setq bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-org)
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
    (default       . bibtex-completion-format-citation-default)))

(setq org-image-actual-width '(500))

;;;;
;; PDF
;;;;
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
	(setq-default pdf-view-display-size 'fit-page))

;; (eval-after-load "tex"
;;   '(progn
;; 		 (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
;; 		 (setq tex-view-program-list '(("pdf-tools" "tex-pdf-tools-sync-view")))
;; 		 (add-to-list 'TeX-view-program-list
;; 									'("pdf-tools" TeX-pdf-tools-sync-view))))

;;;;
;; SPELLING
;;;;
(setq ispell-program-name "aspell")
;; (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))

(define-key flyspell-mode-map (kbd "C-c $") #'flyspell-popup-correct)
(add-hook 'flyspell-mode 'auto-fill-mode)

;; Set shortcut for auto correction
(global-set-key (kbd "C-M-i") 'flyspell-auto-correct-word)

;;;;
;; FLYCHECK
;;;;
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;;;;
;; ORG MODE
;;;;
;; (setq org-startup-indented t)
(setq org-startup-folded "overview")
(setq org-directory "~/Dropbox/org/")
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook 'org-bullets-mode)

(setq org-agenda-files '("~/Dropbox/org/gtd/inbox.org"
                         "~/Dropbox/org/gtd/gtd.org"
                         "~/Dropbox/org/gtd/tickler.org"
                         "~/Dropbox/org/ref/notes.org"))

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Dropbox/org/gtd/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Dropbox/org/gtd/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets '(("~/Dropbox/org/gtd/gtd.org" :maxlevel . 1)
                           ("~/Dropbox/org/gtd/someday.org" :level . 1)
                           ("~/Dropbox/org/gtd/tickler.org" :maxlevel . 1)))

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

(setq org-agenda-custom-commands 
      '(("c" "Clockworks" tags-todo "Clockworks"
         ((org-agenda-overriding-header "Clockworks")))
				("d" "3DF" tags-todo "3DF"
         ((org-agenda-overriding-header "3DF")))
				("e" "ETH" tags-todo "ETH"
         ((org-agenda-overriding-header "ETH")))))

(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-support-shift-select 'always)

(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.75))

(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

;;;;
;; DEFT
;;;;
(use-package deft
  :ensure t
	:init (setq deft-directory "~/Dropbox/org/deft"
							deft-extensions '("org" "txt" "tex")
							deft-use-filename-as-title t
							deft-text-mode 'org-mode
							deft-auto-save-interval 0)
	:config
	(global-set-key [f8] 'deft))

;;;;
;; FZF
;;;;
(setenv "FZF_DEFAULT_COMMAND"
				(string-trim (shell-command-to-string ". ~/.zshrc; echo -n $FZF_DEFAULT_COMMAND")))

;;;;
;; SCREENSHOT
;;;;
(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  ; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
  ; insert into file if correctly taken
  (if (file-exists-p filename)
    (insert (concat "[[file:" filename "]]"))))

;;;;
;; JOURNAL
;;;;
(setq org-journal-dir "~/Dropbox/org/journal")

(setq org-journal-file-format "%Y%m%d.org")

;;;;
;; MISC
;;;;
(defun insert-todays-date (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d-%m-%Y")
            (format-time-string "%Y-%m-%d "))))


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
(load "org-recoll.el")

;; clojure environment
(setq org-babel-clojure-backend 'cider)
(load "clojure.el")

;; python environment
(load "python-conf.el")

;; rust environment
(load "rust.el")

;; Calibre query
(load "books.el")

(add-hook 'org-mode-hook (lambda () 
  (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-mode t)
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(org-file-apps
	 (quote
		((auto-mode . emacs)
		 ("\\.mm\\'" . default)
		 ("\\.x?html?\\'" . default)
		 ("\\.pdf\\'" . emacs))))
 '(org-modules
	 (quote
		(org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m org-mac-link)))
 '(package-selected-packages
	 (quote
		(0blayout org-journal ebib pdfgrep dracula-theme night-owl-theme interleave deft flyspell-popup flycheck-pycheckers aggressive-indent use-package smex rg rainbow-mode rainbow-delimiters racer python-mode pyenv-mode py-autopep8 org-ref org-bullets neotree ivy-bibtex ipython hlinum highlight-symbol flycheck-rust flycheck-inline flx evil-terminal-cursor-changer evil-surround evil-mc evil-leader evil-indent-textobject evil-commentary evil-cleverparens elpy ein drag-stuff counsel-projectile clojure-mode-extra-font-locking cider cargo better-defaults avy auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
