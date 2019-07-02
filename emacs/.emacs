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
;"~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src/libstd/"  (setenv "PATH" (concat (getenv "PATH") ":/opt/local/bin"))
;; (setenv "FZF_DEFAULT_COMMAND" "rg --files --hidden --follow --glob "!.git/*"")
(setenv "PKG_CONFIG_PATH" "/usr/local/Cellar/zlib/1.2.11/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig:/usr/local/opt/libffi/lib/pkgconfig")

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
											org-pdfview
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
(setq evil-disable-insert-state-bindings t)
;; (setq evil-want-keybinding nil)
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

;; (use-package evil-collection
;;   :ensure t
;;   :config
;;   (evil-collection-init))

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

;; (define-key evil-normal-state-map (kbd "M-.")
;;   `(menu-item "" evil-repeat-pop :filter
;;               ,(lambda (cmd) (if (eq last-command 'evil-repeat-pop) cmd))))

;; Use words and symbols
;; (with-eval-after-load 'evil
;;     (defalias #'forward-evil-word #'forward-evil-symbol))

(add-hook 'org-capture-mode-hook 'evil-insert-state)


;;;;
;; AUTO-COMPLETION
;;;;
(global-company-mode)
(setq company-minimum-prefix-length 1)
(eval-after-load 'company
  '(add-to-list 'company-frontends 'company-tng-frontend))
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

;;;;
;; MARKDOWN
;;;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/local/bin/pandoc"))

(add-hook 'markdown-mode-hook 'auto-fill-mode)

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
;; TIKZ
;;;;
; (add-to-list 'org-latex-packages-alist
;              '("" "tikz" t))
;
; (eval-after-load "preview"
;   '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))
;

;;;;
;; MINTED
;;;;
(setq org-latex-create-formula-image-program 'imagemagick)
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;;;;
;; IVY
;;;;

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
				'((t . ivy--regex-plus)))
  ;; extend ivy-bibtex
	)

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

(defun bibtex-completion-finder (entry)
  (let ((pdf (bibtex-completion-find-pdf entry)))
    (call-process "open" nil 0 nil "-R" (car pdf))))

(defun bibtex-completion-skim (entry)
  (let ((pdf (bibtex-completion-find-pdf entry)))
    (call-process "open" nil 0 nil "-a" "skim" (car pdf))))

(use-package org-ref :ensure t
	:config
	(setq org-ref-bibliography-notes "~/Dropbox/org/ref/notes.org"
				org-ref-default-bibliography '("~/Dropbox/org/ref/master.bib" "~/Dropbox/University/ETH/Master/Masterthesis/thesis.bib")
				org-ref-pdf-directory "~/Dropbox/org/ref/pdfs/")

	(setq bibtex-completion-bibliography '("~/Dropbox/org/ref/master.bib"  "~/Dropbox/University/ETH/Master/Masterthesis/thesis.bib")
				bibtex-completion-library-path "~/Dropbox/org/ref/pdfs"
				bibtex-completion-notes-path "~/Dropbox/org/ref/notes.org")

	;; (ivy-add-actions
	;;  'ivy-bibtex
	;;  '(("P" bibtex-completion-pdf "Open pdf with mac's preview")
	;; 	 ("S" bibtex-completion-skim "Open pdf with skim")
	;; 	 ("F" bibtex-completion-finder "Open finder on the pdf")))

	(setq org-latex-pdf-process
				'("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
					"bibtex %b"
					"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
					"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

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
;; DIRED
;;;;

;; Sort dired buffer differently
(add-hook 'dired-load-hook
          (lambda () (require 'dired-sort-menu)))

;;;;
;; ORG MODE
;;;;
;; (setq org-startup-indented t)
(setq org-startup-folded "overview")
(setq org-directory "~/Dropbox/org/")
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook 'org-bullets-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

;; (setq org-ellipsis "  ")
;; (setq org-bullets-bullet-list '("⬢" "◆" "▲" "■"))

(require 'org-protocol)
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

(global-set-key (kbd "<f12>") 'org-agenda)


;; We do not want to indent when pressing o in org-mode
(add-hook 'org-mode-hook
					(lambda ()
						(make-local-variable 'evil-auto-indent)
						(setq evil-auto-indent nil)))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)

;; Enable org-pdf-view
(eval-after-load 'org '(require 'org-pdfview))

(defun capture-comment-line (&optional line)
  (let ((c
        (save-excursion
          (save-window-excursion
            (switch-to-buffer (plist-get org-capture-plist :original-buffer))
          comment-start)
          )))
    (while (string-prefix-p c line)
      (setq line (string-remove-prefix c line)))
    (comment-string-strip line t t))) 

;; AGENDA
(setq org-agenda-files '("~/Dropbox/org/"
												 "~/Dropbox/org/clients/"
												 "~/Dropbox/org/projects/"
                         "~/Dropbox/org/ref/notes.org"))



;; Set default column view headings: Task Total-Time Time-Stamp
(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

;; Dim blocked tasks (and other settings)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-inhibit-startup nil)
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view (disabled)
(setq org-agenda-compact-blocks nil)

(setq org-deadline-warning-days 30)


;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              (" " "Agenda"
               ((agenda "" ((org-agenda-overriding-header "Today's Schedule:")
														(org-agenda-span 'day)
														(org-agenda-ndays 1)
														(org-agenda-skip-scheduled-if-done t)
														(org-agenda-skip-deadline-if-done t)
														(org-agenda-start-on-weekday nil)
														(org-agenda-start-day "+0d")
														(org-agenda-show-all-dates t)
														(org-agenda-todo-ignore-deadlines nil)))
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED-REFILE/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
								(tags-todo "-CANCELLED/!WAITING|HOLD"
                           ((org-agenda-overriding-header "Waiting and Postponed Tasks")
														(org-tags-match-list-sublevels nil)))
								(agenda "" ((org-agenda-use-time-grid nil)))
								(tags-todo "-CANCELLED-REFILE/!TODO"
                           ((org-agenda-overriding-header (concat "Projects"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
								)
               nil))))

;; CAPTURE

(setq org-default-notes-file "~/Dropbox/org/inbox.org")

(add-to-list 'load-path "~/.emacs.d/customizations")
(load "org-helper.el")

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/Dropbox/org/inbox.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
							("r" "respond" entry (file "~/Dropbox/org/inbox.org")
               "* NEXT Respond to %? on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t)
              ("n" "note" entry (file "~/Dropbox/org/inbox.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry  (file "~/Dropbox/org/inbox.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
							("i" "Idea" entry (file org-default-notes-file)
							 "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file "~/Dropbox/org/inbox.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/Dropbox/org/inbox.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t))))

(setq org-refile-targets '((org-agenda-files :maxlevel . 9)))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))


(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-support-shift-select 'always)

(org-babel-do-load-languages 'org-babel-load-languages
    '((shell . t)))

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

(add-to-list 'evil-insert-state-modes 'deft-mode)

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
  (insert (format-time-string "%Y-%m-%d %A")))


(defun insert-current-week (arg)
  (interactive "P")
  (insert (format-time-string "Week %U")))

;;;;
;; PROJECTILE
;;;;

(use-package projectile
  :ensure projectile
  :config
  ;; (setq projectile-indexing-method 'git)
	;; (setq projectile-globally-ignored-directories
	;; 			(append '(
	;; 								".git"
	;; 								".svn"
	;; 								"ltximg"
	;; 								)
	;; 							projectile-globally-ignored-directories))
  (projectile-mode +1))

(counsel-projectile-mode)

;;;;
;; CUSTOMIZATION
;;;;

;; Go to scratch buffer and clear contents
(defun clear-scratch nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
	(erase-buffer)
  (lisp-interaction-mode))

;; yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't know why but I have to do it here otherwise stuff breaks
(require 'cider)

;; Load all the things
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

(setq custom-file (concat user-emacs-directory ".custom.el")) ; tell Customize to save customizations to ~/.emacs.d/.custom.el
(ignore-errors                                                ; load customizations from ~/.emacs.d/.custom.el
  (load-file custom-file))
