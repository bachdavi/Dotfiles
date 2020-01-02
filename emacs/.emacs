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
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin" ":/Users/david/.ghcup/bin"))
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
(defvar my-packages '(ace-window
                      aggressive-indent
                      auctex
                      avy
                      better-defaults
                      cider
                      clojure-mode
                      clojure-mode-extra-font-locking
                      company-auctex
                      counsel
                      counsel-projectile
                      deft
                      ein
                      elpy
                      evil
                      evil-cleverparens
                      fixme-mode
                      flx
                      flycheck
                      flycheck-inline
                      flyspell
                      flyspell-popup
                      gnuplot-mode
                      highlight-symbol
                      hledger-mode
                      ipython
                      ivy
                      ivy-bibtex
                      jedi
                      julia-mode
                      jupyter
                      key-chord
                      markdown-mode
                      ob-ipython
                      org
                      org-bullets
                      org-journal
                      org-pdfview
                      org-ref
                      pdf-tools
                      projectile
                      py-autopep8
                      python-mode
                      rainbow-delimiters
                      rainbow-mode
                      rg
                      rustic
                      smex))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;;;;
;; GENERIC
;;;;
(fixme-mode 1)

(setq-default indent-tabs-mode nil)

;;;;
;; EVIL
;;;;
(use-package evil
  :ensure t
  :init
  (setq evil-vsplit-window-right t)
  (setq evil-disable-insert-state-bindings t)
  (setq evil-auto-indent t)
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
;; HLEDGER
;;;;
;;; Basic configuration
(require 'hledger-mode)

(add-to-list 'evil-insert-state-modes 'hledger-mode)
(add-to-list 'evil-insert-state-modes 'hledger-view-mode)

;; To open files with .journal extension in hledger-mode
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

;; Provide the path to you journal file.
;; The default location is too opinionated.
(setq hledger-jfile "~/Dropbox/Finances/hledger.journal")

;; For company-mode users,
(add-to-list 'company-backends 'hledger-company)

(add-hook 'hledger-mode-hook 'company-mode)

(defun hledger/next-entry ()
  "Move to next entry and pulse."
  (interactive)
  (hledger-next-or-new-entry)
  (hledger-pulse-momentary-current-entry))

(defface hledger-warning-face
  '((((background dark))
     :background "Red" :foreground "White")
    (((background light))
     :background "Red" :foreground "White")
    (t :inverse-video t))
  "Face for warning"
  :group 'hledger)

(defun hledger/prev-entry ()
  "Move to last entry and pulse."
  (interactive)
  (hledger-backward-entry)
  (hledger-pulse-momentary-current-entry))

;; Personal Accounting
(global-set-key (kbd "C-c e") 'hledger-jentry)
(global-set-key (kbd "C-c j") 'hledger-run-command)

(define-key hledger-mode-map (kbd "M-p") #'hledger/prev-entry)
(define-key hledger-mode-map (kbd "M-n") #'hledger/next-entry)

(add-hook 'hledger-input-mode-hook 'auto-fill-mode)

(add-hook 'hledger-input-mode-hook
					(lambda ()
						(make-local-variable 'company-idle-delay)
						(setq-local company-idle-delay 0.1)))

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
(add-hook 'markdown-mode-hook 'flyspell-mode)

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
;; (add-to-list 'org-latex-packages-alist
;;              '("" "tikz" t))

(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

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
(defun org-ref-rescan-labels nil
		"Rescan buffer local labels."
		(interactive)
		(save-excursion
			(org-ref-add-labels (point-min) (point-max)))
		(reverse org-ref-labels))

;; This is necessary for the resolution of image labels when exporting
;; to latex from org mode.
;; use `org-ref-insert-link' to auto complete all figures.
(setq org-latex-prefer-user-labels t)

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
		org-ref-default-bibliography '("~/Dropbox/org/ref/master.bib")
		org-ref-pdf-directory "~/Dropbox/org/ref/pdfs/")

  (setq bibtex-completion-bibliography '("~/Dropbox/org/ref/master.bib")
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

(setq org-image-actual-width '(550))

;;;;
;; PDF
;;;;
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

;;;;
;; SPELLING
;;;;
(setq ispell-program-name "aspell")

(define-key flyspell-mode-map (kbd "C-c $") #'flyspell-popup-correct)
(add-hook 'flyspell-mode 'auto-fill-mode)

;; Set shortcut for auto correction
(global-set-key (kbd "C-M-i") 'flyspell-auto-correct-word)

;; Use flyspell for comments
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Activate this to only check comments!
;; (setq flyspell-prog-text-faces
;;       (delq 'font-lock-string-face
;;             flyspell-prog-text-faces))

;;;;
;; FLYCHECK
;;;;
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;;;;
;; DIRED
;;;;
(require 'dired-x)

;; Sort dired buffer differently
(add-hook 'dired-load-hook
          (lambda () (require 'dired-sort-menu)))

;;;;
;; ORG MODE
;;;;
(setq org-startup-folded "overview")
(setq org-directory "~/Dropbox/org/")
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook 'org-bullets-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(require 'org-inlinetask)

(setq org-log-done 'time)

;; Local variable evaluation.
(setq enable-local-eval t)

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
						 "~/Projects/anarres/project.org"
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

(setq org-deadline-warning-days 7)

;; How to show tasks
(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
								 (timeline . "  % s")
								 (todo .
									   " %i %?-12(concat \"[ \"(org-format-outline-path (list (nth 1 (org-get-outline-path)))) \" ]\") ")
								 (tags .
									   " %i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
								 (search . " %i %-12:c")))

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
                (tags "+REFILE-noexport"
                      ((org-agenda-overriding-header "Tasks to Refile")
											 (org-agenda-prefix-format " %i %-12:c%l%s")
											 (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED-REFILE-Research/!NEXT|CURRENT"
                           ((org-agenda-overriding-header (concat "Project Next & Current Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
														(org-agenda-prefix-format " %i %-15:c%l%s")
														(org-tags-match-list-sublevels nil)))
								(tags-todo "-CANCELLED/!WAITING|HOLD"
													 ((org-agenda-overriding-header "Waiting and Postponed Tasks")
														(org-agenda-prefix-format " %i %-22:c%l%s")
														(org-tags-match-list-sublevels nil)))
								(tags "IDEA"
											((org-agenda-overriding-header "Current Ideas")
											 (org-agenda-prefix-format " %i %-4:c%l%s")
											 (org-tags-match-list-sublevels nil)))
								(agenda "" ((org-agenda-use-time-grid nil)
														(org-agenda-skip-function '(eoxxs/org-agenda-skip-tag "Reoccurring" nil))))
								(tags-todo "-CANCELLED-REFILE/!TODO"
													 ((org-agenda-overriding-header (concat "Tasks"
																																	(if bh/hide-scheduled-and-waiting-next-tasks
																																			""
																																		" (including WAITING and SCHEDULED tasks)")))
														(org-tags-match-list-sublevels t)
														(org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
														(org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
														(org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
														(org-agenda-sorting-strategy
														 '(todo-state-down effort-up category-keep))))
								(tags "-REFILE-MEETING-Research-Outline/"
											((org-agenda-overriding-header "Tasks to Archive")
											 (org-agenda-skip-function 'skip-non-archivable-tasks)
											 (org-tags-match-list-sublevels nil))))
               nil))))

(defun eoxxs/org-agenda-skip-tag (tag &optional others)
  "Skip all entries that correspond to TAG.
         If OTHERS is true, skip all entries that do not
         correspond to TAG."
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
				(current-headline (or (and (org-at-heading-p)
																	 (point))
															(save-excursion (org-back-to-heading)))))
	(if others
		(if (not (member tag (org-get-tags-at current-headline)))
			next-headline
		  nil)
	  (if (member tag (org-get-tags-at current-headline))
		  next-headline
		nil))))

(defun skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
	(widen)
	;; Consider only tasks with done todo headings as archivable candidates
	(let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
		  (subtree-end (save-excursion (org-end-of-subtree t))))
	  (if (member (org-get-todo-state) org-todo-keywords-1)
		  (if (member (org-get-todo-state) org-done-keywords)
			  (let* (
					 (daynr (string-to-number (format-time-string "%d" (current-time))))
					 (a-month-ago (* 60 60 24 (+ daynr 1)))
					 (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
					 (this-month (format-time-string "%Y-%m-" (current-time)))
					 (subtree-is-current (save-excursion
										   (forward-line 1)
										   (and (< (point) subtree-end)
														(or
														 (re-search-forward this-month subtree-end t)
														 (re-search-forward last-month subtree-end t))))))
					(if subtree-is-current
					subtree-end ; Has a date in this month or last month, skip it
				  nil))  ; available to archive
			(or subtree-end (point-max)))
		next-headline))))

;; CAPTURE
(setq org-default-notes-file "~/Dropbox/org/refile.org")


(add-to-list 'load-path "~/.emacs.d/customizations")
(load "org-helper.el")

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "Todo" entry (file "~/Dropbox/org/refile.org")
               "* TODO %?\n%U\n" :clock-in t :clock-resume t)
              ("c" "Code Todo" entry (file "~/Dropbox/org/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "Respond" entry (file "~/Dropbox/org/refile.org")
               "* NEXT Respond to %? on %:subject\nSCHEDULED: %t\n%U\n" :clock-in t :clock-resume t)
              ("n" "Note" entry (file "~/Dropbox/org/refile.org")
               "* %? :NOTE:\n%U\n" :clock-in t :clock-resume t)
              ("i" "Idea" entry (file org-default-notes-file)
               "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file "~/Dropbox/org/refile.org")
               "* MEETING %u %? :MEETING:\n** Summary\n** Attendees\n** Questions\n** Notes\n** Actions\n" :clock-in t :clock-resume t))))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-refile-targets '((org-agenda-files :maxlevel . 9)))

																				; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

																				; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "CURRENT(u)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("CURRENT" :foreground "orange" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("INACTICE" :foreground "magenta" :weight bold))))
;;;;
;; BABEL
;;;;
(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

;; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
	(condition-case nil
			(org-display-inline-images)
		(error nil)))

(setq org-edit-src-content-indentation 0
			org-src-tab-acts-natively t
			org-src-fontify-natively t
			org-confirm-babel-evaluate nil
			org-support-shift-select 'always)

(setq inferior-julia-program-name "/usr/local/bin/julia")

(add-hook 'ob-async-pre-execute-src-block-hook
          '(lambda ()
             (setq inferior-julia-program-name "/usr/local/bin/julia")))

;; Org Babel julia support
(load "ob-julia.el")

(org-babel-do-load-languages 'org-babel-load-languages
														 '((shell . t)
															 (gnuplot . t)
															 (julia . t)
															 (dot . t)
															 (latex . t)
															 (ipython . t)))

(setq org-babel-python-command "python3")


(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.75))

(defun my-org-archive-done-tasks ()
	(interactive)
	(org-map-entries 'org-archive-subtree "/DONE/CANCELLED" 'file))

(setq org-publish-project-alist
			'(("blog"
				 :base-directory "~/Dropbox/Blog/"
				 :base-extension "org"
				 :publishing-directory "~/Projects/eoxxs.github.io/_posts/"
				 :publishing-function org-jekyll-md-export-to-md)))

;;;;
;; ACE-WINDOW
;;;;
(global-set-key (kbd "M-o") 'ace-window)

;;;;
;; YASNIPPET
;;;;
(setq yas-snippet-dirs
			'("~/.emacs.d/snippets"))

(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.

;;;;
;; HYDRA
;;;;

(global-set-key
 (kbd "C-M-o")
 (defhydra hydra-window ()
	 "window"
	 ("h" windmove-left)
	 ("j" windmove-down)
	 ("k" windmove-up)
	 ("l" windmove-right)
	 ("v" (lambda ()
					(interactive)
					(split-window-right)
					(windmove-right))
		"vert")
	 ("x" (lambda ()
					(interactive)
					(split-window-below)
					(windmove-down))
		"horz")
	 ("o" delete-other-windows "one" :color blue)
	 ("a" ace-window "ace")
	 ("s" ace-swap-window "swap")
	 ("d" delete-window "del")
	 ("i" ace-maximize-window "ace-one" :color blue)
	 ("b" ivy-switch-buffer "buf")
	 ("q" nil "cancel")))

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
;; MAGIT
;;;;
(add-to-list 'evil-insert-state-modes 'git-timemachine-mode)

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
									"_files/"
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
;; DIARY
;;;;
(setq diary-file "~/Dropbox/org/diary")
(setq org-agenda-include-diary t)
(setq diary-display-function 'diary-fancy-display)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)

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
	(projectile-mode +1))

(counsel-projectile-mode)

;;;;
;; ELFEED
;;;;
(setq elfeed-feeds
			'("http://cachestocaches.com/feed/"
				"http://nullprogram.com/feed/"  
				"https://thume.ca/atom.xml"
				"https://jvns.ca/atom.xml"
				"https://oremacs.com/atom.xml"
				"https://rjlipton.wordpress.com/feed/"
				"http://lambda-the-ultimate.org/rss.xml"
				"https://danluu.com/atom.xml"
				"https://dragan.rocks/feed.xml"
				"https://www.scottaaronson.com/blog/?feed=rss2"
				"http://matt.might.net/articles/feed.rss"))

;;;;
;; FUNCTIONS
;;;;
(defun org-include-img-from-pdf (&rest _)
	"Convert pdf files to image files in org-mode bracket links.

    # ()convertfrompdf:t # This is a special comment; tells that the upcoming
                         # link points to the to-be-converted-to file.
    # If you have a foo.pdf that you need to convert to foo.png, use the
    # foo.png file name in the link.
    [[./foo.png]]
"
	(interactive)
	(if (executable-find "convert")
			(save-excursion
				(goto-char (point-min))
				(while (re-search-forward "^[ \t]*#\\s-+()convertfrompdf\\s-*:\\s-*t"
																	nil :noerror)
					;; Keep on going to the next line till it finds a line with bracketed
					;; file link.
					(while (progn
									 (forward-line 1)
									 (not (looking-at org-bracket-link-regexp))))
					;; Get the sub-group 1 match, the link, from `org-bracket-link-regexp'
					(let ((link (match-string-no-properties 1)))
						(when (stringp link)
							(let* ((imgfile (expand-file-name link))
										 (pdffile (expand-file-name
															 (concat (file-name-sans-extension imgfile)
																			 "." "pdf")))
										 (cmd (concat "convert -density 96 -quality 85 "
																	pdffile " " imgfile)))
								(when (and (file-readable-p pdffile)
													 (file-newer-than-file-p pdffile imgfile))
									;; This block is executed only if pdffile is newer than
									;; imgfile or if imgfile does not exist.
									(shell-command cmd)
									(message "%s" cmd)))))))
		(user-error "`convert' executable (part of Imagemagick) is not found")))

(defun clear-scratch nil
	"create a scratch buffer"
	(interactive)
	(switch-to-buffer (get-buffer-create "*scratch*"))
	(erase-buffer)
	(lisp-interaction-mode))

;;;;
;; CUSTOMIZATION
;;;;
;; yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't know why but I have to do it here otherwise stuff breaks
(require 'cider)

(require 'org-mac-link)

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

;; rust environment
(load "julia.el")

;; haskell environment
(load "haskell.el")

;; Calibre query
(load "books.el")

;; Org Subfigure
(load "ox-latex-subfigure.el")

;; Dired+
;; (load "dired+.el")

(add-hook 'org-mode-hook (lambda () 
													 (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

(setq custom-file (concat user-emacs-directory ".custom.el")) ; tell Customize to save customizations to ~/.emacs.d/.custom.el
(ignore-errors                                                ; load customizations from ~/.emacs.d/.custom.el
	(load-file custom-file))
(put 'erase-buffer 'disabled nil)
