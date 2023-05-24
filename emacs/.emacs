;; Emacs configuration file
;; Author: David Bach
;; 17-11-2021

;; set up package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)

(package-initialize)

(add-to-list 'exec-path "/opt/homebrew/bin/")
(add-to-list 'exec-path "/Applications/Julia-1.8.app/Contents/Resources/julia/bin/")
(add-to-list 'exec-path "/Library/TeX/texbin/")
(setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin"))

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
                      add-node-modules-path
                      aggressive-indent
                      async
                      auctex
                      avy
                      better-defaults
                      calibredb
                      cider
                      clojure-mode
                      clojure-mode-extra-font-locking
                      company-auctex
                      counsel
                      counsel-projectile
                      deft
                      ein
                      eglot
                      elpy
                      elfeed
                      evil
                      evil-cleverparens
                      flx
                      flycheck
                      flycheck-inline
                      flyspell
                      flyspell-popup
                      gnuplot-mode
                      git-timemachine
                      highlight-symbol
                      ivy
                      ivy-bibtex
                      jedi
                      julia-mode
                      jupyter
                      key-chord
                      lsp-mode
                      lsp-ui
                      markdown-mode
                      magit
                      ob-ipython
                      org
                      org-bullets
                      org-journal
                      org-ref
                      pdf-tools
                      projectile
                      protobuf-mode
                      py-autopep8
                      python-mode
                      rainbow-delimiters
                      rainbow-mode
                      rg
                      rustic
                      smex
                      tide
                      undo-tree
                      web-mode
                      with-editor
                      yaml-mode
                      zig-mode))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))


;;;;
;; GENERIC
;;;;

(setq-default indent-tabs-mode nil)

(use-package hl-todo
       :ensure t
       :config
       (setq hl-todo-highlight-punctuation ":"
          hl-todo-keyword-faces
          `(("TODO"       error bold)
            ("FIXME"      error bold)
            ("HACK"       font-lock-constant-face bold)
            ("REVIEW"     font-lock-keyword-face bold)
            ("NOTE"       success bold)
            ("DEPRECATED" font-lock-doc-face bold)))
       :hook ((prog-mode . hl-todo-mode)
              (yaml-mode . hl-todo-mode)
              (web-mode .  hl-todo-mode)))

(use-package savehist
  :init
  (savehist-mode))

;;;;
;; EVIL
;;;;
(use-package evil
  :ensure t
  :init
  (setq evil-vsplit-window-right t)
  (setq evil-disable-insert-state-bindings t)
  (setq evil-auto-indent t)
  (setq evil-undo-system 'undo-tree)
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


(define-key evil-motion-state-map (kbd "TAB") nil)
(define-key evil-normal-state-map (kbd "M-.") nil)
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

(evil-set-initial-state 'ivy-occur-mode 'emacs)

(add-hook 'org-capture-mode-hook 'evil-insert-state)

;;;;
;; UNDO-TREE
;;;;
(global-undo-tree-mode)
(setq undo-tree-auto-save-history nil)

;;;;
;; AUTO-COMPLETION
;;;;
(global-company-mode t)
(setq company-minimum-prefix-length 1)
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

;;;;
;; XREF
;;;;
(use-package xref
  :bind (("M-g o" . xref-find-definitions-other-window)
         ("M-g j" . xref-find-definitions)
         ("M-g b" . xref-pop-marker-stack))
  :ensure)

;;;;
;; LSP
;;;;
(use-package lsp-mode
  :init
  ;; Use flycheck instead of flymake (better lsp-ui integration)
  (setq lsp-prefer-flymake nil)

  :config
  ;; Prevent long documentation showing up in the echo area from messing up the
  ;; window configuration -> only show the first line
  (defun dba/lsp-eldoc-advice (orig-fun &rest args)
    (let ((msg (car args)))
      (if msg
          (funcall orig-fun (->> msg (s-trim-left)
                                     (s-split "\n")
                                     (first))))))
  (advice-add 'lsp--eldoc-message :around #'dba/lsp-eldoc-advice)

  ;; Avoid questions about restarting the LSP server when quitting emacs
  (defun dba/lsp-disable-server-autorestart ()
    (setq lsp-restart nil))
  (add-hook 'kill-emacs-hook #'dba/lsp-disable-server-autorestart))

(use-package lsp-ui
  :ensure t

  :init
  (setq lsp-ui-doc-enable t
      lsp-ui-peek-enable t
      lsp-ui-sideline-enable t
      lsp-ui-imenu-enable t
      lsp-ui-flycheck-enable t)

  :config)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

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
(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

;;;;
;; MINTED
;;;;
(setq org-latex-create-formula-image-program 'imagemagick)

;;(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.7))

;;;;
;; IVY
;;;;
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
  ;; Allow selecting a non-match
  (setq ivy-use-selectable-prompt t))

;;;;
;; VAULT
;;;;

(defvar vault-dir "~/Library/Mobile Documents/com~apple~CloudDocs/Vault")

;;;;
;; BIBTEX
;;;;

(use-package org-ref)

(setq bibtex-completion-bibliography `(,(concat (file-name-as-directory vault-dir) "org/ref/master.bib"))
  bibtex-completion-library-path `(,(concat (file-name-as-directory vault-dir) "org/ref/pdfs/"))
  bibtex-completion-notes-path (concat (file-name-as-directory vault-dir) "org/ref/notes.org")
  bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

  bibtex-completion-additional-search-fields '(keywords)
  bibtex-completion-display-formats
  '((article       . "${year:4} ${author:36} ${title:*} ${journal:40}")
    (inbook        . "${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
    (incollection  . "${year:4} ${author:36} ${title:*} ${booktitle:40}")
    (inproceedings . "${year:4} ${author:36} ${title:*} ${booktitle:40}")
    (t             . "${year:4} ${author:36} ${title:*}"))
  bibtex-completion-pdf-open-function
  (lambda (fpath)
    (call-process "open" nil 0 nil fpath)))

(defun bibtex-completion-format-citation-org (keys)
  "Formatter for ebib references."
  (s-join ", "
          (--map (format "cite:%s" it) keys)))

(setq bibtex-completion-format-citation-functions
      '((org-mode      . bibtex-completion-format-citation-org)
        (latex-mode    . bibtex-completion-format-citation-cite)
        (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
        (default       . bibtex-completion-format-citation-default)))

(require 'bibtex)

(setq bibtex-autokey-year-length 4
  bibtex-autokey-name-year-separator "-"
  bibtex-autokey-year-title-separator "-"
  bibtex-autokey-titleword-separator "-"
  bibtex-autokey-titlewords 2
  bibtex-autokey-titlewords-stretch 1
  bibtex-autokey-titleword-length 5
  org-ref-bibtex-hydra-key-binding (kbd "H-b"))

(define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)

(require 'org-ref-ivy)

(setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
      org-ref-insert-label-function 'org-ref-insert-label-link
      org-ref-insert-ref-function 'org-ref-insert-ref-link
      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))

(defun bibtex-completion-pdf (entry)
  (let ((pdf (bibtex-completion-find-pdf entry)))
    (call-process "open" nil 0 nil (car pdf))))

(defun bibtex-completion-finder (entry)
  (let ((pdf (bibtex-completion-find-pdf entry)))
    (call-process "open" nil 0 nil "-R" (car pdf))))

(defun bibtex-completion-skim (entry)
  (let ((pdf (bibtex-completion-find-pdf entry)))
    (call-process "open" nil 0 nil "-a" "skim" (car pdf))))

(require 'ivy-bibtex)

(ivy-add-actions
 'ivy-bibtex
 '(("P" bibtex-completion-pdf "Open pdf with mac's preview")
   ("S" bibtex-completion-skim "Open pdf with skim")
   ("F" bibtex-completion-finder "Open finder on the pdf")))

(defun org-ref-copy-pdf-at-point ()
    "Copies a potential pdf in the `~/Downloads' folder into the
    org-ref-pdf folder."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((key (bibtex-completion-get-key-bibtex))
           (pdf-file-name (concat (concat (file-name-as-directory vault-dir) "org/ref/pdfs/") key ".pdf"))
           (file (counsel-find-file "~/Downloads")))
      (rename-file file pdf-file-name))))

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

;; if (aspell installed) { use aspell}
;; else if (hunspell installed) { use hunspell }
;; whatever spell checker I use, I always use English dictionary
;; I prefer use aspell because:
;; 1. aspell is older
;; 2. looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(setq ispell-program-name "aspell"
      ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
      ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))

(with-eval-after-load 'flyspell
 (define-key flyspell-mode-map (kbd "C-c $") #'flyspell-popup-correct)
 (add-hook 'flyspell-mode 'auto-fill-mode))

;; Set shortcut for auto correction
(global-set-key (kbd "M-i") 'flyspell-auto-correct-word)

;; Use flyspell for comments
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

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
(setq org-directory (concat (file-name-as-directory vault-dir) "org/"))
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook 'org-bullets-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(setq org-adapt-indentation nil)

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
;; (eval-after-load 'org '(require 'org-pdfview))

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
(setq org-agenda-files `(, (concat (file-name-as-directory vault-dir) "org/refile.org")
                         , (concat (file-name-as-directory vault-dir) "org/inbox.org")
                         , (concat (file-name-as-directory vault-dir) "org/life.org")
                         , (concat (file-name-as-directory vault-dir) "org/research.org")
                         , (concat (file-name-as-directory vault-dir) "org/clients/")))

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
              ("A" "Agenda"
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
                (agenda "" ((org-agenda-use-time-grid nil)
                            (org-agenda-skip-function '(dba/org-agenda-skip-tag "Reoccurring" nil))))
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

(defun dba/org-agenda-skip-tag (tag &optional others)
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
(setq org-default-notes-file (concat (file-name-as-directory vault-dir) "org/refile.org"))


(add-to-list 'load-path "~/.emacs.d/customizations")
(load "org-helper.el")

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
       `(("t" "Todo" entry (file ,(concat (file-name-as-directory vault-dir) "org/refile.org"))
               "* TODO %?\n%U\n" :clock-in t :clock-resume t)
              ("c" "Code Todo" entry (file ,(concat (file-name-as-directory vault-dir) "org/refile.org"))
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "Respond" entry (file ,(concat (file-name-as-directory vault-dir) "org/refile.org"))
               "* NEXT Respond to %? on %:subject\nSCHEDULED: %t\n%U\n" :clock-in t :clock-resume t)
              ("n" "Note" entry (file ,(concat (file-name-as-directory vault-dir) "org/refile.org"))
               "* %? :NOTE:\n%U\n" :clock-in t :clock-resume t)
              ("i" "Idea" entry (file org-default-notes-file)
               "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file ,(concat (file-name-as-directory vault-dir) "org/refile.org"))
               "* MEETING %u %? :MEETING:\n** Summary\n** Attendees\n** Questions\n** Notes\n** Actions\n" :clock-in t :clock-resume t)))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-refile-targets '((org-agenda-files :maxlevel . 9)))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "CURRENT(u)" "|" "DONE(d)" "MIGRATE(g)" )
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("CURRENT" :foreground "orange" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("MIGRATE" :foreground "magenta" :weight bold)
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

(setq inferior-julia-program-name "/Applications/Julia-1.8.app/Contents/Resources/julia/bin/julia")

(add-hook 'ob-async-pre-execute-src-block-hook
          '(lambda ()
             (setq inferior-julia-program-name "/Applications/Julia-1.8.app/Contents/Resources/julia/bin/julia")))

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
         :base-directory (concat (file-name-as-directory vault-dir) "Blog/")
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
  :init (setq deft-directory (concat (file-name-as-directory vault-dir) "slip-box")
              deft-extensions '("org" "md" "txt")
              deft-use-filename-as-title t
              deft-auto-save-interval 0)
  :config
  (global-set-key [f8] 'deft))

(add-to-list 'evil-insert-state-modes 'deft-mode)

;;;;
;; ZETTELDEFT
;;;;

(use-package zetteldeft
  :load-path "/Users/david/Projects/zetteldeft"
  :ensure t
  :after deft
  :config (zetteldeft-set-classic-keybindings))

(setq zetteldeft-link-indicator "ztl:"
      zetteldeft-link-suffix ""
      zetteldeft-backlink-prefix "#+BACKLINK: ")

(defun ztl-complete-link (&optional arg)
  (format "ztl:%s"
          (completing-read "Choose a link: " (deft-find-all-files-no-prefix))))

(defun ztl-help-echo (window object position)
  "A help-echo function for ztl links."
  (save-excursion
    (goto-char position)
    (let ((s (zetteldeft--id-to-title
              (org-element-property :path (org-element-context)))))
      (with-temp-buffer
        (insert s)
        (fill-paragraph)
        (buffer-string)))))

(org-link-set-parameters
 "ztl"
 :follow (lambda (path) (zetteldeft--search-filename path))
 :complete #'ztl-complete-link
 :display 'full
 :help-echo #'ztl-help-echo
 :face 'org-link)

(setq help-at-pt-display-when-idle t)

;; Custom ID function similar to Obsidian.
(defun dba/zetteldeft-id-function (tile filename)
  (concat (format-time-string "%Y%m%d%H%M") (dba/random-al)))

(defun dba/random-al()
  (let* ((al"abcdefghijklmnopqrstuvwxyz")
         (i (% (abs (random)) (length al))))
    (substring al i (1+ i))))

(setq zetteldeft-custom-id-function 'dba/zetteldeft-id-function)
(setq zetteldeft-id-regex  "[0-9]\\{12\\}[a-z]?")
(setq zetteldeft-home-id "202110161039i index.org")

(font-lock-add-keywords 'markdown-mode
   `((,zetteldeft-id-regex
      . font-lock-warning-face)))

(font-lock-add-keywords 'org-mode
   `((,zetteldeft-id-regex
      . font-lock-warning-face)))

;;;;
;; CALIBREDB
;;;;
(require 'calibredb)
(setq calibredb-root-dir (concat (file-name-as-directory vault-dir) "Calibre"))
(setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))

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
(setq org-journal-dir (concat (file-name-as-directory vault-dir) "org/journal"))

(setq org-journal-file-format "%Y%m%d.org")

;;;;
;; DIARY
;;;;
(setq diary-file (concat (file-name-as-directory vault-dir) "org/diary"))
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
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien))

(counsel-projectile-mode)

;;;;
;; ELFEED
;;;;
(setq elfeed-feeds
      '("http://nullprogram.com/feed/"
        "feed://www.milans.name/forum/files/feed.xml"
        "https://thume.ca/atom.xml"
        "https://jvns.ca/atom.xml"
        "https://oremacs.com/atom.xml"
        "https://rjlipton.wordpress.com/feed/"
        "http://lambda-the-ultimate.org/rss.xml"
        "https://danluu.com/atom.xml"
        "https://dragan.rocks/feed.xml"
        "https://www.scottaaronson.com/blog/?feed=rss2"
        "http://matt.might.net/articles/feed.rss"
        "https://www.juliabloggers.com/feed/"))

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

(defun dba/clear-scratch nil
  "create a scratch buffer"
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (erase-buffer)
  (lisp-interaction-mode))

(defun dba/arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

(defun dba/untabify-buffer ()
  "Untabify current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defun dba/find-isbn-by-title ()
  ""
  (interactive)
  (let ((title (read-string "Title: ")))
    (url-retrieve
     (format "https://www.googleapis.com/books/v1/volumes?q=%s" title)
     )))


;;;;
;; CUSTOMIZATION
;;;;
;; yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't know why but I have to do it here otherwise stuff breaks
(require 'cider)

(require 'org-mac-link)

(add-hook 'org-mode-hook (lambda ()
                           (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

;;;;
;; ZIG
;;;;
(require 'lsp-mode)
(setq lsp-zig-zls-executable "/Users/david/Projects/zls/zig-out/bin/zls")

;; Load all the things
(load "ui.el")
(load "editing.el")
(load "navigation.el")
(load "keyboard.el")
(load "org-recoll.el")

;; Clojure environment
(setq org-babel-clojure-backend 'cider)
(load "clojure.el")

;; JavaScript environment
(load "javascript.el")

;; TypeScript environment
(load "typescript.el")

;; Python environment
(load "python-conf.el")

;; Rust environment
(load "rust.el")

;; Julia environment
(load "julia.el")

;; Haskell environment
(load "haskell.el")

;; Calibre query
(load "books.el")

;; Org Subfigure
(load "ox-latex-subfigure.el")

;; Delve Mode
(load "delve-mode.el")

(setq-default indent-tabs-mode nil)

(setq custom-file (concat user-emacs-directory ".custom.el")) ; tell Customize to save customizations to ~/.emacs.d/.custom.el
(ignore-errors                                                ; load customizations from ~/.emacs.d/.custom.el
  (load-file custom-file))
(put 'erase-buffer 'disabled nil)
