;; Customizations relating to editing a buffer.

;; multiple cursors
; (require 'evil-mc)
; (global-evil-mc-mode  1) 

;; Auto add pairs
(electric-pair-mode 1)

;; Don't use hard tabs
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

;; yay rainbows!
;; (global-rainbow-delimiters-mode t)

(use-package drag-stuff
  :ensure t)
(drag-stuff-global-mode 1)
(global-set-key (kbd "M-k") 'drag-stuff-up)
(global-set-key (kbd "M-j") 'drag-stuff-down)

;; Automatically revert files when changed externally
(global-auto-revert-mode 1)
