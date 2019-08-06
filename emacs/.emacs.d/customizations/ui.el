;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

;; Color Theme
;; (load-theme 'dracula t) ;; load dracula theme

;; set up fonts
(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Operator Mono-17:Medium"))
;; Fontify current frame
(fontify-frame nil)
;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions) 
(setq-default line-spacing 4)

;; Set up italic comments and ligatures if using mac port
(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

(copy-face 'italic 'font-lock-comment-face)
(set-face-foreground 'font-lock-comment-face "red")


;; (toggle-frame-fullscreen)

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)

;; Show line numbers
;; (global-linum-mode)
;; (setq linum-format " %1d ")

(use-package hlinum
  :ensure t)
(set-face-foreground 'linum-highlight-face "white")
(set-face-background 'linum-highlight-face nil)
(hlinum-activate)

(line-number-mode 1)
(column-number-mode 1)

;; Show visual line at current cursor position
(global-visual-line-mode t)
(global-hl-line-mode +1)

;; Delay
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Do not show start up messages
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; You can uncomment this to remove the graphical toolbar at the top. After
;; awhile, you won't need the toolbar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Uncomment the lines below by removing semicolons and play with the
;; values in order to set the width (in characters wide) and height
;; (in lines high) Emacs will have whenever you start it
;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 177) (height . 53)))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

;; Rainbow delimiters
(use-package rainbow-delimiters
  :init
    (add-hook 'web-mode-hook #'rainbow-delimiters-mode)
    (add-hook 'rust-mode-hook #'rainbow-delimiters-mode))

;; Help window
(setq help-window-select t)

;; Size of orgs inline images
(setq org-image-actual-width 400)
