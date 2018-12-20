;; Keyboard and shortcuts

;; Mac issues
(define-key key-translation-map [(meta ?5)] [?\[])
(define-key key-translation-map [(meta ?6)] [?\]])
(define-key key-translation-map [(meta ?9)] [?\}])
(define-key key-translation-map [(meta ?8)] [?\{])
(define-key key-translation-map [(meta ?7)] [?\|])
(define-key key-translation-map [(meta ?3)] [?\#])
(define-key key-translation-map [(meta ?n)] [?\~])
(define-key key-translation-map [(meta ?g)] [?\@])
(global-set-key (kbd "s-7") "\\")

;;Exit insert mode by pressing j and then j quickly
(setq key-chord-two-keys-delay 0.2)
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-mode 1)

(key-chord-define evil-insert-state-map "$1" "(")
(key-chord-define evil-insert-state-map "$2" "[")
(key-chord-define evil-insert-state-map "$3" "{")

;; Ivy
(global-set-key (kbd "M-x") 'counsel-M-x)

;; Fuzzy search everywhere
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

;; Space for search
(define-key evil-normal-state-map (kbd "SPC") 'counsel-grep-or-swiper)

;; Neotree
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)

;; Flyspell
(global-set-key (kbd "C-M-i") 'flyspell-auto-correct-word)

;; Move around between windows
(define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)

