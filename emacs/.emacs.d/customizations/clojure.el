;; Clojure config file
;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; Set the default comment column to 70
(setq-default comment-column 70)

;; Cleverparens
(add-hook 'clojure-mode-hook 'evil-cleverparens-mode)

;; Line numbers
;; (add-hook 'clojure-mode-hook 'linum-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
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

(global-set-key [f9] 'cider-jack-in)
(global-set-key [f10] 'cider-jack-in-clojurescript)

;; Enter cider mode when entering the clojure major mode
(add-hook 'clojure-mode-hook 'cider-mode)

;; Use dev 
;; (setq cider-clojure-cli-global-options "-A:dev")
;; (setq cider-clojure-cli-global-options nil)

;; Turn on auto-completion with Company-Mode
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

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

;; Repl evaluation
(add-hook 'cider-repl-mode-hook
      '(lambda () (define-key cider-repl-mode-map (kbd "C-c M-o")
            'cider-repl-clear-buffer)))

(defun cider-direct-doc ()
	(interactive)
	(cider-doc-lookup (cider-symbol-at-point)))


(evil-leader/set-key-for-mode 'clojure-mode   "." 'cider-find-dwim
                                                "e" 'cider-eval-sexp-at-point
                                                "b" 'cider-eval-buffer
                                                "r" 'cider-eval-region
                                                "k" 'cider-direct-doc) 

(evil-leader/set-key-for-mode 'clojurec-mode "." 'cider-find-dwim
                                                "e" 'cider-eval-sexp-at-point
                                                "b" 'cider-eval-buffer
                                                "r" 'cider-eval-region
                                                "k" 'cider-direct-doc) 

(evil-leader/set-key-for-mode 'clojurescript-mode "." 'cider-find-dwim
                                                "e" 'cider-eval-sexp-at-point
                                                "b" 'cider-eval-buffer
                                                "r" 'cider-eval-region
                                                "k" 'cider-direct-doc) 

;; Paredit
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
