;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; Set the default comment column to 70
(setq-default comment-column 70)

(add-hook 'clojure-mode-hook 'paredit-mode)

;; (use-package parinfer
;;   :ensure t
;;   :config (parinfer-strategy-add 'default 'newline-and-indent)
;;   :bind
;;   (:map parinfer-mode-map
;; 	("<tab>" . parinfer-smart-tab:dwim-right)
;; 	("S-<tab>" . parinfer-smart-tab:dwim-left)
;; 	("C-u" . parinfer--reindent-sexp)
;; 	("C-M-i" . parinfer-auto-fix)
;; 	("C-," . parinfer-toggle-mode))
;;   (:map parinfer-region-mode-map
;; 	("C-i" . indent-for-tab-command)
;; 	("<tab>" . parinfer-smart-tab:dwim-right)
;; 	("S-<tab>" . parinfer-smart-tab:dwim-left))
;;   :init
;;   (progn
;;     (setq parinfer-extensions
;; 	  '(defaults       ; should be included.
;; 	     paredit
;; 	     pretty-parens  ; different paren styles for different modes.
;; 	     smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;; 	     smart-yank     ; Yank behavior depend on mode.
;; 	     one))
;;     (add-hook 'clojure-mode-hook #'parinfer-mode)
;;     (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'scheme-mode-hook #'parinfer-mode)
;;     (add-hook 'lisp-mode-hook #'parinfer-mode)))

;; (setq parinfer-auto-switch-indent-mode t)


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

(require 'cider)

;; Enter cider mode when entering the clojure major mode
(add-hook 'clojure-mode-hook 'cider-mode)

(global-set-key [f9] 'cider-jack-in)
(global-set-key [f10] 'cider-jack-in-clojurescript)

;; Turn on auto-completion with Company-Mode
(global-company-mode)
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


(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))

;; better figwheel integration
(setq cider-cljs-lein-repl
      "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")
