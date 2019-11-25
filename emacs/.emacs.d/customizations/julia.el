;; Julia config file

(add-to-list 'load-path "/Users/david/Projects/David/julia-emacs/")
(require 'julia-mode)

(defun eoxxs-run-julia ()
  "Just run julia in a term buffer."
  (interactive)
  (set-buffer (make-term "julia" "julia"))
  (term-mode)
  (term-char-mode)
  (switch-to-buffer "*julia*"))
