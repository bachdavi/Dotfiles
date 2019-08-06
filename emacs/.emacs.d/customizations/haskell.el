;; All things related to Haskell

(defun haskell-setup ()
  "Setup variables for editing Haskell files."
  (setq whitespace-line-column 70)
  (make-local-variable 'tab-stop-list)
  (setq tab-stop-list (number-sequence 2 80 2))
  (haskell-indentation-mode 0)
  (setq indent-line-function 'indent-relative))

(add-hook 'haskell-mode-hook 'haskell-setup)
