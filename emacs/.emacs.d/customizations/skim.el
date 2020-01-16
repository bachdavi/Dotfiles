;; Some functions to access notes from skim

(defun save-skim-notes (pdf)
	"save notes as .txt file into temp directory"
	(call-process "/Applications/Skim.app/Contents/SharedSupport/skimnotes" nil 0 nil "get" "-format" "text" pdf "/tmp/temp_skim_notes.txt"))

(defun read-skim-notes ()
	"read the latest temp skim file"
	(shell-command-to-string "cat /tmp/temp_skim_notes.txt"))

(defun counsel-skim-notes ()
	"show notes content in counsel."
	(interactive)
	(let* ((notes (read-skim-notes)))
		(ivy-read "%d Choose note:  " notes
							:action #'read-skim-notes
							:require-match t)))
