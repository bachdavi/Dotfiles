;; counsel-books
;; use this to query calbire library
;; You need to symlink the `calibredb' from the /Applications/ folder

(defun book-list-to-ivy (book-list)
  "Transform BOOK-LIST into a list of strings for `counsel-books'."
  (let (result)
    (dolist (item book-list result)
      (let* ((author (alist-get 'authors item))
						 (title (alist-get 'title item))
						 (book-id (alist-get 'id item))
						 (formats (alist-get 'formats item))
						 (series (alist-get 'series item))
						 (series-index (alist-get 'series_index item)))
				
				(push (cons (format "%s - %s" author title)
										(cons (alist-get 'id item) (alist-get 'formats item)))
							result)))))

(defun counsel-books-action-open-book (book)
  "open selected book"
  (let ((formats (cdr (cdr book))))
    (if (< (length formats) 2)
      (find-file (car formats))
      (find-file (ivy-read "%d Choose format: " formats
			   :require-match t)))))


(defun counsel-books-action-open-skim (book)
  "open selected book with skim"
  (let ((formats (cdr (cdr book))))
    (if (< (length formats) 2)
      (call-process "open" nil 0 nil "-a" "skim" (car formats))
      (call-process "open" nil 0 nil "-a" "skim" (ivy-read "%d Choose format: " formats
			   :require-match t)))))

(defun counsel-books-action-open-native (book)
  "open selected book with native pdf view"
  (let ((formats (cdr (cdr book))))
    (if (< (length formats) 2)
      (call-process "open" nil 0 nil (car formats))
      (call-process "open" nil 0 nil (ivy-read "%d Choose format: " formats
			   :require-match t)))))

(defun counsel-books-action-get-info (book)
  "show metadata for selected book"
  (let* ((book-id (cadr book))
				 (meta (shell-command-to-string (format "calibredb show_metadata %s" book-id)))
				 (metadata (assoc 'metadata (with-temp-buffer
																			(insert meta)
																			(libxml-parse-xml-region (point-min) (point-max))))))
    (with-current-buffer (get-buffer-create "*book-info*")
			(let ((inhibit-read-only t))
				(erase-buffer)
				(goto-char (point-min))
				(insert meta)
				(special-mode)
				(visual-line-mode)
				(display-buffer (current-buffer))))))

(ivy-set-actions
 'counsel-books
 '(("p" counsel-books-action-open-book "open book")
	 ("P" counsel-books-action-open-native "open native")
	 ("s" counsel-books-action-open-skim "open skim")
	 ("I" counsel-books-action-get-info "show metadata")))

(defun counsel-books ()
	"show a list of books in calibre database"
	(interactive)
	(let* ((json-array-type 'list)
				 (json-object-type 'alist)
				 (book-list (json-read-from-string (shell-command-to-string "calibredb list -f \"authors, title, formats,series,series_index\" --for-machine"))))
		(ivy-read "%d Choose book: " (book-list-to-ivy book-list)
							:action #'counsel-books-action-open-skim
							:require-match t
							:sort t)))
