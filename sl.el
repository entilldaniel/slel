;;; package --- Utility functions for calling SL -*- lexical-binding: t -*-
;;; Commentary:
;;; This is just something for fun so that I can call my own Emacs functions
;;; Author: Daniel Figueroa
;;; Maintainer: Daniel Figueroa <daniel@figueroa.se>
;;; URL: http://github.com/entilldaniel/slel.git


;;; Code:

(require 'dash)


(setq sl/my-stations '(("Skanstull" . 9190) ("Björkhagen" . 9143) ("Gullmarsplan" . 9189) ("Slussen" . 9192)))

(defun sl/locations ()
  "Retrieve the id of a station."
  (let ((buffer (url-retrieve-synchronously "https://transport.integration.sl.se/v1/sites?expand=true&transportation=METRO")))
	(with-current-buffer buffer
	  (set-buffer-multibyte t)
	  (prefer-coding-system 'utf-8)
	  (goto-char (point-min))
	  (search-forward "\n\n")
	  (let* ((json-array-type 'list)
			 (json-object-type 'alist)
			 (items (json-read))
			 (filtered (-filter (lambda (x) (alist-get 'abbreviation x)) items))
			 (choices (mapcar (lambda (obj)
  								`(,(alist-get 'name obj) . ,(alist-get 'id obj))) filtered))
			 (key (completing-read "Pick a destination: " (mapcar #'car choices)  nil t)))
		(alist-get key choices nil nil 'string=)))))

(defun sl/show-departures (site-id)
  "Show departures from selected SITE-ID."
  (let ((buffer (url-retrieve-synchronously (format "https://transport.integration.sl.se/v1/sites/%s/departures?transport=METRO" site-id))))
	(with-current-buffer buffer
	  (set-buffer-multibyte t)
	  (prefer-coding-system 'utf-8)
	  (goto-char (point-min))
	  (search-forward "\n\n")
	  (let* ((json-object-type 'alist)
			 (data (json-read))
			 (departures (alist-get 'departures data)))
		(erase-buffer)
		(print-results departures)
		(goto-char (point-min))
		(local-set-key (kbd "q") #'quit-window)
		(switch-to-buffer buffer)))))


(defun print-results (departures)
  "Iterate over DEPARTURES and print them nicely."
  (let ((result '()))
	(-each departures (lambda (departure)
						(let* ((line-id (alist-get 'id (alist-get 'line departure)))
							   (direction (alist-get 'direction_code departure))
							   (state (alist-get 'state departure))
							   (destination (alist-get 'destination departure))
							   (display (alist-get 'display departure))
							   (object `((state . ,state)
										 (destination . ,destination)
										 (display . ,display)
										 (line . ,line-id))))
						  (unless (assoc line-id result)
							(push `(,line-id . ((1 . ()) (2 . ()))) result))
						  
						  (push object
								(alist-get direction (alist-get line-id result))))))
	;; Print the data nicely
	(dolist (line (sort result))
	  (let ((key (car line))
			(value (cdr line)))
		(insert (format "\nLinje: %s" key))

		(dolist (printable (-zip-fill '((destination . "") (display . ""))
									  (reverse (cl-remove-if-not (lambda (x) (eq key (alist-get 'line x))) (alist-get 1 value)))
									  (reverse (cl-remove-if-not (lambda (x) (eq key (alist-get 'line x))) (alist-get 2 value)))
									  ))
		  (let ((left (car printable))
				(right (cdr printable)))
			(insert (format "\n%-6s - %s %-25s %-6s - %s %s"
							(alist-get 'display left) (alist-get 'state left) (alist-get 'destination left)
							(alist-get 'display right) (alist-get 'state right) (alist-get 'destination right)))
			)
		  )
		)
	  (insert "\n"))
	))

(defun sl/select-and-show ()
  "Select a site and show departures."
  (interactive)
  (let ((site-id (sl/locations)))
	(sl/show-departures site-id)))

(defun sl/show-selected ()
  "Show departures from a preconfigured list of stations."
  (interactive)
  (let ((station (completing-read "Pick a station: " (mapcar #'car sl/my-stations))))
	(sl/show-departures (alist-get station sl/my-stations nil nil 'string=))))

(provide 'sl)
;;; sl.el ends here.



