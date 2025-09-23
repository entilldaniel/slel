;;; package --- Utility functions for calling SL -*- lexical-binding: t -*-
;;; Commentary:
;;; This is just something for fun so that I can call my own Emacs functions
;;; Author: Daniel Figueroa (entilldaniel)
;;; Code:

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

(defun sl/present (obj)
  "Convert a departure OBJ to something readable."
  (let ((destination (alist-get 'destination obj))
		(state (alist-get 'state obj))
		(display (alist-get 'display obj))
		(designation (alist-get 'designation (alist-get 'line obj))))
	(if (not (string= "CANCELLED" state))
		(concat "linje " designation " " destination "\n" display "\n")
	  "")))

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
			 (departures (alist-get 'departures data))
			 (strings (mapcar 'sl/present  departures)))
		(erase-buffer)
		(-each strings (lambda (str) (insert str "\n")))
		(goto-char (point-min))
		(local-set-key (kbd "q") #'quit-window)
		(switch-to-buffer buffer)))))

(defun sl/select-and-show ()
  "Select a site and show departures."
  (interactive)
  (let ((site-id (sl/locations)))
	(sl/show-departures site-id)))

(setq sl/my-stations '(("Skanstull" . 9190) ("Björkhagen" . 9143) ("Gullmarsplan" . 9189) ("Slussen" . 9192)))

(defun sl/show-selected ()
  "Show departures from a preconfigured list of stations."
  (interactive)
  (let ((station (completing-read "Pick a station: " (mapcar #'car sl/my-stations))))
	(sl/show-departures (alist-get station sl/my-stations nil nil 'string=))))

(provide 'sl)
;;; sl.el ends here.

