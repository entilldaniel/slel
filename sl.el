;;; package --- Utility functions for calling SL -*- lexical-binding: t -*-
;;; Commentary:
;;; This is just something for fun so that I can call my own Emacs functions
;;; Author: Daniel Figueroa
;;; Maintainer: Daniel Figueroa <daniel@figueroa.se>
;;; URL: http://github.com/entilldaniel/slel.git


;;; Code:

(require 'dash)


(define-minor-mode sl-departures-mode
  "Minor mode for displaying departures from a selected station using the SL API."
  :lighter " Departures"
  :keymap (let ((map (make-sparse-keymap)))
			(define-key map (kbd "g") #'sl/refresh-selected)
			(define-key map (kbd "q") 'quit-window)
			map))


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

(defun sl/get-departures (site-id)
  "Show departures from selected SITE-ID."
  (let ((result-buffer (url-retrieve-synchronously  (format "https://transport.integration.sl.se/v1/sites/%s/departures?transport=METRO" site-id))))
	(unwind-protect
		(with-current-buffer result-buffer
		  (set-buffer-multibyte t)
		  (prefer-coding-system 'utf-8)
		  (goto-char (point-min))
		  (search-forward "\n\n")
		  (let* ((json-object-type 'alist)
				 (data (json-read)))
			(alist-get 'departures data)))
	  (kill-buffer result-buffer))))

(defun sl/format-departures (raw-departures)
  "Filter and format RAW-DEPARTURES and return a nicely formatted string."
  (with-temp-buffer
	(let ((lines '())
		  (departures '()))
	  (-each raw-departures (lambda (departure)
							  (let* ((line-id (alist-get 'id (alist-get 'line departure)))
									 (direction (alist-get 'direction_code departure))
									 (state (alist-get 'state departure))
									 (destination (alist-get 'destination departure))
									 (display (alist-get 'display departure))
									 (object `((direction . ,direction)
											   (state . ,state)
											   (destination . ,destination)
											   (display . ,display)
											   (line . ,line-id))))
								(setf lines (push line-id lines))
								(setf departures (push object departures)))))
	  ;; First split the list into directions
	  (let ((dir1 (seq-filter (lambda (x) (eq 1 (alist-get 'direction x))) departures))
			(dir2 (seq-filter (lambda (x) (eq 2 (alist-get 'direction x))) departures)))
		(dolist (line (sort (seq-uniq lines)))
		  (insert (format "Called at: %s\n" (propertize (format-time-string "%H:%M") 'face 'bold)))
		  (insert (propertize (format "\nLinje: %s" line) 'face 'bold))
		  ;; Get only the ones we're after
		  (let ((lefties (sl/filter-side line dir1))
				(righties (sl/filter-side line dir2)))
			(dolist (departure (-zip-fill '((destination . "") (display . "")) lefties righties))
			  (let ((left (cdr departure))
					(right (car departure)))
				(insert (sl/departure-line left right)))))
		  (insert "\n\n")))
	  (buffer-string))))

(defun sl/filter-side (line direction)
  "Filter out which side for printing we want from LINE and DIRECTION."
  (reverse (seq-filter (lambda (x) (eq line (alist-get 'line x))) direction)))

(defun sl/departure-line (left right)
  "Return a formatted string with LEFT and RIGHT data."
  (let ((left-format (format "\n%-6s %-18s" (alist-get 'display left) (alist-get 'destination left)))
		(right-format (format " %-6s %s" (alist-get 'display right) (alist-get 'destination right))))
	(concat
	 (if (string= "CANCELLED" (alist-get 'state left))
		 (propertize left-format 'face '(:foreground "red"))
	   left-format)
	 (if (string= "CANCELLED" (alist-get 'state right))
		 (propertize right-format 'face '(:foreground "red"))
	   right-format))))

(defun sl/show-departures (site-id data)
  "Store SITE-ID for extra functionality.  Print DATA into a buffer and show it."
  (with-current-buffer (get-buffer-create "*SL Departures*")
	(read-only-mode -1)
	(sl-departures-mode 1)
	(setq-local local-site-id site-id)
	(erase-buffer)
	(enriched-mode 1)
	(insert data)
	(read-only-mode)
	(goto-char (point-min))
	(switch-to-buffer "*SL Departures*")))

(defun sl/select-and-show ()
  "Select a site and show departures."
  (interactive)
  (let ((site-id (sl/locations)))
	(sl/show-departures
	 site-id
	 (sl/format-departures
	  (sl/get-departures site-id)))))

(defun sl/show-selected ()
  "Show departures from a preconfigured list of stations."
  (interactive)
  (let ((station (completing-read "Pick a station: " (mapcar #'car sl/my-stations))))
	(let ((site-id (alist-get station sl/my-stations nil nil 'string=)))
	  (sl/show-departures
	   site-id
	   (sl/format-departures
		(sl/get-departures site-id))))))

(defun sl/refresh-selected ()
  "Refresh from SITE-ID and show new departures."
  (interactive)
  (sl/show-departures
   local-site-id
   (sl/format-departures
	(sl/get-departures local-site-id))))


(provide 'sl)
;;; sl.el ends here.

