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

(defgroup sl nil "SL departures customization group.")

(defcustom sl/my-stations
  '(("Skanstull" . 9190) ("Björkhagen" . 9143) ("Gullmarsplan" . 9189) ("Slussen" . 9192))
  "A predefined list with SL stations and their codes."
  :type '(alist :key-type string :value-type integer)
  :group 'sl)

(defvar sl/locations-cache nil "Caches the stations and their ID's.")

(defun marginalia-sl/location-annotation (cand)
  "Show site-id for SL location candidate CAND."
  (propertize
   (format " [%s]" (cdr (assoc cand sl/locations-cache)))
   'face '(:foreground "dim gray")))

(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-annotators
			   '(sl/location . (marginalia-sl/location-annotation))))

(defun sl/show-completions-menu (choices)
  "Show the menu based on entries in CHOICES."
  (let ((completion-category-defaults '((sl/location (annotation-function . marginalia-sl/location-annotation))))
		(completion-extra-properties '(:category sl/location)))
	(cdr (assoc (completing-read "Pick a destination: " (mapcar 'car choices) nil t) choices))))

(defun sl/get-locations (continue)
  "Get locations to select and pass CONTINUE to callback."
  (if sl/locations-cache
	  (funcall continue)
	(url-retrieve "https://transport.integration.sl.se/v1/sites" #'sl/get-locations-callback `(,continue))))

(defun sl/get-locations-callback (status continue)
  "Callback for get-locations-new that handles STATUS and will call CONTINUE."
  (unwind-protect
	  (progn
		(set-buffer-multibyte t)
		(prefer-coding-system 'utf-8)
		(goto-char url-http-end-of-headers)
		(let* ((json-array-type 'list)
			   (json-object-type 'alist)
			   (items (json-read))
			   (filtered (-filter (lambda (x) (alist-get 'abbreviation x)) items))
			   (choices (mapcar (lambda (obj)
								  (cons (alist-get 'name obj)
										(alist-get 'id obj)))
								filtered)))
		  (setq sl/locations-cache choices))
		(kill-buffer (current-buffer))))
  (funcall continue))

(defun sl/get-departures (site-id continue)
  "Get departures from SITE-ID and pass CONTINUE through to callback."
  (url-retrieve (format "https://transport.integration.sl.se/v1/sites/%s/departures?transport=METRO" site-id)
				#'sl/departures-callback `(,site-id ,continue)))

(defun sl/departures-callback (status site-id continue)
  "Callback that handles STATUS, SITE-ID and CONTINUE."
  (unwind-protect
	  (progn
		(set-buffer-multibyte t)
		(prefer-coding-system 'utf-8)
		(goto-char url-http-end-of-headers)
		(let* ((json-object-type 'alist)
			   (data (json-read)))
		  (funcall continue site-id (alist-get 'departures data)))
		(kill-buffer (current-buffer)))))

(defun sl/format-departures-old (raw-departures)
  "Filter and format RAW-DEPARTURES and return a nicely formatted string."
  (with-temp-buffer
	(insert (format "Called at: %s\n" (propertize (format-time-string "%H:%M") 'face 'bold)))
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

;; NEW FORMAT DEPARTURES

(defun sl/format-departures (raw-departures)
  "Filter and format RAW-DEPARTURES and return a nicely formatted string."
  (with-temp-buffer
	(insert (format "\nCalled at: %s" (propertize (format-time-string "%H:%M") 'face 'bold)))
	(let ((line-sorted-departures
		   (seq-sort (lambda (a b)
					   (< (car a) (car b)))
					 (seq-group-by (lambda (departure)
									 (alist-get 'id (alist-get 'line departure))) raw-departures))))
	  (seq-do (lambda (line)
				(insert (format "\n\nLinje: %s" (car line)))
				(let ((directions (seq-sort (lambda (a b) (> (car a) (car b))) (seq-group-by
																				(lambda (x) (alist-get 'direction_code x)) (cdr line)))))

				  (dolist (departure (-zip-fill '((destination . "") (display . "")) (alist-get 1 directions) (alist-get 2 directions)))
					(insert (sl/departure-line (car departure) (cdr departure)))
					)
				  )) line-sorted-departures))
	(buffer-string)))

;; NEW FORMAT DEPARTURES END


(defun sl/filter-side (line direction)
  "Filter out which side for printing we want from LINE and DIRECTION.
Reverse the list so that we see the earliest departures first."
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
	(setq-local sl/local-site-id site-id)
	(erase-buffer)
	(enriched-mode 1)
	(insert data)
	(read-only-mode)
	(goto-char (point-min))
	(switch-to-buffer "*SL Departures*")))

(defun sl/select-and-show ()
  "Select a site and show departures."
  (interactive)
  (sl/get-locations (lambda ()
					  (let ((site-id (sl/show-completions-menu sl/locations-cache)))
						(sl/get-departures site-id #'sl/show-and-format)))))

(defun sl/show-selected ()
  "Show departures from a preconfigured list of stations."
  (interactive)
  (let ((station (completing-read "Pick a station: " (mapcar #'car sl/my-stations))))
	(let ((site-id (alist-get station sl/my-stations nil nil 'string=)))
	  (sl/get-departures site-id #'sl/show-and-format))))

(defun sl/refresh-selected ()
  "Refresh from SITE-ID and show new departures."
  (interactive)
  (sl/get-departures sl/local-site-id #'sl/show-and-format))

(defun sl/show-and-format (site-id departures)
  "Handle SITE-ID and DEPARTURES."
  (sl/show-departures
   site-id
   (sl/format-departures departures)))

(defun sl/clear-cache ()
  "Clears the cache, used for development mainly."
  (interactive)
  (setq sl/locations-cache nil))

(defun sl/show-local-site-id ()
  "Show the local site id.  Used primarily for testing."
  (interactive)
  (message (format "Site ID: %s" sl/local-site-id)))

(provide 'sl)
;;; sl.el ends here.


