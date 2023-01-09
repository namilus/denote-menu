;;; denote-menu.el --- View denote files in a tabulated list. -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Mohamed Suliman <sulimanm@tcd.ie>
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (denote "1.2.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'tabulated-list)
(require 'denote)
(require 'dired)

(defgroup denote-menu ()
  "View Denote files"
  :group 'files)

(defcustom denote-menu-date-column-width 17
  "Width for the date column."
  :type 'number
  :group 'denote-menu)

(defcustom denote-menu-title-column-width 85
  "Width for the title column."
  :type 'number
  :group 'denote-menu)


(defcustom denote-menu-keywords-column-width 30
  "Width for the keywords column."
  :type 'number
  :group 'denote-menu)


(defcustom denote-menu-action (lambda (path) (find-file path))
  "Function to execute when a denote file button action is
invoked. Takes a single argument which is the path of the
denote file corresponding to the button."
  :type 'function
  :group 'denote-menu)

(defcustom denote-menu-initial-regex "."
  "A regex that is used to initially populate the buffer with
matching denote files."
  :type 'string
  :group 'denote-menu)

(defvar denote-menu-current-regex denote-menu-initial-regex
  "The current regex used to match denote filenames.")

(defun list-denotes ()
  "Displays a list of the denote files located in
`denote-directory'."
  (interactive)
  (let ((buffer (get-buffer-create "*Denote*")))
    (with-current-buffer buffer
      (setq buffer-file-coding-system 'utf-8)
      (setq denote-menu-current-regex denote-menu-initial-regex)
      (denote-menu-mode))
    
    (pop-to-buffer-same-window buffer)))

(defun denote-menu-update-entries ()
  "Sets `tabulated-list-entries' to a function that maps currently
displayed denote file names
matching the value of `denote-menu-current-regex' to a tabulated
list entry following the defined form."
  (if tabulated-list-entries
      (progn
        (let
            ((current-entry-paths (denote-menu--entries-to-paths)))
          (setq tabulated-list-entries
                (lambda ()
                  (let ((matching-denote-files
                         (denote-menu-files-matching-regexp current-entry-paths denote-menu-current-regex)))
                    (mapcar #'denote-menu--path-to-entry matching-denote-files))))))
    (setq tabulated-list-entries
          (lambda ()
            (let ((matching-denote-files
                   (denote-directory-files-matching-regexp denote-menu-current-regex)))
              (mapcar #'denote-menu--path-to-entry matching-denote-files))))))

(defun denote-menu--entries-to-filenames ()
  "Returns a list of the file names of the Denote files currenty
 presented in the *Denote* buffer."
  (mapcar (lambda (entry)
            (let ((id (car entry)))
              (file-name-nondirectory (denote-get-path-by-id id))))
          (funcall tabulated-list-entries)))

(defun denote-menu--entries-to-paths ()
  "Returns a list of the file paths of the Denote files currenty
 presented in the *Denote* buffer."
  (mapcar (lambda (entry)
            (let ((id (car entry)))
              (denote-get-path-by-id id)))
          (funcall tabulated-list-entries)))

(defun denote-menu-files-matching-regexp (files regexp)
  "Return list of files matching REGEXP from FILES."
  (seq-filter (lambda (f) (string-match-p regexp f)) files))

(defun denote-menu--path-to-entry (path)
  "Converts a denote file PATH to an entry matching the form of
`tabulated-list-entries'."
  `(,(denote-retrieve-filename-identifier path)
    [(,(denote-menu-date path) . (action ,(lambda (button) (funcall denote-menu-action path))))
     ,(denote-menu-title path)
     ,(propertize (format "%s" (denote-extract-keywords-from-path path)) 'face 'italic)]))
  
(defun denote-menu-date (path)
  (let* ((timestamp (split-string (denote-retrieve-filename-identifier path) "T"))
         (date (car timestamp))
         (year (substring date 0 4))
         (month (substring date 4 6))
         (day (substring date 6 8))
               
         (time (cadr timestamp))
         (hour (substring time 0 2))
         (seconds (substring time 2 4)))
                  
    (format "%s-%s-%s %s:%s" year month day hour seconds)))

(defun denote-menu-title (path)
  "If the denote file PATH has no title, returns the string \"(No
Title)\". Otherwise returns the path's title. Determines whether
a denote file has a title based on the following rule derived from the file naming scheme:

1. If the path does not have a \"--\", it has no title."
  (if (or (not (string-match-p "--" path)))
      (propertize "(No Title)" 'face 'font-lock-comment-face)
    (denote-retrieve-filename-title path)))

(defun denote-menu-filter (regex)
  "Filters the `tabulated-list-entries' with denote files whose name
matches REGEX and reverts the *Denotes* buffer to reflect the
changes."
  (setq denote-menu-current-regex regex)
  (denote-menu-update-entries)
  (revert-buffer))

(defun denote-menu-filter-by-keyword ()
  "Prompts for keywords and filters the list for denote files tagged
with the input keywords."
  (interactive)
  (let* ((keywords (denote-keywords-prompt))
         (regex (concat "\\(" (mapconcat (lambda (keyword) (format "_%s" keyword)) keywords "\\|") "\\)")))
    (setq denote-menu-current-regex regex)
    (denote-menu-update-entries)
    (revert-buffer)))
    
(defun denote-menu-clear-filters ()
  "Resets `denote-menu-current-regex' to be
`denote-menu-initial-regex' and updates the list."
  (interactive)
  (setq denote-menu-current-regex denote-menu-initial-regex)
  (setq tabulated-list-entries nil)
  (denote-menu-update-entries)
  (revert-buffer))

(defun denote-menu-export-to-dired ()
  "Switches to a dired buffer showing `denote-directory' with the
currently displayed denote
files marked."
  (interactive)
  (let ((files-to-mark (denote-menu--entries-to-filenames)))
    (dired denote-directory)
    (revert-buffer)
    (dired-mark-if
     (and (not (looking-at-p dired-re-dot))
	  (not (eolp))			; empty line
	  (let ((fn (dired-get-filename t t)))
            (and fn (member fn files-to-mark))))
     "matching file")))

  

(define-derived-mode denote-menu-mode tabulated-list-mode "Denote Menu"
  "Major mode for browsing a list of Denote files."
  :interactive nil
  (setq tabulated-list-format `[("Date" ,denote-menu-date-column-width t)
                                ("Title" ,denote-menu-title-column-width nil)
                                ("Keywords" ,denote-menu-keywords-column-width nil)])

  (define-key denote-menu-mode-map (kbd "c") #'denote-menu-clear-filters)
  (define-key denote-menu-mode-map (kbd "/ r") #'denote-menu-filter)
  (define-key denote-menu-mode-map (kbd "/ k") #'denote-menu-filter-by-keyword)
  (define-key denote-menu-mode-map (kbd "e") #'denote-menu-export-to-dired)
  (denote-menu-update-entries)
  (setq tabulated-list-sort-key '("Date" . t))
  (tabulated-list-init-header)
  (tabulated-list-print))

(provide 'denote-menu)
;;; denote-menu.el ends here
