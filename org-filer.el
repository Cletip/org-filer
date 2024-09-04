;;; org-filer.el --- Alternative to org-attach to manage with org file: links other types files. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Clément Payard

;; Author: Clément Payard <emacs@clementpayard.com>
;; URL: https://github.com/Cletip/org-filer
;; Version: 1.0
;; Package-Requires: ((emacs "26.1") (org "9.4"))
;; Keywords: org-mode org-attach org-link files attachments

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the Readme.

;;; Code:

(require 'org)

(defgroup org-filer nil
  "Manage file attachments with links in Org mode."
  :prefix "org-filer-"
  :group 'org
  :link '(url-link :tag "GitHub Repository" "https://github.com/Cletip/org-filer")
  :tag "Org Filer")

(defcustom org-filer-directory (expand-file-name (concat org-directory "org-filer/"))
  "The path where to store all files imported."
  :group 'org-filer
  :type 'file)

(defcustom org-filer-directory-to-gather-files 'org-attach-dir
  "Function that return a directory where to gather files with `org-filer-gather-files'."
  :group 'org-filer
  :type 'function)

(defcustom org-filer-automatic-description-function 'file-name-nondirectory
  "This function takes a path as input and returns a string used for the description for the function `org-filer-import-file-dired' and `org-filer-import-org-attached-files'"
  :group 'org-filer
  :type 'function)

(defcustom org-filer-notes-directory org-directory
  "The path used to have relative link to `org-filer-directory' if `org-filer-dir-relative' is set to t."
  :group 'org-filer
  :type 'file)

(defcustom org-filer-dir-relative nil
  "Non-nil means use relative path to link file from `org-filer-notes-directory' to `org-filer-directory'."
  :group 'org-filer
  :type 'boolean)

(defun org-filer--select-method ()
  "Select the org filer method interactively."
  (intern (completing-read "Choose attachment method: "
			   '("mv" "cp" "ln" "lns") nil t)))

(defun org-filer--ask-description (file-or-directory-name)
  "Ask to the user the description for a file, and return as a string."
  (read-string (format "Description of the link in org (for file : %s) : " file-name) file-name))

(defun org-filer--file-or-directory-name-with-uuid (file-or-directory-name)
  "Return the new unique file name, keeping the extension of the file."
  (concat
   (org-id-uuid)
   ;; get extension of the file, or nothing
   (when (not (file-directory-p file-or-directory-name))
     (let ((extension (file-name-extension file-or-directory-name)))
       (when extension
	 (concat "." extension))))))

(defun org-filer--insert-file-link (destination &optional description)
  "Insert the link of a org-filer link.

If `org-filer-dir-relative' is p, insert the link relative to the `org-filer-notes-directory'."
  (let ((org-link-file-path-type
	 (if org-filer-dir-relative 'relative org-link-file-path-type))
	(default-directory
	  (if org-filer-dir-relative org-filer-notes-directory default-directory)))
    (org-insert-link nil (concat "file:" destination) description)))

(defun org-filer-import-file (path method &optional description)
  "Import a file from FILEPATH :
- select the method to use (same as org-attach-method)
- apply the method for the file in the `org-filer-directory`
- create a `file:` link at point.

When this function is called with a `\\[universal-argument]' prefix, it will demand a name and create a file (or a directory if the name has an extension).
"
  (interactive 

   (if current-prefix-arg
       (let* ((name (read-string "Enter name for new file (with .extensionName) or directory (without .extensionName): "))
	      (path (concat org-filer-directory name)))
	 (if (not (file-name-extension path))
	     (make-directory path)
	   ;; create file
	   (with-temp-file path))
	 ;; pass to function the path, move to same place with name given for the description
	 (list path 'mv name))

     (let* ((path (read-file-name "Select file to import: " nil nil t))
	    (method (org-filer--select-method))
	    (description (org-filer--ask-description (file-name-nondirectory path))))
       (list path method description))))

  (let ((destination (concat org-filer-directory (org-filer--file-or-directory-name-with-uuid (file-name-nondirectory path)))))
    (pcase method
      ('mv (rename-file path destination))
      ('cp 
       (if (file-directory-p path)
	   (copy-directory path destination t)
	 (copy-file path destination t t)))
      ('ln (add-name-to-file path destination))
      ('lns (make-symbolic-link path destination 1)))
    ;; insert the link
    (org-filer--insert-file-link destination description)
    (insert "\n")))

;; todo variable "place"
(defun org-filer-import-file-dired (files &optional method automatic)
  "Attach FILES marked or current file in `dired'.
Open a new buffer with links.

If call with universal-argument, ask each time the method for each file.
"
  (interactive
   (list (dired-get-marked-files) 
	 (if current-prefix-arg nil (org-filer--select-method))
	 (yes-or-no-p (format "Give description automatically based of the function %s ?") org-filer-automatic-description-function) ))
  (unless (eq major-mode 'dired-mode)
    (user-error "This command must be triggered in a `dired' buffer."))
  (switch-to-buffer-other-window (generate-new-buffer "*org-filer importation*"))
  (org-mode)
  (insert "Here is the list of your file imported. Copy and paste the file where you want !\n\n")
  (dolist (file files)
    (org-filer-import-file 
     file 
     (or method (org-filer--select-method))
     (if automatic
	 (funcall org-filer-automatic-description-function path)
       (org-filer--ask-description file-name)))))

(defun org-filer--retrieve-link ()
  "Return list of cons with path and description of link in file if before subtree, subtree, or region if active."
  (save-restriction
    (if (region-active-p)
	(narrow-to-region (region-beginning) (region-end))
      (when (not (org-before-first-heading-p)) (org-narrow-to-subtree)))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
	(when (string= (org-element-property :type link) "file")
	  (cons (org-element-property :path link) (substring-no-properties (car (cdr (cdr link))))))))))

(defun org-filer-gather-files ()
  "Gather all file: links in the current Org node to the directory return by `org-filer-directory-to-gather-files' (or property org-filer-directory is set in the subtree).

In other word, for each file: link in the subtree, a symlink will be created in `org-filer-directory-to-gather-files' to the path of the link.

When this function is called with a `\\[universal-argument]' prefix, all files will be copied (and not symlinked).

"
  (interactive)
  (save-excursion
    (org-back-to-heading-or-point-min)
    (let ((directory-to-gather-files (or 
				      (org-entry-get (point) "org-filer-directory")
				      (funcall org-filer-directory-to-gather-files)))
          (links (org-filer--retrieve-link)))

      ;; Ensure the directory exists
      (unless (file-exists-p directory-to-gather-files)
	(make-directory directory-to-gather-files t))

      ;; Create symlinks for each file
      (dolist (link links)
	(let* ((file-path (car link))
               (link-name (cdr link))
               (symlink-path (expand-file-name link-name directory-to-gather-files)))
	  (when (file-exists-p symlink-path)
	    (delete-file symlink-path))
	  (if current-prefix-arg
	      (copy-file file-path symlink-path)
	    (make-symbolic-link file-path symlink-path))))

      ;; Open the directory-to-gather-files
      (org-filer-open-directory-to-gather-files))))

(defun org-filer-open-directory-to-gather-files ()
  "Open the directory associate with the function `org-filer-gather-files'."
  (interactive)
  (dired (funcall org-filer-directory-to-gather-files)))

(defun org-filer-import-org-attached-files (&optional gather)
  "Transform org-attached files into org-filer files.

When gather is t, call the function `org-filer-gather-files' after.
"
  (interactive)
  (save-excursion
    (org-back-to-heading-or-point-min)
    (when-let ((attach-dir (org-attach-dir)))
      (save-excursion
	(org-end-of-meta-data t)
	(dolist (file (directory-files attach-dir t))
	  (when (not (file-symlink-p file))
	    (org-filer-import-file file 'mv
				   (funcall org-filer-automatic-description-function file)))))
      (delete-directory attach-dir t t)
      (message "Attachment directory %s removed" attach-dir))
    ;; remove tag attached
    (org-set-tags (remove "ATTACHED" (org-get-tags (point) t)))
    (when gather
      (org-filer-gather-files))))

;; todo finir
(defun org-filer-transform-org-attach (&optional gather)
  "Transform each org-attach heading to the standard of org-filer. The file are inserted just after the heading. The scope are all files with extension .org in org-directory.

  Can call the command to \"synchronise\" to keep previous behavior of org-attach.

If with gather, will call with argument gather, will call `org-filer-gather-files'.
"
  (when (yes-or-no-p "That will remove all ATTACHED tags and import file as link with file: . Are you sure to import files ? ")
    (org-map-entries 
     (lambda ()
       (org-filer-import-org-attached-files gather))
     "+ATTACH"
     (directory-files-recursively org-directory "\\.org$"))
    ;; take in account files attached to a file of org-mode ?
    ))

(defun org-filer--get-all-org-file-links ()
  "Return a list of all `file:` links in all Org files in `org-directory`."
  (let (links)
    (dolist (file (directory-files-recursively org-filer-notes-directory "\\.org$"))
      (with-temp-buffer
        (insert-file-contents file)
        (org-element-map (org-element-parse-buffer) 'link
          (lambda (link)
            (when (string= (org-element-property :type link) "file")
              (push (file-name-nondirectory (org-element-property :path link)) links))))))
    links))

(defun org-filer--find-unlinked-files ()
  "Return a list files that are not linked in any org-filer file."
  (let* ((all-files (directory-files org-filer-directory nil directory-files-no-dot-files-regexp))
         (linked-files (org-filer--get-all-org-file-links)))
    (seq-difference all-files linked-files)))

(defun org-filer-dired-unlinked-files ()
  "Open a dired buffer with only the file that have no link."
  (interactive)
  (dired org-filer-directory)
  (dired-mark-files-regexp (regexp-opt (org-filer--find-unlinked-files)))
  (dired-toggle-marks)
  (dired-do-kill-lines))

(defun org-filer--copy-file (path &optional description)
  "Import a copy of a file already imported."
  (org-filer-import-file path 'cp (concat "Copy of " description)))

(defun org-filer-copy-files (path &optional description)
  "Copies linked files in an Org mode buffer. It retrieves links from either an active region or link at point, then copies the files and add a link to the copie."
  (interactive)
  (if (region-active-p)
      (let ((links (org-filer--retrieve-link)))
	(end-of-line)
	(insert "\n")
	(dolist (link links)
	  (org-filer--copy-file (car link) (concat "Copy of "(cdr link)))))

    ;; todo here, because how to have the link ?
    (when-let ((link-at-point (when (equal 'link (car (org-element-at-point)))
				(org-element-at-point))))
      (if (string= "file "(org-element-property :type link))
	  (let ((description (concat "Copy of "(org-link-display-format (buffer-substring (org-element-property :begin link-at-point) (org-element-property :end link-at-point))))))
	    (org-filer--copy-file (expand-file-name (org-element-property :path link-at-point)) description))

	(message "No link file inside org-filer at point to copy.")))))

(provide 'org-filer)

;;; org-filer.el ends here
