;;; org-filer.el --- Alternative to org-attach to manage with org file: links other types files. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Clément Payard

;; Author: Clément Payard <emacs@clementpayard.com>
;; URL: https://github.com/Cletip/org-filer
;; Version: 1.1
;; Package-Requires: ((emacs "26.1") (org "9.4") (cl-lib "0.5") (seq "2.20"))
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
(require 'find-lisp)

(defgroup org-filer nil
  "Manage file attachments with links in Org mode."
  :prefix "org-filer-"
  :group 'org
  :link '(url-link :tag "GitHub Repository" "https://github.com/Cletip/org-filer")
  :tag "Org Filer")

(defcustom org-filer-directory (expand-file-name (concat org-directory "org-filer/"))
  "The path where to store all files imported."
  :group 'org-filer
  :type 'directory) ; Changed to 'directory' type for better completion/validation

(defcustom org-filer-notes-directory org-directory
  "The path used to have relative link to `org-filer-directory' if `org-filer-dir-relative' is set to t."
  :group 'org-filer
  :type 'directory)

(defcustom org-filer-directory-to-gather-files 'org-attach-dir-get-create
  "Function that return a directory where to gather files with `org-filer-gather-files'."
  :group 'org-filer
  :type 'function)

(defcustom org-filer-automatic-description-function 'file-name-nondirectory
  "This function takes a path as input and returns a string used for the description for the function `org-filer-import-file-dired' and `org-filer-import-org-attached-files'"
  :group 'org-filer
  :type 'function)

(defcustom org-filer-dir-relative nil
  "Non-nil means use relative path to link file, from `org-filer-notes-directory' to `org-filer-directory'."
  :group 'org-filer
  :type 'boolean)

(defcustom org-filer-preferred-method 'cp
  "The preferred method to use for org-filer.
Allowed values are the same as `org-attach-method'."
  :group 'org-filer
  :type '(choice
          (const :tag "Copy" cp)
          (const :tag "Move/Rename" mv)
          (const :tag "Hard Link" ln)
          (const :tag "Symbol Link" lns)))

(defun org-filer--select-method ()
  "Select the org filer method interactively."
  (intern (completing-read "Choose attachment method: "
                           '("cp" "mv" "ln" "lns") nil t nil nil 'cp))) ; Default to 'cp'

(defun org-filer--ask-description (file-or-directory-name)
  "Ask to the user the description for a file, and return as a string.
Default description is the base name of FILE-OR-DIRECTORY-NAME."
  (let ((default-desc (file-name-nondirectory file-or-directory-name)))
    (read-string (format "Description for '%s' (default: %s): " default-desc default-desc)
                 nil nil default-desc))) ; Use default if user enters empty string

(defun org-filer--file-or-directory-name-with-uuid (file-or-directory-name)
  "Return the new unique file name based on UUID, keeping the extension."
  (concat
   (org-id-uuid)
   ;; get extension of the file, or nothing
   (when (and file-or-directory-name (not (file-directory-p file-or-directory-name)))
     (let ((extension (file-name-extension file-or-directory-name)))
       (when extension
         (concat "." extension))))))

(defun org-filer--insert-file-link (destination &optional description)
  "Insert the link of a org-filer link.
If `org-filer-dir-relative' is non-nil, insert the link relative
to the `org-filer-notes-directory'."
  (let ((org-link-file-path-type ; Temporarily set link type based on relative setting
         (if org-filer-dir-relative 'relative org-link-file-path-type))
        (default-directory ; Temporarily set default-directory for relative path calculation
          (if org-filer-dir-relative org-filer-notes-directory default-directory)))
    ;; Ensure description is a string, use filename if nil/empty
    (let ((desc (if (or (null description) (string-empty-p description))
                    (file-name-nondirectory destination)
                  description)))
      (org-insert-link nil (concat "file:" (expand-file-name destination)) desc))))

(defun org-filer-import-file (path method &optional description)
  "Import file/directory from PATH using METHOD into `org-filer-directory'.
METHOD can be 'cp, 'mv, 'ln, or 'lns.
Inserts an Org link to the imported file at point.
Asks for DESCRIPTION if not provided.

With universal prefix argument `\\[universal-argument]', prompts for METHOD.
Otherwise uses `org-filer-preferred-method'.
"
  (interactive
   (let* ((path (read-file-name "Select file or directory to import: " nil nil t))
          (method (if current-prefix-arg
                      (org-filer--select-method)
                    org-filer-preferred-method))
          ;; Ask for description here, passing the original path for context
          (description (org-filer--ask-description path)))
     (list path method description)))

  (let* ((target-dir (file-name-as-directory org-filer-directory)) ; Ensure trailing slash
         (new-name (org-filer--file-or-directory-name-with-uuid path))
         (destination (expand-file-name new-name target-dir)))

    ;; Ensure target directory exists
    (unless (file-directory-p target-dir)
      (make-directory target-dir 'parents)
      (message "Created org-filer directory: %s" target-dir))

    ;; Perform the file operation
    (condition-case err

	;; link is not available yet : for each case, define what happen. For mv, remove the link, for cp, make a right link, etc... 

        (progn
          (message "Importing %s to %s using method '%s'..." path destination method)
          (pcase method
            ('mv (rename-file path destination t)) ; t = OK if target exists (UUID makes collision unlikely)
            ('cp
             (if (file-directory-p path)
                 (copy-directory path destination t) ; t = keep mode/timestamps
               (copy-file path destination t t)))    ; t = OK if exists, t = keep mode/timestamps
            ('ln (add-name-to-file path destination)) ; Hard link
            ('lns (make-symbolic-link path destination t))) ; t = OK if exists
          ;; insert the link on success
          (org-filer--insert-file-link destination description)
          (insert "\n")
          (message "Successfully imported %s as %s" path destination))
      (error (user-error "Failed to import %s using method %s: %s" path method err)))))


(defun org-filer-create-and-import (name &optional description)
  "Create a new empty file or directory directly within `org-filer-directory'.

If NAME provided by the user contains an extension (e.g., 'report.pdf'),
creates an empty file. If NAME has no extension (e.g., 'project_images'),
creates an empty directory.

The final unique filename is generated using `org-id-uuid' while preserving
the original extension (if any). A `file:' link to the new item, using
the provided DESCRIPTION, is inserted at point.
"
  (interactive
   (let* (;; Prompt for a base name, used to determine type (file/dir) and get extension
          (name (read-string "Enter base name for new item (e.g., 'report.pdf' or 'project_images'): "))
          ;; Ask for the description for the Org link
          (description (org-filer--ask-description name)))
     (list name description)))

  ;; --- Prepare paths and names ---
  (let* ((target-dir (file-name-as-directory org-filer-directory)) ; Ensure trailing slash
         (extension (file-name-extension name)) ; Extract extension (".pdf", ".txt", or nil)
         (is-dir (null extension)) ; It's a directory if no extension was found
         (uuid (org-id-uuid)) ; Generate the unique ID part (this is the correct function)
         ;; Construct final filename: UUID.extension (or just UUID if it's a dir)
         (final-filename (concat uuid (if extension (concat "." extension) "")))
         ;; Construct the full absolute path for the new item
         (final-path (expand-file-name final-filename target-dir)))

    ;; --- Perform creation and linking ---
    (condition-case err ; Catch errors during file operations or linking
        (progn
          ;; 1. Ensure the main org-filer directory exists
          (unless (file-directory-p target-dir)
            (make-directory target-dir t) ; t = create parent directories if needed
            (message "Created org-filer directory: %s" target-dir))

          ;; 2. Create the actual empty file or directory
          (message "Creating %s: %s"
                   (if is-dir "directory" "file")
                   final-path)
          (if is-dir
              (make-directory final-path) ; Create the directory
            (with-temp-file final-path)) ; Creates an empty file at final-path

          ;; 3. Insert the Org link pointing to the new item
          (org-filer--insert-file-link final-path description)
          (insert "\n") ; Add a newline for better formatting

          (message "Successfully created and linked: %s" final-path))

      ;; Error handling
      (error (user-error "Failed to create or link '%s' (as %s): %s"
                         name final-filename err)))))


(defun org-filer-import-file-dired (files &optional method automatic)
  "Import FILES marked or current file in `dired' using `org-filer-import-file'.
Opens a new Org buffer listing the links to the imported files.

If called with `\\[universal-argument]', prompts for the import METHOD for each file.
Otherwise, uses `org-filer-preferred-method'.

Prompts whether to generate descriptions automatically using
`org-filer-automatic-description-function' or ask for each file.
"
  (interactive
   (list (dired-get-marked-files)
         (if current-prefix-arg nil org-filer-preferred-method) ; Use preferred method if no prefix arg
         (yes-or-no-p (format "Use automatic descriptions via '%s'? (No = ask for each) "
                              org-filer-automatic-description-function))))
  (unless (eq major-mode 'dired-mode)
    (user-error "This command must be run from a `dired' buffer."))
  (when files
    (switch-to-buffer-other-window (generate-new-buffer "*org-filer importation*"))
    (org-mode)
    (insert "#+TITLE: Org Filer Import Results\n\n")
    (insert "Imported files from dired:\n\n")
    (dolist (file files)
      (condition-case err
          (org-filer-import-file
           file
           (or method (org-filer--select-method)) ; Ask if method is nil (prefix arg case)
           (if automatic
               (funcall org-filer-automatic-description-function file)
             (org-filer--ask-description file))) ; Pass full path to ask-description
        (error (message "Error importing file %s: %s" file err)
               (insert (format "- Error importing %s: %s\n" file err)))))
    (goto-char (point-min))))

(defun org-filer--retrieve-link ()
  "Return list of cons (PATH . DESCRIPTION) for `file:' links.
Scope is the active region, current subtree, or whole buffer
if before the first heading."
  (save-restriction
    (if (region-active-p)
        (narrow-to-region (region-beginning) (region-end))
      (unless (org-before-first-heading-p) (org-narrow-to-subtree)))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (string= (org-element-property :type link) "file")
          (let* ((path (org-element-property :path link))
                 (contents (org-element-contents link))
                 ;; Use filename as description if link has no explicit description part
                 (description (if contents (car contents) (file-name-nondirectory path))))
            (cons path description)))))))


(defun org-filer-gather-files ()
  "Gather all file: links in the current Org node to a target directory.
The target directory is determined by the `org-filer-directory' property
in the current subtree, or by `org-filer-directory-to-gather-files'.

For each `file:' link found, creates a symlink in the target directory.
The symlink name is based on the link's description (or filename if none).
Name conflicts are resolved by appending `_ (n)'.

Old symlinks in the target directory are removed before creating new ones.

With `\\[universal-argument]' prefix, files are *copied* instead of symlinked.
"
  (interactive)
  (save-excursion
    (org-back-to-heading-or-point-min)
    (let* ((target-dir-base (or (org-entry-get (point) "org-filer-directory")
                                (funcall org-filer-directory-to-gather-files)))
           (target-dir (file-name-as-directory target-dir-base)) ; Ensure trailing slash
           (links (org-filer--retrieve-link))
           (copy-method current-prefix-arg)) ; Use prefix arg to decide copy vs symlink

      ;; Ensure the target directory exists
      (unless (file-directory-p target-dir)
        (make-directory target-dir t)
        (message "Created directory: %s" target-dir))

      ;; Clear previous symlinks ONLY if we are creating symlinks
      (unless copy-method
        (message "Clearing old symlinks in %s..." target-dir)
        (let ((count 0)
              (files-to-check (directory-files target-dir t nil t))) ; t=no-sort, t=no-stat-requests
          (dolist (file files-to-check)
            (when (file-symlink-p file)
              (condition-case err
                  (progn (delete-file file) (setq count (1+ count)))
                (error (message "Could not delete symlink %s: %s" file err)))))
          (message "Cleared %d old symlinks." count)))

      ;; Create new links/copies
      (message "Processing %d links..." (length links))
      (dolist (link links)
        (condition-case err
            (let* ((source-path-raw (car link))
                   ;; Assume relative paths in links are relative to current buffer's dir
                   (source-path (expand-file-name source-path-raw (file-name-directory buffer-file-name)))
                   (description (cdr link)) ; Already defaults to filename in retrieve-link if needed
                   (base-name description) ; Use description (or fallback filename) as base
                   (target-path (expand-file-name base-name target-dir))
                   (final-target-path target-path)
                   (count 1))

              ;; Resolve name conflicts by appending _(n)
              (while (and (file-exists-p final-target-path) ; Check file-exists-p OR file-symlink-p
                          ;; Avoid infinite loop if we are relinking the exact same source
                          (not (and (file-symlink-p final-target-path)
                                    (equal (read-link final-target-path nil) source-path)))) ; nil = dont signal error
                (setq final-target-path (expand-file-name
                                         (format "%s_(%d)%s"
                                                 (file-name-sans-extension base-name)
                                                 count
                                                 (let ((ext (file-name-extension base-name)))
                                                   (if ext (concat "." ext) "")))
                                         target-dir))
                (setq count (1+ count)))

              ;; Check if source file exists before trying to link/copy
              (if (file-exists-p source-path)
                  (progn
                    (if copy-method
                        (progn
                          (message "Copying %s to %s" source-path final-target-path)
                          (if (file-directory-p source-path)
                              (copy-directory source-path final-target-path t)
                            (copy-file source-path final-target-path t t))) ; ok-if-exists, preserve-mode
                      (progn
                        ;; Check if target already exists and is the correct link
                        (if (and (file-symlink-p final-target-path)
                                 (equal (read-link final-target-path nil) source-path))
                            (message "Skipping identical symlink: %s" final-target-path)
                          (message "Linking %s -> %s" final-target-path source-path)
                          (make-symbolic-link source-path final-target-path t))))) ; t = ok-if-exists
                (message "Warning: Source file does not exist or is not accessible: %s" source-path)))
          (error (message "Error processing link for %s: %s" (car link) err))))

      ;; Open the directory
      (message "Finished processing links. Opening directory %s." target-dir)
      (org-filer-open-directory-to-gather-files))))


(defun org-filer-open-directory-to-gather-files ()
  "Open the directory associated with the function `org-filer-gather-files'."
  (interactive)
  (let ((dir (funcall org-filer-directory-to-gather-files)))
    (unless dir (user-error "Could not determine directory to gather files"))
    (dired dir)))

(defun org-filer-import-org-attached-files (&optional gather follow-links)
  "Convert `org-attach` files for the current entry to `org-filer` links.
Finds the attachment directory for the current Org entry, imports
each file within it using `org-filer-import-file' (method 'mv'),
and inserts `file:' links at the end of the entry metadata.
Removes the attachment directory and the 'ATTACH' tag from the heading.

If GATHER is non-nil, calls `org-filer-gather-files' afterwards.
If FOLLOW-LINKS is non-nil, symbolic links in the attachment directory
will be resolved and the target files will be imported instead of the links themselves.

Should be called with point at the heading of the entry.
"
  (interactive)
  (save-excursion ; Keep point around the heading
    (org-back-to-heading-or-point-min t) ; Ensure we are at heading start
    (if-let ((attach-dir (org-attach-dir)) ; Get attach dir for current entry
	     (imported-count 0))
	
	(progn


	  (when (file-directory-p attach-dir) ; Check if it actually exists
            (message "Importing attachments from %s" attach-dir)
            (save-excursion ; Go to end of metadata to insert links
              (org-end-of-meta-data t)

              (dolist (file (directory-files attach-dir t "[^.]")) ; Get files, skip . and ..
		(unless (file-directory-p file) ; Skip subdirectories in attach dir for now. Todo implement for dir
		  (let ((path-to-import file))

		    ;; case of link
                    (when (and (file-symlink-p file) follow-links)
		      (let ((link-truename (file-truename path-to-import)))
			(when (file-exists-p link-truename)
			  (setq path-to-import link-truename))))

		    ;; import file
                    (let ((desc (funcall org-filer-automatic-description-function file)))
                      (org-filer-import-file path-to-import 'mv desc)
                      
		      ;; when that was a link, remove this one. Have to be in org-filer-import-file, but not available yet
		      (when (and (file-symlink-p file) follow-links)
			(delete-file file))
		      
		      (setq imported-count (1+ imported-count)))
                    )))))

	  ;; Delete the original attachment directory
	  ;; (condition-case err
	  (delete-directory attach-dir t t) ; recursive, trash
	  ;; (error (message "Could not remove original attachment directory %s: %s" attach-dir err)))

	  ;; Remove the "ATTACH" tag
	  (org-set-tags (remove "ATTACH" (org-get-tags nil t))) ; nil = current heading, t = include inherited
	  (message "Removed 'ATTACH' tag.")

	  ;; Call gather if requested
	  (when gather
	    (message "Gathering files for the entry...")
	    (org-filer-gather-files))

	  (message "org-file : Imported %d files, %s" imported-count attach-dir)

	  )


      ;; case no attachment
      (message "No attachment directory found or it's not a directory for this entry.")

      )
    )

  )


(defun org-filer-transform-org-attach (&optional gather)
  "Transform headings tagged 'ATTACH' in Org files within `org-directory`.

This function searches recursively for all `.org` files in `org-directory`.
For each file, it finds headings tagged 'ATTACH'. For each such heading,
it calls `org-filer-import-org-attached-files' to convert
`org-attach`-style attachments to `org-filer` links.

The original attachment directory is removed, and the 'ATTACH' tag is
removed from the heading. Modified files are saved automatically.

If GATHER is non-nil (e.g., called with prefix `\\[universal-argument]`),
`org-filer-gather-files' is called for each transformed heading.

This is a potentially long-running and destructive operation.
It will modify and save your Org files. Backup is recommended.
"
  (interactive "P") ; Pass prefix arg to gather
  (if (yes-or-no-p (format "Transform ALL 'ATTACH' tagged entries in '%s'?\nThis modifies and saves .org files.\nBackup recommended. Proceed? "
                           org-directory))
      (let ((org-files (find-lisp-find-files org-directory "\\.org$"))
            (transformed-count 0)
            (file-count 0)
            (error-count 0))
        (message "Starting transformation of ATTACHed entries...")
        (dolist (file org-files)
          (setq file-count (1+ file-count))
          (message "[%d/%d] Processing file: %s" file-count (length org-files) file)
          (condition-case- P err ; Catch errors during file processing
			   (with-current-buffer (find-file-noselect file) ; Open file in background
			     (let ((buffer-modified nil)) ; Track if buffer was modified
			       (save-excursion
				 (goto-char (point-min))
				 ;; Use org-map-entries for robust heading finding
				 (org-map-entries
				  (lambda ()
				    ;; We are already at the heading by org-map-entries
				    (message "  Found ATTACH tag at: %s" (nth 4 (org-heading-components)))
				    (condition-case entry-err
					;; Call the import function for this entry
					(org-filer-import-org-attached-files gather) ; gather is passed correctly
				      (error (message "    Error transforming entry: %s" entry-err)
					     (setq error-count (1+ error-count))))
				    (setq buffer-modified t) ; Mark buffer as modified
				    (setq transformed-count (1+ transformed-count)))
				  "+ATTACH" ; Match entries with ATTACH tag
				  'file)) ; Scope is current file
			       ;; Save the buffer only if it was modified
			       (when buffer-modified
				 (message "  Saving modified file: %s" file)
				 (save-buffer))))
			   ;; Error handler for with-current-buffer or save-buffer
			   (error
			    (setq error-count (1+ error-count))
			    (message "  Error processing file %s: %s" file err))))
        (message "Transformation complete. Processed %d files, transformed %d entries, encountered %d errors."
                 file-count transformed-count error-count))
    (message "Transformation cancelled.")))


(defun org-filer--get-all-org-file-links ()
  "Return a list of all `file:` link *target basenames* in Org files.
Searches recursively in `org-filer-notes-directory` for `.org` files."
  (let (links-basenames
        (org-files (find-lisp-find-files org-filer-notes-directory "\\.org$")))
    (dolist (file org-files links-basenames) ; Return links-basenames at the end
      (with-temp-buffer
        (condition-case err ; Handle errors reading/parsing files
            (progn
              (insert-file-contents file)
              (org-mode) ; Ensure org mode is active for parsing
              (org-element-map (org-element-parse-buffer) 'link
                (lambda (link)
                  (when (string= (org-element-property :type link) "file")
                    ;; We need the BASENAME of the TARGET file for comparison
                    ;; with files in org-filer-directory. Assume filer files
                    ;; have unique UUID names.
                    (let ((target-path (org-element-property :path link)))
                      ;; Check if the link points inside org-filer-directory
                      (when (string-prefix-p (file-truename org-filer-directory) ; Compare canonical paths
                                             (file-truename (expand-file-name target-path (file-name-directory file))))
                        (push (file-name-nondirectory target-path) links-basenames)))))))
          (error (message "Warning: Could not process file %s for links: %s" file err)))))))


(defun org-filer--find-unlinked-files ()
  "Return a list of files in `org-filer-directory` not linked from Org files.
Compares basenames of files in `org-filer-directory` against
basenames found in `file:` links pointing to that directory within
Org files located in `org-filer-notes-directory`.
"
  (let* ((filer-dir (file-name-as-directory org-filer-directory))
         (all-files-in-filer (directory-files filer-dir nil directory-files-no-dot-files-regexp)) ; Get basenames
         (linked-files-basenames (delete-dups (org-filer--get-all-org-file-links))) ; Get unique basenames from links
         (unlinked-files))
    (message "Found %d files in %s." (length all-files-in-filer) filer-dir)
    (message "Found %d unique file link targets pointing to %s." (length linked-files-basenames) filer-dir)
    ;; Use seq-difference to find files in filer dir not present in link targets
    (setq unlinked-files (seq-difference all-files-in-filer linked-files-basenames #'string=))
    (message "Found %d potentially unlinked files." (length unlinked-files))
    unlinked-files))

(defun org-filer-dired-unlinked-files ()
  "Open Dired in `org-filer-directory` showing potentially unlinked files.
Lists files present in `org-filer-directory` that do not seem to be
referenced by any `file:` link within the Org files found under
`org-filer-notes-directory`.

Note: This check relies on comparing basenames and may not be
perfect if files are linked using complex relative paths or if
`org-filer-notes-directory` doesn't cover all relevant Org files.
"
  (interactive)
  (let ((unlinked (org-filer--find-unlinked-files)))
    (if unlinked
        (progn
          (dired org-filer-directory)
          ;; Mark the unlinked files
          (dired-mark-files-regexp (regexp-opt unlinked t)) ; t = exact match
          ;; Invert marks and kill unmarked lines (leaves only marked = unlinked)
          (dired-toggle-marks)
          (dired-do-kill-lines)
          (message "Showing %d potentially unlinked files in %s" (length unlinked) org-filer-directory))
      (message "No unlinked files found in %s" org-filer-directory))))


(defun org-filer--get-link-at-point ()
  "Return (SOURCE-PATH . DESCRIPTION) of the `file:' link at point, or nil.
SOURCE-PATH is expanded. DESCRIPTION defaults to filename if link has none."
  (let ((element (org-element-context)))
    (when (and element (eq (org-element-type element) 'link)
               (string= (org-element-property :type element) "file"))
      (let* ((path-raw (org-element-property :path element))
             ;; Expand path relative to the buffer's directory
             (path-expanded (expand-file-name path-raw (file-name-directory buffer-file-name)))
             ;; Get description from contents, default to filename if no description
             (contents (org-element-contents element))
             (description (if contents (car contents) (file-name-nondirectory path-raw)))) ; Use original basename for desc default
        (cons path-expanded description)))))

;; Internal helper for copying
(defun org-filer--copy-file (source-path &optional description)
  "Internal helper: Import a copy of SOURCE-PATH using 'cp' method.
Inserts a link to the new copy using DESCRIPTION.
If DESCRIPTION is nil or empty, uses the source filename."
  (unless (file-exists-p source-path)
    (user-error "Source file for copy does not exist: %s" source-path))
  (let ((desc (if (or (null description) (string-empty-p description))
                  (file-name-nondirectory source-path)
                description)))
    ;; Call import-file non-interactively with 'cp' method
    (org-filer-import-file source-path 'cp desc)))


(defun org-filer-copy-files ()
  "Copies linked file(s) and inserts new link(s) to the copies.

If a region is active, processes all `file:' links within the region.
If no region is active, processes the `file:' link at point.

The new link points to a copy of the original file, stored in
`org-filer-directory' with a unique name. The description
defaults to the original link's description. New links are inserted
after the original link or after the region.
"
  (interactive)
  (if (region-active-p)
      ;; --- Region Active ---
      (let ((links (org-filer--retrieve-link))) ; Gets (raw-path . description)
        (if links
            (progn
              (goto-char (region-end)) ; Move to end of region
              (unless (looking-at-p "^\\s-*$") (insert "\n")) ; Add newline if needed
              (message "Copying %d links from region..." (length links))
              (dolist (link links)
                (condition-case err
                    (let* ((raw-path (car link))
                           (desc (cdr link))
                           ;; Expand path relative to current buffer
                           (source-path (expand-file-name raw-path (file-name-directory buffer-file-name))))
                      (org-filer--copy-file source-path desc)) ; Use the internal copy func
                  (error (message "Error copying link %s: %s" (car link) err))))
              (message "Finished copying links from region."))
          (message "No 'file:' links found in the active region.")))
    ;; --- No Region Active ---
    (let ((link-info (org-filer--get-link-at-point))) ; Gets (expanded-path . description)
      (if link-info
          (let ((source-path (car link-info))
                (description (cdr link-info)))
            (message "Copying link at point: %s" source-path)
            ;; Go to end of the link element before inserting
            (goto-char (org-element-property :end (org-element-context)))
            (insert "\n") ; Insert the new link on a new line after the original
            (condition-case err
                (org-filer--copy-file source-path description)
              (error (message "Error copying link at point %s: %s" source-path err))))
        (message "No 'file:' link found at point.")))))


(provide 'org-filer)

;;; org-filer.el ends here
