;;; init-functions.el --- Define custom functions -*- lexical-binding: t -*-

;;; Commentary:
;;; Define Functions

;;; Code:

(require 'cl-lib)

(declare-function async-inject-variables 'async)
(declare-function flycheck-buffer 'flycheck)
(declare-function upgrade-packages 'init-package)

(unless (fboundp 'caadr)
  (defalias 'caadr #'cl-caadr))

(defun sn-convert-to-unix-coding-system ()
  "Change the current buffer encoding to unix."
  (interactive)
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (set-buffer-file-coding-system 'unix))))

(defun sn-save-all-buffers-silently ()
  "Save all modified buffers without prompting."
  (interactive)
  (save-some-buffers t)
  (message "Saved all buffers."))

(defun sn-switch-last-buffer ()
  "Switch back and forth between two buffers easily."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun sn--increment-number (&optional arg)
  "Increment the number forward point by ARG."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
	(setq inc-by (if arg 1))
	(skip-chars-backward "0123456789")
	(when (re-search-forward "[0-9]+" nil t)
	  (setq field-width (- (match-end 0) (match-beginning 0)))
	  (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
	  (when (< answer 0)
	    (setq answer (+ (expt 10 field-width) answer)))
	  (replace-match (format (concat "%0" (int-to-string field-width) "d") answer)))))))

(defun sn-increment-number (arg)
  "Adjust the height of the default face by ARG.
ARG may be passed as a numberic prefix."
  (interactive "p")
  (let ((event last-command-event)
	(echo-keystrokes nil))
    (let* ((base (event-basic-type event))
	   (step (pcase base
		   ((or ?+ ?=) arg)
		   (?- (- arg))
		   (?0 0)
		   (_ arg))))
      (sn--increment-number step)
      (set-transient-map
       (let ((map (make-sparse-keymap)))
	 (dolist (mods '(() (control)))
	   (dolist (keys '(?- ?+ ?= ?0))
	     (define-key map (vector (append mods (list keys)))
	       (lambda () (interactive) (sn-increment-number (abs arg))))))
	 map)))))

(defmacro sn-save-column(&rest body)
  "Helper to move a line(BODY)."
  `(let ((column (current-column)))
     (unwind-protect (progn ,@body) (move-to-column column))))
(put 'sn-save-column 'lisp-indent-function 0)

(defun sn-move-line-up ()
  "Move the current line."
  (interactive)
  (sn-save-column (transpose-lines 1) (forward-line -2)))

(defun sn-move-line-down ()
  "Move the current line."
  (interactive)
  (sn-save-column (forward-line 1) (transpose-lines 1) (forward-line -1)))

(defun sn-delete-current-file ()
  "Delete current file and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently beign edited!!"))
  (when (yes-or-no-p (format "Are you sure to delete this file? '%s'" (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name) t)
    (kill-this-buffer)))

(defun sn-sudoedit (&optional arg)
  "Open current or ARG file as root."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
			 (read-file-name "Find file as root: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sn-rename-current-file (new-name)
  "Rename both current buffer and file to NEW-NAME."
  (interactive "FNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (unless filename (error "Buffer '%s' is not visiting a file!!" name))
    (if (get-buffer new-name)
	(message "A buffer named '%s' already exists!!" new-name)
      (progn (when (file-exists-p filename)
	       (rename-file filename new-name 1))
	     (rename-buffer new-name)
	     (set-visited-file-name new-name)))))

(defun sn-copy-file-content (file)
  "Copy the FILE contents to the clipboard."
  (interactive "f")
  (with-current-buffer (find-file-noselect file)
    (kill-new (buffer-substring-no-properties (point-min) (point-max))))
  (message "Copied contents of '%s' to the clipboard" file))

(defun sn-copy-file-name ()
  "Copy the current file name to the clipboard."
  (interactive)
  (if-let ((filename (if (equal major-mode 'dired-mode)
			default-directory
		      (buffer-file-name))))
      (progn
	(kill-new filename)
	(message "Copied '%s' to the clipboard." filename))
    (warn "Current buffer is not attached to a file!!")))

(defun sn-reload-init-file ()
  "Reload Emacs configuration."
  (interactive)
  (load user-init-file)
  (message "Emacs configuration reloaded ."))

(defun sn-surround-with-quotes ()
  "Enclose the word under cursor in quotes."
  (interactive)
  (forward-word)
  (backward-word)
  (insert "\"")
  (forward-word)
  (insert "\""))

(defun sn-surround-with-single-quotes ()
  "Enclose the word under cursor in single quotes."
  (interactive)
  (forward-word)
  (backward-word)
  (insert "'")
  (forward-word)
  (insert "'"))

(defun sn-surround-with-backquotes ()
  "Enclose the word under cursor in backquotes."
  (interactive)
  (forward-word)
  (backward-word)
  (insert "`")
  (forward-word)
  (insert "`"))

(provide 'init-functions)

;;; init-functions.el ends here
