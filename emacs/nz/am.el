;;; am.el --- Amake-cache based file finder and grepper

(defvar am-workspace nil
  "Root of workspace for current command invocation")

(defvar am-files-directory nil
  "Directory whence file list was loaded")

(defvar am-files-alist nil
  "Associative list of files after scanning the amake dump file")

(defvar am-files-list nil
  "Simple list of files after scanning the amake dump file")

(defvar am-file-opener 'find-file-read-only
  "Set to file opener of your choice.  am-find-file uses this opener.
 Example: (setq am-file-opener 'view-file)")

(defvar am-file-opener-other-window 'find-file-read-only-other-window
  "Set to file opener of your choice.  am-find-file-other-window
 uses this opener.
 Example: (setq am-file-opener 'view-file)")

;;;
(defvar am-find-file-history nil
  "Amake Find File history")

;;;
(defun am-load-file-lists (&optional force-recache)
  ; cannot search for amake image because there is an earlier amake directory
  (setq am-workspace (locate-dominating-file "." "src/amake/amake.bsh"))
  (when (not am-workspace)
    (user-error "Working directory (%s) is not in a workspace" default-directory))
  (when (not (eq am-files-directory am-workspace))
    (setq am-files-list nil))
  (let ((default-directory am-workspace))
    (when (not (file-exists-p "amake"))
      (shell-command-to-string "cd src/amake && ./amake.bsh LINUXPRE")
      (when (not (file-exists-p "amake"))
        (user-error "Unable to amake image for workspace (%s)" am-workspace)))
    (when (or (not (file-exists-p ".am-files.el"))
              (file-newer-than-file-p "amake" ".am-files.el")
              force-recache)
      (shell-command-to-string "rm -f .am-files.el*")
      (shell-command-to-string "./amake -cache")
      (shell-command-to-string "./amake -emacs")
      (when (not (file-exists-p ".am-files.el"))
        (error "amake -emacs failed to create .am-files.el"))
      (when (not (byte-recompile-file ".am-files.el" nil 0))
        (error "Error(s) while recompiling .am-files.el"))
      (setq am-files-list nil))
    (when (not am-files-list)
      (setq am-files-alist nil)
      (load-file ".am-files.el")
      (setq am-files-directory am-workspace))
    (when (not am-files-list)
        (error "Empty amake file list"))))

(defun am-force-recache ()
  "Run 'amake -cache' and reload files lists"
  (interactive)
  (am-load-file-lists t))

;;;
(defun am-find-file (&optional other-window-p)
  "Find a file known to amake"
  (interactive)
  (am-load-file-lists)
  (let ((filename
         (ido-completing-read
          (concat "Workspace " am-workspace "- find file: ")
          am-files-list nil t nil am-find-file-history)))
    (if (> (length filename) 0)
        (let ((this-list (assoc filename am-files-alist)))
          (if (null this-list)
              (message "Can't find %s" filename)
            (funcall
             (if other-window-p am-file-opener-other-window am-file-opener)
             (concat (eval (cdr this-list)) (car this-list)))
            (if (buffer-file-name)
                (message
                 (if (file-writable-p buffer-file-name) "File %s" "File %s   (File is write protected)")
                 buffer-file-name)))))))

(defun am-find-file-other-window ()
  "Find a file known to amake, open it in other window"
  (interactive)
  (am-find-file t))



;; History of grep commands.
(defvar am-grep-history nil "History list for amake -(p)grep.")

(defun am-grep-execute (command-args)
  "Execute an amake -pgrep command and position cursor above first hit.

The asynchronous nature of filling the *grep* buffer precludes
actually positioning the cursor on the first hit."
  (compilation-start command-args 'grep-mode)
  (pop-to-buffer "*grep*")
  (forward-line 3))

(defun am-grep (command-args)
  "Run amake -pgrep, with user-specified args, collecting output in *grep*.
While the scan runs asynchronously, you can use \\[next-error] (M-x next-error),
or \\<grep-mode-map>\\[compile-goto-error] in the *grep* \
buffer, to go to the lines where the scan found
matches.  To kill the scan job before it finishes, type \\[kill-compilation].

This command uses a special history list for its COMMAND-ARGS, so you
can easily repeat an earlier amake -pgrep command."
  (interactive
   (let ((junk (am-load-file-lists))
         (amake-command (concat am-workspace "amake -pgrep ")))
     (list (read-shell-command "Run amake -pgrep (like this): "
                               amake-command
                               'am-grep-history
                               (or (car am-grep-history) amake-command)))))
  (am-grep-execute command-args))

(defun am-grep-command-wth-tag ()
  "Compute the default command for \\[am-grep-tag] to offer."
  (am-load-file-lists)
  (let ((tag-default (shell-quote-argument (grep-tag-default)))
	;; This a regexp to match single shell arguments.
	;; Could someone please add comments explaining it?
	(sh-arg-re "\\(\\(?:\"\\(?:[^\"]\\|\\\\\"\\)+\"\\|'[^']+'\\|[^\"' \t\n]\\)+\\)")
	(am-grep-default (or (car am-grep-history) (concat am-workspace "amake -pgrep TAG /src/"))))
    (message "tag-default= %s" tag-default)
    ;; In the default command, find the arg that specifies the pattern.
    (when (or (string-match
	       (concat "[^ ]+ -pgrep +" sh-arg-re " +[^ ]+")
	       am-grep-default)
	      ;; If the string is not yet complete.
	      (string-match "\\(\\)\\'" am-grep-default))
      ;; Now replace the pattern with the default tag.
      (replace-match tag-default t t am-grep-default 1))))

(defun am-grep-tag (command-args)
  "Run amake -pgrep for the tag at cursor, collecting output in *grep*.
While the scan runs asynchronously, you can use \\[next-error] (M-x next-error),
or \\<grep-mode-map>\\[compile-goto-error] in the *grep* \
buffer, to go to the lines where the scan found
matches.  To kill the scan job before it finishes, type \\[kill-compilation].

This command uses a special history list for its COMMAND-ARGS, so you
can easily repeat an earlier amake -pgrep command."
  (interactive
   (list (read-shell-command "Run amake -pgrep (like this): "
                             (am-grep-command-wth-tag)
                             'am-grep-history
                             nil)))
  (am-grep-execute command-args))
