;;; sbf.el --- Mathworks sandbox-based file finder  -*- lexical-binding: t -*-

;;====================================================
;; Various magic strings and path fragments
;;====================================================

;; File system visible paths and names
(defconst sbf--STATE_DIR   ".sbtools")  ; relative to sbf--sandbox
(defconst sbf--HASH_TABLE  "path-hash.el")

;; Magic shell invocations
(defconst sbf--cmd-find "find . -type d \\( -name .git -o -name .sbtools -o -name derived \\) -prune -o ! -type d -print")


;;====================================================
;; Global data
;;====================================================

(defvar sbf--common-prefix nil
  "Accumulated a prefix common to all paths.")

(defvar sbf--common-prefix-length 0
  "Length of sbf--common-prefix.")

(defvar sbf--find-file-history nil
  "MW Find File history.")

(defvar sbf--from-scratch t
  "When non-nil force a full recompuation of all state.")

(defvar sbf--hash-table nil
  "Map a file name to one or more directory paths.

Key is a filename, value is either a single directory path string
if the filename is unique or a a vector of directory path strings
when the filename is duplicated.  All directory path strings start
and end with slashes.  The longest prefix of complete path elements
common to all directory paths is factored out.")

(defvar sbf--path-hash nil "")
(defvar sbf--path-state-dir nil "")

(defvar sbf--reconstitute-path-prefix nil
  "A version of sandbox path appropriate for path reconstitution.")

(defvar sbf--sandbox nil
  "Sandbox with which in-memory state is associated.")

(defvar sbf--uniquified-list nil
  "Argument to completion functions (built or reloaded from disk).")

(defvar sbf--vector-hash nil
  "")


;;====================================================
;; Autoloaded entrypoints for IVY access
;;====================================================

;;;###autoload
(defun sbf-ivy-find-file ()
  "Use IVY completion with find-file in a Mathworks sandbox."
  (interactive)
  (sbf--ivy-find-file-helper #'find-file))

  ;; (sbf--current-completions)
  ;; (let ((filename
  ;;        (ivy-read
  ;;         (concat "Find-file in sandbox " sbf--sandbox ": ")
  ;;         sbf--uniquified-list
  ;;         :preselect (file-name-nondirectory (thing-at-point 'filename))
  ;;         :history 'sbf--find-file-history
  ;;         )))
  ;;   (when (> (length filename) 0)
  ;;     (find-file (sbf--reconstitute-file-path filename)))))


;;;###autoload
(defun sbf-ivy-find-file-read-only ()
  "Use IVY completion with find-file-read-only in a Mathworks sandbox."
  (interactive)
  (sbf--ivy-find-file-helper #'find-file-read-only))

  ;; (sbf--current-completions)
  ;; (let ((filename
  ;;        (ivy-read
  ;;         (concat "Find-file-read-only in sandbox " sbf--sandbox ": ") ;
  ;;         sbf--uniquified-list
  ;;         :preselect (file-name-nondirectory (thing-at-point 'filename))
  ;;         :history 'sbf--find-file-history
  ;;         )))
  ;;   (when (> (length filename) 0)
  ;;     (find-file-read-only (sbf--reconstitute-file-path filename)))))

;;;###autoload
(defun sbf-ivy-view-file ()
  "Use IVY completion with view-file in a Mathworks sandbox."
  (interactive)
  (sbf--ivy-find-file-helper #'view-file))

  ;; (sbf--current-completions)
  ;; (let ((filename
  ;;        (ivy-read
  ;;         (concat "View-file in sandbox " sbf--sandbox ": ")
  ;;         sbf--uniquified-list
  ;;         :preselect (file-name-nondirectory (thing-at-point 'filename))
  ;;         :history 'sbf--find-file-history
  ;;         )))
  ;;   (when (> (length filename) 0)
  ;;     (view-file (sbf--reconstitute-file-path filename)))))


;;====================================================
;; Lesser entrypoints
;;====================================================

;;;###autoload
(defun sbf-force-from-scratch ()
  "Recompute and reload all structures"
  (interactive)
  (setq sbf--from-scratch t)
  (sbf--current-completions))


;;====================================================
;; Find-file-helper and final path reconstruction
;;====================================================

(defun sbf--ivy-find-file-helper (find-file-func)
  "Use IVY completion with find-file-func in a Mathworks sandbox."
  (sbf--current-completions)
  (let* ((prompt (concat "Find-file in sandbox " sbf--sandbox ": "))
         (filename (let ((preselect (thing-at-point 'filename)))
                     (cond
                      (preselect
                       (ivy-read prompt sbf--uniquified-list
                                 :history 'sbf--find-file-history
                                 :preselect (file-name-nondirectory preselect)))
                      (t
                       (ivy-read prompt sbf--uniquified-list
                                 :history 'sbf--find-file-history))))))
    (when (> (length filename) 0)
      (funcall find-file-func (sbf--reconstitute-file-path filename)))))

(defun sbf--reconstitute-file-path (filename)
  ""
  (let ((space (string-match-p " " filename))
        (tag nil))
    (when space
      (setq tag (concat "/" (substring filename (1+ space)) "/"))
      (setq filename (substring filename 0 space)))
    (let ((value (gethash filename sbf--hash-table)))
      (unless (stringp value)
        (catch 'found-match
          (loop for path across value do
                (when (string-match-p tag path)
                  (setq value path)
                  (throw 'found-match nil)))
          (error "Could not locate matching directory path: filename= %s, tag= %s" filename tag)))
      (concat sbf--reconstitute-path-prefix sbf--common-prefix value filename))))


;;====================================================
;; Top level
;;====================================================

(defun sbf--current-completions ()
  "Return a completions list appropriate to the current context"
  (let ((default-directory (sbf--get-sandbox)))
    (unless (sbf--try-reload)
      (setq sbf--from-scratch nil)
      (sbf--build-hash-table)
      (sbf--build-uniquified)
      (sbf--persist)))
  sbf--uniquified-list)

;;====================================================
;; Building caching and switching
;;====================================================

(defun sbf--get-sandbox ()
  ""
  (let ((sandbox (locate-dominating-file "." 'sbf--sandbox-p)))
    (if sandbox
        ;; Convert to absolute with trailing "/"
        (setq sandbox
              (concat (substring (shell-command-to-string
                                  (concat "cd " sandbox "; pwd"))
                                 0 -1) "/"))
      (let ((msg (format "Working directory (%s) is not in a sandbox" default-directory)))
        (if (null sbf--sandbox)
            (user-error "%s; no prior sandbox available" msg)
          (setq sandbox sbf--sandbox)
          (message "%s: reusing %s" msg sandbox))))
    ;; If switching sandboxes then start with a clean slate
    (when (or sbf--from-scratch (not (string= sbf--sandbox sandbox)))
      (setq sbf--sandbox sandbox)
      (setq sbf--hash-table nil)
      (setq sbf--uniquified-list nil)
      (let ((abs-state-dir (concat sbf--sandbox sbf--STATE_DIR "/")))
        (setq sbf--path-state-dir abs-state-dir)
        (setq sbf--path-hash   (concat abs-state-dir sbf--HASH_TABLE)))))
  sbf--sandbox)

(defun sbf--sandbox-p (dir)
  ""
  (cond
   ((file-exists-p (concat dir "/.git"))
    t)
   ((file-exists-p (concat dir "/" sbf--STATE_DIR))
    t)))


;;====================================================
;; Reload persisted state
;;====================================================

(defun sbf--try-reload ()
  "Return t if able to restore persisted state and nil otherwise."
  (when (and (not sbf--from-scratch) (file-exists-p sbf--path-hash))
    (unless sbf--uniquified-list
      ;; Reload path-hash and uniquified-list from disk (hopefully from an .elc)
      (load sbf--path-hash)))
  sbf--uniquified-list)


;;====================================================
;; Hash table accumulation
;;====================================================

(defun sbf--build-hash-table ()
  "Construct a hash table from file of paths."
  ;; Start with a completely empty hash table
  (setq sbf--hash-table (make-hash-table :test 'equal))
  (with-temp-buffer
    (call-process-shell-command sbf--cmd-find nil t)
    ;; Ensure trailing '\n'
    (goto-char (point-max))
    (unless (eq (char-before) ?\n)
      (insert-char ?\n))
    ;; Init sbf--common-prefix
    (goto-char (point-min))
    (forward-line)
    (sbf--set-reconstitute-path-prefix (buffer-substring (point-min) (1- (point))))
    (search-backward "/")
    (setq sbf--common-prefix (buffer-substring (point-min) (1+ (point))))
    ;; Process buffer lines to build initial hash table (values are lists)
    (let ((line-end (point-min))
          (path-beg nil)
          (name-end nil))
      (loop until (eq line-end (point-max)) do
            (setq path-beg line-end)
            (goto-char path-beg)
            (forward-line 1)
            (setq line-end (point))
            (backward-char 1)
            (setq name-end (point))
            (when (sbf--keep-line-p path-beg (point))
              (search-backward "/" path-beg)
              (forward-char 1)
              (let ((path (buffer-substring path-beg (point))))
                (sbf--adjust-common-prefix path)
                (sbf--puthash-initial (buffer-substring (point) name-end) path)))))
    ;; Strip common prefix back to a path element boundary
    (erase-buffer)
    (insert sbf--common-prefix)
    (goto-char (point-max))
    (setq sbf--common-prefix (buffer-substring (point-min) (search-backward "/")))
    (setq sbf--common-prefix-length (length sbf--common-prefix))
    ;; Convert hash table value lists to vectors of shrunk strings
    (setq sbf--vector-hash (make-hash-table :test 'equal))
    (maphash 'sbf--convert-list-to-vector sbf--hash-table)
    (setq sbf--vector-hash nil)))

(defun sbf--set-reconstitute-path-prefix (path)
  ""
  (message (format "path= \"%s\"" path))
  (setq
   sbf--reconstitute-path-prefix
   (cond
    ((not (string= "/" (substring path 0 1)))
     sbf--sandbox)
    ((file-exists-p path)
     "")
    ((file-exists-p (concat "." path))
     (substring sbf--sandbox 0 -1))
    (t
     (user-error "Cannot determine relative or absolute nature of non-existent file: %s" path)))))

(defun sbf--keep-line-p (_line-beg _line-end)
  ;; check for /derived/ and well-formedness (distinct path and file)
  t)

(defun sbf--adjust-common-prefix (path)
  "If necessary shorten the current notion of the common prefix"
  (loop until (or (zerop (length sbf--common-prefix))
                  (string-prefix-p sbf--common-prefix path))
        ;; FIXME: must strip an entire path element
        do (setq sbf--common-prefix (substring sbf--common-prefix 0 -1))))

(defun sbf--puthash-initial (file path)
  "Record an occurrence of FILE in directory PATH"
  (let ((value (gethash file sbf--hash-table)))
    (cond
     ((null value)
      (setq value path))
     ((stringp value)
      (setq value (list path value)))
     (t
      (setq value (cons path value))))
    (puthash file value sbf--hash-table)))

(defun sbf--convert-list-to-vector (file paths)
  "Replace file's paths list with a vector of prefix-stripped paths"
  (if (stringp paths)
      (setq paths (substring paths sbf--common-prefix-length))
    (setq paths (vconcat paths))
    (loop for path across-ref paths do
          (setf path (substring path sbf--common-prefix-length)))
    (sort paths 'string<))
  (puthash file (gethash paths sbf--vector-hash paths) sbf--hash-table))


;;====================================================
;; Uniquify file names
;;====================================================

(defun sbf--build-uniquified ()
  "Build a list of \"uniquified\" file names.

Prefer unique path elements higher in the tree.  Use multiple
adjacent elements if necessary.  Return non-nil if a new list
was constructed and hence should be presisted to disk.

The search algorithm depends on the paths stored in the hash
table having slashes at either end.  This allows searching for
\"needles\" flanked by slashes which guarantees correctness.

Yes there really are 5 nested loops here... The assumption
is that no single filename will be duplciated too many times.
Also all loops over duplicates have numerous early exits."
  (let ((uniquified nil))
    (with-temp-buffer
      (loop for file being the hash-keys of sbf--hash-table
            using (hash-values paths) do
            (if (stringp paths)
                (setq uniquified (cons file uniquified))
              (loop for this across paths do
                    (let ((suffix nil))
                      (catch 'found-unique
                        (erase-buffer)
                        (insert this)
                        (catch 'path-exhausted
                          (loop for path-elements from 1 do
                                (catch 'sequence-too-long
                                  (loop for start-element from 1 do
                                        (catch 'not-unique
                                          (goto-char (point-min))
                                          (let ((start (search-forward "/" nil t start-element)))
                                            (unless start
                                              (throw 'path-exhausted nil))
                                            (let ((end (search-forward "/" nil t path-elements)))
                                              (unless end
                                                (when (= start-element 1)
                                                  (setq suffix this)
                                                  (throw 'found-unique nil))
                                                (throw 'sequence-too-long nil))
                                              (let ((needle (buffer-substring (1- start) end)))
                                                (loop for other across paths do
                                                      (when (and (not (string= this other))
                                                                 (string-match-p needle other))
                                                        (throw 'not-unique nil)))
                                                (setq suffix needle)
                                                (throw 'found-unique nil)))))))
                                )))
                      ;; Add a possibly "uniquified" file to the list
                      (setq uniquified
                            (cons (if suffix (concat file " " (substring suffix 1 -1)) file) uniquified)))))))

    (setq sbf--uniquified-list (sort uniquified 'string<))))


;;====================================================
;; Persist new sandbox state
;;====================================================

(defun sbf--persist ()
  "Write the completion list and hash table to disk as a .el / .elc pair"
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (insert"(setq sbf--reconstitute-path-prefix ")
      (prin1 sbf--reconstitute-path-prefix)
      (insert")\n(setq sbf--common-prefix ")
      (prin1 sbf--common-prefix)
      (insert")\n(setq sbf--hash-table ")
      (prin1 sbf--hash-table)
      (insert")\n(setq sbf--uniquified-list ")
      (prin1 sbf--uniquified-list)
      (insert ")\n"))
    (write-file sbf--path-hash))
  (let ((path-hash-elc (concat sbf--path-hash "c")))
    (shell-command-to-string (concat "rm -f " sbf--path-hash path-hash-elc))
    (byte-compile-file sbf--path-hash)))

(provide 'sbf)
