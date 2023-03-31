;;; wsf.el --- Workspace-based file finder  -*- lexical-binding: t -*-

;; TODO:
;; * Support wsf--workspace-root-marker-directories
;; ** Turn it into a defcustom
;; * Flesh out wsf--keep-line-p
;; ** omit prefix # and suffix ~ files

;;====================================================
;; Various magic strings and path fragments
;;====================================================

;; File system visible paths and names
(defconst wsf--HASH_TABLE "path-hash.el")

(defconst wsf--exclude-path-re "'/matlab/derived/\\|/\\._\\|\\.#\\|~$'")

;;====================================================
;; Global data
;;====================================================

(defvar wsf--workspace-root-marker-alist
  '((".sbtools" . ( "matlab/config"
                    "matlab/src"
                    "matlab/test"
                    "matlab/toolbox/shared/cgir_fe"
                    "matlab/toolbox/shared/cgxe" ))
    ( ".git"    . ( "." )))
  "An alist of STATE-DIR names and associated SEARCH-ROOTS.")

(defvar wsf--common-prefix nil
  "Accumulated a prefix common to all paths.")

(defvar wsf--common-prefix-length 0
  "Length of wsf--common-prefix.")

(defvar wsf--find-file-history nil
  "MW Find File history.")

(defvar wsf--from-scratch nil
  "When non-nil force a full recompuation of all state.")

(defvar wsf--hash-table nil
  "Map a file name to one or more directory paths.

Key is a filename, value is either a single directory path string
if the filename is unique or a a vector of directory path strings
when the filename is duplicated.  All directory path strings start
and end with slashes.  The longest prefix of complete path elements
common to all directory paths is factored out.")

(defvar wsf--path-hash nil "")
(defvar wsf--path-state-dir nil "")

(defvar wsf--reconstitute-path-rule nil
  "")

(defvar wsf--workspace nil
  "Workspace with which in-memory state is associated.")

(defvar wsf--uniquified-list nil
  "Argument to completion functions (built or reloaded from disk).")

(defvar wsf--vector-hash nil
  "")


;;====================================================
;; Autoloaded entrypoints for IVY access
;;====================================================

;;;###autoload
(defun wsf-find-file ()
  "Use IVY completion with find-file in a workspace."
  (interactive)
  (wsf--find-file-helper #'find-file))

;;;###autoload
(defun wsf-find-file-read-only ()
  "Use IVY completion with find-file-read-only in a workspace."
  (interactive)
  (wsf--find-file-helper #'find-file-read-only))

;;;###autoload
(defun wsf-view-file ()
  "Use IVY completion with view-file in a workspace."
  (interactive)
  (wsf--find-file-helper #'view-file))

;;====================================================
;; Lesser entrypoints
;;====================================================

;;;###autoload
(defun wsf-force-from-scratch ()
  "Recompute and reload all structures"
  (interactive)
  (setq wsf--from-scratch t)
  (wsf--current-completions))


;;====================================================
;; Find-file-helper and final path reconstruction
;;====================================================

(defun wsf--find-file-helper (find-file-func)
  "Use IVY completion with find-file-func in a Mathworks workspace."
  (wsf--current-completions)


  (let* ((prompt (concat "Find-file in workspace " wsf--workspace ": "))
         (filename (let ((preselect (thing-at-point 'filename)))
                     (completing-read-default prompt                    ; prompt
                                              wsf--uniquified-list      ; collection
                                              nil                       ; predicate
                                              t                         ; require match
                                              nil                       ; initial input
                                              'wsf--find-file-history   ; history
                                              (if preselect             ; default
                                                  (file-name-nondirectory preselect)
                                                nil))))))
    (when (> (length filename) 0)
      (funcall find-file-func (wsf--reconstitute-file-path filename))))

(defun wsf--reconstitute-file-path (filename)
  ""
  (let ((prefix (cond
                 ((eq wsf--reconstitute-path-rule 'workspace-relative)
                  wsf--workspace)
                 ((eq wsf--reconstitute-path-rule 'absolute)
                  "")
                 ((eq wsf--reconstitute-path-rule 'workspace-relative-no-final-slash)
                  (substring wsf--workspace 0 -1))
                 (t
                  (error "(WSF) Unrecognized wsf--reconstitute-path-rule: %s" wsf--reconstitute-path-rule))))
        (space (string-match-p " " filename))
        (tag nil))
    (when space
      (setq tag (concat "/" (substring filename (1+ space)) "/"))
      (setq filename (substring filename 0 space)))
    (let ((value (gethash filename wsf--hash-table)))
      (unless (stringp value)
        (catch 'found-match
          (loop for path across value do
                (when (string-match-p tag path)
                  (setq value path)
                  (throw 'found-match nil)))
          (error "(WSF) No matching directory path: filename= %s, tag= %s" filename tag)))
      (concat prefix wsf--common-prefix value filename))))


;;====================================================
;; Top level
;;====================================================

(defun wsf--current-completions ()
  "Return a completions list appropriate to the current context"
  (let ((default-directory (wsf--get-workspace)))
    (unless (wsf--try-reload)
      (setq wsf--from-scratch nil)
      (wsf--build-hash-table)
      (wsf--build-uniquified)
      (wsf--persist)))
  wsf--uniquified-list)

;;====================================================
;; Building caching and switching
;;====================================================

(defun wsf--get-workspace ()
  ""
  (let ((workspace (locate-dominating-file "." 'wsf--workspace-root-marker-directory)))
    (if workspace
        ;; Convert to absolute with trailing "/"
        (setq workspace
              (concat (substring (shell-command-to-string
                                  (concat "cd " workspace "; pwd"))
                                 0 -1) "/"))
      (let ((msg (format "Working directory (%s) is not in a workspace" default-directory)))
        (if (null wsf--workspace)
            (user-error "%s; no prior workspace available" msg)
          (setq workspace wsf--workspace)
          (message "(WSF) %s: reusing %s" msg workspace))))
    ;; If switching workspacees then start with a clean slate
    (when (or wsf--from-scratch (not (string= wsf--workspace workspace)))
      (setq wsf--workspace workspace)
      (setq wsf--hash-table nil)
      (setq wsf--uniquified-list nil)
      (let ((abs-state-dir (concat wsf--workspace (wsf--workspace-root-marker-directory wsf--workspace) "/")))
        (setq wsf--path-state-dir abs-state-dir)
        (setq wsf--path-hash   (concat abs-state-dir wsf--HASH_TABLE)))))
  wsf--workspace)

(defun wsf--workspace-root-marker-directory (path)
  "Return workspace root mark directory name if one exists at PATH"
  (loop for (state-dir _search-roots) in wsf--workspace-root-marker-alist
        if (file-exists-p (concat path "/" state-dir)) return state-dir))


;;====================================================
;; Reload persisted state
;;====================================================

(defun wsf--try-reload ()
  "Return t if able to restore persisted state and nil otherwise."
  (when (and (not wsf--from-scratch) (file-exists-p wsf--path-hash))
    (unless wsf--uniquified-list
      ;; Reload path-hash and uniquified-list from disk (hopefully from an .elc)
      (load wsf--path-hash)))
  wsf--uniquified-list)


;;====================================================
;; Hash table accumulation
;;====================================================

(defun wsf--build-hash-table ()
  "Construct a hash table from file of paths."
  ;; Start with a completely empty hash table
  (setq wsf--hash-table (make-hash-table :test 'equal))
  (with-temp-buffer
    (buffer-disable-undo)
    ;; find <search-roots> -type d ( -name <state-dir> / -o ) -prune -o ! -type d -print
    (let* ((search-roots (cdr (assoc (wsf--workspace-root-marker-directory wsf--workspace)
                                     wsf--workspace-root-marker-alist)))
           (find-cmd (concat "cd " wsf--workspace
                             " ; find"
                             (loop for dir in search-roots concat (concat " " dir))
                             " -type d \\( "
                             (loop for (state-dir _search-roots) in wsf--workspace-root-marker-alist
                                   with dash-o = ""
                                   concat (concat dash-o "-name " state-dir)
                                   do (setq dash-o " -o "))
                             " \\) -prune -o ! -type d -print"
                             " | grep -v " (shell-quote-argument wsf--exclude-path-re))))
      (message (format "(WSF) find-cmd= \"%s\"" find-cmd))
      (call-process-shell-command find-cmd nil t)
      ;(write-file (concat wsf--path-state-dir "wsf-file-list.txt"))
      )
    ;; Ensure trailing '\n'
    (goto-char (point-max))
    (unless (eq (char-before) ?\n)
      (insert-char ?\n))
    ;; Init wsf--common-prefix
    (goto-char (point-min))
    (forward-line)
    (wsf--set-reconstitute-path-prefix (buffer-substring (point-min) (1- (point))))
    (search-backward "/")
    (setq wsf--common-prefix (buffer-substring (point-min) (1+ (point))))
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
            (search-backward "/" path-beg)
            (forward-char 1)
            (let ((path (buffer-substring path-beg (point))))
              (wsf--adjust-common-prefix path)
              (wsf--puthash-initial (buffer-substring (point) name-end) path))))
    ;; Strip common prefix back to a path element boundary
    (erase-buffer)
    (insert wsf--common-prefix)
    (goto-char (point-max))
    (setq wsf--common-prefix (buffer-substring (point-min) (search-backward "/")))
    (setq wsf--common-prefix-length (length wsf--common-prefix))
    ;; Convert hash table value lists to vectors of shrunk strings
    (setq wsf--vector-hash (make-hash-table :test 'equal))
    (maphash 'wsf--convert-list-to-vector wsf--hash-table)
    (setq wsf--vector-hash nil)))

(defun wsf--set-reconstitute-path-prefix (find-result)
  "Given exemplar FIND-RESULT get prefix to generate absolute paths."
  (message (format "(WSF) examplar find-result= \"%s\"" find-result))
  (setq
   wsf--reconstitute-path-rule
   (cond
    ((not (string= "/" (substring find-result 0 1)))
     'workspace-relative)
    ((file-exists-p find-result)
     'absolute)
    ((file-exists-p (concat "." find-result))
     'workspace-relative-no-final-slash)
    (t
     (user-error "Cannot determine relative or absolute nature of non-existent file: %s" find-result)))))

(defun wsf--adjust-common-prefix (path)
  "If necessary shorten the current notion of the common prefix"
  (loop until (or (zerop (length wsf--common-prefix))
                  (string-prefix-p wsf--common-prefix path))
        ;; FIXME: must strip an entire path element
        do (setq wsf--common-prefix (substring wsf--common-prefix 0 -1))))

(defun wsf--puthash-initial (file path)
  "Record an occurrence of FILE in directory PATH"
  (let ((value (gethash file wsf--hash-table)))
    (cond
     ((null value)
      (setq value path))
     ((stringp value)
      (setq value (list path value)))
     (t
      (setq value (cons path value))))
    (puthash file value wsf--hash-table)))

(defun wsf--convert-list-to-vector (file paths)
  "Replace file's paths list with a vector of prefix-stripped paths"
  (if (stringp paths)
      (setq paths (substring paths wsf--common-prefix-length))
    (setq paths (vconcat paths))
    (loop for path across-ref paths do
          (setf path (substring path wsf--common-prefix-length)))
    (sort paths 'string<))
  (puthash file (gethash paths wsf--vector-hash paths) wsf--hash-table))


;;====================================================
;; Uniquify file names
;;====================================================

(defun wsf--build-uniquified ()
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
      (loop for file being the hash-keys of wsf--hash-table
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

    (setq wsf--uniquified-list (sort uniquified 'string<))))


;;====================================================
;; Persist new workspace state
;;====================================================

(defun wsf--persist ()
  "Write the completion list and hash table to disk as a .el / .elc pair"
  (with-temp-buffer
    (let ((standard-output (current-buffer)))
      (insert"(setq wsf--reconstitute-path-rule '")
      (prin1 wsf--reconstitute-path-rule)
      (insert")\n(setq wsf--common-prefix ")
      (prin1 wsf--common-prefix)
      (insert")\n(setq wsf--hash-table ")
      (prin1 wsf--hash-table)
      (insert")\n(setq wsf--uniquified-list '")
      (prin1 wsf--uniquified-list)
      (insert ")\n"))
    (write-file wsf--path-hash))
  (let ((path-hash-elc (concat wsf--path-hash "c")))
    (shell-command-to-string (concat "rm -f " wsf--path-hash path-hash-elc))
    (byte-compile-file wsf--path-hash)))

(provide 'wsf)
