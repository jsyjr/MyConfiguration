;;; sbf.el --- Mathworks sandbox-based file finder

(defvar sbf--file-open-function 'find-file-read-only
  "")

(defvar sbf--common-prefix nil
  "Accumulated a prefix common to all paths.")

(defvar sbf--common-prefix-length 0
  "Length of sbf--common-prefix.")

(defvar sbf--completion-list nil
  "Argument to completion functions (built or reloaded from disk).")

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

(defvar sbf--reconstitute-path-prefix nil
  "")

(defvar sbf--sandbox nil
  "Sandbox with which in-memory state is associated.")


(defun sbf--force-recache ()
  "Recompute and reload all structures"
  (interactive)
  (setq sbf--from-scratch t)
  (sbf--current-completions))

;;;
(defun sbf--find-file ()
  "Find a file known to amake"
  (interactive)
  (sbf--current-completions)
  (let ((filename
         (completing-read
          (concat "Sandbox " sbf--sandbox " - find file: ")
          sbf--completion-list nil t nil sbf--find-file-history)))
    (when (> (length filename) 0)
      (let ((path (sbf--reconstitute-file-path filename)))
        (message "Path= %s" path)))))
        
        ;; (funcall sbf--file-open-function path)
        ;; (let ((buf-name (buffer-file-name)))
        ;;   (when buf-name
        ;;     (message "File %s%s" buf-name
        ;;              (if (file-writable-p path) "" " (File is write protected)"))))))))

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

(defun sbf--current-completions ()
  "Return a completions list appropriate to the current context"
  ;; (setq sbf--sandbox (locate-dominating-file "." sbf--STATE_DIR))
  (let ((sandbox (locate-dominating-file "." ".git")))
    (if sandbox
        (setq sandbox (concat sandbox (if (string= "/" (substring sandbox -1)) "" "/")))
      (let ((msg (format "Working directory (%s) is not in a sandbox" default-directory)))
        (if (null sbf--sandbox)
            (user-error "%s; no prior sandbox available" msg)
          (setq sandbox sbf--sandbox)
          (message "%s: reusing %s" msg sandbox))))
    (when (or (not (string= sbf--sandbox sandbox))
              sbf--from-scratch)
      (setq sbf--sandbox sandbox)
      (setq sbf--hash-table nil)
      (setq sbf--completion-list nil)))
  (let ((default-directory sbf--sandbox))
    (with-temp-buffer
      (when (sbf--get-completion-list)
        (sbf--persist))))
;;  (setq sbf--from-scratch nil)
  )

;; File system visible paths and names
(defconst sbf--STATE_DIR   ".sbtools/global")  ; relative to sbf--sandbox
(defconst sbf--ADDED_FILES "added-files.txt")
(defconst sbf--FULL_LIST   "all-files.txt")
(defconst sbf--CODE_LIST   "code-files.txt")
(defconst sbf--CODE_ELISP  "code-files.el")

;; Magic shell invocations
(defconst sbf--cmd-add-files "p4 opened | grep '#1 - add ' | sed -e 's!//mw/B[^/]*/\\(matlab/.*\\)#1 - add .*$!\\1!' >")
(defconst sbf--cmd-full-list " $(find .sbtools/global/matlab/ -name 'project-file-list.txt') | fgrep -v '/derived/' >")
(defconst sbf--cmd-code-list " | grep -e '\\.\\([hHlycC]\\|h\\.in\\|hh\\|cc\\|[hc]\\(pp\\|xx\\)\\|lex\\|yacc\\|java\\)$' >")

(defun sbf--get-completion-list ()
  "Build a list of \"uniquified\" file names.

Prefer unique path elements higher in the tree.  Use multiple
adjacent elements if necessary.  Return non-nil if a new list
was constructed and hence should be presisted to disk.

The search algorithm depends on the paths stored in the hash
table having slashes at either end.  This allows searching for
\"needles\" flanked by slashes which guarantees correctness.

(Yes there really are 5 nested loops here... The assumption
is that no single filename will be duplciated too many times.
Also all loops over duplicates have numerous early exits.)"
  (sbf--get-hash-table)
  (let ((completion-list sbf--completion-list))
    (when (null completion-list)
      (loop for file being the hash-keys of sbf--hash-table
            using (hash-values paths) do
            (loop for this across paths do
                  (let ((suffix nil))
                    (catch 'found-unique
                      (when (stringp paths)
                        (throw 'found-unique nil))
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
                                                (user-error "Cannot uniquify path: %s" this))
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
                    (setq completion-list
                          (cons (if suffix (concat file " " (substring suffix 1 -1)) file) completion-list)))))
      ;; Record the final result
      (setq sbf--completion-list completion-list))))

(defun sbf--persist ()
  "Write the completion list and hash table to disk as a .el / .elc pair"
  (erase-buffer)
  (let ((buf (current-buffer)))
    (insert"(setq sbf--reconstitute-path-prefix ")
    (prin1 sbf--reconstitute-path-prefix buf)
    (insert")\n(setq sbf--common-prefix ")
    (prin1 sbf--common-prefix buf)
    (insert")\n(setq sbf--completion-list '")
    (prin1 sbf--completion-list buf)
    (insert")\n(setq sbf--hash-table ")
    (prin1 sbf--hash-table buf)
    (insert ")\n"))
  (let* ((path-elisp (concat sbf--STATE_DIR "/" sbf--CODE_ELISP))
         (path-elc (concat path-elisp "c")))
    (shell-command-to-string (concat "rm -f " path-elisp path-elc))
    (write-region nil nil path-elisp)
    (byte-compile-file path-elisp)))

(defun sbf--get-hash-table ()
  "Populate the hash table either from elisp or from list of paths."
  (unless sbf--hash-table
    (let ((path-added (concat sbf--STATE_DIR "/" sbf--ADDED_FILES))
          (path-full  (concat sbf--STATE_DIR "/" sbf--FULL_LIST))
          (path-code  (concat sbf--STATE_DIR "/" sbf--CODE_LIST))
          (path-elisp (concat sbf--STATE_DIR "/" sbf--CODE_ELISP)))
      ;; Collect a list of newly added files not yet under version control
      (unless (sbf--can-reuse path-added)
        (message (concat sbf--cmd-add-files path-added))
        (shell-command-to-string (concat sbf--cmd-add-files path-added)))
      ;; Collect a cleansed list of files that are under version control
      (unless (sbf--can-reuse path-full path-added)
        (shell-command-to-string (concat "cat " path-added sbf--cmd-full-list path-full)))
      ;; Collect the subset of files that might interest a programmmer
      (unless (sbf--can-reuse path-code path-full)
        (message (concat "cat " path-full sbf--cmd-code-list path-code))
        (shell-command-to-string (concat "cat " path-full sbf--cmd-code-list path-code)))
      (if (sbf--can-reuse path-elisp path-code)
          ;; Just load from disk (hopefully from a .elc)
          (load path-elisp)
        ;; Build hash table from lines in path-code and save as elisp
        (insert-file-contents-literally path-code)
        (sbf--build-hash-table)))))

(defun sbf--can-reuse (result &optional depends)
  "Return t IFF reusing file RESULT is safe"
  (and (not sbf--from-scratch)
       (file-exists-p result)
       (or (null depends)
           (file-newer-than-file-p result depends))))

(defun sbf--build-hash-table ()
  "Construct a hash table from file paths in current buffer"
  ;; Start with a completely empty hash table
  (setq sbf--hash-table (make-hash-table :test 'equal))
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
        (line-beg nil))
    (loop until (eq line-end (point-max)) do
          (setq line-beg line-end)
          (goto-char line-beg)
          (forward-line 1)
          (backward-char 1)
          (setq line-end (point))
          (when (sbf--keep-line-p line-beg line-end)
            (search-backward "/" line-beg)
            (forward-char 1)
            (let ((path (buffer-substring line-beg (point))))
              (sbf--adjust-common-prefix path)
              (sbf--puthash-initial (buffer-substring (point) line-end) path)))
          (setq line-end (1+ line-end))))
  ;; Strip common prefix back to a path element boundary
  (erase-buffer)
  (insert sbf--common-prefix)
  (goto-char (point-max))
  (setq sbf--common-prefix (buffer-substring (point-min) (search-backward "/")))
  (setq sbf--common-prefix-length (length sbf--common-prefix))
  ;; Convert hash table value lists to vectors of shrunk strings
  (maphash 'sbf--convert-list-to-vector sbf--hash-table))

(defun sbf--set-reconstitute-path-prefix (path)
  ""
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

(defun sbf--keep-line-p (line-beg line-end)
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

(defun sbf--convert-list-to-vector (key value)
  "Replace key's value list with a vector of prefix-stripped paths"
  (if (stringp value)
      (setq value (substring value sbf--common-prefix-length))
    (setq value (vconcat value))
    (loop for path across-ref value do
          (setf path (substring path sbf--common-prefix-length))))
  (puthash key value sbf--hash-table))
