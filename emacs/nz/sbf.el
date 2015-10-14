;;; sbf.el --- Mathworks sandbox-based file finder  -*- lexical-binding: t -*-

(require 'helm)

;;====================================================
;; Various magic strings and path fragments
;;====================================================

;; File system visible paths and names
(defconst sbf--STATE_DIR   ".sbtools")  ; relative to sbf--sandbox
(defconst sbf--ADDED_FILES "added-files.txt")
(defconst sbf--FULL_LIST   "all-files.txt")
(defconst sbf--CODE_LIST   "code-files.txt")
(defconst sbf--HASH_TABLE  "path-hash.el")
(defconst sbf--UNIQUIFIED  "uniquified.txt")

(defconst sbf--BUF_UNIQUE  "*helm files*")


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

(defvar sbf--path-added nil "")
(defvar sbf--path-code nil "")
(defvar sbf--path-full nil "")
(defvar sbf--path-hash nil "")
(defvar sbf--path-state-dir nil "")
(defvar sbf--path-unique nil "")

(defvar sbf--reconstitute-path-prefix nil
  "A version of sandbox path appropriate for path reconstitution.")

(defvar sbf--sandbox nil
  "Sandbox with which in-memory state is associated.")

(defvar sbf--uniquified-buffer nil
  "Argument to completion functions (built or reloaded from disk).")

(defvar sbf--uniquified-list nil
  "Argument to completion functions (built or reloaded from disk).")

(defvar sbf--vector-hash nil
  "")

;;====================================================
;; Autoloaded entrypoints for HELM access
;;====================================================

;;;###autoload
(defun sbf-helm-find-file ()
  "Return a HELM arglist to perform find-file in a sandbox."
  (sbf--current-completions)
  `(:prompt "Sandbox find-file: X "
    :history 'sbf--find-file-history
    :must-match t
    :nomark t
    :action 'sbf--action
    :sources
    ,(helm-build-in-buffer-source "Sandbox find-file"
                 :init (lambda ()
                         (helm-init-candidates-in-buffer
                             sbf--uniquified-buffer
                           sbf--uniquified-list))
                 ;:history 'sbf--find-file-history
                 ;:reverse-history t
                 ;:must-match t
                 ;:nomark t
                 ;:action 'sbf--action
                 )))

(defun sbf--action (filename)
  ""
  (let ((path (sbf--reconstitute-file-path filename)))
    (message-box "Path= %s" path)))



;; (defun XYZ ()
;;   ""
;;     ((name . "Sandbox find-file")
;;      ;(candidates . ,(lambda () nil))
;;      (prompt . "Sandbox file-file: ")
;;      (buffer . sbf--uniquified-buffer)
;;      (init . ,(lambda ()
;;                 (helm-init-candidates-in-buffer
;;                     sbf--uniquified-buffer
;;                   sbf--uniquified-list)))
;;      ;(history . sbf--find-file-history)
;;      ;(reverse-history . t)
;;      ;(del-input . nil)
;;      ;(help-message . helm-M-x-help-message)
;;      (must-match . t)
;;      ;(fuzzy . helm-M-x-fuzzy-match)
;;      (nomark . t)
;;      ;(candidates-in-buffer . ,(lambda () nil))
;;      ;(fc-transformer . 'helm-M-x-transformer)
;;      ;(hist-fc-transformer . 'helm-M-x-transformer-hist)
;;      (action . (lambda (filename)
;;                  (let ((path (sbf--reconstitute-file-path filename)))
;;                    (message-box "Path= %s" path))))))





;; (helm :sources (helm-build-in-buffer-source "test"
;;                  :init (lambda ()
;;                          (helm-init-candidates-in-buffer 'global
;;                            '("foo" "bar" "baz")))
;;                  :fuzzy-match t)
;;       :buffer "*helm test*")




;;;###autoload
(defun sbf-helm-find-file-read-only ()
  "Return a HELM arglist to perform find-file-read-only in a sandbox."
  '(:prompt
    "Sandbox find-file-read-only: "
    :sources
    ((name . "Sandbox find-file-read-only")
     (candidates . sbf--current-completions)
     (prompt . "Sandbox file-file-read-only: ")
     (action . (lambda (filename)
                 (let ((path (sbf--reconstitute-file-path filename)))
                   (message-box "Path= %s" path)))))))

;;;###autoload
(defun sbf-helm-view-file ()
  "Return a HELM arglist to perform view-file in a sandbox."
  '(:prompt
    "Sandbox find-file-read-only: "
    :sources
    ((name . "Sandbox view-file")
     (prompt . "Sandbox view-file: ")
     (candidates . sbf--current-completions)
     (action . (lambda (filename)
                 (let ((path (sbf--reconstitute-file-path filename)))
                   (message-box "Path= %s" path)))))))


;;====================================================
;; Lesser entrypoints
;;====================================================

;;;###autoload
(defun sbf-find-file ()
  "Find a file in a sandbox using a Emacs' traditional completion framework."
  (interactive)
  (sbf--current-completions)
  (let ((filename
         (completing-read
          (concat "Sandbox " sbf--sandbox " - find file: ")
          sbf--uniquified-list nil t nil sbf--find-file-history)))
    (when (> (length filename) 0)
      (find-file-read-only (sbf--reconstitute-file-path filename)))))


(defun sbf-force-from-scratch ()
  "Recompute and reload all structures"
  (interactive)
  (setq sbf--from-scratch t)
  (sbf--current-completions)
  (setq sbf--from-scratch nil))


;;====================================================
;; Final path reconstruction
;;====================================================

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
      (sbf--build-hash-table)
      (sbf--build-uniquified)
;;      (helm-init-candidates-in-buffer sbf--uniquified-buffer sbf--uniquified-list)
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
    (when (or (not (string= sbf--sandbox sandbox))
              sbf--from-scratch)
      (setq sbf--sandbox sandbox)
      (setq sbf--hash-table nil)
      (setq sbf--uniquified-list nil)
      (setq sbf--uniquified-buffer (get-buffer-create sbf--BUF_UNIQUE))
      (buffer-disable-undo sbf--uniquified-buffer)
      (let ((abs-state-dir (concat sbf--sandbox sbf--STATE_DIR "/")))
        (setq sbf--path-state-dir abs-state-dir)
        (setq sbf--path-added  (concat abs-state-dir sbf--ADDED_FILES))
        (setq sbf--path-full   (concat abs-state-dir sbf--FULL_LIST))
        (setq sbf--path-code   (concat abs-state-dir sbf--CODE_LIST))
        (setq sbf--path-hash   (concat abs-state-dir sbf--HASH_TABLE))
        (setq sbf--path-unique (concat abs-state-dir sbf--UNIQUIFIED)))))
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
  ;; Collect a list of newly added files not yet under version control
  (unless (sbf--can-reuse sbf--path-added nil)
    (message (concat sbf--cmd-add-files sbf--path-added))
    (shell-command-to-string (concat sbf--cmd-add-files sbf--path-added))
    (shell-command-to-string (concat "touch " sbf--path-added)))
  ;; Collect a cleansed list of files that are under version control
  (unless (sbf--can-reuse sbf--path-full sbf--path-added)
    (shell-command-to-string (concat "cat " sbf--path-added sbf--cmd-full-list sbf--path-full)))
  ;; Collect the subset of files that might interest a programmmer
  (unless (sbf--can-reuse sbf--path-code sbf--path-full)
    (message (concat "cat " sbf--path-full sbf--cmd-code-list sbf--path-code))
    (shell-command-to-string (concat "cat " sbf--path-full sbf--cmd-code-list sbf--path-code)))
  (when (sbf--can-reuse sbf--path-hash sbf--path-code)
    ;; Reload hash tabke from disk (hopefully from a .elc)
    (load sbf--path-hash)
    ;; Reload buffer of unique strings and rebuild list
    (with-current-buffer sbf--uniquified-buffer
      (erase-buffer)
      (insert-file-contents-literally sbf--path-unique)
      (setq sbf--uniquified-list nil)
      (let ((line-end (point-min))
            (line-beg nil))
        (loop until (eq line-end (point-max)) do
              (setq line-beg line-end)
              (goto-char line-beg)
              (forward-line 1)
              (setq line-end (point))
              (setq sbf--uniquified-list (cons (buffer-substring line-beg (1- (point))) sbf--uniquified-list)))))
    t))

(defun sbf--can-reuse (file depends)
  "Return t IFF reusing FILE is safe"
  (when sbf--from-scratch
    (shell-command-to-string (concat "rm -f " file "*")))
  (and (file-exists-p file)
       (or (null depends)
           (file-newer-than-file-p file depends))))


;;====================================================
;; Hash table accumulation
;;====================================================

;; Magic shell invocations
(defconst sbf--cmd-add-files "p4 opened | grep '#1 - add ' | sed -e 's!//mw/B[^/]*/\\(matlab/.*\\)#1 - add .*$!\\1!' >")
(defconst sbf--cmd-full-list " $(find .sbtools/global/matlab/ -name 'project-file-list.txt') | fgrep -v '/derived/' >")
(defconst sbf--cmd-code-list " | grep -e '\\.\\([hHlycC]\\|h\\.in\\|hh\\|cc\\|[hc]\\(pp\\|xx\\)\\|lex\\|yacc\\|java\\)$' >")


(defun sbf--build-hash-table ()
  "Construct a hash table from file of paths."
  ;; Start with a completely empty hash table
  (setq sbf--hash-table (make-hash-table :test 'equal))
  (with-temp-buffer
    (insert-file-contents-literally sbf--path-code)
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
  (with-current-buffer sbf--uniquified-buffer
    ;; Populate sbf--uniquified-buffer with sorted uniquified names
    (erase-buffer)
    (loop for name in sbf--uniquified-list do (insert name "\n"))
    (write-region nil nil sbf--path-unique))
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (insert"(setq sbf--reconstitute-path-prefix ")
      (prin1 sbf--reconstitute-path-prefix buf)
      (insert")\n(setq sbf--common-prefix ")
      (prin1 sbf--common-prefix buf)
      (insert")\n(setq sbf--hash-table ")
      (prin1 sbf--hash-table buf)
      (insert ")\n")
      (write-region nil nil sbf--path-hash)))
  (let ((path-hash-elc (concat sbf--path-hash "c")))
    (shell-command-to-string (concat "rm -f " sbf--path-hash path-hash-elc))
    (byte-compile-file sbf--path-hash)))

(provide 'sbf)
