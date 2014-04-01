;;; am.el --- Amake-cache based file finder and grepper
;;
;; INSTRUCTIONS:
;;
;; 1.  Run amake -dumponly to produce an amake dump file in your home dir
;;       example$ amake -dumponly ~/am.dmp
;;
;; 2.  Place the snippet of code starting between  ';; -- start --'
;;     and ';; -- end --' lines in your ~/.emacs file.
;;     remove leading ';' characters from each line.
;;
;; 3.  To invoke the file finder, type C-x,  (Control-X comma) or invoke
;;     'am-find-file'
;;
;;     To use the amake grepper, invoke 'am-grep' by typing M-x am-grep
;;
;; -- start --
; (let (nz-dir home-dir)
;   (setq nz-dir (expand-file-name "~/"))
;   (setq home-dir (expand-file-name "~"))
;   (when (and (file-exists-p (concat home-dir "/am.dmp"))
;              (file-exists-p (concat nz-dir "/nz/src/tools/am.el")))
;     (load-file (concat nz-dir "/nz/src/tools/am.el"))
;     (am-scan (concat home-dir "/am.dmp"))
;     (global-set-key "\C-x," 'am-find-file)))
;     (global-set-key "\C-x4," 'am-find-file-other-window)))
;; -- end --
;;
;; QUESTIONS:
;;
;; If you have questions, contact Sanjay Dixit (ext. 845)
;;

(defvar am-files-alist nil
  "Associative list of files after scanning the amake dump file")

(defvar am-filepat-files-alist nil
  "Associative list of file pat and list of files matching the file pat")

(defvar am-file-opener 'find-file-read-only
  "*Set to file opener of your choice.  am-find-file uses this opener.
 Example: (setq am-file-opener 'view-file)")

(defvar am-file-opener-other-window 'find-file-read-only-other-window
  "*Set to file opener of your choice.  am-find-file-other-window
 uses this opener.
 Example: (setq am-file-opener 'view-file)")

;;; 
(defvar am-other-alist nil
  "Associative list of files outside of those from the amake dump file")

;;; 
(defvar am-other-file "~/.am-other"
  "*Name of file where the am-other-list list is stored")
;;;
(defvar am-prev-filepat "/"
  "*Previous file pattern auto filled into am-grep prompt")

;;; 
(defvar am-skip-ext-list
  '(".o" ".z" ".rpm" ".exe" ".tgz" ".tar" ".Z" ".zip"
    ".a" ".o" ".out" ".bin" ".so" ".bit" ".elf" ".linux")
  "*List of file extensions to skip while building file list from the amake
dump file")

;;;
(defvar am-grep-filepat-history nil
  "*Amake Grep mode file pattern history")

;;;
(defvar am-grep-greppat-history nil
  "*Amake Grep mode grep-pattern history")
;;;
(defvar am-find-file-history nil
  "*Amake Find File history")

;;; 
(defvar am-skip-dir-list '("AOS/obj/caches/dbx_all/"
;;                         "AOS/bin/"
                           "AOS/compilers/"
                           "AOS/lib/"
                           "/usr/lib")
  "*Skip patterns for dirs.  Leading occurances of AOS are substituted with the
value of am-aos-dir.  The value of am-aos-dir is gotten from the cache dump file.")

;;;
(defvar am-grep-buffer nil "The amake grep buffer")

;;;
(defvar am-aos-dir nil "Value of the -DAOS line in the amake dump file")

;;;
(defvar am-grep-progress-bar t
  "*Set to `nil' if you do not want a progress bar with am-grep")

;;;
(defconst am-progress-bar-list
  '(
    "|                    |"
    "|=                   /"
    "|==                  -"
    "|===                 \\"
    "|====                |"
    "|=====               /"
    "|======              -"
    "|=======             \\"
    "|========            |"
    "|=========           /"
    "|==========          -"
    "|===========         \\"
    "|============        |"
    "|=============       /"
    "|==============      -"
    "|===============     \\"
    "|================    |"
    "|=================   /"
    "|==================  -"
    "|=================== \\"
    "|====================|")
  "Progress bar")

;;; 
(defun am-scan (dumpfile)
  (interactive "fAmake dump file: ")
  (message "Scanning %s wait..." dumpfile)
  (setq am-files-alist nil)
  (setq am-filepat-files-alist nil)
  (save-current-buffer
    (am-scan-internal dumpfile)
    (am-other-eval))
  (message "Done scanning %s" dumpfile))

;;; 
(defun am-scan-internal (dumpfile)
  (setq am-files-alist nil)
  (let (buf-name tmp-name file-name file-dir ext source-pos)
    (setq buf-name "*amdump*")
    (get-buffer-create buf-name)
    (set-buffer (get-buffer buf-name))
    (setq buffer-read-only nil)
    (goto-char (point-min))
    (insert-file-contents-literally dumpfile nil nil nil t)
    (setq buffer-read-only t)
    (goto-char (point-min))

    ;; Find the AOS line
    (while (and (not am-aos-dir)
                (not (eobp)))
      (beginning-of-line)
      (when (looking-at "^\\-DAOS=\\(.*\\)$")
        (setq am-aos-dir (match-string 1)))
      (forward-line))
    (if (eobp)
        (error (format "%s: -DAOS line not found" dumpfile)))

    ;; substitute leading AOS instances in am-skip-dir-list to the value
    ;; of am-aos-dir
    (let ((tmp-dir-list nil) (tmp-elt nil))
      (while am-skip-dir-list
        (setq tmp-elt (car am-skip-dir-list))
        (if (string-match "^AOS.*$" tmp-elt)
            (setq tmp-elt (concat am-aos-dir (substring tmp-elt 3))))
        (setq tmp-dir-list (cons tmp-elt tmp-dir-list))
        (setq am-skip-dir-list (cdr am-skip-dir-list)))
      (setq am-skip-dir-list tmp-dir-list))

    ;; continue scanning
    (while (not (eobp))
      (beginning-of-line)
      (if (looking-at "^name:\\s +\\(.*\\)\\s +([0-9]*)$")
          (progn
            (setq tmp-name (match-string 1))
            (setq file-dir (file-name-directory tmp-name))
            (setq file-name (file-name-nondirectory tmp-name))
            (setq ext (substring file-name
                                 (length (file-name-sans-extension file-name))
                                 (length file-name)))
            (if (and (not (member ext am-skip-ext-list))
                     (not (am-match-skip-dir file-dir)))
                (progn
                  (setq source-pos (string-match "/source/" file-dir))
                  (if (not source-pos)
                      (setq source-pos (string-match "/allsrc/" file-dir)))
                  (if source-pos
                      (setq file-dir (concat (substring file-dir 0 source-pos)
                                             "/src/"
                                             (substring file-dir (+ source-pos 8)))))
                  (setq am-files-alist
                        (cons (cons file-name file-dir) am-files-alist))))))
      (end-of-line)
      (forward-line))
    (kill-buffer buf-name)))

;;; 
(defun am-find-file-internal (filename other-window-p)
  (let (this-list)
    (setq this-list (assoc filename am-files-alist))
    (if (null this-list)
        (setq this-list (assoc filename am-other-alist)))
    (if (not (null this-list))
        (progn
          (if other-window-p
              (funcall am-file-opener-other-window
                       (concat (eval (cdr this-list)) (car this-list)))
            (funcall am-file-opener
                     (concat (eval (cdr this-list)) (car this-list))))
          (if (buffer-file-name)
              (if (file-writable-p buffer-file-name)
                  (message "File %s" buffer-file-name)
                (message "File %s   (File is write protected)"
                         buffer-file-name))))
      (message "Can't find %s" filename))))

;;; 
(defun am-match-skip-dir (dirname)
  (let (the-car the-cdr found-match)
    (setq the-cdr am-skip-dir-list)
    (setq found-match nil)
    (while (and (null found-match)
                (setq the-car (car the-cdr)))
      (if (string-match the-car dirname)
          (setq found-match t))
      (setq the-cdr (cdr the-cdr)))
    found-match))

;;; 
(defun am-write-other-assoc (filename)
  (let (buf-name this-cdr this-car)
    (setq buf-name filename)
    (get-buffer-create buf-name)
    (set-buffer (get-buffer buf-name))
    (setq buffer-read-only nil)
    (setq backup-inhibited t)
    (goto-char (point-min))
    (setq this-cdr am-other-alist)
    (princ "(setq am-other-alist'(" (current-buffer))
    (write-char ?\n (current-buffer))
    (while (setq this-car (car this-cdr))
      (write-char ?\  (current-buffer))
      (prin1 this-car (current-buffer))
      (write-char ?\n (current-buffer))
      (setq this-cdr (cdr this-cdr)))
    (write-char ?\) (current-buffer))
    (write-char ?\) (current-buffer))
    (write-char ?\n (current-buffer))
    (write-file filename)
    (kill-buffer (buffer-name))))

;;; Add file and dir corresponding to the current buffer to 'am-other-alist
;;; skip exts or skip dirs not applied here
(defun am-add-current-buffer ()
  (interactive)
  (let (the-file file-name file-dir)
    (setq the-file (buffer-file-name))
    (if (= (length the-file) 0)
        (error "Buffer %s is not a disk file" (buffer-name)))
    (setq file-dir (file-name-directory the-file))
    (setq file-name (file-name-nondirectory the-file))
    (setq am-other-alist
          (cons (cons file-name file-dir) am-other-alist))
    (setq am-filepat-files-alist nil)
    (am-write-other-assoc am-other-file)))

;;;
(defun am-other-eval ()
  (let (buf-name)
    (setq buf-name "*am-other*")
    (get-buffer-create buf-name)
    (set-buffer (get-buffer buf-name))
    (if (file-exists-p am-other-file)
        (insert-file-contents-literally am-other-file nil nil nil t))
    (goto-char (point-min))
    (eval-buffer)
    (kill-buffer (buffer-name))))

;;;
(defun prompt-project-filename (other-window-p)
  (let (filename the-list prompt-str)
    (setq the-list (append am-other-alist am-files-alist))
    (setq prompt-str (if other-window-p
                         "Find project file in other window: "
                       "Find project file: "))
    (setq filename
          (completing-read prompt-str the-list nil t nil
                           nil ;; am-find-file-history
                           ))
    filename))
;;;
(defun am-find-file (&optional other-window-p)
  "Find file known to amake"
  (interactive)
  (let (filename the-list)
    (setq the-list (append am-other-alist am-files-alist))
    (setq filename (prompt-project-filename other-window-p))
    (if (> (length filename) 0)
        (am-find-file-internal filename other-window-p))))

(defun am-find-file-other-window ()
  "Find file known to amake, open it in other window"
  (interactive)
  (am-find-file t))

;;;  Taken from etags.el
(defun am-nearest-word ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (or (re-search-backward "\\sw\\|\\s_"
				(save-excursion (beginning-of-line) (point))
				t)
	    (re-search-forward "\\(\\sw\\|\\s_\\)+"
			       (save-excursion (end-of-line) (point))
			       t))
	(progn (goto-char (match-end 0))
	       (buffer-substring (point)
				 (progn (forward-sexp -1)
					(while (looking-at "\\s'")
					  (forward-char 1))
					(point))))
      nil)))

;;;
(defun am-grep ()
  "Search for strings in files known to amake"
  (interactive)
  (let (file-pat srch-pat am-tmp-buffer
                 (total-matches 0)
                 (total-files 0)
                 (default-pat (am-nearest-word)))
    ;; Read and validate grep pattern and file pattern
    (setq srch-pat (read-string
                    (format "Amake Grep Pattern: %s"
                            (if default-pat
                                (format "(default %s) " default-pat)
                              ""))
                    nil
                    nil ;; am-grep-greppat-history
                    default-pat
                    nil))
    (setq file-pat (read-string
                    (format "File Pattern: (default %s) " am-prev-filepat)
                    nil
                    nil ;; am-grep-filepat-history
                    am-prev-filepat
                    nil))
    (if (or (= 0 (length file-pat))
            (= 0 (length srch-pat)))
        (error "Nothing to grep"))
    (setq am-prev-filepat file-pat)
;;    (setq am-grep-greppat-history (cons srch-pat am-grep-greppat-history))
;;    (setq am-grep-filepat-history (cons file-pat am-grep-filepat-history))
    ;; Get rid of active am-grep-buffer
    (am-grep-quit)

    ;; Create and cleanup *Amake-grep* buffer
    (setq am-grep-buffer (get-buffer-create "*Amake-grep*"))
    (with-current-buffer am-grep-buffer
      (setq buffer-read-only nil)
      (kill-region (point-min) (point-max)))

    ;; Create and cleanup  *am-tmp* buffer
    (setq am-tmp-buffer (get-buffer-create " *am-tmp*"))
    (with-current-buffer am-tmp-buffer
      (setq buffer-read-only nil)
      (kill-region (point-min) (point-max)))

    (let (the-list this-assoc cmd file-name
                   (found-in-cache nil)
                   (file-list nil)
                   (prev-file nil)
                   (dex 0)
                   (num-ticks 0)
                   (last-tick -1)
                   (num-matches 0))
      (setq the-list (append am-other-alist am-files-alist))
      (message "Am-Grep: working...")
      ;; see if we have the file-list available for file-pat
      (setq file-list
            (let (am-cache-ent)
              (setq am-cache-ent (assoc file-pat am-filepat-files-alist))
              (if (not (null am-cache-ent))
                  (progn
                    (setq found-in-cache t)
                    (cdr am-cache-ent))
                nil)))
      ;; collect list of files in file-list
      (when (null file-list)
        (while (prog1 the-list
                 (setq this-assoc (car the-list))
                 (setq the-list (cdr the-list)))
          (setq file-name (concat (eval (cdr this-assoc)) (car this-assoc)))
          (when (and (string-match (regexp-quote file-pat) file-name)
                     (file-exists-p file-name)
                     (file-regular-p file-name)
                     (> 1000000 (nth 7 (file-attributes file-name))));;filesize
            (setq file-list (cons file-name file-list)))))
      (when (null found-in-cache)
        (if (> (length am-filepat-files-alist) 4)
            (progn
              (setq am-filepat-files-alist nil)
              (garbage-collect)))
        (setq am-filepat-files-alist
              (cons (cons file-pat file-list) am-filepat-files-alist)))
      ;; grep each file in file-list

(if nil
    (progn
      (setq the-list (sort file-list (function string-lessp)))
      (setq file-list the-list))
  (setq the-list file-list))

      (while (prog1 the-list
               (setq file-name (car the-list))
               (setq the-list (cdr the-list)))
        ;; show progress bar if necessary
        (when am-grep-progress-bar
          (setq dex (1+ dex))
          (setq num-ticks (/ (* 20 dex) (length file-list)))
          (unless (= last-tick num-ticks)
            (message (format "Am-Grep: working... %s "
                             (nth num-ticks am-progress-bar-list)))
            (setq last-tick num-ticks)))
        ;; perform grep on file-name
        (when (and (not (equal prev-file file-name))
                   (file-readable-p file-name))
          (save-excursion
            (setq num-matches (am-grepper am-tmp-buffer file-name srch-pat))
            (when (> num-matches 0)
              (setq total-matches (+ total-matches num-matches))
              (setq total-files (1+ total-files)))))
        (setq prev-file file-name)))

    (kill-buffer am-tmp-buffer)
    (switch-to-buffer-other-window am-grep-buffer)
    (set-buffer am-grep-buffer)
    (goto-char (point-min))
    (am-grep-mode)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (message "%d files, %d matches" total-files total-matches)))

;;;
(defun am-grepper (am-tmp-buffer file-name srch-pat)
  (if (not (buffer-live-p am-grep-buffer))
      (error "Invalid amake grep buffer"))
  (let (this-line out-lines out-points start end num-matches)
    ;; Go to end of am-grep-buffer
    (set-buffer am-grep-buffer)
    (goto-char (point-max))

    ;; Setup tmp buffer for grepping
    (set-buffer am-tmp-buffer)
    (kill-region (point-min) (point-max))
    (insert-file-contents-literally file-name nil nil nil t)
    (goto-char (point-min))

    ;; Perform grep in tmp buffer
    (setq out-lines nil)
    (setq out-points nil)
    (condition-case nil
        (while (search-forward srch-pat)
          (beginning-of-line)
          (setq start (point))
          (end-of-line)
          (setq end (point))
          (setq this-line (buffer-substring start end))
          (setq out-lines (cons this-line out-lines))
          (setq out-points (cons start out-points))
          (forward-line))
      (error nil))

    ;; Write out to am-grep-buffer
    (set-buffer am-grep-buffer)
    (setq num-matches (length out-lines))
    (when out-lines
      (insert "============================================================\n")
      (insert (format "\"%s\"\n" file-name))
      (setq out-lines (nreverse out-lines))
      (setq out-points (nreverse out-points))
      (while out-lines
        (insert (format "  (%s)%s\n" (car out-points) (car out-lines)))
        (setq out-lines (cdr out-lines))
        (setq out-points (cdr out-points))))
    num-matches))

;;;
(defvar am-grep-mode-map ())
(if am-grep-mode-map
    ()
  (setq am-grep-mode-map (make-sparse-keymap))
  (suppress-keymap am-grep-mode-map)
  (define-key am-grep-mode-map "q" 'am-grep-quit)
  (define-key am-grep-mode-map "n" 'am-grep-next-file)
  (define-key am-grep-mode-map "p" 'am-grep-prev-file)
  (define-key am-grep-mode-map "N" 'am-grep-prev-file)
  (define-key am-grep-mode-map " " 'am-grep-view-match-other-window)
  (define-key am-grep-mode-map "\C-m" 'am-grep-view-match)
  (define-key am-grep-mode-map "j" 'next-line)
  (define-key am-grep-mode-map "k" 'previous-line))
;;;
(defun am-grep-mode ()
  "Major mode for am-grep output
Keybindings:
\\{am-grep-mode-map}"
  (kill-all-local-variables)
  (use-local-map am-grep-mode-map)
  (setq major-mode 'am-grep-mode)
  (setq mode-name "Amake Grep Mode")
  (run-hooks 'am-grep-mode-hook))

;;;
(defun am-grep-quit ()
  "Exit Amake Grep Mode"
  (interactive)
  (when (buffer-live-p am-grep-buffer)
    (set-buffer am-grep-buffer)
    (delete-windows-on am-grep-buffer)
    (kill-buffer am-grep-buffer)
    (setq am-grep-buffer nil)
    t))

(defun am-grep-next-file ()
  "Position cursor on the next file in an *Amake-grep* buffer"
  (interactive)
  (am-grep-next-prev-file t))

(defun am-grep-prev-file ()
  "Position cursor on the previous file in an *Amake-grep* buffer"
  (interactive)
  (am-grep-next-prev-file nil))

;;;
(defun am-grep-next-prev-file (scan-forward)
  (if (not (buffer-live-p am-grep-buffer))
      (error "No *Amake-grep* buffer"))
  (if (not (equal (current-buffer) am-grep-buffer))
      (error "Not an *Amake-grep* buffer"))
  (let ((new-point nil)
        (file-pat "^\".*$"))
    (save-excursion
      (beginning-of-line)
      ;; leave current line if sitting on a 'file line'
      (if (and (not (if scan-forward (eobp) (bobp)))
               (looking-at file-pat))
          (forward-line (if scan-forward 1 -1)))
      ;; find next/prev file
      (while (and (not (if scan-forward (eobp) (bobp)))
                  (not new-point))
        (beginning-of-line)
        (if (looking-at file-pat)
            (setq new-point (point))
          (forward-line (if scan-forward 1 -1)))))
    (if new-point
        (goto-char new-point)
      (error "No more files"))))
;;;
(defun am-grep-view-match-other-window ()
  "Display matching file in other window with the matched line in the center"
  (interactive)
  (am-grep-view-match t))

;;;
(defun am-grep-view-match (&optional other-window-p)
  "Display matching file with the matched line in the center"
  (interactive)
  (if (not (buffer-live-p am-grep-buffer))
      (error "No *Amake-grep* buffer"))
  (if (not (equal (current-buffer) am-grep-buffer))
      (error "Not an *Amake-grep* buffer"))
  (let ((match-pat "^\\s \\s (\\([0-9]+\\)).*$")
        (match-file nil)
        (match-file-point nil))
    (save-excursion
      (beginning-of-line)
      ;; Gather match-file and match-file-point if we are on a match line
      (when (looking-at match-pat)
        (setq match-file-point (match-string 1))
        (setq match-file (am-grep-get-match-file (point))))
      ;; 
      (if (and match-file match-file-point)
          (progn
            (setq match-file-point (string-to-number match-file-point))
            (am-grep-display-file match-file match-file-point other-window-p)
            (message (format "%s (%d)" match-file match-file-point)))
        (ding)))))

;; modify function to work better with ECB persistent compile window
(defun am-grep-display-file-ECB (match-file match-file-point other-window-p)
  (let ((match-buffer nil)
        (o-buffer (current-buffer))
        (o-point (point)))
    (find-file match-file)
    (goto-char match-file-point)
    (switch-to-buffer o-buffer)
    (set-window-point (get-buffer-window o-buffer) o-point)))

;;;
(defun am-grep-display-file (match-file match-file-point other-window-p)
  (let ((match-buffer nil)
        (o-buffer (current-buffer))
        (o-point (point)))
    (if other-window-p
        (find-file-other-window match-file)
      (find-file match-file))
    (goto-char match-file-point)
    (if other-window-p
        (progn
          (switch-to-buffer-other-window o-buffer)
          (set-window-point (get-buffer-window o-buffer) o-point)))))

;;;
(defun am-grep-get-match-file (the-point)
  (save-excursion
    (let ((file-pat "^\"\\(.*\\)\"$")
          (file-name nil))
      (goto-char the-point)
      (beginning-of-line)
      (if (and (not (bobp))
               (looking-at file-pat))
          (setq file-name (match-string 1)))
      (while (and (not (bobp))
                  (not file-name))
        (beginning-of-line)
        (if (looking-at file-pat)
            (setq file-name (match-string 1)))
        (forward-line -1))
      file-name)))
