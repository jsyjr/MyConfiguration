;; John Yates's .emacs  -*- emacs-lisp -*-

(defconst copyright-owner "John S Yates Jr")
(put 'copyright-owner 'safe-local-variable 'stringp)

;; This program is free software; you can redistribute it and/or
;; modify it as you will.  I place it in the public domain.  I do
;; not need to credited in anyway.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;; Set to t when bringing up a new machine or reinstalling the full
;; complement of packages.  (When t customizations get suppressed.)
(defvar my/el-get-refetch-all-packages nil)

;; Too many false alarms
;; (setq debug-on-error t)

;; Suppress redefinition warnings during startup
(setq ad-redefinition-action 'accept)

;;=== Notes ============================================================
;;{{{  Goals

;; Package sanity:
;; - I fully concur with el-get author Dimitri Fontaine's sentiments.
;;   I too want a single file specifying all the packages I use, whence
;;   to obtain them and how they should be configured.

;; Custom file sanity:
;; - grouping
;; - commentary
;; - no repeating detault settings

;; Directory hygiene:
;;
;; Apart for this file (~/.emacs) all state that I maintain manually
;; I place into my ~/emacs/ directory (e.g. custom-file, spelling
;; dictionaries, templates I write/modify for yasnippet, packages I
;; write or modify).  I then maintain the contents of that directory
;; in git and replicate to all sites where I use emacs.
;;
;; By contrast I reserve my ~/.emacs.d directory for cached state
;; transparently managed by various packages (e.g. semanticdb, el-get,
;; backup, auto-save, etc).  This is information that is either
;; ephemeral or recreatable from the combination of my .emacs file and
;; the contents of my ~/emacs/ directory.

;; Speed: - I strive to have all functionality either autoloaded or
;; handled via eval-after-load.  The few files that I tolerate being
;; loaded as part of this .emacs are those that will truly be used
;; immediately (cua-base for blinking cursor).

;;}}}
;;{{{  Sharing and credits

;; Everyone's .emacs rips off someone else's...

;; In the emacs world there is a bit of a convention to prefix one's
;; private code with some version of the one's name or initials.  This
;; might make sense if users copied code amongst .emacs files while
;; preserving the original author's name.  In my experience this is
;; not what happens.  Rather imported code gets reprefixed.  Yet the
;; real goal is not so much to claim authorship as to avoid collisions
;; in the emacs name space.  Hence I do not use my own name, only a
;; "my/" prefix.  Anyone willing to adopt a similar convention could
;; then easily crib any of my customizations.

;; It is too hard and too noisy to attribute ideas in the body of this
;; file.  Here I simply list sources from which I have either cribbed
;; outright or else have drawn inspiration.
;;
;; Alex Ott             https://github.com/alexott/emacs-configs
;; Ryan Barrett         http://snarfed.org/dotfiles/.emacs
;; Mickey Petersen      mickey@masteringemacs.org
;; Mark Shroyer         http://markshroyer.com/tag/linux/

;;}}}
;;{{{  Setup

;; Gnome system:  http://www.emacswiki.org/emacs/MovingTheCtrlKey#toc2
;;                I chose "Make Caps Lock an additional Ctrl"

;; Prerequisites:
;; - git, bzr, cvs, svn, hg, autoconf, makeinfo (from texinfo)
;; - doxymacs requires that libxml2-dev be installed (presently not used)

;; Remove older versions of emacs

;; - uncomment (el-get-update-all t) in section Sync and update (below)
;; - el-get-emacswiki-refresh (no separate subdirectory)
;; - el-get-install every el-get-sources entry in this file

;; Check *Messages* for files being loaded as source

;; profile:  emacs -Q -l ~/.emacs.d/el-get/profile-dotemacs/profile-dotemacs.el -f profile-dotemacs

;; Launcher: /usr/bin/emacs-snapshot --no-site-file -bg black -fg white -fn dina -mm --debug-init

;;}}}
;;{{{  Missing and TODO

;; TODO
;; - mode-line:
;;   position-widget could change color is cursor exceeds limit column
;;   see http://www.emacswiki.org/emacs/ModeLinePosition
;; - Searching:
;;   see https://github.com/nschum/highlight-symbol.el
;; - Window resizing:
;;   see https://github.com/roman/golden-ratio.el

;; swap caps lock and control
;; Sanjay Dixit's am package - add autoload cookies, use el-get
;; align
;; occur
;; grep
;; gnus (see Mathworks' configuration in load-sb-tools.el)
;; sort lines
;; e/vtags
;; gtags?
;; setnu?
;; defer which-func until we initialize a buffer in one of the following
;;   modes: emacs-lisp-mode c-mode c++-mode  python-mode makefile-mode
;;          sh-mode perl-mode or cperl-mode

;; (add-to-list 'compilation-error-regexp-alist
;;             '("^\\(?:..\\[[ 0-9]+\\]+ \\)*\\([a-zA-Z0-9:\\/\\.-]+\.lp0\\)\(\\([0-9]+\\)\)"
;;               1 2 nil))

;;}}}

;;=== Package management ===============================================
;;{{{  ensure necessary directories

(setq el-get-dir "/home/jyates/.emacs.d/el-get/")

(mapc (lambda (path)
	(unless (file-accessible-directory-p path)
	  (make-directory path t)))
      `("~/.emacs.d/autosave"
        "~/.emacs.d/autosave-list"
        "~/.emacs.d/backup"
        "~/.emacs.d/semanticdb"
        "~/.emacs.d/srecode"
        "~/.emacs.d/url"
;        "/Hub/share/sbtools/apps/emacs-add-ons/src/sb-tools"
        ,el-get-dir
	))

;;}}}
;;{{{  add installed packages to load-path

; Various packages reference c-common-mode-hook.  Update load-path
; early to ensure that we pick up the latest cc-mode sources.
(add-to-list 'load-path "~/repos/cc-mode")

(add-to-list 'load-path "~/repos/phw")

(let ((entries (reverse (directory-files el-get-dir t))))
  (mapc (lambda (path)
          (when (and (file-directory-p path)
		     (/= ?. (aref path (1- (length path))))
		     (not (memq path load-path)))
            (add-to-list 'load-path path))) entries))

;;}}}
;;{{{  el-get

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CAVEAT: functions in packages acquired via el-get cannot be invoked
;; until (el-get 'sync) has completed in the el-get epilog toward the
;; bottom of this file.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq el-get-sources '(el-get)) ; built incrementally via add-to-list
(setq el-get-git-shallow-clone t)

(defalias 'seq-copy #'copy-sequence)

;; Minimal bootstrap
(unless (file-directory-p (concat el-get-dir "el-get"))
  (url-retrieve "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
                (lambda (s)
		    (goto-char (point-max))
		    (eval-print-last-sexp)))
  (el-get-elpa-build-local-recipes)
  (add-to-list 'load-path (concat el-get-dir "el-get"))
  )

(require 'el-get)

(defvar my/missing-el-get-packages nil)
(defvar my/all-el-get-packages nil)
(defun my/el-get-install (pkg)
  "Ensure that PKG has actually been installed."
  (let ((path (concat el-get-dir pkg)))
    (add-to-list 'my/all-el-get-packages pkg)
    (unless (file-directory-p path)
      (message "Package '%s' currently is missing" path)
      (add-to-list 'my/missing-el-get-packages pkg))))

;;}}}
;;{{{  John Wiegley's use-package

(add-to-list 'el-get-sources
             '(:name use-package
                     :description "A use-package declaration for simplifying your .emacs."
                     :website     "https://github.com/jwiegley/use-package"
                     :type        github
                     :pkgname     "jwiegley/use-package"
                     :features    (use-package bind-key)))
(my/el-get-install "use-package")

;;}}}

;;=== Customization ====================================================
;;{{{  Customization auditing framework

(eval-when-compile (defvar my/accum-custom-variables nil
  "Accumulate throughout this file a list of customized variables."))

(eval-when-compile (defvar my/accum-custom-faces nil
  "Accumulate throughout this file a list of customized faces."))

(defmacro my/custom-set-variables (&rest args)
  "Accumulate ARGS on 'my/custom-variables at compile-time."
  `(eval-when-compile (mapc (lambda (x) (add-to-list 'my/accum-custom-variables x)) (list ,@args))))

(defmacro my/custom-set-faces (&rest args)
  "Accumulate ARGS on 'my/custom-faces at compile-time."
  `(eval-when-compile (mapc (lambda (x) (add-to-list 'my/accum-custom-faces x)) (list ,@args))))


(defun my/check-custom-list (kind custom local)
  "Write to *Custom KIND audit* an audit of CUSTOM versus LOCAL."
  (with-current-buffer (get-buffer-create
                        (concat "*Custom " kind " audit*"))
    (let* ((standard-output (current-buffer))
           (remaining (sort local
                            (lambda (a b)
                              (string-lessp (car a) (car b))))))
      (dolist (centry (mapcar (lambda (x) (cdr x)) custom))
        (let ((csym (caar centry))
              (cval (cdar centry)))
          (let ((rentry (assoc csym remaining)))
            (cond
             ((not rentry)
              (cond ((not (equal cval (get csym 'standard-value)))
                     (insert "\n--- MISSING ---\nSaved to custom-file:\n")
                     (pp (car centry)))))
             (t
              (while (not (equal rentry (car remaining)))
                (insert "\n--- UNKNOWN (not saved to custom-file?) ---\nSpecified in .emacs:\n")
                (pp (car remaining))
                (setq remaining (cdr remaining)))
              (cond
               ((not (equal cval (cdr rentry)))
                (insert "\n--- CONFLICT ---\nSaved to custom-file:\n")
                (pp (car centry))
                (insert "\nSpecified in .emacs:\n")
                (pp rentry))
               ((equal cval (get csym 'standard-value))
                (insert "\n--- REDUNDANT (matches default) ---\nIn both .emacs and custom-file:\n")
                (pp rentry)))
              (setq remaining (cdr remaining))))))))
    (if (>= 2 (point-max))
        (kill-buffer)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;}}}
;;{{{  Load customizations

;; Added by package.el: must precede config of installed packages.
(package-initialize)

(setq custom-file "~/emacs/custom-file")

(defvar my/saved-custom-variables nil
  "List of customizations read from the custom file.")

(defvar my/saved-custom-faces nil
  "List of face customizations read from the custom file.")

(defun process-customization-list ()
  "Evaluate the next form in *Custom File*; return the argument list"
  (let ((form (read (current-buffer))))
    (eval form)
    (cdr form)))

;; Read custom-file, save the customization lists and evaluate the forms.
(unless my/el-get-refetch-all-packages
  (with-current-buffer (get-buffer-create "*Custom File*")
    (insert-file-contents-literally custom-file )
    (setq my/saved-custom-variables (process-customization-list))
    (setq my/saved-custom-faces     (process-customization-list))
    (kill-buffer)))

;; Host specific initialization if it exists (my-rc-local-HOST.el[c])
(require (intern (concat "my-rc-local-"
                         (let* ((host (system-name))
                                (idx (string-match "\\." host)))
                           (if idx
                               (substring host 0 idx)
                             host))))
         nil t)

;;}}}
;;{{{  List of packages installed from ELPA

(my/custom-set-variables
 '(package-selected-packages
   '(ace-window
     avy
     )))

;;}}}

;;=== Protection =======================================================
;;{{{  Auto-save and backup

(my/custom-set-variables
 '(auto-save-file-name-transforms
   '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t) ; tramp to /tmp
     (".*" "~/.emacs.d/autosave/\\1" t))) ; everything else
 '(auto-save-list-file-prefix "~/.emacs.d/autosave-list/saves-")
 '(backup-by-copying t)
 '(backup-directory-alist '((".*" . "/home/jyates/.emacs.d/backup/")))
 '(delete-old-versions t)
 '(kept-old-versions 5)
 '(vc-make-backup-files t)
 '(version-control t)                   ; add version # to backup
 )

;;}}}
;;{{{  Immortal buffers

(defun my/make-buffer-immortal (name mode)
  "Imbue buffer NAME with major MODE and immortality."
  (with-current-buffer (get-buffer-create name)
    (set-auto-mode-0 mode)
    ;; clobber any pre-existing hooks
    (set (make-local-variable 'kill-buffer-query-functions) nil)
    (add-hook 'kill-buffer-query-functions 'my/kill-immortal-buffer)))

(defun my/kill-immortal-buffer ()
  "Kill, then recreate buffer preserving name, mode and immortality."
  (let* ((name (buffer-name))
         (mode major-mode))
    (setq kill-buffer-query-functions nil)
    (kill-buffer (current-buffer))
    (my/make-buffer-immortal name mode))
  ;; buffer already killed so do not let caller repeat
  nil)

(my/make-buffer-immortal "*Messages*" 'fundamental-mode)
(my/make-buffer-immortal "*scratch*"  'lisp-interaction-mode)

;;}}}
;;{{{  Safe local variables

(my/custom-set-variables
 '(safe-local-variable-values
   ;; List variable-value pairs that are considered safe.  Each
   ;; element is a cons cell (VAR . VAL), where VAR is a variable
   ;; symbol and VAL is a value that is considered safe.
   ;;
   ;; Add elements to this list to avoid being queried when visiting
   ;; file with Local Variables: sections.
   '((eval add-to-list 'auto-mode-alist
         '("\\.cgr\\'" . c++-mode))
     (eval add-to-list 'auto-mode-alist
         '("\\.inc\\'" . c++-mode))
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require 'whitespace)
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face tabs trailing lines-tail)
     (folded-file . t)
     (folding-mode . t)
     )))

;;}}}
;;{{{  Less safe: y/n instead of yes/no

(fset 'yes-or-no-p 'y-or-n-p)

;;}}}

;;=== Visuals ==========================================================
;;{{{  Xresources

;; For faster startup (becasue of fewer interactions with xserver) use
;; 'xrdb ~/.Xresources' to kill menubar, toolbar & vertical scrollbars.
;;
;; Emacs.fullscreen: maximized
;; Emacs.background: black
;; Emacs.menuBar: off
;; Emacs.toolBar: off
;; Emacs.verticalScrollBars: off

;;}}}
;;{{{  Proportional fonts

(mapc
 (lambda (hook)
   (add-hook hook (lambda () (variable-pitch-mode t))))
 '(erc-mode-hook
   edit-server-start-hook
   markdown-mode-hook
   twittering-mode
   text-mode
   ))

;;}}}
;;{{{  Icon text when minimized

(setq icon-title-format
      '((((winring-show-names
   (" " winring-name))))
        "Emacs:  %b"))

;;}}}
;;{{{  Filename [ path ] in title bar

(setq frame-title-format
      '((((winring-show-names
           (" " winring-name))))
        "Emacs:  %b"
        (buffer-file-name
         ("  [ "
          (:eval
           (file-name-directory
            (abbreviate-file-name
             (file-truename buffer-file-name))))
          " ]")
         nil)))

;;}}}
;;{{{  Frame appearance

(my/custom-set-variables
 '(default-frame-alist
    '((foreground-color . "white")
      (background-color . "black")
      (font . "dina-13")
      (cursor-type . bar)
      ;; (width . 100)
      ;; (height . 60)
      ;; (top . 0)
      ;; (left . 0)
      (fullscreen . maximized)
      (minibuffer . t)
      ;; Replaced by 'Emacs.verticalScrollBars: off' in ~/.Xresources
      ;; (vertical-scroll-bars . right)
      (icon-type)))
 '(indicate-buffer-boundaries 'right) ; graphics in fringe
 )

(my/custom-set-faces
 '(fringe ((((class color) (background dark)) (:background "gray15"))))
 )

(horizontal-scroll-bar-mode -1)

;;}}}
;;{{{  Menu bar

;; Replaced by 'Emacs.menuBar: off' in ~/.Xresources
;;
;; (my/custom-set-variables
;;  '(menu-bar-mode nil)
;;  )

;;}}}
;;{{{  Minibuffer

(my/custom-set-variables
 '(minibuffer-frame-alist
   '((height . 2)
     (fullscreen . fullwidth)
     (user-size . t)
     (top - 0)
     (left . 0)
     (user-position . t)
     ;; (name . "Emacs minibuffer    (CTRL + click to show/hide other Emacs frames)")
     )))

(my/custom-set-faces
 '(minibuffer-prompt ((t (:foreground "orange" :weight bold))))
 )

;;}}}
;;{{{  Cursor and parenthesis matching

(my/custom-set-variables
 '(blink-cursor-blinks 0)
 '(blink-cursor-delay 0)
 '(cua-mode t nil (cua-base))
 '(cua-enable-cua-keys nil)
 '(cua-enable-cursor-indications t)
 '(cua-normal-cursor-color    '(bar . "Orange"))
 '(cua-overwrite-cursor-color '(box . "HotPink1"))
 '(cua-read-only-cursor-color '(box . "SeaGreen1"))
 '(x-stretch-cursor t)
 )

(my/custom-set-faces
 '(cursor ((t (:background "gold"))))
 '(show-paren-match ((t (:background "light green" :foreground "black" :weight bold))))
 '(show-paren-mismatch ((t (:background "firebrick" :foreground "white"))))
 )

(defun blink-cursor-start ()
  "Timer function called from the timer `blink-cursor-idle-timer'.
This starts the timer `blink-cursor-timer', which makes the cursor blink
if appropriate.  It also arranges to cancel that timer when the next
command starts, by installing a pre-command hook."
  (when (null blink-cursor-timer)
    ;; Set up the timer first, so that if this signals an error,
    ;; blink-cursor-end is not added to pre-command-hook.
    (setq blink-cursor-blinks-done 1)
    (setq blink-cursor-timer
	  (run-with-timer blink-cursor-interval blink-cursor-interval
			  'blink-cursor-timer-function))
    (add-hook 'pre-command-hook 'blink-cursor-end)
    (internal-show-cursor nil t)))

(defun paren-close-dwim ()
  "Insert closing parenthesis from syntax table.
Use a normal parenthesis if not inside any."
  (interactive "*")
  (insert (or (ignore-errors
                (save-excursion (backward-up-list)
                                (cdr (syntax-after (point)))))
              ?\))))

;;}}}
;;{{{  Basic faces

(my/custom-set-faces
 '(fixed-pitch              ((t nil)))
 '(variable-pitch           ((t (:height 0.9 :family "Sans Serif"))))
 '(widget-documentation     ((t (:inherit font-lock-comment-face))))
 '(widget-field             ((t (:background "gray20"))))
 '(widget-single-line-field ((t (:inherit widget-field))))
 )

;;}}}
;;{{{  Highlight line

(my/custom-set-variables
 '(global-hl-line-mode t)
 '(hl-line-sticky-flag nil)
 )

(my/custom-set-faces
 '(hl-line ((t (:underline "gray50"))))
 )

;;}}}
;;{{{  Font lock and other basic faces

(my/custom-set-faces
 '(error ((t (:foreground "brown2"))))
 '(font-lock-builtin-face ((((class color) (background dark)) (:foreground "Turquoise"))))
 '(font-lock-comment-face ((t (:foreground "MediumAquamarine"))))
 '(font-lock-constant-face ((((class color) (background dark)) (:bold t :foreground "DarkOrchid"))))
 '(font-lock-doc-string-face ((t (:foreground "green2"))))
 '(font-lock-function-name-face ((t (:foreground "SkyBlue"))))
 '(font-lock-keyword-face ((t (:bold t :foreground "CornflowerBlue"))))
 '(font-lock-preprocessor-face ((t (:italic nil :foreground "CornFlowerBlue"))))
 '(font-lock-reference-face ((t (:foreground "DodgerBlue"))))
 '(font-lock-string-face ((t (:foreground "LimeGreen"))))
 '(font-lock-type-face ((t (:foreground "#9290ff"))))
 '(font-lock-variable-name-face ((t (:foreground "PaleGreen"))))
 '(font-lock-warning-face ((((class color) (background dark)) (:foreground "yellow" :background "red"))))
 '(success ((t (:foreground "PaleGreen1"))))
 )

;;}}}
;;{{{  Selection and highlighting

(my/custom-set-faces
;'(highlight ((t (:background "#091830"))))
 '(highlight ((t (:background "#123060"))))
;'(highlight-beyond-fill-column-face ((t (:inverse-video t))))
 '(region ((t (:background "DarkSlateBlue"))))
 )

;;}}}
;;{{{  mode-line basics

(defface mode-line-highlight-bold
  '((t (:inherit mode-line-highlight
        :weight bold)))
  "Basic mode line face for bolding in highlights."
  :version "22.1"
  :group 'mode-line-faces
  :group 'basic-faces)

(my/custom-set-faces
 '(mode-line                ((t (:background "tan2" :foreground "black" :box (:line-width -1 :style released-button) :height 1.2))))
 '(mode-line-buffer-id      ((t (:weight bold))))
 '(mode-line-highlight      ((t (:background "wheat2" :box (:line-width -1 :style released-button)))))
 '(mode-line-highlight-bold ((t (:background "wheat2" :weight bold :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive       ((t (:inherit mode-line :background "gray65"))))
 )

(my/custom-set-variables
 '(mode-line-format
   '("%e" mode-line-window-id
     #("-" 0 1 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy enitre frame
mouse-3: Remove current window from display"))
     mode-line-mule-info
     mode-line-client
     mode-line-modified
     mode-line-remote
     #(" " 0 1 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy enitre frame
mouse-3: Remove current window from display"))
     mode-line-position
     mode-line-frame-identification
     mode-line-buffer-identification
     #(" " 0 1 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy enitre frame
mouse-3: Remove current window from display"))
     (which-func-mode
      ("" which-func-format
       #(" " 0 1 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy enitre frame
mouse-3: Remove current window from display"))))
     mode-line-modes
     (vc-mode ("" vc-mode #(" " 0 1 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy enitre frame
mouse-3: Remove current window from display"))))
     (global-mode-string (#("-" 0 1 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy enitre frame
mouse-3: Remove current window from display")) global-mode-string))
     #("-%-" 0 3 (help-echo "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy enitre frame
mouse-3: Remove current window from display"))))

 ;; If vc-mode ever gets turned on try to minize its impact.
 '(vc-ignore-dir-regexp
   "\"\\\\`\\\\(?:/mathworks\\\\|/mathworks/[A-Z]+\\\\|/mathworks/[^/]+\\\\|/mathworks/[A-Z]+/[^/]+\\\\|/mathworks/devel/[^/]+\\\\|/mathworks/[A-Z]+/devel/[^/]+\\\\|/mathworks/devel/bat/[^/]+\\\\|/mathworks/[A-Z]+/devel/bat/[^/]+\\\\|/mathworks/devel/jobarchive\\\\|/mathworks/[A-Z]+/devel/jobarchive\\\\|/mathworks/hub/[^/]+\\\\|/mathworks/[A-Z]+/hub/[^/]+\\\\|/mathworks/hub/scratch\\\\|/mathworks/[A-Z]+/hub/scratch\\\\|/mathworks/hub/site-local\\\\|/mathworks/[A-Z]+/hub/site-local\\\\|/sandbox\\\\|/home\\\\|/hub\\\\|/public\\\\|/src\\\\|/scratch\\\\|\\\\)/\\\\'\"\n")
 )

;;}}}
;;{{{  mode-line eol-mnemonic

(my/custom-set-variables
 '(eol-mnemonic-mac ":")                ; defaults to '/'
 '(eol-mnemonic-undecided "?")          ; defaults to ':'
 '(eol-mnemonic-unix "/")               ; defaults to ':'
 )

;;}}}
;;{{{  mode-line window-id

(defconst mode-line-window-id
  '(:eval (mode-line-window-id)))

(defun mode-line-window-id ()
  "If possible return a phw-mode window ID."
   (if (fboundp 'phw-window-ordinal)
       (phw-window-ordinal (selected-window))
     nil))

;;}}}
;;{{{  mode-line position-widget

;; Use a fancy widget to display buffer position on the mode line.
;; -------------------------------------------------------------------
;;
;; Initial inspiration from an email thread between David Engster's and
;; Drew Adams:
;;
;;   http://lists.gnu.org/archive/html/emacs-devel/2010-03/msg00523.html
;;   http://permalink.gmane.org/gmane.emacs.devel/122038
;;
;; Further learnings from Lennart Borgman's sml-modeline.el
;;   https://www.emacswiki.org/emacs/sml-modeline.el

(defvar buffer-max-column-visited 1
  "Accumulate max column visited to prevent mode-line jitter.")
(make-variable-buffer-local 'buffer-max-column-visited)

;; (defface mode-line-bold-highlight
;;   '((t (:group 'modeline :inherit mode-line-highlight :weight bold)))
;;   "")

(defun my/position-widget ()
  (let*
      ((c-n-m column-number-mode)
       (l-n-m line-number-mode)
       (wbeg (window-start))
       (wend (window-end)))

    (save-restriction
      (widen)

      (let*
          ((eob-line  (line-number-at-pos (point-max)))
           (wbeg-line (line-number-at-pos wbeg))
           (wend-line (line-number-at-pos wend))

           (widget
            (concat
             ;; Leading [
             (if (= wbeg-line 1)
                 #("[" 0 1 (face mode-line-highlight-bold))
               "[")
             ;; Body
             (if (not (or l-n-m c-n-m))
                 (replace-regexp-in-string "%" "%%" (format-mode-line '(-3 "%P")))
               (let*
                   ((wlines (1+ (- wend-line wbeg-line)))
                    (expanded
                     (format-mode-line
                      (concat
                       (cond
                        ((not l-n-m) "")
                        ((> 10      eob-line) "%1l")
                        ((> 100     eob-line) "%2l")
                        ((> 1000    eob-line) "%3l")
                        ((> 10000   eob-line) "%4l")
                        ((> 100000  eob-line) "%5l")
                        ((> 1000000 eob-line) "%6l")
                        (t                    "%7l"))
                       (if (and l-n-m c-n-m) #("," 0 1  (face bold)) "")
                       (cond
                        ((not c-n-m) "")
                        (t (let*
                               ((max-col (max (current-column)
                                              buffer-max-column-visited))
                                (field   (cond
                                          ((> 10   max-col) 3)
                                          ((> 100  max-col) 4)
                                          ((> 1000 max-col) 5)
                                          (t                6)))
                                (cur-col (current-column))
                                (digits  (cond
                                          ((> 10   cur-col) 1)
                                          ((> 100  cur-col) 2)
                                          ((> 1000 cur-col) 3)
                                          (t                4))))
                             (setq buffer-max-column-visited max-col)
                             (substring "%c     " 0 (- field digits))))))))

                    (len (length expanded))
                    (hilen (max 1           ; at least one column
                                (min len    ; no more than full string
                                     (round (/ (* wlines len)
                                               (float eob-line))))))
                    (lpad (round (/ (* wbeg-line (- len hilen))
                                    (float (- eob-line wlines -2)))))
                    (rpad (+ lpad hilen)))

                 (put-text-property lpad rpad 'face 'mode-line-highlight expanded)
                 expanded))
             ;; Trailing ]
             (if (= wend-line eob-line)
                 #("]" 0 1 (face mode-line-highlight-bold))
               "]"))))

        (propertize
         widget
         'help-echo "Buffer position widget\nmouse-1: Line and Column Mode Menu"
         'mouse-face 'mode-line-highlight
         'local-map '(keymap
                      (mode-line
                       keymap
                       (down-mouse-1
                        keymap
                        (line-number-mode
                         menu-item "Global Line Number Mode" line-number-mode
                         :help "Toggle line number display"
                         :button (:toggle . line-number-mode))
                        (column-number-mode
                         menu-item "Global Column Number Mode" column-number-mode
                         :help "Toggle column number display"
                         :button (:toggle . column-number-mode))
                        "Control Line and Column Display Globally"))))))))

(my/custom-set-variables
 '(mode-line-position '(:eval (my/position-widget)) t)
 )

;;}}}
;;{{{  mode-line which-func

(my/custom-set-variables
 '(which-func-format
   '("["
     (:propertize which-func-current local-map
                  (keymap
                   (mode-line keymap
                              (mouse-1 . beginning-of-defun)
                              (mouse-2 . #[nil "e\300=\203	 \301 \207~\207"
                                               [1 narrow-to-defun]
                                               2 nil nil])
                              (mouse-3 . end-of-defun)))
                  face which-func help-echo
"Function (enclosing or preceding)
mouse-1: go to beginning
mouse-2: toggle function narrowing
mouse-3: go to end")
     "]"))
 '(which-function-mode t nil (which-func))
 )

(my/custom-set-faces
 '(which-func ((t (:foreground "dark blue" :weight bold))))
 )

;;}}}
;;{{{  diminish

(add-to-list 'el-get-sources 'diminish)
(my/el-get-install "diminish")

;; To diminish minor mode FOO:
;;
;; (eval-after-load "FOO" '(diminish 'FOO-mode))     ; FOO displayed as ""
;; (eval-after-load "FOO" '(diminish 'FOO-mode "X")) ; FOO displayed as "X"

;; To diminish major mode BAR:
;;
;; (defun my/dimish-BAR-mode ()
;;  "Display BAR-mode in mode line as \"X\"."
;;  (setq mode-name "X"))
;;
;; (eval-after-load "BAR" '(add-hook 'my/dimish-BAR-mode))

;;}}}
;;{{{  Uniquify buffer names

(require 'uniquify)

(my/custom-set-variables
 '(uniquify-buffer-name-style 'forward nil (uniquify)) ; prefix '/'
 )

;;}}}
;;{{{  Pretty ^L and other stuff

;; Display Windows extended glyphs (instead of things like \223)
(standard-display-8bit 128 255)

(add-to-list 'el-get-sources
             '(:name  pp-c-l
                     :description "Display Control-l characters as a full-width rule"
                     :after       (progn
                                    (add-hook 'window-setup-hook
                                              'refresh-pretty-control-l)
                                    (add-hook 'window-configuration-change-hook
                                              'refresh-pretty-control-l))))
(my/el-get-install "pp-c-l")

;; Universally display ^L as a window-width horizontal rule
(my/custom-set-variables
;'(pretty-control-l-mode t nil (pp-c-l))
 '(pretty-control-l-mode t)
;'(pp^L-^L-string-function (lambda (win) (make-string (1- (window-width win)) 32)) nil (pp-c-l))
 '(pp^L-^L-string-function (lambda (win) (make-string (1- (window-width win)) 32)))
;;'(pp^L-^L-string-pre "")               ; eliminate preceding blank line
 )

(my/custom-set-faces
 '(pp^L-highlight ((((type x w32 mac graphic) (class color)) (:inherit shadow :strike-through t))))
 )

;;}}}
;;{{{  Random custom items

(my/custom-set-variables
 '(column-number-mode t)                ; enable magic line/column format
 '(custom-buffer-done-kill t)           ; kill custom buffer on exit
 '(global-font-lock-mode t)             ; font-lock in all buffers
 '(initial-scratch-message nil)         ; do not seed *scratch* w/ a msg
 '(inhibit-startup-screen t)            ; splash screens are for wimps
 '(scalable-fonts-allowed t)            ; no restrictions (can be slow!)
 '(scroll-conservatively 1)             ; scroll window a line at a line
 '(show-paren-mode t)                   ; highlight matching parens
 '(show-paren-delay 0)                  ;  and do it immediately
; Replaced by 'Emacs.toolBar: off' in ~/.Xresources
;'(tool-bar-mode nil)                   ; recover screen space, no toolbar
 '(truncate-lines t)                    ; no wrapped lines
 '(transient-mark-mode t)               ; hightlight region, etc.
 '(use-dialog-box nil)                  ; dialog boxes are also for wimps
 '(visible-bell t)                      ; subtle blink in response to ^G
 )

;;}}}
;;{{{  Tool tips

(my/custom-set-variables
 '(tooltip-delay 0.3)
 )

(my/custom-set-faces
 '(tooltip ((((class color)) (:inherit variable-pitch :background "#f5f5b5" :foreground "black"))))
 )

;;}}}
;;{{{  Width to use for hard tabs

(defun my/set-buffer-local-tab-width-to-4 ()
  "Make display engine's tab-width buffer local and set it to 4."
  (interactive)
  (setq tab-width 4))

(defun my/set-buffer-local-tab-width-to-8 ()
  "Make display engine's tab-width buffer local and set it to 8."
  (interactive)
  (setq tab-width 8))

;;}}}
;;{{{  Ansi colors

(my/custom-set-variables
 '(ansi-color-names-vector
   ["black"
    "#ee9090"
    "pale green"
    "khaki"
    "steelblue1"
    "dark violet"
    "DarkSlateGray1"
    "white"])
 )

;; From the distant past...
;;  '(ansi-color-faces-vector [default bold default italic underline bold bold-italic modeline])
;;  '(ansi-color-names-vector ["black" "IndianRed1" "medium spring green" "khaki1" "DodgerBlue1" "maroon1" "DarkSlateGray1" "white"])

;;}}}
;;{{{  Extra color themes

(my/custom-set-variables
 '(custom-theme-directory "~/emacs/themes")
 )

;;}}}
;;{{{  Window splitting and deletion

(my/custom-set-variables
 '(split-height-threshold nil)
 )

;; Cribbed from http://www.emacswiki.org/emacs/HorizontalSplitting

(defun split-horizontally-for-temp-buffers ()
  "Split the window horizontally for temp buffers."
  (when (and (one-window-p t)
     	     (not (active-minibuffer-window)))
    (split-window-horizontally)))

; (add-hook 'temp-buffer-setup-hook 'split-horizontally-for-temp-buffers)

;;}}}
;;{{{  Popwin and guide-key

;; (add-to-list 'el-get-sources 'popwin)
;; (my/el-get-install "popwin")


;; (add-to-list 'el-get-sources
;;              '(:name guide-key
;;                      :description "Guide the following key bindings automatically and dynamically"
;;                      :website     "https://github.com/kbkbkbkb1/guide-key.git"
;;                      :type        github
;;                      :pkgname     "kbkbkbkb1/guide-key"
;;                      :features    (guide-key)))
;; (my/el-get-install "guide-key")

;;}}}

;;=== Moving, searching and editing ====================================

(my/custom-set-variables
 '(kill-whole-line t)
;'(view-read-only t)    ; Use view-mode if file is or opened read-only
 )

;;{{{  Navigation: avy

(autoload 'avy-goto-word-or-subword-1 "avy"
  "Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'."
  t)

;;}}}
;;{{{  Files and directories

(ffap-bindings)

;;}}}
;;{{{  Smarter move-beginning-of-line, move-end-of-line

(defun my/move-beginning-or-indentation (arg)
  "Move point back to beginning of line or of indentation.

Move point to the beginning of the line.  If point is already
there, move to the first non-whitespace character on this line.
Effectively toggle between the beginning of the line and the
first non-whitespace character.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (move-beginning-of-line 1)
    (when (= orig-point (point))
      (back-to-indentation))))

(substitute-key-definition
   'move-beginning-of-line 'my/move-beginning-or-indentation global-map)

(defun my/move-end-comment-or-code (arg)
  "Move point to end of line, start of comment or end of code.

Move point to the end of the line.  If point is already there,
move to the start of any trailing comment or to the end of code.
If point is not at end of line but is within a comment then move
to end of code.  Finally if point is at the end of code move to
the end of the line.  Effectively cycle through end of line,
start of any trailing comment and end of code.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((start (point))
        (bol (line-beginning-position))
        (code nil)
        (comment nil)
        (eol (line-end-position)))
    (goto-char eol)
    (skip-chars-backward " \t")
    (when (/= bol (point))
      (goto-char (if (setq comment
                           (if comment-start-skip
                               (comment-search-backward bol t)))
                     comment
                   eol))
      (skip-chars-backward " \t")
      (when (/= bol (point))
        (setq code (point))))
    (goto-char (cond
                ((and (eq start eol    ) comment) comment)
                ((and (eq start eol    ) code   ) code   )
                ((and (eq start comment) code   ) code   )
                (     (eq start code   )          eol    )
                (t                                eol    )))))

(substitute-key-definition
   'move-end-of-line 'my/move-end-comment-or-code global-map)

;;}}}
;;{{{  Delete selection mode

(defun my/turn-on-delete-selection-mode ()
  (delete-selection-mode 1)
  (remove-hook activate-mark-hook 'my/turn-on-delete-selection-mode)
  )

(my/custom-set-variables
 '(activate-mark-hook 'my/turn-on-delete-selection-mode t)
 )

;;}}}
;;{{{  Simple editing operations

(defun my/delete-whitespace-forward (arg)
  "Delete all white space from point to the next word.  With prefix ARG
delete across newlines as well.  The only danger in this is that you
don't have to actually be at the end of a word to make it work.  It
skips over to the next whitespace and then whacks it all to the next
word."
  (interactive "P")
  (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
    (re-search-forward regexp nil t)
    (replace-match "" nil nil)))

;;}}}
;;{{{  White space hygiene

;; Electic indentation creates trailing whitespace.  These hook
;; functions cleanup only modified lines.

;; (defvar my/modified-line nil)
;;
;; (defun my/modified-line-note (beg end old-len)
;;   (let ((bol (line-beginning-position)))
;;     (if (/= (point) bol)
;;         (setq-local my/modified-line bol))))
;;
;; (defun my/modified-line-cleanup-after-leaving ()
;;   (when my/modified-line
;;     (when (not buffer-read-only)
;;       (save-excursion
;;         (beginning-of-line 1)
;;         (when (/= (point) my/modified-line)
;;           (goto-char my/modified-line)
;;           (end-of-line)
;;           (while (memq (char-before) '(?\  ?\t))
;;             (delete-char -1)))))
;;     (setq-local my/modified-line nil)))
;;
;; (defun my/modified-line-first-change ()
;;   (add-hook 'post-command-hook 'my/modified-line-cleanup-after-leaving t))
;;
;; (add-hook 'after-change-functions 'my/modified-line-note)
;; (add-hook 'first-change-hook 'my/modified-line-first-change)

;; Other bits of hygiene
(my/custom-set-variables
 '(indicate-empty-lines t)
 '(require-final-newline 'query)
 '(show-trailing-whitespace t)
 '(whitespace-global-mode t)
 )

(my/custom-set-faces
 '(trailing-whitespace ((((class color) (background dark)) (:background "red3"))))
 )

;;}}}
;;{{{  Indent yanked text (cribbed from Alex Ott)

;; (defconst my/yank-indent-modes
;;   '(c++-mode
;;     c-mode
;;     cperl-mode
;;     emacs-lisp-mode
;;     lisp-interaction-mode
;;     lisp-mode
;;     sql-mode
;;     tcl-mode)
;;   "List of major modes in which to indent yanked (or yank-popped) text.")

;; (defun my/maybe-indent-yanked ()
;;   "If mode in my/yank-indent-modes then indent yanked text (prefix arg inhibits)."
;;   (if (member major-mode my/yank-indent-modes)
;;       (let ((mark-even-if-inactive t))
;;         (indent-region (region-beginning) (region-end) nil))))

;; (defadvice yank (after my/maybe-indent activate)
;;   "If mode in my/yank-indent-modes then indent yanked text (prefix arg inhibits)."
;;   (my/maybe-indent-yanked))

;; (defadvice yank-pop (after my/maybe-indent activate)
;;   "If mode in my/yank-indent-modes then indent yanked text (prefix arg inhibits)."
;;   (my/maybe-indent-yanked))

;;}}}
;;{{{  Tabs

(my/custom-set-variables
 '(indent-tabs-mode nil)        ; no hard tabs
 '(tab-always-indent nil)       ; indent, complete else insert whitespace
 '(tab-stop-list '(  4   8  12  16  20  24  28  32  36  40
                    44  48  52  56  60  64  68  72  76  80
                    84  88  92  96 100 104 108 112 116 120
                   124 128 132 136 140 144 148 152 165 160
                   164 168 172 176 180 184 188 192 196 200
                   204 208 212 216 220 224 228 232 236 240
                   244 248 252 265 260 264 268 272 276 280
                   284 288 292 296 300 304 308 312 316 320))
 )

(defun my/canonicalize-tab4 ()
  (interactive)
  (let ((tab-width 4))
    (untabify (point-min) (point-max))
    (tabify (point-min) (point-max))))

;;}}}
;;{{{  Buffer life cycle

;; Cribbed from:
;;; disk.el --- simplified find-file, revert-file, save-buffer interface
;; Copyright (C) 2002  Alex Schroeder <alex@gnu.org>
;; URL: http://www.emacswiki.org/cgi-bin/wiki.pl?DiskKey

(defvar disk-access nil)
(make-variable-buffer-local 'disk-access)

(add-hook 'find-file-hooks 'disk-notice)
(add-hook 'view-mode-hooks 'disk-notice)
(add-hook 'after-save-hook 'disk-notice)

(defun disk-file-modification-time ()
  "Return modification time of the visited file."
  (nth 5 (file-attributes (buffer-file-name))))

(defun disk-notice ()
  "Store access time in `disk-acess'."
  (setq disk-access (disk-file-modification-time)))

(defun disk-file-modified-p ()
  "Return non-nil if the visited file has been modified."
  (not (equal disk-access
      (disk-file-modification-time))))

(defun disk ()
  "Do the right thing with files.

If buffer has no file, view a file.  If buffer needs saving, and
file is unchanged, save buffer returning to readonly/view-mode.
If buffer needs saving, and file has changed, warn the user.  If
buffer is unchanged, and file has changed on disk, revert buffer,
switch to readonly/view-mode.  If buffer is readonly convert it
to writable, dropping view-mode.  Otherwise buffer is writable so
convert it to readonly/view-mode."
  (interactive)
  (cond ((not (buffer-file-name))
 (call-interactively 'view-file))
	((and (buffer-modified-p)
      (not (disk-file-modified-p)))
 (save-buffer)
         (setq buffer-read-only t)
         (view-mode 1))
	((and (buffer-modified-p)
      (disk-file-modified-p))
 (error "Buffer must be saved, but the file has also changed."))
	((and (not (buffer-modified-p))
      (disk-file-modified-p))
 (revert-buffer t t)
         (setq buffer-read-only t)
         (view-mode 1))
        (buffer-read-only
         (view-mode -1)
         (setq buffer-read-only nil))
	(t
         (setq buffer-read-only t)
         (view-mode 1))))

;;}}}
;;{{{  Iedit

(add-to-list 'el-get-sources
             '(:name iedit
                     :description "Edit multiple regions with the same content simultaneously."
                     :type        github
                     :pkgname     "victorhge/iedit"
                     :features iedit))
(my/el-get-install "iedit")

;;}}}

;;=== VC, diff, merge, patch ===========================================
;;{{{  git and magit

;(add-to-list 'el-get-sources '(:name magit :branch "pu"))
(add-to-list 'el-get-sources 'magit)
(my/el-get-install "magit")

(add-to-list 'el-get-sources 'git-timemachine)
(my/el-get-install "git-timemachine")

(my/custom-set-variables
 '(git-commit-setup-hook
   '(git-commit-save-message
     magit-revert-buffers
     git-commit-save-message
     git-commit-setup-changelog-support
     git-commit-turn-on-auto-fill
     git-commit-propertize-diff
     with-editor-usage-message))
 '(magit-backup-mode nil)
;'(magit-diff-refine-hunk t)
 '(magit-emacsclient-executable "/etc/alternatives/emacsclient")
 '(magit-refs-show-commit-count 'all)
 '(magit-repository-directories
   '("~/repos/awesome"
     "~/repos/doxygen"
     "~/repos/st"))
 '(magit-repository-directories-depth 0)
 '(magit-save-repository-buffers nil))

(my/custom-set-faces
 '(magit-item-highlight             ((t (                                 :background "gray9"        ))))

 '(magit-blame-heading              ((t (                                 :background "gray20"       ))))
 '(magit-branch-local               ((t (:foreground "LightSkyBlue1"      :underline t               ))))
 '(magit-branch-remote              ((t (:inherit magit-branch-local      :foreground "DarkSeaGreen2"))))
 '(magit-diff-added                 ((t (:inherit diff-added                                         ))))
 '(magit-diff-added-highlight       ((t (:inherit diff-refine-added                                  ))))
 '(magit-diff-context               ((t (:inherit magit-dimmed                                       ))))
 '(magit-diff-context-highlight     ((t (:inherit magit-diff-context      :background "grey10"       ))))
 '(magit-diff-file-heading          ((t (:inherit magit-diff-hunk-heading :foreground "LightSkyBlue1"))))
 '(magit-diff-file-heading-highlight((t (:inherit (magit-diff-hunk-heading-highlight magit-diff-file-heading)))))
 '(magit-diff-hunk-heading          ((t (                                 :background "grey15"       ))))
 '(magit-diff-hunk-heading-highlight((t (:inherit magit-section-highlight :weight bold               ))))
 '(magit-diff-removed               ((t (:inherit diff-removed                                       ))))
 '(magit-diff-removed-highlight     ((t (:inherit diff-refine-removed                                ))))
 '(magit-diffstat-added             ((t (:inherit magit-diff-added                                   ))))
 '(magit-diffstat-removed           ((t (:inherit magit-diff-removed                                 ))))
 '(magit-dimmed                     ((t (:foreground "gray70"                                        ))))
 '(magit-hash                       ((t (:foreground "thistle1"                                      ))))
 '(magit-log-head-label-tags        ((t (:foreground "black"              :background "wheat2" :box 1))))
 '(magit-section-heading            ((t (:foreground "SteelBlue1"         :weight bold               ))))
 '(magit-tag                        ((t (:foreground "LightGoldenrod2"    :weight bold               ))))
 )

(add-hook 'magit-mode-hook
          (lambda ()
            (magit-define-popup-action 'magit-ediff-popup ?S "Show staged"   'magit-ediff-show-staged)
            (magit-define-popup-action 'magit-ediff-popup ?U "Show unstaged" 'magit-ediff-show-unstaged)))

(autoload 'magit-status-internal "magit" nil t)

(defun my/magit-status ()
  "Switch to (or create) the magit status buffer for current context"
  (interactive)
  (let ((dir (locate-dominating-file "." ".git")))
    (if (not dir)
        (error "Directory not within a git workspace (%s)" default-directory)
      (let ((mbuf (get-buffer (concat "*magit: " (file-name-as-directory (substring dir 0 -1)) "*"))))
        (if mbuf
            (switch-to-buffer mbuf)
          (magit-status-internal dir))))))

;;}}}
;;{{{  diff

(my/custom-set-faces
 '(diff-added          ((t (:inherit diff-changed :foreground "#c8ffe0"))))
 '(diff-file-header    ((t (:inherit diff-header  :foreground "light goldenrod yellow" :weight bold))))
 '(diff-header         ((t (:background "gray15"                       ))))
 '(diff-nonexistent    ((t (:strike-through "red"                      ))))
 '(diff-refine-added   ((t (:inherit diff-added   :background "#001800"))))
 '(diff-refine-change  ((t (                      :background "#1c3850"))) t)
 '(diff-refine-removed ((t (:inherit diff-removed :background "#200000"))))
 '(diff-removed        ((t (:inherit diff-changed :foreground "#ffd0e0"))))
 )

;;}}}
;;{{{  ediff

(my/custom-set-variables
 '(ediff-cmp-program "git")
 '(ediff-cmp-options '("diff --histogram"))
 '(ediff-keep-variants nil)
 '(ediff-make-buffers-readonly-at-startup t)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-use-last-dir t)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 )

(my/custom-set-faces
 '(ediff-current-diff-A ((t (:inherit highlight :foreground "DarkSeaGreen1"))))
 '(ediff-current-diff-Ancestor ((t (:inherit highlight :foreground "yellow"))))
 '(ediff-current-diff-B ((t (:inherit highlight :foreground "MistyRose1"))))
 '(ediff-current-diff-C ((t (:inherit highlight :foreground "pale goldenrod"))))
 '(ediff-even-diff-A ((t (:background "#300000"))))
 '(ediff-even-diff-Ancestor ((t (:background "#300000"))))
 '(ediff-even-diff-B ((t (:background "#300000"))))
 '(ediff-even-diff-C ((t (:background "#300000"))))
 '(ediff-fine-diff-A ((t (:inherit ediff-current-diff-A :background "#1c3850"))))
 '(ediff-fine-diff-Ancestor ((t (:inherit ediff-current-diff-Ancestor :background "#1c3850"))))
 '(ediff-fine-diff-B ((t (:inherit ediff-current-diff-B :background "#1c3850"))))
 '(ediff-fine-diff-C ((t (:inherit ediff-current-diff-C :background "#1c3850"))))
 '(ediff-odd-diff-A ((t (:background "#281400"))))
 '(ediff-odd-diff-Ancestor ((t (:background "#281400"))))
 '(ediff-odd-diff-B ((t (:background "#281400"))))
 '(ediff-odd-diff-C ((t (:background "#281400"))))
 )

;;}}}
;;{{{  smerge

(my/custom-set-variables
 '(smerge-auto-leave nil)
 '(smerge-command-prefix ""))

(my/custom-set-faces
 '(smerge-base            ((t (:inherit ediff-even-diff-A))))
 '(smerge-mine            ((t (:inherit ediff-even-diff-A))) t)
 '(smerge-other           ((t (:inherit ediff-odd-diff-a))) t)
 '(smerge-refined-added   ((t (:inherit ediff-current-diff-A))))
 '(smerge-refined-changed ((t (:inherit ediff-current-diff-C))))
 '(smerge-refined-removed ((t (:inherit ediff-current-diff-B))))
 )

;;}}}
;;{{{  Applying patches

(eval-when-compile (require 'diff-mode))

;; make using patch mode more convenient
(defun my/diff-advance-apply-next-hunk-and-recenter ()
  "Advance to next hunk, apply, and recenter windows for review."
  (interactive)
  (diff-hunk-next)
  (diff-apply-hunk)
  (when diff-advance-after-apply-hunk
    (diff-hunk-prev))
  (recenter 0)
  (other-window 1)
  (recenter 0)
  (other-window 1))


(add-hook 'diff-mode-hook
          (function (lambda ()
                      (local-set-key "\C-c\C-c" 'my/diff-advance-apply-next-hunk-and-recenter)
                      (local-set-key "\C-c\C-i" 'diff-goto-source)))) ; mnemonic: inspect

;;}}}

;;=== Abbreviation and expansion =======================================
;;{{{  yasnippet

(my/el-get-install "yasnippet")

(my/custom-set-variables
 '(yas-global-mode t                       nil (yasnippet))
 '(yas-snippet-dirs '("~/emacs/yasnippet") nil (yasnippet)))

(add-hook 'yas/minor-mode-hook 'yas-reload-all)

;; reload modified snippets
(defun my/yasnippet-reload-on-save ()
  "Reload the entire collection of snippets when one gets modified."
  (if (eq major-mode 'snippet-mode)
      ;;  (mapc 'yas/load-directory yas/snippet-dirs)))
      (yas-reload-all))) ; no mapc with just a single directory root

(eval-after-load "yasnippet"
    '(add-hook 'after-save-hook 'my/yasnippet-reload-on-save))

;;;; auto-mode-alist
(add-to-list 'auto-mode-alist '("emacs/yasnippet/" . snippet-mode))
(add-to-list 'auto-mode-alist '("emacs/yasnippet/.+\\.el$" . emacs-lisp-mode))


;; ;; Another note: The new 0.7 yasnippet.el messes things up with
;; ;; anything.el. You need to do this:
;; ;;
;; ;; Need to replace the following in anything-c-yasnippet.el:
;; ;;   yas/snippets/table-hash      -> yas/table-hash
;; ;;   yas/snippets/table-templates -> yas/table-templates
;; ;;
;; ;; (require 'anything-c-yasnippet)

;;}}}
;;{{{  auto-complete

;; http://truongtx.me/2013/01/06/config-yasnippet-and-autocomplete-on-emacs
;; suggests that auto-complete needs to be activated _after_ yasnippet.

;; Not autoloaded in current sources
;; (autoload 'ac-set-trigger-key "auto-complete"
;;   "Set `ac-trigger-key' to `KEY'. It is recommemded to use this function instead of calling `setq'.

;; \(fn KEY)" t)


(add-to-list 'el-get-sources
             '(:name auto-complete
                     :after       (progn
                                    (setq-default ac-sources '(ac-source-abbrev
                                                               ac-source-dictionary
                                                               ac-source-words-in-same-mode-buffers))

                                    (defun my/ac-cc-mode-setup ()
                                      (setq ac-sources (append '(ac-source-yasnippet
                                                                 ac-source-c-headers)
                                                                 ;; ac-source-semantic
                                                                 ;; ac-source-semantic-raw
                                                                 ;; ac-source-gtags ; no "using namespace XX;"
                                                                 ) ac-sources)
                                      (setq ac-source-yasnippet nil))

                                    (add-hook 'c-mode-common-hook 'my/ac-cc-mode-setup)
                                    (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)

                                    (ac-set-trigger-key "TAB")
                                    (ac-set-trigger-key "<tab>"))
                     :depends (yasnippet popup fuzzy)))

(my/el-get-install "auto-complete")
(require 'auto-complete nil t)

;; (add-to-list 'el-get-sources
;;              (:name auto-complete-c-headers
;;                     :website "https://github.com/mooz/auto-complete-c-headers"
;;                     :description "An auto-complete source for C/C++ header files."
;;                     :type github
;;                     :pkgname "mooz/auto-complete-c-headers"
;;                     :depends auto-complete))


(my/custom-set-variables
;'(global-auto-complete-mode t nil (auto-complete))
 '(global-auto-complete-mode t)
 '(ac-auto-start nil)
 '(ac-auto-show-menu t)
 '(ac-delay 0.5)
 '(ac-use-menu-map t)
 '(ac-comphist-file            "~/.emacs.d/auto-complete/history.dat")
 '(ac-dictionary-directories '("~/emacs/auto-complete/mode-dicts"
                               ))
 '(ac-dictionary-files       '("~/emacs/auto-complete/user-dict"
                               ))
 )

(my/custom-set-faces
 '(ac-candidate-face           ((t (:background "LightCyan1"     :foreground "black"))))
 '(ac-selection-face           ((t (:background "aquamarine1"    :foreground "black"))))
 '(ac-yasnippet-candidate-face ((t (:background "lavender blush" :foreground "black"))))
 '(ac-yasnippet-selection-face ((t (:background "PeachPuff2"     :foreground "black"))))
 '(popup-isearch-match         ((t (:inherit match))))

 )

;; In auto-complete.el:
;;
;; ac-define-source abbrev
;; ac-define-source dictionary
;; ac-define-source features
;; ac-define-source filename
;; ac-define-source files-in-current-dir
;; ac-define-source functions
;; ac-define-source symbols
;; ac-define-source variables
;; ac-define-source words-in-all-buffer
;; ac-define-source words-in-buffer
;; ac-define-source words-in-same-mode-buffers
;;
;; In auto-complete-config.el:
;;
;; ac-define-source css-property
;; ac-define-source eclim
;; ac-define-source ghc-mod
;; ac-define-source gtags
;; ac-define-source imenu
;; ac-define-source semantic
;; ac-define-source semantic-raw
;; ac-define-source slime
;; ac-define-source yasnippet

;;}}}

;;=== Utilities ========================================================
;;{{{  Help (apropos, info, etc)

(add-to-list 'el-get-sources
             '(:name apropos-toc
                     :description "XEmacs-ish hyper-apropos for GNUEmacs"
                     :type        http
                     :url         "http://www.cbrunzema.de/download/apropos-toc/apropos-toc.el"
                     :features    (apropos-toc)))
(my/el-get-install "apropos-toc")

(defvar my/apropos-toc-font-lock-keywords
  (list
   '("^\\(Function\\|Variable\\)s:" . font-lock-keyword-face))
  "Additional expressions to highlight in Apropos TOC mode")

(defadvice apropos-toc (after my/customized-apropos-toc activate)
  "Color 'Functions' & 'Variables'; position cursor correctly."
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(my/apropos-toc-font-lock-keywords nil t))
  (goto-char (point-min)) (forward-line (1- 4)))


(my/custom-set-variables
 '(apropos-do-all t)                    ; invert sense of prefix arg
 )

;; This is serach order so we hope that function names are unique
(defconst my/elisp-manuals
  '("cl"
    "ediff"
    "efs"
    "elisp"
    "gnus"
    "lispref"
    "vm"
    "w3"
    ))

;; Not autoloaded in current sources
(autoload 'Info-find-node "info"
  "Go to an Info node specified as separate FILENAME and NODENAME.

\(fn FILENAME NODENAME &optional NO-GOING-BACK)" t)

(autoload 'info-lookup-add-help "info-look" nil t)

(defun my/elisp-function-reference (func)
  "Look up an elisp function in Info's Elisp manual.
This command is designed to be used whether you are already in Info or not."
  (interactive (let ((fn (function-called-at-point))
                     val)
                 (setq val (completing-read
                            (format "Look up Emacs Lisp function%s: "
                                    (if fn
                                        (format " (default %s)" fn)
                                      ""))
                            obarray 'fboundp t))
                 (list (if (equal val "")
                           fn (intern val)))))
  (save-window-excursion
    (info)
    (let ((lst my/elisp-manuals))
      (catch 'found
        (while lst
          (condition-case ()
              (progn
                (Info-find-node (car lst) "Top")
                (Info-index (symbol-name func))
                (throw 'found t))
            (error nil))
          (setq lst (cdr lst))))))
  (pop-to-buffer "*info*"))

(defun my/elisp-find-symbol-definition (name)
  "Jump to the definition of the elisp symbol at point."
  (interactive (list (thing-at-point 'symbol)))
  (cond (name
         (let ((symbol (intern-soft name))
               (search (lambda (fun sym)
                         (let* ((r (save-excursion (funcall fun sym)))
                                (buffer (car r))
                                (point (cdr r)))
                           (cond ((not point)
                                  (error "Found no definition for %s in %s"
                                         name buffer))
                                 (t
                                  (switch-to-buffer buffer)
                                  (goto-char point)
                                  (recenter 1)))))))
           (cond ((fboundp symbol)
                  (elisp-push-point-marker)
                  (funcall search 'find-function-noselect symbol))
                 ((boundp symbol)
                  (elisp-push-point-marker)
                  (funcall search 'find-variable-noselect symbol))
                 (t
                  (message "Symbol not bound: %S" symbol)))))
        (t (message "No symbol at point"))))

(defun my/describe-major-mode-bindings ()
  (interactive)
  (call-interactively 'describe-bindings)
  (with-current-buffer (help-buffer)
    (search-forward "Major Mode Bindings")
    (narrow-to-page)))

(substitute-key-definition
 'describe-bindings 'my/describe-major-mode-bindings help-map)

(my/custom-set-faces
 '(info-header-xref ((t (:foreground "LightSalmon4")))))

;;}}}
;;{{{  Browsing and completion (helm, ido, smex)

;(add-to-list 'el-get-sources 'helm)
;(my/el-get-install "helm")

;(require 'helm)
;(require 'helm-config)

(my/custom-set-variables
 '(helm-command-prefix-key "C-c h")
 '(helm-mode t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-split-window-default-side 'same)
 )

;; Repair helm "angry salad"
(my/custom-set-faces
 '(header-line             ((t (:inherit mode-line-inactive :box nil :weight bold))))
 '(helm-buffer-not-saved   ((t (:foreground "plum1"))))
 '(helm-buffer-process     ((t (:foreground "DarkSeaGreen1"))))
 '(helm-buffer-size        ((t (:foreground "gray66"))))
 '(helm-ff-directory       ((t (:inherit dired-directory))))
 '(helm-ff-executable      ((t (:foreground "DarkSeaGreen1"))))
 '(helm-ff-file            ((t nil)))
 '(helm-ff-invalid-symlink ((t (:inherit dired-warning))))
 '(helm-ff-symlink         ((t (:inherit dired-symlink))))
 '(helm-header             ((t (:background "gray50" :foreground "black"))))
 '(helm-match              ((t (:background "#1c3850"))))
 '(helm-selection          ((t (:background "gray15"))))
 '(helm-source-header      ((t (:inherit default :foreground "RosyBrown4" :underline t :weight bold)))))


;; (my/custom-set-variables
;; '(ido-mode 'both nil (ido))
;; '(ido-save-directory-list-file "~/.emacs.d/ido.last")
;; '(ido-setup-hook 'my/ido-setup)
;; '(ido-minibuffer-setup-hook 'my/ido-minibuffer-setup)
;; )

;; SMEX is built on IDO
;; (add-to-list 'el-get-sources 'smex)
;; (my/el-get-install "smex")

(my/custom-set-variables
 '(smex-save-file "~/.emacs.d/smex.save")
 )

(my/custom-set-variables
 '(completion-ignored-extensions
   '(".a"
     ".aux"
     ".bak"
     ".bbl"
     ".bin"
     ".blg"
     ".class"
     ".cp"
     ".cps"
     ".dvi"
     ".elc"
     ".fas"
     ".fmt"
     ".fn"
     ".fns"
     ".glo"
     ".idx"
     ".ky"
     ".kys"
     ".lib"
     ".ln"
     ".lof"
     ".log"
     ".lot"
     ".map"
     ".o"
     ".obj"
     ".pg"
     ".pgs"
     ".pyc"
     ".toc"
     ".tp"
     ".tps"
     ".vr"
     ".vrs"
     ".x86f"
     "~"
     )))

;; from minibuf-electric-gnuemacs.el
(defun my/ido-electric-slash ()
  (interactive)
  (and (eq ?/ (preceding-char))
       ;; permit `//hostname/path/to/file'
       (not (eq (point) (1+ (minibuffer-prompt-end))))
       ;; permit `http://url/goes/here'
       (not (eq ?: (char-after (- (point) 2))))
       (delete-region (minibuffer-prompt-end) (point)))
  (insert ?/))

(defun my/ido-setup ()
  (define-key ido-completion-map "/" 'my/ido-electric-slash)
  (define-key ido-completion-map "~" 'my/ido-electric-tilde))

(defun my/ido-electric-tilde ()
  (interactive)
  (if (eq ?/ (preceding-char))
      (delete-region (minibuffer-prompt-end) (point)))
  (insert ?~))

(defun my/ido-minibuffer-setup ()
  (setq truncate-lines (default-value 'truncate-lines)))

;;}}}
;;{{{  grep

(my/custom-set-variables
 '(grep-command "grep --color -nH -e ")
 '(grep-find-command
   '("find . -type f -exec grep --color -nH -e  {} +" . 42))
 '(grep-find-template "find <D> <X> -type f <F> -exec grep <C> -nH -e <R> {} +")
 '(grep-highlight-matches nil)
 '(grep-template "grep <X> <C> -nH -e <R> <F>")
 '(grep-use-null-device nil)
 )

;;}}}
;;{{{  Chasing URL's. See:  browse-url.el

;; (if (file-exists-p (concat "/usr/bin/x-www-browser"))
;;     (with-no-warnings
;;       (setq browse-url-browser-function (quote browse-url-generic)
;; 	    browse-url-generic-program "/usr/bin/x-www-browser"))
;;   )

;;}}}
;;{{{  Spell checking

(my/custom-set-variables
 '(ispell-personal-dictionary "~/emacs/aspell.en.pws")
 )

;;}}}
;;{{{  Directoris (ls, dired)

(my/custom-set-variables
 '(dired-listing-switches "-agGh --group-directories-first")
 '(list-directory-verbose-switches "-l --group-directories-first")
 '(ls-lisp-dirs-first t)
 '(ls-lisp-ignore-case t)
 )

;;}}}
;;{{{  Recent files

(my/custom-set-variables
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/recentf")
 )

;;}}}
;;{{{  ilocate-library

;; My changes from https://github.com/emacsmirror/ilocate-library.git
;;  - (require 'cl)
;;  - support gzipped files (adjust regexps and doc strings)
;;  - visit files in view mode
;;
;; ~/emacs/patched/ilocate-library--support-.gz-visit-files-in-view-mode.patch

(add-to-list 'el-get-sources
             '(:name ilocate-library
                     :description "Interactive locate-library (or source) with completion"
                     :type        http
                     :url         "file://localhost/home/jyates/emacs/patched/ilocate-library.el"
                     :features    (ilocate-library)))
(my/el-get-install "ilocate-library")

;;}}}
;;{{{  Update copyright and timestamp before saving files

(my/custom-set-variables
 '(before-save-hook '(copyright-update
                      time-stamp
;                      my/modified-line-cleanup-after-leaving
                      )))

;;}}}
;;{{{  Comint

(my/custom-set-variables
 '(tramp-default-method "ssh")          ; uses ControlMaster
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
;; '(comint-scroll-to-bottom-on-output nil) ; [DEF] always add output at the bottom
;; '(comint-scroll-show-maximum-output t) ; [DEF] scroll to show max output
;; '(comint-completion-autolist t)      ; show completion list when ambiguous
;; '(comint-eol-on-send t)              ; [DEF] see following defadvice
 '(comint-input-ignoredups t)           ; no duplicates in command history
;; '(comint-completion-addsuffix t)     ; [DEF] completion adds space/slash
;; '(comint-buffer-maximum-size 20000)  ; max length of the buffer in lines
;; '(comint-prompt-read-only nil)       ; [DEF] breaks shell-command if t
 '(comint-get-old-input (lambda () "") t); submit as process input when <RET> typed
                                        ; line above the current prompt
 '(comint-input-ring-size 5000)         ; max shell history size
;; '(protect-buffer-bury-p nil)           ; revive if we setup protbuf
)

(defadvice comint-send-input (around my/go-to-end-of-multiline activate)
  "To capture multiline input jump to end of buffer on [enter].
(This has no effect if `comint-eol-on-send' is nil.)"
  (cl-flet ((end-of-line () (goto-char (point-max))))
    ad-do-it))

(defadvice comint-previous-matching-input (around my/quiet-comint-history-isearch activate)
  "Suppress annoying 'History item : NNN' messages from comint history isearch."
  (let ((old-message (symbol-function 'message)))
    (unwind-protect
        (progn (fset 'message 'ignore) ad-do-it)
      (fset 'message old-message))))

;; not sure why, but comint needs to be reloaded from the source (*not*
;; compiled) elisp to make the above advise stick.
;; (load "comint.el.gz")

(defun my/comint-output-read-only (text)
  "Add to comint-output-filter-functions to make output read-only."
  (let ((inhibit-read-only t)
        (output-end (process-mark (get-buffer-process (current-buffer)))))
    (put-text-property comint-last-output-start output-end 'read-only t)))
(add-hook 'comint-output-filter-functions 'my/comint-output-read-only)

(defun my/unblock-comint ()
  "Sometimes comint's input area goes read only.  Fix it."
  (interactive)
  (let ((inhibit-read-only t))
    (comint-send-input)))



(defvar my/comint-based-modes '(shell-mode gud-mode))

(defun my/enter-again-if-enter ()
  "Make the return key select the current item in minibuf and shell history isearch.
An alternate approach would be after-advice on isearch-other-meta-char."
  (when (and (not isearch-mode-end-hook-quit)
             (equal (this-command-keys-vector) [13])) ; == return
    (cond ((active-minibuffer-window) (minibuffer-complete-and-exit))
          ((memq major-mode my/comint-based-modes) (comint-send-input)))))

(my/custom-set-variables
 '(isearch-mode-end-hook 'my/enter-again-if-enter t)
 )

;;}}}
;;{{{  Shells (built upon comint-mode)

;; Recognize .bsh and .dsh as shell scripts
(add-to-list 'auto-mode-alist '("\\.[bd]sh\\'" . sh-mode))

(defun my/named-shell (BUFFER)
  "Create or switch to a running shell process in BUFFER."
  (interactive "BShell name: ")
  (shell BUFFER))

(setenv "TERM eterm-color")
(setenv "PAGER" "cat")  ; Use the transcript in lieu of a classic pager

(my/custom-set-variables
 '(explicit-shell-file-name "/bin/bash")
 '(shell-mode-hook 'ansi-color-for-comint-mode-on)
 )

;; The dirtrack package can be more reliable than shell-dirtrack-mode.
;; This is because it depends on the working directory being advertised
;; in the shell prompt.
;;
;; (defun my-dirtrack-mode ()
;;   "Add to shell-mode-hook to use dirtrack mode in my shell buffers."
;;   (shell-dirtrack-mode 0)
;;   (set-variable 'dirtrack-list '(":\\([^ :>]*\\)> *$" 1 nil))
;;   (dirtrack-mode 1))
;; (add-hook 'shell-mode-hook 'my-dirtrack-mode)

;; Consider for when shells dump largish amount into the transcript.
;;
;; (defun set-scroll-conservatively ()
;;   "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
;;   (set (make-local-variable 'scroll-conservatively) 10))
;; (add-hook 'shell-mode-hook 'set-scroll-conservatively)


;; make it harder to kill my shell buffers
;; (require 'protbuf)
;; (add-hook 'shell-mode-hook 'protect-process-buffer-from-kill-mode)

;; (defun make-comint-directory-tracking-work-remotely ()
;;   "Add this to comint-mode-hook to make directory tracking work
;; while sshed into a remote host, e.g. for remote shell buffers
;; started in tramp. (This is a bug fix backported from Emacs 24:
;; http://comments.gmane.org/gmane.emacs.bugs/39082"
;;   (set (make-local-variable 'comint-file-name-prefix)
;;        (or (file-remote-p default-directory) "")))
;; (add-hook 'comint-mode-hook 'make-comint-directory-tracking-work-remotely)

;;}}}

;;=== Minor modes ======================================================
;;{{{  folding

(add-to-list 'el-get-sources 'folding)
(my/el-get-install "folding")

(my/custom-set-variables
 '(folding-mode-prefix-key ",")       ; also changed for hideshow
 '(folding-advice-instantiate nil)      ; not advising M-g g
 '(folding-goto-key "\M-gf")            ; Restore M-g's prefix behavior
 )

;;}}}
;;{{{  hideshow

;; My version handles block comments in C++ better.
;; ~/emacs/patched/hideshow--better-handling-of-c++-comments.patch

;; (add-to-list 'el-get-sources
;; 	     '(:name hideshow
;; 		     :description "Minor mode cmds to selectively display code/comment blocks"
;; 		     :type        http
;; 		     :url         "file://localhost/home/jyates/emacs/patched/hideshow.el"
;; 		     :features    (hideshow)))
;; (my/el-get-install "hideshow")
;; (require 'hideshow)

;; Display the size of a collapsed function body
(defun my/display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let ((str (format " %d " (count-lines (overlay-start ov)
                                           (overlay-end ov)))))
      (put-text-property 0 (length str) 'face 'glyphless-char str)
      (overlay-put ov 'display str))))

(eval-after-load "hideshow"
  '(setq hs-set-up-overlay 'my/display-code-line-counts))

;; My edits to correct the order of save-excursion save-restriction sequence
;; have been incorporated into the version at emacswiki.org.
(add-to-list 'el-get-sources
             '(:name hideshowvis
                     :description "Add fringe markers for hide/show foldable regions."
                     :type        emacswiki
                     :url         "http://www.emacswiki.org/emacs/download/hideshowvis.el"
                     :features    (hideshowvis)))
(my/el-get-install "hideshowvis")

;; (defun my/fringe-click-hs (event)
;;   (interactive "e")
;;   (mouse-set-point event)
;;   (end-of-line)
;;   (if (save-excursion
;;         (end-of-line 1)
;;         (or (hs-already-hidden-p)
;;             (progn
;;               (forward-char 1)
;;               (hs-already-hidden-p))))
;;       (hs-show-block)
;;     (hs-hide-block)
;;     (beginning-of-line)))

;; (defvar my/fringe-click-hs-mode-map
;;   (let ((keymap (make-sparse-keymap)))
;;     (define-key keymap [left-fringe mouse-1]
;;       'my/fringe-click-hs)
;;     keymap)
;;   "Keymap for interpretting mouse-1 on the left-fringe")

(defvar hs1-regexp
  "\\(\n[[:blank:]]*///\\|///<\\).*$"
  "List of regular expressions of blocks to be hidden.")

(define-minor-mode hs1-mode
  "Hide/show predefined blocks."
  :lighter " hs1"
  (if hs1-mode
      (let (ol)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp hs1-regexp nil 'noErr)
        (when (eq (syntax-ppss-context (syntax-ppss (match-end 1))) 'comment)
          (setq ol (make-overlay (match-beginning 0) (match-end 0)))
          (overlay-put ol 'hs1 t)
          (overlay-put ol 'invisible t)
          ))))
    (remove-overlays (point-min) (point-max) 'hs1 t)
    ))

(add-hook 'c++-mode-hook '(lambda () (local-set-key (kbd "M-s M-s") 'hs1-mode)))

;;}}}
;;{{{  auto-fill and filladapt

;; Redirect to a patched version more compatible with cc-mode
(add-to-list 'el-get-sources
             '(:name filladapt
                     :description "Adaptively set fill-prefix and overload filling functions"
                     :type        http
                     :url         "http://cc-mode.sourceforge.net/filladapt.el"
;;                   :url         "file://localhost/home/jyates/emacs/filladapt.el"
                     :features    (filladapt)))
(my/el-get-install "filladapt")


;; As distributed filladapt.el contains no autoload cookies
(autoload 'turn-on-filladapt-mode "filladapt"
  "Unconditionally turn on Filladapt mode in the current buffer.

\(fn)" t)

(eval-after-load "filladapt" '(diminish 'filladapt-mode "f"))


(defun my/turn-on-filling ()
  (text-mode-hook-identify)  ; mark buffer for toggle-text-mode-auto-fill
  (turn-on-auto-fill)
  ;; Typically filladapt's "FA" indicates that filling is active
  (diminish 'auto-fill-function)
  (require 'filladapt)
  (turn-on-filladapt-mode)
  )

(defun my/text-mode ()
  (my/turn-on-filling)
  (setq fill-column 80)
  )

(my/custom-set-variables
 '(text-mode-hook '(my/text-mode))
 )

;;}}}
;;{{{  visual-lines-mode

(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; longlines has been deprecated
;;
;; (eval-after-load "longlines"
;;   '(progn
;;      (defvar longlines-mode-was-active nil)
;;      (make-variable-buffer-local 'longlines-mode-was-active)
;;
;;      (defun longlines-suspend ()
;;        (if longlines-mode
;;            (progn
;;              (setq longlines-mode-was-active t)
;;              (longlines-mode 0))))
;;
;;      (defun longlines-restore ()
;;        (if longlines-mode-was-active
;;            (progn
;;              (setq longlines-mode-was-active nil)
;;              (longlines-mode 1))))
;;
;;      ;; longlines doesn't play well with ediff, so suspend it during diffs
;;      (defadvice ediff-make-temp-file (before my/make-temp-file-suspend-ll
;;                                              activate compile preactivate)
;;        "Suspend longlines when running ediff."
;;        (with-current-buffer (ad-get-arg 0)
;;          (longlines-suspend)))
;;
;;      (add-hook 'ediff-cleanup-hook
;;                (function (lambda ()
;;                            (dolist (tmp-buf (list ediff-buffer-A
;;                                                   ediff-buffer-B
;;                                                   ediff-buffer-C))
;;                              (if (buffer-live-p tmp-buf)
;;                                  (with-current-buffer tmp-buf
;;                                    (longlines-restore)))))))))

;;}}}

;;=== Major modes ======================================================
;;{{{  Org

(my/custom-set-variables
 '(org-hide-leading-stars t)
 '(org-default-notes-file "~/org/capture.org")
 '(org-modules
   '(org-docview                ; Links to doc-view buffers
     org-info                   ; Links to Info nodes
     org-habit                  ; Track your consistency with habits
     org-inlinetask             ; Tasks independent of outline hierarchy
     org-protocol               ; Intercept calls from emacsclient
     org-mouse                  ; Additional mouse support
     ))
 '(org-mobile-directory "~/Dropbox/MobileOrg")
 '(org-mobile-files '(org-agenda-files "~/org"))
 '(org-support-shift-select t)
 )

;; Interesting org-mode completion, integrate with auto-complete?
;; http://www.emacswiki.org/emacs/download/completion-ui-more-sources.el

;;}}}

;;=== Programming ======================================================
;;{{{  Sandbox file access

(add-to-list 'load-path "~/emacs/nz")

(autoload 'sbf-helm-find-file "sbf"
  "Return a HELM arglist to perform find-file in a sandbox."
  t)

(autoload 'sbf-helm-find-file-read-only "sbf"
  "Return a HELM arglist to perform find-file-read-only in a sandbox."
  t)

(autoload 'sbf-helm-view-file "sbf"
  "Return a HELM arglist to perform view-file in a sandbox."
  t)

(autoload 'sbf-find-file "sbf"
  "Find a file in a sandbox using a completion framework."
  t)

;;}}}
;;{{{  Find file in project

(add-to-list 'el-get-sources 'find-file-in-project)
(my/el-get-install "find-file-in-project")

;;}}}
;;{{{  Find a "tag" in a project

(add-to-list 'el-get-sources
             '(:name ggtags
                     :description "Use GNU Global in Emacs."
                     :type github
                     :pkgname "jsyjr/ggtags"))
(my/el-get-install "ggtags")

(my/custom-set-variables
 '(ggtags-auto-jump-to-match nil)
 '(ggtags-sort-by-nearness t)
 '(ggtags-use-sqlite3 t)
 '(tags-revert-without-query t)
 )

(require 'ggtags)
(add-hook 'buffer-list-update-hook (lambda () (ggtags-mode 1)))

;; For idutils:
;; /hub/share/sbtools/external-apps/idutils/idutils-4.6-sbmod1/install/share/id-lang.map

;; (add-to-list 'el-get-sources
;;              '(:name vtags
;;                      :description "Edward Bishop's fork of emacs' etags"
;;                      :type        http
;;                      :url         "file://localhost/home/jyates/emacs/vtags/vtags.el"
;;                      :features    (vtags)))
;; (my/el-get-install "vtags")

;; (add-to-list 'el-get-sources
;;              '(:name rtags
;;                      :description "A C/C++ client/server indexer based on clang."
;;                      :type        github
;;                      :pkgname     "Andersbakken/rtags"
;;                      :url         "https://github.com/Andersbakken/rtags"
;;                      :features    (rtags)))
;; (my/el-get-install "rtags")
;;
;; (require 'rtags)
;; (rtags-enable-standard-keybindings)
;;
;; (call-process-shell-command "/home/jyates/bin/start-rdm" nil nil)
;;
;; (defun my/quit-rdm ()
;;   (call-process-shell-command "/home/jyates/bin/quit-rdm" nil nil))
;; (add-hook 'kill-emacs-hook 'my/quit-rdm)

;;}}}
;;{{{  Compilation and next exrror

(my/custom-set-variables
;; '(compile-command "/usr/bin/make -k")
 '(compilation-scroll-output 'first-error)   ; follow compilation output
;; '(compilation-skip-threshold 2); next-errormpil should only stop at errors
 '(next-error-hook 'compile-goto-error)
 '(next-error-recenter '(4))
 )

(my/custom-set-faces
 '(compilation-column-number ((t (:inherit file-name-shadow           ))))
 '(compilation-info          ((t (:inherit font-lock-comment-face     ))))
 '(compilation-line-number   ((t (:inherit font-lock-preprocessor-face))))
 )

;; Fix next error so that it does not visit every file in the include stack
;; leading up the the error site.  Lifted directly from stackoverflow
;; how-can-i-skip-in-file-included-from-in-emacs-c-compilation-mode
(eval-after-load "compile"
  '(setf (nth 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0))

(eval-after-load "yasnippet"
  '(add-hook 'after-save-hook 'my/yasnippet-reload-on-save))

;;}}}
;;{{{  Emacs' elisp (with auto-compile on save)

(add-to-list 'auto-mode-alist '("custom-file" . emacs-lisp-mode))

;; (my/custom-set-variables
;;  '(eldoc-idle-delay 1.5)
;;  )

;(global-eldoc-mode -1)

(defun my/byte-compile-saved-elisp-buffer ()
  "Byte compile an elisp buffer anytime it is saved."
  (if (and (eq major-mode 'emacs-lisp-mode)
           (not (string-prefix-p "phw" (buffer-name))))
      (byte-compile-file (buffer-file-name))))

(add-hook 'after-save-hook 'my/byte-compile-saved-elisp-buffer)

;;}}}
;;{{{  C/C++ mode

;; After applying patch, make sure you recompile first cc-langs.el, then
;; cc-engine.el and cc-mode.el.  (You can do this by M-x byte-compile-file
;; <CR;; <file-name;; in Emacs).  These files are in a directory something
;; like /usr/local/src/emacs/lisp/progmodes.  You might have to reinstall
;; the newly compiled files to your installed directory.  Reload CC Mode
;; (e.g. by restarting Emacs) in order to test the patch.  Please confirm to
;; me that it solves the problem (or else tell me what it doesn't solve).

(eval-when-compile (require 'cc-mode))

;; Treat .gt, .h and .imp files as C++ source
(setq auto-mode-alist (append '(("\\.gt\\'" . c++-mode)
                                ("\\.h\\'" . c++-mode)
                                ("\\.imp\\'" . c++-mode))
      auto-mode-alist))


(my/custom-set-variables
 '(c-tab-always-indent nil)
 ;; '(c-insert-tab-function 'my/c-insert-tab-function)
 )

(defun my/c-public-private-boundary (langelem)
  "Line up line of slashes preceding 'private:' at the left edge."
  (save-excursion
    (back-to-indentation)
    (if (and (looking-at "//////////*$")
             (forward-line 1)
             (looking-at "private:"))
        [0]
      nil)))

;; Closely parallels cc-align.el's c-lineup-arglist-intro-after-paren
(defun my/c-lineup-vertical-comma-list (langelem)
  "Line up a line starting with a comma beneath open paren of
the surrounding paren or brace block."
  (save-excursion
    (beginning-of-line 1)
    (backward-up-list 1)
    (skip-chars-forward " \t" (c-point 'eol))
    (vector (current-column))))

(defun my/c-lineup-topmost-intro-cont (langelem)
  "Indent comment or arglist open parenthesis beneath topmost-intro.
Comments documenting scopes (class, struct, union, namespace) receive
no indentation as these are typically fair size chunks of text.  By
contrast comments documenting functions get ident"
  (save-excursion
    (beginning-of-line 1)
    (skip-chars-forward " \t" (c-point 'eol))
    (cond
     ((looking-at "(" ) '+)
     ((looking-at "//" )
      (forward-line -1)
      (skip-chars-forward " \t" (c-point 'eol))
      (if (looking-at "(class\\|struct\\|union\\|namespace)[^[:alnum:]_]" ) 0 '+))
     (t 0))))

;; Closely parallels cc-align.el's c-lineup-arglist-operators
(defun my/c-lineup-arglist-&&-or-|| (langelem)
  "Line up lines starting with && or || under the open paren.
Return nil on lines that start with neither && nor ||, to leave
those cases to other line-up functions.  Example:

if ( x < 10
  || at_limit (x,       <- my/c-lineup-arglist-&&-or-||
               list)    <- my/c-lineup-arglist-&&-or-|| returns nil
   )

Since this function doesn't do anything for lines without && or
|| you typically want to use it together with some other line-up
settings, e.g. as follows \(the arglist-close setting is just a
suggestion to get a consistent style):

\(c-set-offset 'arglist-cont '(c-lineup-arglist-operators 0))
\(c-set-offset 'arglist-cont-nonempty '(c-lineup-arglist-operators
                                        c-lineup-arglist))
\(c-set-offset 'arglist-close '(c-lineup-arglist-close-under-paren))

Works with: arglist-cont, arglist-cont-nonempty."
  (save-excursion
    (back-to-indentation)
    (when (looking-at "\\(&&\\|||\\)")
      (let ((ret (c-lineup-arglist-close-under-paren langelem)))
        (message "my/c-lineup-arglist-&&-or-||: ret = %s" ret)
        (if (vectorp ret) (vector (1- (aref ret 0))) nil)))))

(eval-after-load "cc-mode"
  `(progn
     (setq c-style-variables-are-local-p nil) ; when tweaking sytles

     (c-add-style
      "jsy"
      '((c-echo-syntactic-information-p . t)
        (c-basic-offset . 2)
        (c-comment-only-line-offset 0 . 0)
        ;(c-auto-align-backslashes nil)
        (c-cleanup-list
         '(brace-else-brace
           brace-elseif-brace
           brace-catch-brace
           empty-defun-braces
           defun-close-semi
           list-close-comma
           scope-operator))
        (c-doc-comment-style 'set-from-style)
        (c-offsets-alist
         (access-label . -)
         (annotation-top-cont . 0)
         (annotation-var-cont . +)
         (arglist-close . my/c-lineup-vertical-comma-list)
         (arglist-cont c-lineup-gcc-asm-reg 0)
         (arglist-cont-nonempty my/c-lineup-arglist-&&-or-|| c-lineup-arglist-close-under-paren)
         (arglist-intro . c-lineup-arglist-intro-after-paren)
         (block-close . 0)
         (block-open . 0)
         (brace-entry-open . 0)
         (brace-list-close . 0)
         (brace-list-entry my/c-lineup-vertical-comma-list)
         (brace-list-intro . +)
         (brace-list-open . +)
         (c . c-lineup-C-comments)
         (case-label . 0)
         (catch-clause . 0)
         (class-close . 0)
         (class-open . 0)
         (comment-intro . c-lineup-comment)
         (composition-close . 0)
         (composition-open . 0)
         (cpp-define-intro c-lineup-cpp-define +)
         (cpp-macro . -1000)
         (cpp-macro-cont . +)
         (defun-block-intro . +)
         (defun-close . 0)
         (defun-open . 0)
         (do-while-closure . 0)
         (else-clause . 0)
         (extern-lang-close . 0)
         (extern-lang-open . 0)
         (friend . -)
         (func-decl-cont . +)
         (inclass . +)
         (incomposition . +)
         (inexpr-class . +)
         (inexpr-statement . +)
         (inextern-lang . +)
         (inher-cont . c-lineup-multi-inher)
         (inher-intro . +)
         (inlambda . c-lineup-inexpr-block)
         (inline-close . 0)
         (inline-open . 0)
         (inmodule . +)
         (innamespace . [0])
         (knr-argdecl . 0)
         (knr-argdecl-intro . 5)
         (label . 0)
         (lambda-intro-cont . +)
         (member-init-cont . c-lineup-multi-inher)
         (member-init-intro . 0)
         (module-close . 0)
         (module-open . 0)
         (namespace-close . 0)
         (namespace-open . 0)
         (objc-method-args-cont . c-lineup-ObjC-method-args)
         (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
         (objc-method-intro . [0])
         (statement . 0)
         (statement-block-intro . +)
         (statement-case-intro . +)
         (statement-case-open . +)
         (statement-cont . 7)
         (stream-op . c-lineup-streamop)
         (string . -1000)
         (substatement . +)
         (substatement-label . 0)
         (substatement-open . 0)
         (template-args-cont c-lineup-template-args +)
         (topmost-intro my/c-public-private-boundary 0)
         (topmost-intro-cont . my/c-lineup-topmost-intro-cont))))

     (c-add-style
      "mathworks"
      '((c-echo-syntactic-information-p . t)
        (c-basic-offset . 4)
        (c-comment-only-line-offset 0 . 0)
        ;(c-auto-align-backslashes nil)
        (c-cleanup-list
         '(brace-else-brace
           brace-elseif-brace
           brace-catch-brace
           empty-defun-braces
           defun-close-semi
           list-close-comma
           scope-operator))
        (c-doc-comment-style 'set-from-style)
        (c-offsets-alist
         (access-label . *)
         (annotation-top-cont . 0)
         (annotation-var-cont . +)
         (arglist-close . my/c-lineup-vertical-comma-list)
         (arglist-cont c-lineup-gcc-asm-reg 0)
         (arglist-cont-nonempty my/c-lineup-arglist-&&-or-|| c-lineup-arglist-close-under-paren)
         (arglist-intro . c-lineup-arglist-intro-after-paren)
         (block-close . 0)
         (block-open . 0)
         (brace-entry-open . 0)
         (brace-list-close . 0)
         (brace-list-entry my/c-lineup-vertical-comma-list)
         (brace-list-intro . +)
         (brace-list-open . +)
         (c . c-lineup-C-comments)
         (case-label . *)
         (catch-clause . 0)
         (class-close . 0)
         (class-open . 0)
         (comment-intro . c-lineup-comment)
         (composition-close . 0)
         (composition-open . 0)
         (cpp-define-intro c-lineup-cpp-define +)
         (cpp-macro . -1000)
         (cpp-macro-cont . +)
         (defun-block-intro . +)
         (defun-close . 0)
         (defun-open . 0)
         (do-while-closure . 0)
         (else-clause . 0)
         (extern-lang-close . 0)
         (extern-lang-open . 0)
         (friend . -)
         (func-decl-cont . +)
         (inclass . +)
         (incomposition . +)
         (inexpr-class . +)
         (inexpr-statement . +)
         (inextern-lang . +)
         (inher-cont . c-lineup-multi-inher)
         (inher-intro . +)
         (inlambda . c-lineup-inexpr-block)
         (inline-close . 0)
         (inline-open . 0)
         (inmodule . +)
         (innamespace . [0])
         (knr-argdecl . 0)
         (knr-argdecl-intro . 5)
         (label . 0)
         (lambda-intro-cont . +)
         (member-init-cont . c-lineup-multi-inher)
         (member-init-intro . 0)
         (module-close . 0)
         (module-open . 0)
         (namespace-close . 0)
         (namespace-open . 0)
         (objc-method-args-cont . c-lineup-ObjC-method-args)
         (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
         (objc-method-intro . [0])
         (statement . 0)
         (statement-block-intro . +)
         (statement-case-intro . *)
         (statement-case-open . *)
         (statement-cont . 7)
         (stream-op . c-lineup-streamop)
         (string . -1000)
         (substatement . +)
         (substatement-label . 0)
         (substatement-open . 0)
         (template-args-cont c-lineup-template-args +)
         (topmost-intro my/c-public-private-boundary 0)
         (topmost-intro-cont . my/c-lineup-topmost-intro-cont))))
     ))

(defun my/c-mode-common-hook ()
  ""
  ;; Semantic does a better job supporting which-func in mode-line
  ;; (require 'semantic/imenu)
  ;; (setq imenu-create-index-function 'semantic-create-imenu-index)

  ;; cc-mode uses abbrev-mode to implement electric keywords
  (diminish 'abbrev-mode)

  (hs-minor-mode)
  (hideshowvis-enable)

  (setq tab-width 8)
  (setq comment-column 40)

  (c-set-style (if (locate-dominating-file "." "matlab") "mathworks" "jsy"))

  ;; Doxygen end of line comments are introduced by "///<"
  (add-to-list 'c-comment-prefix-regexp '(other . "//+<?\\|\\**"))

  (c-setup-paragraph-variables)
  (my/turn-on-filling)
  (setq fill-column 78)
  (c-setup-filladapt)  ; not really setup, more like post-configure

  (c-toggle-auto-hungry-state -1)

  ;;(define-key c-mode-base-map "\C-m" 'newline-and-indent)
  ;;(define-key c-mode-base-map ")" 'jsy-c-electric-close-paren)
  )

(eval-after-load "cc-vars"
  (progn
    ;; Cannot use the customization interface to establish this hook
    ;; function because yasnippet clobbers it.
    (add-hook 'c-mode-common-hook 'my/c-mode-common-hook)

    (info-lookup-add-help
     :mode 'c-mode
     :regexp "[^][()'\" \t\n]+"
     :ignore-case t
     :doc-spec '(("(libc)Symbol Index" nil nil nil)))
    ))

;;}}}
;;{{{  Makefile mode

(setq auto-mode-alist
      (append
       '(
	 ("\\.mk_.+\\'"              . makefile-mode) ;; \' is end of string
	 ("[mM]akefile[._][^/]+\\'"  . makefile-mode)
	 ("\\.tmf\\'"                . makefile-mode)
	 ("\\.tmf_[^/]+\\'"          . makefile-mode)
	 ("\\.gnu\\'"                . makefile-mode)
	 ("\\.gnu_[^/]+\\'"          . makefile-mode)
	 ("\\.mak\\'"                . makefile-mode)
	 ("\\.mak_[^/]+\\'"          . makefile-mode)
	 ("\\.d\\'"                  . makefile-mode)
	 ("\\.d_[^/]+\\'"            . makefile-mode)
	 )
       auto-mode-alist))

;; Makefiles require tabs!
(add-hook 'makefile-mode-hook ((lambda () (setq indent-tabs-mode t))))

;;}}}
;;{{{  Matlab mode

(add-to-list 'el-get-sources 'matlab-mode)
(my/el-get-install "matlab-mode")

;;}}}
;;{{{  Other Mathworks stuff

(when (file-exists-p "/hub/share/sbtools/emacs_setup.el")
  (add-to-list 'load-path "/hub/share/sbtools/apps/emacs-add-ons/src/sb-tools" t)
  (require 'sbtools-locations)

  (require 'mathworks-maps)
  ;; C-x C-q         mathworks-toggle-read-only
  ;;
  ;; C-c m C-f       mathworks-sbfi
  ;; C-c m C-g       mathworks-view-gecko
  ;; C-c m SPC       mathworks-do-next-thing
  ;; C-c m !         mathworks-ediff-versions
  ;; C-c m &         mathworks-ediff-sync-src
  ;; C-c m *         mathworks-ediff
  ;; C-c m ,         gtags-find-symbol
  ;; C-c m -         w80
  ;; C-c m .         gtags-find-tag
  ;; C-c m 1         w100
  ;; C-c m 2         w200
  ;; C-c m =         mathworks-diff
  ;; C-c m @         mathworks-ediff-subsystem-version
  ;; C-c m B         mathworks-sbbackup
  ;; C-c m D         mathworks-sb-debug-many-windows
  ;; C-c m E         mathworks-sbedits
  ;; C-c m F         mathworks-sblocate-gendb
  ;; C-c m G         mathworks-gen-gtags
  ;; C-c m I         mathworks-mkid
  ;; C-c m L         mathworks-mlog
  ;; C-c m Q         mathworks-local-edit
  ;; C-c m R         mathworks-sb-matlab
  ;; C-c m S         mathworks-mstatus
  ;; C-c m T         mathworks-sb-debug-ut-many-windows
  ;; C-c m U         mathworks-revert-to-version
  ;; C-c m V         mathworks-submit-visit-submit-file
  ;; C-c m ^         mathworks-ediff-latest-pass
  ;; C-c m `         mathworks-ediff-version
  ;; C-c m c         mathworks-compile-matlab
  ;; C-c m d         mathworks-sb-debug
  ;; C-c m e         mathworks-medit
  ;; C-c m f         mathworks-sblocate
  ;; C-c m g         mathworks-view-bearded-dragon-geck
  ;; C-c m h         browse-url-at-point
  ;; C-c m i         mathworks-gid
  ;; C-c m j         gtags-pop-stack
  ;; C-c m l         mathworks-sblocate-grep
  ;; C-c m m         mathworks-man-page
  ;; C-c m n         mathworks-toggle-line-num
  ;; C-c m p         sbperldb
  ;; C-c m q         mathworks-batq
  ;; C-c m r         Prefix Command
  ;; C-c m s         Prefix Command
  ;; C-c m t         mathworks-sb-debug-ut
  ;; C-c m u         mathworks-revert-to-ancestor
  ;; C-c m v         mathworks-submit-add-entry
  ;; C-c m x         mathworks-sbext
  ;; C-c m |         w160
  ;; C-c m ~         mathworks-fetch-version
  ;;
  ;; C-c m s b       mathworks-sbscanlog
  ;; C-c m s d       mathworks-sbscanlog-dir
  ;; C-c m s f       mathworks-sbscanlog-fonly
  ;;
  ;; C-c m r b       mathworks-sbcheck-file
  ;; C-c m r c       mathworks-sbcc-file
  ;; C-c m r l       mathworks-sbcc-lint-file
  ;; C-c m r s       mathworks-sbcheck-submit-file
  ;; C-c m r t       mathworks-sbspell-file-external
  ;; C-c m r v       mathworks-sbcc-view-lint-msg
  ;;
  ;; C-c M *         ediff
  ;; C-c M =         diff
  ;; C-c M @@@       getHaveVersion
  ;; C-c M @@@       my-changes
  ;; C-c M @@@       my-pending-changes
  ;; C-c M a         add
  ;; C-c M c         change
  ;; C-c M C         client
  ;; C-c M \C-c      wclients
  ;; C-c M e         edit
  ;; C-c M f         files
  ;; C-c M h         have
  ;; C-c M H         help
  ;; C-c M i         info
  ;; C-c M l         log
  ;; C-c M o         opened
  ;; C-c M v         submit
  ;; C-c M U         revert
  ;;
  ;; C-x v =         mathworks-vc-diff
  ;; C-x v g         mathworks-vc-annotate
  ;; C-x v l         mathworks-vc-print-log
  ;; C-x v u         mathworks-vc-revert-buffer
  ;; C-x v v         mathworks-vc-next-action
  ;; C-x v ~         mathworks-vc-version-other-window
  (define-key mathworks-prefix-map "D" 'mathworks-sb-debug-many-windows)
  (define-key mathworks-prefix-map "T" 'mathworks-sb-debug-ut-many-windows)

  (defun my/clean-up-gud-buffers (orig-fun &rest args)
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf)))
        (when (>= (length name) 5)
          (let ((prefix (substring name 0 5)))
            (if (or (string= prefix "*gud*")
                    (string= prefix "*gud-"))
                (kill-buffer buf))))))
    (apply orig-fun args))

  (advice-add 'mathworks-sb-debug                 :around #'my/clean-up-gud-buffers)
  (advice-add 'mathworks-sb-debug-many-windows    :around #'my/clean-up-gud-buffers)
  (advice-add 'mathworks-sb-debug-ut              :around #'my/clean-up-gud-buffers)
  (advice-add 'mathworks-sb-debug-ut-many-windows :around #'my/clean-up-gud-buffers)

  (setq locate-dominating-stop-dir-regexp
	(concat
	 "\\`" ;; start of string
         "\\(?:"
         "/mathworks"                                                    "\\|"
         ;; Handle /mathworks/AH, etc.
         "/mathworks/[A-Z]+"                                             "\\|"
         ;; /mathworks/home, /mathworks/hub, /mathworks/public, etc.
         "/mathworks/[^/]+"                                              "\\|"
         "/mathworks/[A-Z]+/[^/]+"                                       "\\|"
         ;; /mathworks/devel/{sandbox,jobarchive}, etc.
         "/mathworks/devel/[^/]+"                                        "\\|"
         "/mathworks/[A-Z]+/devel/[^/]+"                                 "\\|"
         ;; /mathworks/devel/bat/Aslrtw, etc.
         "/mathworks/devel/bat/[^/]+"                                    "\\|"
         "/mathworks/[A-Z]+/devel/bat/[^/]+"                             "\\|"
         ;; /mathworks/AH/devel/jobarchive, etc.
         "/mathworks/devel/jobarchive"                                   "\\|"
         "/mathworks/[A-Z]+/devel/jobarchive"                            "\\|"
         ;; /mahtworks/hub/{blah}
         "/mathworks/hub/[^/]+"                                          "\\|"
         "/mathworks/[A-Z]+/hub/[^/]+"                                   "\\|"
         ;; /mathworks/hub/scratch
         "/mathworks/hub/scratch"                                        "\\|"
         "/mathworks/[A-Z]+/hub/scratch"                                 "\\|"
         ;; /mathworks/AH/hub/site-local, etc.
         "/mathworks/hub/site-local"                                     "\\|"
         "/mathworks/[A-Z]+/hub/site-local"                              "\\|"
         ;; symlinks
	 "/sandbox"   "\\|"              ;; symlink to /mathworks/devel/sandbox
	 "/home"      "\\|"              ;; symlink to /mathworks/home
	 "/hub"       "\\|"              ;; symlink to /mathworks/hub
	 "/public"    "\\|"              ;; symlink to /mathworks/public
	 "/src"       "\\|"              ;; symlink to /mathworks/devel/src
         "/scratch"   "\\|"              ;; symlink to /mathworks/hub/scratch
	 "\\)"
	 "/"   ;; end of directory, see vc-find-root
	 "\\'" ;; end of string
	 ))

  (defvar skip-sbtools-matlab-mode-setup)
  (when (or (not (boundp 'skip-sbtools-matlab-mode-setup))
            (not skip-sbtools-matlab-mode-setup))
    (add-to-list 'load-path "/hub/share/sbtools/apps/emacs-add-ons/src/matlab-emacs" t)
    (autoload 'matlab-mode "matlab" "MATLAB Editing Mode" t)
    (autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t))

  (add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode)) ;; \' is end of string
  (add-to-list 'auto-mode-alist '("\\.m_[^/]+\\'" . matlab-mode))

  (with-no-warnings
    (setq matlab-shell-command "sb"
          matlab-auto-fill nil
          matlab-fill-code nil
          matlab-indent-function-body 'MathWorks-Standard
          ))

  (defun my-matlab-mode-hook ()
    (setq fill-column 80 		; where auto-fill should wrap
          )
    (imenu-add-to-menubar "Find") ; xxx what is this for?
    )
  (add-hook 'matlab-mode-hook 'my-matlab-mode-hook)
  (defun my-matlab-shell-mode-hook ()
    '())
  (add-hook 'matlab-shell-mode-hook 'my-matlab-shell-mode-hook)


  ;; To disable, specify
  ;;  (setq skip-sbtools-mlint-setup t)
  ;; prior to loading emacs_setup.el
  ;;
  ;; See the /matlab/java/extern/EmacsLink/lisp load path addition above
  ;; for mlint lisp code.
  (with-no-warnings
    (if (or (not (boundp 'skip-sbtools-mlint-setup))
            (not skip-sbtools-mlint-setup))
        (progn
          (message "Configuring auto-mlint")
          (autoload 'mlint-minor-mode "mlint" nil t)
          (add-hook 'matlab-mode-hook
                    (lambda ()
                      ;; Following are off by default to help customers
                      ;; which have installed mlint, but didn't install
                      ;; linemark.el, etc. object libraries.
                      (setq matlab-show-mlint-warnings t
                            highlight-cross-function-variables t
                            mlint-flags '("-all" "-id"))
                      (mlint-minor-mode 1)))
          ) ;; progn
      )
    )

  ;;
  ;; Delete file.p if it exists when file.m is saved
  ;;
  (defun matlab-deletep-after-save-hook ()
    "Delete file.p if it exists when file.m is saved"
    (let* ((fname (buffer-file-name (current-buffer)))
           (pfile (concat (file-name-sans-extension fname) ".p"))
           )
      (when (and (file-exists-p pfile)
                 (or noninteractive  ;; sbindent
                     (y-or-n-p (format "Delete %s too? " pfile))))
        (delete-file pfile)
        (message "Deleted %s. Remember to run sbgentbxcache!"
                 (file-name-nondirectory pfile))
        )))

  (add-hook 'matlab-mode-hook (lambda ()
                                (add-hook 'after-save-hook
                                          'matlab-deletep-after-save-hook
                                          t t))) ;; Local hook in matlab-mode

  (require 'gdb-mi)

;; Debugging via 'sb -Dgdb' or 'sb -Ddbx', etc.:
(defun MY/mathworks-sb-debug (&optional many-windows debug-ut)
  "Run 'sb -debug' in specified directory, optionally with many-windows in
emacs23 and later. The many-windows mode is known to be slow/buggy when 
used on MATLAB. Specify sb-default-args use different default arguments 
to sb"
  (interactive)
  (let ((run-with-many-windows
         (and many-windows
              (not (member (framep (selected-frame)) '(nil t pc))))
         )
        )
    (if (equal (get-buffer "*gud*") nil)
        ;; Launch gdb
	(let* ((start-dir (mathworks-get-sb-dir
                           (if debug-ut
                               (concat "Unit test directory to debug: " )
                             "Run sb -debug in dir: ")))
               (default-args (if debug-ut
                                 (concat "-debug-ut " start-dir)
                               "-debug"))
               (sb-args (read-string "Run sb with args: " 
                                     default-args nil default-args))
               (sb-cmd (concat (mathworks-path-to-sbtool-program "sb ")
                               " -gdb-switches -i=mi -debug-exe /home/jyates/bin/gdb-strip-fullname " sb-args))
               )

          ;; Ask for directory to run in.
	  (switch-to-buffer "*gud*")
	  (cd start-dir)
          (setq gud-chdir-before-run nil) ;; use current directory, start-dir

          (if run-with-many-windows  ;; many-windows has a 'studio' interface
              (progn
                (require 'gdb-mi)
                ;; (w160)
                (gdb-many-windows 1)
                (gdb sb-cmd)
                )
            ;; else run in classic mode
            (gud-gdb sb-cmd)
            )
          )
      (progn
        (switch-to-buffer "*gud*")
        (message "*gud* buffer already exists"))
      )
    )
  )

  ;; Mathworks added undesired lambdas as hook functions.
  ;; HACK: assume that there were no pre-existing hook functions.
  (setq gud-gdb-mode-hook nil)
  (setq gdb-mode-hook nil)

  )  ; (when (file-exists-p "/hub/share/sbtools/emacs_setup.el") ...

;; (if (file-exists-p "/hub/share/sbtools/emacs_setup.el")
;;     (let (save-version emacs-major-version)
;;       (setq emacs-major-version 24)
;;       (load-file "/hub/share/sbtools/emacs_setup.el")
;;       (setq emacs-major-version save-version)))

;;}}}
;;{{{  assembly language

(my/custom-set-variables
 '(asm-comment-char 35))                ; = '#'

;;}}}
;;{{{  Wiki mode

(add-to-list 'el-get-sources
             '(:name mediawiki
                     :description "Edit mediawiki sites from Emacs."
                     :type github
                     :pkgname "hexmode/mediawiki-el"))
(my/el-get-install "mediawiki")


(autoload 'mediawiki-open
  "Open a wiki page specified by NAME from the mediawiki engine.

\(fn NAME)" t)

(my/custom-set-variables
 '(mediawiki-pop-buffer-hook nil)
 '(mediawiki-site-alist
 '(("Wikipedia" "http://en.wikipedia.org/w/" "username" "password" "" "Main Page")
   ("Mathworks CGIR" "http://inside.mathworks.com/wiki/" "jyates" "" "" "Category:CGIR")))
 '(mediawiki-site-default "Mathworks CGIR")
 )

;; (add-to-list 'auto-mode-alist '("\\.wiki\\'" . simple-wiki-mode) t)
;; (add-to-list 'auto-mode-alist '("\\.w3mtmp\\'" . simple-wiki-mode) t)

;;}}}
;;{{{  AT&T Graphviz mode

(add-to-list 'el-get-sources
             '(:name graphviz-dot-mode
                     :description "Edit AT&T's dot language."
                     :type github
                     :pkgname "ppareit/graphviz-dot-mode"))
(my/el-get-install "graphviz-dot-mode")

;;}}}
;;{{{  GDB support

(my/custom-set-variables
 '(gdb-create-source-file-list nil)
 '(gdb-many-windows t)
 '(gdb-stack-buffer-addresses t)
 '(gud-gdb-command-name "gdb -i=mi")
 )

;; Mathworks trashes the program to run when requesting gdb
(when (file-exists-p "/hub/share/sbtools/emacs_setup.el")
  (setq gud-gdb-command-name "~/bin/gdb"))

(defvar gdb-source-window nil)

  (defun my/gud-eob ()
    "Select the \"*gud-*\" buffer and jump to EOB."
    (pop-to-buffer "*gud-*")
    (goto-char (point-max)))

(defmacro my/gud-def (func cmd stay-put &optional doc)
  "Define FUNC as sending CMD. See gud.el's gud-def for more details."
  `(defalias ',func (lambda (arg)
                      ,@(if doc (list doc))
                      (interactive "p")
                      (when (not gud-running)
                        ,(if (zerop stay-put)
                             `(select-window (display-buffer "*gud-*"))
                           `(my/gud-eob))
                        ,(if (stringp cmd)
                             `(gud-call ,cmd arg)
                           cmd)))))


  (my/gud-def my/gud-break  "break %f:%l"  0 "Set breakpoint at current line.")
  (my/gud-def my/gud-tbreak "tbreak %f:%l" 0 "Set temporary breakpoint at current line.")
  (my/gud-def my/gud-remove "clear %f:%l"  0 "Remove breakpoint at current line")
  (my/gud-def my/gud-step   "step %p"      1 "Step one or more source lines (step into calls).")
  (my/gud-def my/gud-next   "next %p"      1 "Step one or more source lines (skip over calls).")
  (my/gud-def my/gud-stepi  (progn (gud-call "stepi %p")
                                   (gud-call "x/i $pc"))
                                           1 "Step one or more instructions (step into calls).")
  (my/gud-def my/gud-nexti  (progn (gud-call "nexti %p")
                                   (gud-call "x/i $pc"))
                                           1 "Step one or more instructions (skip over calls).")
  (my/gud-def my/gud-cont   (gud-cont)     1 "Continue with display.")
  (my/gud-def my/gud-finish "finish"       1 "Finish executing current function.")
  (my/gud-def my/gud-jump   (progn (gud-call "tbreak %f:%l")
                                   (gud-call "jump %f:%l"))
                                           0 "Set execution address to current line.")
  (my/gud-def my/gud-up     "up %p"        0 "Up N stack frames (numeric arg).")
  (my/gud-def my/gud-down   "down %p"      0 "Down N stack frames (numeric arg).")
  (my/gud-def my/gud-pprint "pp %e"        0 "Evaluate C expression at point.")
  (my/gud-def my/gud-ppstar "pp* %e"       0 "Evaluate C dereferenced pointer expression at point.")
  (my/gud-def my/gud-print  "print %e"     0 "Evaluate C expression at point.")
  (my/gud-def my/gud-pstar  "print* %e"    0 "Evaluate C dereferenced pointer expression at point.")
  (my/gud-def my/gud-until  "until %f:%l"  0 "Continue to current line.")
  (my/gud-def my/gud-run    "run"          1 "Run the program.")
  (my/gud-def my/gud-frame0 "frame 0"      0 "Restore stack frame 0 in source window.")
  (my/gud-def my/gud-prompt "frame 0"      1 "Select \"*gud-*\" window and move point to end of prompt.")



(eval-after-load "gud" '(progn
  ;; Assume that the *gud- input window is selected
  (add-hook 'gud-mode-hook
    (lambda () (set-window-dedicated-p (selected-window) t))))

;;;;   ;; From http://markshroyer.com/2012/11/emacs-gdb-keyboard-navigation/
;;;; 
;;;;   ;; For the consistency of gdb-select-window's calling convention...
;;;;   (defun gdb-comint-buffer-name ()
;;;;     (buffer-name gud-comint-buffer))
;;;;   (defun gdb-source-buffer-name ()
;;;;     (buffer-name (window-buffer gdb-source-window)))
;;;; 
;;;;   (defun gdb-select-window (header)
;;;;     "Switch directly to the specified GDB window.
;;;; Moves the cursor to the requested window, switching between
;;;; `gdb-many-windows' \"tabs\" if necessary in order to get there.
;;;; 
;;;; Recognized window header names are: 'comint, 'locals, 'registers,
;;;; 'stack, 'breakpoints, 'threads, and 'source."
;;;; 
;;;;     (interactive "Sheader: ")
;;;; 
;;;;     (let* ((header-alternate (case header
;;;;                                ('locals      'registers)
;;;;                                ('registers   'locals)
;;;;                                ('breakpoints 'threads)
;;;;                                ('threads     'breakpoints)))
;;;;            (buffer (intern (concat "gdb-" (symbol-name header) "-buffer")))
;;;;            (buffer-names (mapcar (lambda (header)
;;;;                                    (funcall (intern (concat "gdb-"
;;;;                                                             (symbol-name header)
;;;;                                                             "-buffer-name"))))
;;;;                                  (if (null header-alternate)
;;;;                                      (list header)
;;;;                                    (list header header-alternate))))
;;;;            (window (if (eql header 'source)
;;;;                        gdb-source-window
;;;;                      (or (get-buffer-window (car buffer-names))
;;;;                          (when (not (null (cadr buffer-names)))
;;;;                            (get-buffer-window (cadr buffer-names)))))))
;;;; 
;;;;       (when (not (null window))
;;;;         (let ((was-dedicated (window-dedicated-p window)))
;;;;           (select-window window)
;;;;           (set-window-dedicated-p window nil)
;;;;           (when (member header '(locals registers breakpoints threads))
;;;;             (switch-to-buffer (gdb-get-buffer-create buffer))
;;;;             (setq header-line-format (gdb-set-header buffer)))
;;;;           (set-window-dedicated-p window was-dedicated))
;;;;         t)))
;;;; 
;;;;   ;; Use global keybindings for the window selection functions so that they
;;;;   ;; work from the source window too...
;;;;   (mapcar (lambda (setting)
;;;;             (lexical-let ((key    (car setting))
;;;;                           (header (cdr setting)))
;;;;               (global-set-key (concat "\M-g" key) #'(lambda ()
;;;;                                                           (interactive)
;;;;                                                           (gdb-select-window header)))))
;;;;           '(("A" . disassembly)         ; assembly language
;;;;             ("B" . breakpoints)
;;;;             ("D" . disassembly)
;;;;             ("F" . stack)
;;;;             ("G" . comint)
;;;;             ("I" . disassembly)         ; instructions
;;;;             ("L" . locals)
;;;;             ("O" . input/output)
;;;;             ("R" . registers)
;;;;             ("S" . source)
;;;;             ("T" . threads)))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.
;; Most of the trickiness in here comes from wanting to preserve the current
;; region-restriction if that's possible.  We use an explicit display-buffer
;; to get around the fact that this is called inside a save-excursion.

;; (defadvice gud-display-line (around my/gud-display-line activate)
;;   (let* ((last-nonmenu-event t)	 ; Prevent use of dialog box for questions.
;; 	 (buffer
;; 	  (with-current-buffer gud-comint-buffer
;; 	    (gud-find-file true-file)))
;;          ;;================
;; 	 ;; (window (and buffer
;; 	 ;;              (or (get-buffer-window buffer)
;; 	 ;;        	  (display-buffer buffer))))
;;          ;;================
;;          (window (and buffer
;;                       (or (if (eq gud-minor-mode 'gdbmi)
;;                               (unless (gdb-display-source-buffer buffer)
;;                                 (gdb-display-buffer buffer nil 'visible)))
;;                           (get-buffer-window buffer)
;;                           (display-buffer buffer))))
;;          ;;================
;; 	 (pos))
;;     (when buffer
;;       (with-current-buffer buffer
;; 	(unless (or (verify-visited-file-modtime buffer) gud-keep-buffer)
;; 	  (if (yes-or-no-p
;; 	       (format "File %s changed on disk.  Reread from disk? "
;; 		       (buffer-name)))
;; 	      (revert-buffer t t)
;; 	    (setq gud-keep-buffer t)))
;; 	(save-restriction
;; 	  (widen)
;; 	  (goto-char (point-min))
;; 	  (forward-line (1- line))
;; 	  (setq pos (point))
;; 	  (or gud-overlay-arrow-position
;; 	      (setq gud-overlay-arrow-position (make-marker)))
;; 	  (set-marker gud-overlay-arrow-position (point) (current-buffer))
;; 	  ;; If they turned on hl-line, move the hl-line highlight to
;; 	  ;; the arrow's line.
;; 	  (when (featurep 'hl-line)
;; 	    (cond
;; 	     (global-hl-line-mode
;; 	      (global-hl-line-highlight))
;; 	     ((and hl-line-mode hl-line-sticky-flag)
;; 	      (hl-line-highlight)))))
;; 	(cond ((or (< pos (point-min)) (> pos (point-max)))
;; 	       (widen)
;; 	       (goto-char pos))))
;;       (when window
;; 	(set-window-point window gud-overlay-arrow-position)
;; 	(if (eq gud-minor-mode 'gdbmi)
;; 	    (setq gdb-source-window window))))))

;; (defadvice gud-setup-windows (after my/dedicate-gud-comint-buffer activate)
;;   (set-window-dedicated-p (selected-window) t))

)

;;}}}
;;{{{  eshell

;; Emacs' eshell
(my/custom-set-variables
 '(eshell-aliases-file "/home/jyates/emacs/eshell/alias")
 '(eshell-modules-list
   '(eshell-alias
     eshell-banner
     eshell-basic
     eshell-cmpl
     eshell-dirs
     eshell-glob
     eshell-hist
     eshell-ls
     eshell-pred
     eshell-prompt
     eshell-script
     eshell-term))
 )

;;}}}
;;{{{  PHW

;; (add-to-list 'el-get-sources
;;              '(:name phw
;;                      :description "Persistent Horizontal Window"
;;                      :type git
;;                      :url         "file://localhost/home/jyates/repos/phw/"
;;                      ))
;; (my/el-get-install "phw")

(autoload 'phw-mode "phw"
  "Toggle phw-mode.
Setting this variable directly does not take effect;
use either \\[customize] or the function `phw-mode'." t)

;;}}}

;;=== Uncatergorized ===================================================
;;{{{  email

(my/custom-set-variables
 '(send-mail-function 'mailclient-send-it))

;;}}}
;;{{{  emacsclient and server

;; Support the Chrome Browser's Edit with Emacs extension
(add-to-list 'el-get-sources 'edit-server)
(my/el-get-install "edit-server")


(my/custom-set-variables
 '(display-buffer-reuse-frames t)
 '(server-switch-hook
   '(my/pop-file-name-history))
 '(server-temp-file-regexp "^/tmp/\\|.*\\.tmp")
 '(server-window 'pop-to-buffer)
 )

(defun my/pop-file-name-history ()
  "Pop most recent filename from the history."
  (setq file-name-history (cdr file-name-history)))

;; Code lifted from Mathwork's load-sb-tools.el
;;=======
;; - On UNIX, name server based on desktop id - 0 is the main with name
;;   "server" and others have name "serverId", Id = desktop number - 1.
;;   sbemacsclient.sh assumes this convention and this convention also
;;   handles the case of people using emacsclient directly.
;; - Only start server when it isn't already running
;;
;; An alternative implementation is to use
;;   (windowID (frame-parameter (selected-frame) 'outer-window-id))
;;   ... (concat "xprop -id " windowID " _NET_WM_DESKTOP") ...
;; but this doesn't handle when one rsh's over to a system.

;; See if we should skip server-start. SBTools specifies:
;;    emacs --eval=(setq skip-sbtools-server-start t) ....
;; when using emacs for transient tags and this isn't evaluated before
;; loading emacs_setup.el, thus see if it has been specified and
;; process it.
(defvar skip-sbtools-server-start) ;; suppress compile warning
(let ((first-arg (car (cdr command-line-args))))
  (if (equal first-arg "--eval=(setq skip-sbtools-server-start t)")
      (setq skip-sbtools-server-start t)
    )
  )

;; Emacs 21 doesn't support server-name, Emacs 23 is when
;; Windows client started working.
(when (and (>= emacs-major-version 23)
	   (not noninteractive)  ;; --batch?
	   (or (not (boundp 'skip-sbtools-server-start))
	       (not skip-sbtools-server-start))
	   )
  (let ((sbtools-server-name nil))
    (if (equal system-type 'gnu/linux)
	;; UNIX (Linux)
	(let ((display (getenv "DISPLAY")))
	  (when display
	    (let*
		(
		 (dispNumMatch   (string-match ":\\([0-9]+\\)" display))
		 (dispNum        (when dispNumMatch (match-string 1 display)))
		 (screenNumMatch (string-match ":[0-9]+\\.\\([0-9]+\\)" display))
		 (screenNum      (if screenNumMatch 
				     (match-string 1 display)
				   "0"))
		 (xpropDesktop   (shell-command-to-string
				  (concat "xprop -root -display " 
					  display " _NET_CURRENT_DESKTOP")))
		 (desktopIdMatch (string-match
				  "_NET_CURRENT_DESKTOP(CARDINAL) = \\([0-9]+\\)"
				  xpropDesktop))
		 (desktopId      (when desktopIdMatch (match-string 1 xpropDesktop)))
		 )
	      (let ((one-disp (getenv "SBTOOLS_ONE_EMACS_SESSION_PER_DISPLAY")))
		(if (and one-disp (string= one-disp "1"))
		    ;; server-name is serverDISP_NUM
		    (when dispNum
		      (setq sbtools-server-name (concat "server" dispNum)))
		  ;; else server-name is serverDISP_NUM.SCREEN_NUM.DESKTOP_ID
		  (when (and dispNum screenNum desktopId)
		    (setq sbtools-server-name
			  (concat "server" dispNum "." screenNum "." desktopId)))
		  ))
	      )))
      ;; else maci64/Windows
      (setq sbtools-server-name "server")
      ) ;; end (if (equal system-type 'gnu/linux))

    (defvar server-name) ;; suppress compile warning
    (when sbtools-server-name
      ;; In emacs23, server-running-p came into existence. Prior to
      ;; that (emacs22) each invocation of server-start canceled the
      ;; existing server and starts a new server. Using server-running-p
      ;; we can avoid canceling an existing server.
      (when (or (not (functionp 'server-running-p))
		(not (server-running-p sbtools-server-name)))
	(setq server-name sbtools-server-name)
	(message (concat "SBTools: starting emacs server named: " server-name))
	(if (> emacs-major-version 22)
	    ;; server-force-delete arrived with emacs 23.
	    (server-force-delete server-name)
	  )
	(server-start))
      )
    )
  )
;;=======

;;}}}
;;{{{  Performance

(my/custom-set-variables
 '(gc-cons-threshold 50000000)
 '(message-log-max 10000)
 )

;;}}}

;;=== el-get (epilog) ==================================================
;;{{{  Sync and update

(if my/el-get-refetch-all-packages
    (el-get-update-all t))

(message "======== Install missing packages")
(princ my/missing-el-get-packages)

(mapc (lambda (pkg)
	(progn
	  (message "Install %s" pkg)
	  (el-get-install pkg)))
      my/missing-el-get-packages)

;; (if my/missing-el-get-packages (el-get-update-all t))

(message "======== Load accumulated packagees")

(mapc (lambda (pkg)
        (progn
          (message "Activating %s" pkg)
          (el-get 'sync pkg)))
      my/all-el-get-packages)

(message "========")

;; Add this at the last moment to guarantee position at front of list
(add-to-list 'load-path "~/emacs/patched")

;; Now we can load our patched p4e
(when (file-exists-p "/hub/share/sbtools/emacs_setup.el")
  (require 'p4e))


;;}}}

;;=== Commented out ====================================================
;;{{{  Customizations

;; (my/custom-set-variables
;;  '(align-to-tab-stop t) ; align.el

;;  '(c-basic-offset 4)
;;  '(c-font-lock-extra-types (quote ("FILE" "\\sw+_[hpt]")))

;; This seems to a configuration for a package on emacswiki
;;  '(ebnf-justify-sequence (quote left))
;;  '(ebnf-non-terminal-shape (quote miter))
;;  '(ebnf-production-font (quote (10 Helvetica "White" "White" bold)))
;;  '(ebnf-terminal-shape (quote round))

;;  '(flyspell-incorrect-hook (quote (flyspell-maybe-correct-doubling flyspell-maybe-correct-transposition)))

;;}}}
;;{{{  Alex Ott detritus

;ao: (load "~/emacs/rc/emacs-rc-desktop.el")

;ao: (load "~/emacs/passwords.el.gpg")

;;---------------------------------------------------------------------

;; for org-mode
;ao: (setq comment-start nil)

;; for emacs-jabber (overrides paredit-newline binding)
;(define-key ctl-x-map "\C-j" jabber-global-keymap)

;;}}}

;;=== Now that all packages have been processed ========================
;;{{{  Audit customizations

(defvar my/custom-variables
  (eval-when-compile (sort my/accum-custom-variables
                           (lambda (a b)
                             (string-lessp (car a) (car b)))))
  "List of customizations to be compared to those in the custom file.")

(defvar my/custom-faces
  (eval-when-compile (sort my/accum-custom-faces
                           (lambda (a b)
                             (string-lessp (car a) (car b)))))
  "List of face customizations to be compared to those in the custom file.")

;; (message "my/custom-variables:\n\n%s\n\n" my/custom-variables)
;; (message "my/custom-faces:\n\n%s\n\n"     my/custom-faces)

;; Audit customized variables and faces against the custom file.
(my/check-custom-list "variables" my/saved-custom-variables my/custom-variables)
(my/check-custom-list "faces"     my/saved-custom-faces     my/custom-faces)

(setq
 my/saved-custom-variables nil
 my/custom-variables       nil
 my/saved-custom-faces     nil
 my/custom-faces           nil)

;;}}}
;;{{{  Key bindings

; simpler key definitions, autoloads for free
(add-to-list 'el-get-sources 'keydef)
(my/el-get-install "keydef")


;(keydef "M-D"           my/delete-whitespace-forward)
(keydef "C-c C-c M-x"   execute-extended-command) ; original M-x overridden by smex

(keydef "C-c C-k"       kill-compilation)

(keydef "C-c j"         avy-goto-word-or-subword-1)

(keydef "S-<mouse-3>"   browse-url-at-mouse)
(keydef "C-c z ."       browse-url-at-point)
(keydef "C-c z b"       browse-url-of-buffer)
(keydef "C-c z r"       browse-url-of-region)
(keydef "C-c z u"       browse-url)
(keydef "C-c z v"       browse-url-of-file)
(keydef (dired "z"      browse-url-of-dired-file))

;; easy ECB activation and deactivation
;; (keydef "C-c b"         ecb-activate)
;; (keydef "C-c d"         ecb-deactivate)
(keydef "C-c b"         phw-mode)
;(keydef "C-c d"         phw-deactivate)
;(keydef "C-c D"         my/ecb-run-gdb)


;; python-mode steals C-c C-k for python-mark-block. steal it back.
;(require 'python)
;(define-key python-mode-map [(control c) (control k)] 'kill-compilation)

(eval-after-load "hideshow" '(progn
  (keydef "C-c , C-c"   hs-toggle-hiding)
  (keydef "C-c , C-h"   hs-hide-block)
  (keydef "C-c , C-l"   hs-hide-level)
  (keydef "C-c , C-s"   hs-show-block)
  (keydef "C-c , C-M-h" hs-hide-all)
  (keydef "C-c , C-M-s" hs-show-all)
  ))

(keydef "C-c , ,"       (set-selective-display (if selective-display nil 1)))

(keydef "C-c -"         replace-string)
(keydef "C-c C--"       query-replace)
(keydef "C-c ="         replace-regexp)
(keydef "C-c C-="       query-replace-regexp)
(keydef "C-c 4"         my/set-buffer-local-tab-width-to-4)
(keydef "C-c 8"         my/set-buffer-local-tab-width-to-8)
(keydef "C-c c"         org-capture)
(keydef "C-c l"         org-store-link)

;; Binding from the ggtags package
;;
;; M-.             ggtags-find-tag-dwim
;; M-]             ggtags-find-reference
;; C-M-.           ggtags-find-tag-regexp
;;
;; C-c M-SPC       ggtags-save-to-register
;; C-c M-%         ggtags-query-replace
;; C-c M-/         ggtags-view-search-history
;; C-c M-?         ggtags-show-definition
;; C-c M-b         ggtags-browse-file-as-hypertext
;; C-c M-f         ggtags-find-file
;; C-c M-g         ggtags-grep
;; C-c M-h         ggtags-view-tag-history
;; C-c M-i         ggtags-idutils-query
;; C-c M-j         ggtags-visit-project-root
;; C-c M-k         ggtags-kill-file-buffers
;; C-c M-n         ggtags-next-mark
;; C-c M-o         ggtags-find-other-symbol
;; C-c M-p         ggtags-prev-mark
;; C-c M-DEL       ggtags-delete-tags


;; Additions to the help command
;;
(keydef "C-h A"         apropos-toc)       ; mnemonic: apropos All
(keydef "C-h B"         describe-bindings)
(keydef "C-h L"         (info "elisp"))    ; was describe-language-environment
(keydef "C-h R"         my/elisp-function-reference)

(keydef "C-x C-b"       bs-show)           ; same binding as <f1>

;; Kevin Rodgers <kevinr@ihs.com>
;;
;; I used to be in the habit of including `C-n C-a' in keyboard macros that
;; I intended to run repeatedly (by giving a prefix arg to `C-e' ) over
;; many lines, but I've learned a better way: write your macro to operate
;; on a single line, starting with point at the beginning of the line, and
;; don't include any cursor motion commands to move to the next line; then
;; you can run the macro over a specified set of lines by marking the
;; region and using `M-x apply-macro-to-region-lines' (which I've bound to
;; `C-x E')
(keydef "C-x E"         apply-macro-to-region-lines)

;; amake supported find-file, grep and

;;(keydef    "C-x , f"    (sbf-find-file))
(keydef    "C-x , f"    (apply 'helm (sbf-helm-find-file)))
;; (keydef "C-x , f"       am-find-file)
;; (keydef "C-x , 4 f"     am-find-file-other-window)
;; (keydef "C-x , g"       am-grep)
;; (keydef "C-x , r"       am-force-recache)
;; (keydef "C-x , ."       am-grep-tag)


;; GUD navigation claims the capital letters in the "goto" map:
;;  "M-g A"     *disassembly            ; assembly language
;;  "M-g B"     *breakpoints
;;  "M-g D"     *disassembly
;;  "M-g F"     *stack (Frames)
;;  "M-g G"     *gud-
;;  "M-g I"     *disassembly            ; instructions
;;  "M-g L"     *locals
;;  "M-g O"     *input/output
;;  "M-g R"     *registers
;;  "M-g S"     <source>
;;  "M-g T"     *threads

;; Additions to binding.el's goto-map; prior bindings:
;;  (M-) g   goto-line
;;  (M-) n   next-error
;;  (M-) p   previous-error
;;
(keydef "M-g b"         bookmark-jump)
(keydef "M-g e"         el-get-find-recipe-file)
(keydef "M-g l"         ilocate-library-find-source)
(keydef "M-g m"         my/magit-status)
(keydef "M-g r"         jump-to-register)
(keydef "M-g s"         my/elisp-find-symbol-definition)
(keydef "M-g w"         mediawiki-open)

(keydef "M-["           align)


;; Additions to binding.el's search-map; prior bindings:
;;  h*  highlight-*/hi-lock-*
;;  o   occur
;;  w   isearch-forward-word
;;
(keydef "M-s g"         grep)
(keydef "M-s l"         lgrep)
(keydef "M-s m"         multi-occur-in-matching-buffers)
(keydef "M-s r"         rgrep)

;; (keydef "M-x"           smex)
;; (keydef "M-X"           smex-major-mode-commands)

(keydef "<f1>"          bs-show)
(keydef "C-<f1>"        my/named-shell)
(keydef "M-<f1>"        menu-bar-mode)

(keydef "<f2>"          disk)


;; Strong similarity to MS Visual Studio's function keys
(keydef   "<f4>"        next-error)
(keydef "C-<f4>"        first-error)

(keydef   "<f5>"        my/gud-cont)    ; MS go / continue
(keydef "C-<f5>"        my/gud-until)   ; MS run to cursor
(keydef "S-<f5>"        my/gud-run)     ; restart

(keydef   "<f6>"        my/gud-print*)
(keydef "S-<f6>"        my/gud-pprint*)

(keydef   "<f7>"        my/gud-prompt)  ; focus GUD prompt
(keydef "C-<f7>"        compile)
(keydef "S-<f7>"        kill-compilation)

(keydef   "<f8>"        my/gud-print)
(keydef "C-<f8>"        my/gud-pprint)
(keydef "S-<f8>"        my/gud-frame0)

(keydef   "<f9>"        my/gud-step)    ; MS step into
(keydef "C-<f9>"        my/gud-stepi)   ; step by instruction
(keydef "S-<f9>"        my/gud-down)

(keydef   "<f10>"       my/gud-next)    ; MS step over
(keydef "C-<f10>"       my/gud-finish)  ; MS step out
(keydef "S-<f10>"       my/gud-up)

(keydef   "<f11>"       my/gud-break)
(keydef "C-<f11>"       my/gud-tbreak)
(keydef "S-<f11>"       my/gud-remove)

;; (keydef "C-."           vtags-find)
;; (keydef "<kp-begin>"    vtags-point-to-placeholder)
;; (keydef "<kp-right>"    vtags-next-placeholder)
;; (keydef "<kp-left>"     vtags-prev-placeholder)

(keydef "<f12>"         customize-option)
(keydef "C-<f12>"       customize-group)
(keydef "M-<f12>"       customize-apropos)

(eval-after-load 'helm '(progn
  (keydef "M-x"         helm-M-x)
  (keydef "C-x C-f"     helm-find-files)
  ;; Swap TAB and C-j
  (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-j") 'helm-select-action)
  ))


(eval-after-load "bs"  '(keydef (bs "<f1>") (bs-kill)))

;;}}}

(message "Completed .emacs")

;;======================================================================
;; Local Variables:
;; comment-column: 40
;; folded-file: t
;; End:

;;---------------------------------------------------------------------

;; Experimental trash... do not commit if there is anything here!
