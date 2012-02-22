;; John Yates's .emacs  -*- emacs-lisp -*-
;;
;; Time-stamp: "2012-02-09 01:35:15 jyates"

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;=== Notes ============================================================
;;{{{  Goals

;; Package sanity:
;; - I fully concur with el-get author Dimitri Fontaine's sentiments.
;;   I too want a single file specifying all the packages I use, whence
;;   to obtain them and how they should be configured.

;; Custome file sanity:
;; - grouping
;; - commentary
;; - no repeating detault settings

;; Directory hygiene:
;;
;; Apart for this file (~/.emacs) all state which I maintain manually
;; into my ~/emacs/ directory (e.g. custom-file, spelling dictionaries,
;; templates I write/modify for yasnippet, packages I write or modify).
;; maintain the contents of this directory in git and replicate to all
;; sites where I use emacs.
;;
;; By contrast I reserve my ~/.emacs.d directory for cached state
;; transparently managed by various packages (e.g. semanticdb, el-get,
;; backup, auto-save, etc).  This is information that is either
;; ephemeral or recreatable from the combination of my .emacs file and
;; the contents of my ~/emacs/ directory.

;; Speed:
;; - I strive to have all functionality either autoloaded or handled
;;   via eval-after-load.  The few files that tolerate being loaded as
;;   part of this .emacs are those that will truly be used immediately
;;   (cua-base for blinking cursor).

;;}}}
;;{{{  Sharing and credits

;; Everyone's .emacs rips off someone else's...

;; There is a bit of a convention to prefix private code with some
;; version of the user's name.  This might make sense if users copied
;; code amongst .emacs files while preserving the original author's
;; name.  In my experience this is not what happens.  Rather imported
;; code gets resuffixed.  Yet the real goal is not so much to claim
;; authorship as to avoid collidions in the emacs name space.  Hence I
;; do not use my own name, only a "my/" prefix.  Anyone willing to
;; adopt a similar convention could then easily crib any of my
;; customizations.

;; It is too hard and too noisy to attribute ideas in the body of this
;; file.  Here I simply list source from which I have either cribbed
;; outright or else have drawn inspiration.
;;
;; Alex Ott:            https://github.com/alexott/emacs-configs
;; Ryan Barrett:        http://snarfed.org/dotfiles/.emacs

;;}}}
;;{{{  Setup

;; Gnome system:  http://www.emacswiki.org/emacs/MovingTheCtrlKey#toc2
;;                I chose "Make Caps Lock an additional Ctrl"

;; Prerequisites:
;; - git, bzr, cvs, svn, hg, autoconf, makeinfo (from texinfo)
;; - doxymacs requires that libxml2-dev be installed (presently not used)

;; Remove older versions of emacs and emacs-goodies-el
;;
;; To fetch an initial copy emacs-goodies-el from debian.org or to update:
;;
;; $ cd ~/.emacs-d
;; $ cvs -d :pserver:anonymous@anonscm.debian.org:/cvs/pkg-goodies-el login
;; [blank password]
;; $ cvs -z3 -d :pserver:anonymous@anonscm.debian.org:/cvs/pkg-goodies-el checkout emacs-goodies-el/elisp/emacs-goodies-el

;; Directories
;; (make-directory "~/.emacs.d/autosave" t)
;; (make-directory "~/.emacs.d/autosave-list" t)
;; (make-directory "~/.emacs.d/auto-save-Confluence" t)
;; (make-directory "~/.emacs.d/backup" t)
;; (make-directory "~/.emacs.d/semanticdb" t)
;; (make-directory "~/.emacs.d/smex" t)
;; (make-directory "~/.emacs.d/srecode" t)
;; (make-directory "~/.emacs.d/url" t)

;; - el-get-emacswiki-refresh (no separate subdirectory)
;; - el-get-install every el-get-sources entry in this file

;; Check *Messages* for files being loaded as source

;; profile:  emacs -Q -l ~/.emacs.d/el-get/profile-dotemacs/profile-dotemacs.el -f profile-dotemacs

;; Launcher: /usr/bin/emacs-snapshot --no-site-file -bg black -fg white -fn dina -mm --debug-init

;;}}}
;;{{{  Missing

;; swap caps lock and control
;; Sanjay Dixit's am package - add autoload cookies, use el-get
;; align
;; occur
;; grep
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
;;{{{  load-path

(let ((entries (directory-files "~/.emacs.d/el-get" t)))
  (mapc (lambda (path)
          (when (file-directory-p path)
            (add-to-list 'load-path path))) entries))

;;}}}
;;{{{  el-get

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; CAVEAT: functions in packages acquired via el-get cannot be invoked
;; until (el-get 'wait) has completed in the el-get epilog toward the
;; bottom of this file.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Minimal bootstrap
(unless (require 'el-get nil t)
  (url-retrieve "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
                (lambda (s)
                  (let (el-get-master-branch)
                    (goto-char (point-max))
                    (eval-print-last-sexp)))))

(setq el-get-recipe-path '("~/.emacs.d/el-get/el-get/recipes/"))
(setq el-get-sources '(el-get)) ; built incrementally via add-to-list

(require 'inversion nil t) ; fix broken autoload in cedet/common/cedet-compat.el

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
                     (insert "\n--- MISSING ---\nSaved:\n")
                     (pp (car centry)))))
             (t
              (while (not (equal rentry (car remaining)))
                (insert "\n--- UNKNOWN (not saved?) ---\nInit:\n")
                (pp (car remaining))
                (setq remaining (cdr remaining)))
              (cond
               ((not (equal cval (cdr rentry)))
                (insert "\n--- CONFLICT ---\nSaved:\n")
                (pp (car centry))
                (insert "\nInit:\n")
                (pp rentry))
               ((equal cval (get csym 'standard-value))
                (insert "\n--- REDUNDANT (matches default) ---\nInit:\n")
                (pp rentry)))
              (setq remaining (cdr remaining))))))))
    (if (>= 2 (point-max))
        (kill-buffer)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;}}}
;;{{{  Apply customizations

(setq custom-file "~/emacs/custom-file")
(add-to-list 'auto-mode-alist '("custom-file" . emacs-lisp-mode))

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
(with-current-buffer (get-buffer-create "*Custom File*")
  (insert-file-contents-literally custom-file )
  (setq my/saved-custom-variables (process-customization-list))
  (setq my/saved-custom-faces     (process-customization-list))
  (kill-buffer))

;; Host specific initialization if it exists (my-rc-local-HOST.el[c])
(require (intern (concat "my-rc-local-"
                         (let* ((host (system-name))
                                (idx (string-match "\\." host)))
                           (if idx
                               (substring host 0 idx)
                             host))))
         nil t)

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
   '((folded-file . t)
     (folding-mode . t)
     )))

;;}}}
;;{{{  Less safe: y/n instead of yes/no

(fset 'yes-or-no-p 'y-or-n-p)

;;}}}

;;=== Visuals ==========================================================
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
      (width . 100)
      (height . 60)
      (top . 0)
      (left . 0)
      (minibuffer . t)
      (vertical-scroll-bars . right)
      (icon-type)))
 '(indicate-buffer-boundaries 'right) ; graphics in fringe
 )

(my/custom-set-faces
 '(fringe ((((class color) (background dark)) (:background "grey15"))))
 )

;;}}}
;;{{{  Menu bar

(my/custom-set-variables
 '(menu-bar-mode nil)
 )

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
     (name . "Emacs minibuffer    (CTRL + click to show/hide other Emacs frames)")))
 )

(my/custom-set-faces
 '(minibuffer-prompt ((t (:foreground "orange" :weight bold))))
 )

;;}}}
;;{{{  Cursor and parenthesis matching

(my/custom-set-variables
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

;;}}}
;;{{{  Basic faces

(my/custom-set-faces
 '(fixed-pitch ((t nil)))
 '(variable-pitch ((t (:height 0.9 :family "Sans Serif"))))
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
;;{{{  Font lock faces

(my/custom-set-faces
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
 )

;;}}}
;;{{{  Selection and highlighting

(my/custom-set-faces
 '(highlight ((t (:background "#091830"))))
; '(highlight-beyond-fill-column-face ((t (:inverse-video t))))
 '(region ((t (:background "DarkSlateBlue"))))
 )

;;}}}
;;{{{  mode-line basics

(my/custom-set-faces
 '(mode-line ((t (:background "tan2" :foreground "black" :box (:line-width -1 :style released-button) :height 1.2))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-highlight ((t (:background "wheat2" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((default (:inherit mode-line)) (((class color) (min-colors 88) (background dark)) (:background "grey70"))))
 )

(my/custom-set-variables
 '(mode-line-format
   '("%e"
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
mouse-3: Remove current window from display")))))

;;}}}
;;{{{  mode-line eol-mnemonic

(my/custom-set-variables
 '(eol-mnemonic-mac ":")                ; defaults to '/'
 '(eol-mnemonic-undecided "?")          ; defaults to ':'
 '(eol-mnemonic-unix "/")               ; defaults to ':'
 )

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
;;   http://bazaar.launchpad.net/~nxhtml/nxhtml/main/annotate/head%3A/util/mode-line-pos.el

(my/custom-set-variables
 '(mode-line-position '(:eval (my/position-widget)))
 )

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
                 #("[" 0 1 (face bold))
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
                 #("]" 0 1 (face bold))
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

;;}}}
;;{{{  mode-line which-func

(my/custom-set-variables
 '(which-func-format
   '("["
     (:propertize which-func-current local-map
                  (keymap
                   (mode-line keymap
                              (mouse-3 . end-of-defun)
                              (mouse-2 . #[nil "e\300=\203	 \301 \207~\207"
                                               [1 narrow-to-defun]
                                               2 nil nil])
                              (mouse-1 . beginning-of-defun)))
                  face which-func help-echo
"Function (enclosing or preceding)
mouse-1: go to beginning
mouse-2: narrow to function
mouse-3: go to end")
     "]"))
 '(which-function-mode t nil (which-func))
 )

;;}}}
;;{{{  emacs-goodies/diminish

(add-to-list 'el-get-sources
             '(:name diminish
                     :type        http
                     :description "Shrink or eliminate minor mode modeline display"
                     :url         "file://localhost/home/jyates/.emacs.d/emacs-goodies-el/elisp/emacs-goodies-el/diminish.el"
                     :features    (diminish)))

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
;; (eval-after-load "FOO" '(add-hook 'FOO-mode-hook "X"))

;;}}}
;;{{{  Uniquify buffer names

(require 'uniquify)

(my/custom-set-variables
 '(uniquify-buffer-name-style 'forward nil (uniquify)) ; prefix '/'
 )

;;}}}
;;{{{  Pretty ^L

(add-to-list 'el-get-sources
             '(:name  pp-c-l
                      :after (lambda ()
                               (add-hook 'window-setup-hook
                                         'refresh-pretty-control-l)
                               (add-hook 'window-configuration-change-hook
                                         'refresh-pretty-control-l))
                      :features (pp-c-l)))

;; Universally display ^L as a window-width horizontal rule
(my/custom-set-variables
 '(pretty-control-l-mode t)
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
 '(tool-bar-mode nil)                   ; recover screen space, no toolbar
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
;;{{{  Tab width

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

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setenv "TERM ansi")

;;}}}
;;{{{  Extra color themes

(my/custom-set-variables
 '(custom-theme-directory "~/emacs/themes")
 )

;;}}}
;;{{{  Other stuff

;; Display Windows extended glyphs (instead of things like \223)
(standard-display-8bit 128 255)

;;}}}

;;=== Searching and editing ============================================

(my/custom-set-variables
 '(kill-whole-line t)
 )

;;{{{  Delete selection mode

(defun my/turn-on-delete-selection-mode ()
  (delete-selection-mode 1)
  (remove-hook activate-mark-hook 'my/turn-on-delete-selection-mode)
  )

(my/custom-set-variables
 '(activate-mark-hook 'my/turn-on-delete-selection-mode t)
 )

;;}}}
;;{{{  Occur

;; (autoload 'occur "replace"
;;   "Show all lines in the current buffer containing a match for REGEXP.
;;
;; \(fn REGEXP &optional NLINES)" t)

;;}}}
;;{{{  White space hygine

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

(defconst my/yank-indent-modes
  '(c++-mode
    c-mode
    cperl-mode
    emacs-lisp-mode
    lisp-interaction-mode
    lisp-mode
    sql-mode
    tcl-mode)
  "List of major modes in which to indent yanked (or yank-popped) text.")

(defun my/maybe-indent-yanked ()
  "If mode in my/yank-indent-modes then indent yanked text (prefix arg inhibits)."
  (if (member major-mode my/yank-indent-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice yank (after maybe-indent activate)
  "If mode in my/yank-indent-modes then indent yanked text (prefix arg inhibits)."
  (my/maybe-indent-yanked))

(defadvice yank-pop (after maybe-indent activate)
  "If mode in my/yank-indent-modes then indent yanked text (prefix arg inhibits)."
  (my/maybe-indent-yanked))

;;}}}
;;{{{  Tabs

(my/custom-set-variables
 '(indent-tabs-mode nil)                ; no hard tabs
 '(tab-always-indent 'complete)         ; tab when nothing to complete
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

;;=== VC, diff, merge, patch ===========================================
;;{{{  magit

(add-to-list 'el-get-sources 'magit)
(add-to-list 'el-get-sources 'magithub)

(my/custom-set-variables
 '(magit-diff-refine-hunk 'all)
 )

;;}}}
;;{{{  dvc

;; (add-to-list 'el-get-sources 'dvc)

;;}}}
;;{{{  diff

(my/custom-set-faces
 '(diff-added ((t (:inherit diff-changed :foreground "DarkSeaGreen1"))))
 '(diff-file-header ((t (:inherit diff-header :foreground "light goldenrod yellow" :weight bold))))
 '(diff-header ((t (:background "gray22"))))
 '(diff-nonexistent ((t (:strike-through "red"))))
 '(diff-refine-change ((t (:background "#1c3850"))))
 '(diff-removed ((t (:inherit diff-changed :foreground "pink"))))
 )

;;}}}
;;{{{  ediff

(my/custom-set-variables
 '(ediff-keep-variants nil)
 '(ediff-make-buffers-readonly-at-startup t)
 '(ediff-merge-split-window-function 'split-window-vertically)
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
 '(smerge-command-prefix ""))

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

;;=== Utilities ========================================================
;;{{{  Help (apropos, info, etc)

(add-to-list 'el-get-sources
             '(:name apropos-toc
                     :description "XEmacs-ish hyper-apropos for GNUEmacs"
                     :type        http
                     :url         "http://www.cbrunzema.de/download/apropos-toc/apropos-toc.el"
                     :features    (apropos-toc)))

(defvar my/apropos-toc-font-lock-keywords
  (list
   '("^\\(Function\\|Variable\\)s:" . font-lock-keyword-face))
  "Additional expressions to highlight in Apropos TOC mode")

(defadvice my/customized-apropos-toc (after apropos-toc activate)
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

(defun my/elisp-function-reference (func)
  "Look up an Emacs Lisp function in the Elisp manual in the Info system.
This command is designed to be used whether you are already in Info or not."
  (interactive (let ((fn (function-called-at-point))
                     (enable-recursive-minibuffers t)
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


(autoload 'info-lookup-add-help "info-look" nil t)

;;}}}
;;{{{  Completion (ido, smex)

(add-to-list 'el-get-sources 'smex)

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
     ))
 '(ido-mode 'both nil (ido))
 '(ido-save-directory-list-file "~/.emacs.d/ido.last")
 '(smex-save-file "~/.emacs.d/smex")
 )

;; from minibuf-electric-gnuemacs.el
(defun ido-electric-slash ()
  (interactive)
  (and (eq ?/ (preceding-char))
       ;; permit `//hostname/path/to/file'
       (not (eq (point) (1+ (minibuffer-prompt-end))))
       ;; permit `http://url/goes/here'
       (not (eq ?: (char-after (- (point) 2))))
       (delete-region (minibuffer-prompt-end) (point)))
  (insert ?/))

(defun ido-electric-tilde ()
  (interactive)
  (if (eq ?/ (preceding-char))
      (delete-region (minibuffer-prompt-end) (point)))
  (insert ?~))

(add-hook 'ido-setup-hook
          (function (lambda ()
                      (define-key ido-completion-map "/" 'ido-electric-slash)
                      (define-key ido-completion-map "~" 'ido-electric-tilde))))

;;}}}
;;{{{  Spell checking

(my/custom-set-variables
 '(ispell-personal-dictionary "~/emacs/aspell.en.pws")
 )

;;}}}
;;{{{  Recent files

(my/custom-set-variables
 '(recentf-save-file "~/.emacs.d/recentf")
 )

;;}}}
;;{{{  ilocate-library

(add-to-list 'el-get-sources
             '(:name ilocate-library
                     :description "Interactive locate-library (or source) with completion"
                     :type        http
                     :url         "file://localhost/home/jyates/clones/ilocate-library/ilocate-library.el"
;;                   :type        emacsmirror
;;                   :url         "https://github.com/emacsmirror/ilocate-library.git"
                     :features    (ilocate-library)))

;;}}}
;;{{{  Update timestamps before saving files

;; (add-hook 'before-save-hook 'time-stamp)

;;}}}
;;{{{  Always byte compile after saving elisp

(defun my/byte-compile-saved-elisp-buffer ()
  "Byte compile an elisp buffer anytime it is saved."
  (if (eq major-mode 'emacs-lisp-mode)
      (byte-compile-file (buffer-file-name))))

(add-hook 'after-save-hook 'my/byte-compile-saved-elisp-buffer)

;;}}}
;;{{{  Named shells, comint, etc

(defun my/named-shell (BUFFER)
  "Create or switch to a running shell process in BUFFER."
  (interactive "BShell name: ")
  (shell BUFFER))


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
 '(protect-buffer-bury-p nil)
)

(defadvice comint-send-input (around my/go-to-end-of-multiline activate)
  "When I press enter, jump to the end of the *buffer*, instead of the end of
the line, to capture multiline input. (This only has effect if
`comint-eol-on-send' is non-nil."
  (flet ((end-of-line () (end-of-buffer)))
    ad-do-it))

(defadvice comint-previous-matching-input
  (around my/suppress-history-item-messages activate)
  "Suppress the annoying 'History item : NNN' messages from shell history isearch.
If this isn't enough, try the same thing with
comint-replace-by-expanded-history-before-point."
  (let ((old-message (symbol-function 'message)))
    (unwind-protect
        (progn (fset 'message 'ignore) ad-do-it)
      (fset 'message old-message))))

;; not sure why, but comint needs to be reloaded from the source (*not*
;; compiled) elisp to make the above advise stick.
;; (load "comint.el.gz")

;; Use the transcript in lieu of a classic pager
(setenv "PAGER" "cat")

;; truncate buffers continuously
;(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(defun my/shell-output-read-only (text)
  "Add to comint-output-filter-functions to make output read only."
  (let ((inhibit-read-only t)
        (output-end (process-mark (get-buffer-process (current-buffer)))))
    (put-text-property comint-last-output-start output-end 'read-only t)))
(add-hook 'comint-output-filter-functions 'my/shell-output-read-only)

(defun my/fix-shell ()
  "Sometimes a shell's input area goes read only.  Fix it."
  (interactive)
  (let ((inhibit-read-only t))
    (comint-send-input)))


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

(defvar my/comint-based-modes '(shell-mode gud-mode))

(defun enter-again-if-enter ()
  "Make the return key select the current item in minibuf and shell history isearch.
An alternate approach would be after-advice on isearch-other-meta-char."
  (when (and (not isearch-mode-end-hook-quit)
             (equal (this-command-keys-vector) [13])) ; == return
    (cond ((active-minibuffer-window) (minibuffer-complete-and-exit))
          ((memq major-mode my/comint-based-modes) (comint-send-input)))))
(add-hook 'isearch-mode-end-hook 'enter-again-if-enter)

;;}}}
;;{{{  hippie-expand

;; (set (make-local-variable 'hippie-expand-try-functions-list)
;;      my/XXX-mode-hippie-expand-try-functions-list)

(defconst my/c-mode-hippie-expand-try-functions-list
  '(yas/hippie-try-expand
    ;; try-expand-all-abbrevs
    try-expand-dabbrev
    try-expand-dabbrev-visible
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    insert-tab
    ))

(defconst my/shell-mode-hippie-expand-try-functions-list
  '(try-complete-file-name
    try-complete-file-name-partially
    ;; try-expand-all-abbrevs
    try-expand-dabbrev
    try-expand-dabbrev-visible
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    ))

;;}}}
;;{{{  yasnippet

(add-to-list 'el-get-sources
             '(:name "yasnippet"
                     :description "YASnippet is a template system for Emacs."
                     :type git
                     :url "https://github.com/capitaomorte/yasnippet"))

(eval-when-compile (require 'yasnippet))

(my/custom-set-variables
 '(yas/trigger-key "C-M-~")           ;; TAB invokes hippie-expand
 '(yas/fallback-behavior 'return-nil) ;; continue hippie-expand on failure
 )

(eval-after-load "yasnippet"
  '(progn
     (setq yas/snippet-dirs "~/emacs/yasnippet")
     ;; reload modified snippets
     (defun my/yasnippet-reload-on-save ()
       (when (string-match "/emacs/yasnippet/" buffer-file-name)
;;         (mapc 'yas/load-directory yas/snippet-dirs)))
         (yas/reload-all))) ; no mapc with just a single directory
     (add-hook 'after-save-hook 'my/yasnippet-reload-on-save)
     (yas/reload-all)))

;; Another note: The new 0.7 yasnippet.el messes things up with
;; anything.el. You need to do this:
;;
;; Need to replace the following in anything-c-yasnippet.el:
;;   yas/snippets/table-hash      -> yas/table-hash
;;   yas/snippets/table-templates -> yas/table-templates
;;
;; (require 'anything-c-yasnippet)

;;}}}

;;=== Minor modes ======================================================
;;{{{  folding

(add-to-list 'el-get-sources
             '(:name  folding
                      :after folding-mode-add-find-file-hook))

(my/custom-set-variables
 '(folding-mode-prefix-key ",")       ; also changed for hideshow
 '(folding-advice-instantiate nil)      ; not advising M-g g
 '(folding-goto-key "\M-gf")            ; Restore M-g's prefix behavior
 )

;;}}}
;;{{{  hideshow...

;; Only needed until emacs maintainers accept my two patches.
(add-to-list 'el-get-sources
             '(:name hideshow
                     :type http
                     :description "Selectively collapse/expand code/comment blocks"
                     :url "file://localhost/home/jyates/clones/emacs/hideshow.el"
;;                   :url "file:/usr/share/emacs/24.0.93/lisp/progmodes/hideshow.el"
                     :features (hideshow)))

;; Display the size of a collapsed function body
(defun my/display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let ((str (format " %d " (count-lines (overlay-start ov)
                                           (overlay-end ov)))))
      (put-text-property 0 (length str) 'face 'glyphless-char str)
      (overlay-put ov 'display str))))

(eval-after-load "hideshow"
  '(setq hs-set-up-overlay 'my/display-code-line-counts))

;; My version corrects the order of save-excursion save-restriction sequence
(add-to-list 'el-get-sources
             '(:name hideshowvis
;;                     :type emacswiki
                     :type http
                     :url "file://localhost/home/jyates/clones/emacswiki/hideshowvis.el"
                     :description "Add fringe markers for hide/show foldable regions."
                     :features (hideshowvis)))

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

;;}}}
;;{{{  auto-fill and filladapt

;; Let filladapt's "FA" indicate that filling is active
;(eval-after-load "fill" '(diminish 'auto-fill-mode))

;; Redirect to a patched version more captible with cc-mode
(add-to-list 'el-get-sources
             '(:name filladapt
                     :url "http://cc-mode.sourceforge.net/filladapt.el"))


;; As distributed filladapt.el contains no autoload cookies
(autoload 'turn-on-filladapt-mode "filladapt"
  "Unconditionally turn on Filladapt mode in the current buffer.

\(fn)" t)

(eval-after-load "filladapt" '(diminish 'filladapt-mode "FA"))

(defun my/text-mode ()
  (setq fill-column 80)
  (text-mode-hook-identify)
  (turn-on-auto-fill)
  (turn-on-filladapt-mode)
  )

(my/custom-set-variables
 '(text-mode-hook '(my/text-mode))
 )

;;}}}
;;{{{  longlines

(eval-after-load "longlines"
  '(progn
     (defvar longlines-mode-was-active nil)
     (make-variable-buffer-local 'longlines-mode-was-active)

     (defun longlines-suspend ()
       (if longlines-mode
           (progn
             (setq longlines-mode-was-active t)
             (longlines-mode 0))))

     (defun longlines-restore ()
       (if longlines-mode-was-active
           (progn
             (setq longlines-mode-was-active nil)
             (longlines-mode 1))))

     ;; longlines doesn't play well with ediff, so suspend it during diffs
     (defadvice ediff-make-temp-file (before make-temp-file-suspend-ll
                                             activate compile preactivate)
       "Suspend longlines when running ediff."
       (with-current-buffer (ad-get-arg 0)
         (longlines-suspend)))

     (add-hook 'ediff-cleanup-hook
               (function (lambda ()
                           (dolist (tmp-buf (list ediff-buffer-A
                                                  ediff-buffer-B
                                                  ediff-buffer-C))
                             (if (buffer-live-p tmp-buf)
                                 (with-current-buffer tmp-buf
                                   (longlines-restore)))))))))

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
 )

;;}}}
;;{{{  Confluence wiki

(add-to-list 'el-get-sources 'xml-rpc) ; a dependency from emacswiki

(add-to-list 'el-get-sources
             '(:name confluence
                     :description "Interact with confluence wikis"
                     :type        svn
                     :url         "http://confluence-el.googlecode.com/svn/trunk/"
                     :features    (ilocate-library)))

(my/custom-set-variables
 '(confluence-url "http://wiki2.netezza.com:8080/rpc/xmlrpc")
 '(confluence-save-credentials t)
 '(confluence-min-page-repeat-completion-length 1)
 '(confluence-auto-save-dir "~/.emacs.d/auto-save-Confluence")
 )

(my/custom-set-faces
 '(confluence-panel-face ((t (:background "gray8"))))) ; subtle panels

(autoload 'confluence-get-page "confluence" nil t)

;; setup confluence mode
;;(eval-when-compile (require 'xml-rpc))
;;(eval-when-compile (require 'confluence))
;; (add-hook 'confluence-mode-hook
;;           (function (lambda () (local-set-key "\C-cw" confluence-prefix-map))))

(eval-after-load "confluence"
  '(progn
     (require 'longlines)
     (progn
       (add-hook 'confluence-mode-hook 'longlines-mode)
       (add-hook 'confluence-before-save-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-before-revert-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-mode-hook
                 (function (lambda ()
                             (local-set-key "\C-j" 'confluence-newline-and-indent)
                             (local-set-key "\C-cw" confluence-prefix-map)))))))

;;}}}

;;=== Programming ======================================================
;;{{{  Sanjay Dixit's am package

(add-to-list 'load-path "~/emacs/nz")

(autoload 'am-scan "am" nil t)

(autoload 'am-find-file "am"
  "Find file known to amake."
  t)

(autoload 'am-find-file-other-window "am"
  "Find file known to amake, open it in another window."
  t)

;;}}}
;;{{{  Compilation and next exrror

(my/custom-set-variables
;; '(compile-command "/usr/bin/make -k")
 '(compilation-scroll-output t)   ; follow compilation output
;; '(compilation-skip-threshold 2); next-error should only stop at errors
 '(next-error-hook 'next-error-recenter)
 )

(defun next-error-recenter ()
  "Center error location in window."
  (save-excursion
    (pop-to-buffer next-error-last-buffer nil t)
    (recenter)))

;;}}}
;;{{{  C/C++ mode

(eval-when-compile (require 'cc-mode))

;; Treat .h and .imp files as C++ source
(setq auto-mode-alist (append '(("\\.h\\'" . c++-mode)
                                ("\\.imp\\'" . c++-mode))
			      auto-mode-alist))


(defun my/c-public-private-boundary (langelem)
  "Line up line of slashes preceding 'private:' at the left edge."
  (save-excursion
    (message "my/c-public-private-boundary: %s" langelem)
    (back-to-indentation)
    (if (and (looking-at "//////////*$")
             (forward-line 1)
             (looking-at "private:"))
        [0]
      nil)))


;; Closely parallels cc-align.el's c-lineup-arglist-operators
(defun my/c-lineup-arglist-&&-or-|| (langelem)
  "Line up lines starting with && or ||  under the open paren.
Return nil on lines that start with neither && nor ||, to leave
those cases to other line-up functions.  Example:

if ( x < 10
  || at_limit (x,       <- my/c-lineup-arglist-&&-or-||
               list)    <- my/c-lineup-arglist-&&-or-|| returns nil
   )

Since this function doesn't do anything for lines without && or
|| typically want to use it together with some other line-up
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
        (c-basic-offset . 4)
        (c-comment-only-line-offset 0 . 0)
    ;;    (c-comment-prefix-regexp `('other. "//+<?\\|\\**"))
        (c-offsets-alist
         (access-label . -)
         (annotation-top-cont . 0)
         (annotation-var-cont . +)
         (arglist-close . c-lineup-close-paren)
         (arglist-cont c-lineup-gcc-asm-reg 0)
         (arglist-cont-nonempty my/c-lineup-arglist-&&-or-|| c-lineup-arglist-close-under-paren)
         (arglist-intro . c-lineup-arglist-intro-after-paren)
         (block-close . 0)
         (block-open . 0)
         (brace-entry-open . 0)
         (brace-list-close . 0)
         (brace-list-entry . 0)
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
         (friend . 0)
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
         (member-init-cont . -)
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
         (topmost-intro-cont . +))))
     ))

(defun my/c-mode-common-hook ()
  ""
  ;; Semantic does a better job supporting which-func in mode-line
  ;;(require 'semantic/imenu)
  ;;(setq imenu-create-index-function 'semantic-create-imenu-index)

  ;; cc-mode uses abbrev-mode to implement electric keywords
  (diminish 'abbrev-mode)

  (hs-minor-mode)
  (hideshowvis-enable)

  (setq tab-width 4)
  (setq comment-column 40)

  (c-set-style "jsy")
  (add-to-list 'c-comment-prefix-regexp ' (other . "//+<?\\|\\**"))

  (c-setup-paragraph-variables)

  (setq fill-column 75)

  (turn-on-auto-fill)

  (require 'filladapt)
  (c-setup-filladapt)
  (turn-on-filladapt-mode)

  (c-toggle-auto-hungry-state 1)

  (require 'yasnippet)

  (set (make-local-variable 'hippie-expand-try-functions-list)
       my/c-mode-hippie-expand-try-functions-list)

  ;;(define-key c-mode-base-map "\C-m" 'newline-and-indent)
  ;;(define-key c-mode-base-map ")" 'jsy-c-electric-close-paren)
  )

(my/custom-set-variables
 '(c-tab-always-indent nil)
 '(c-insert-tab-function 'my/c-insert-tab-function)
 )

(defun my/c-insert-tab-function ()
  "Try hippie-expand devolving ultimately to insert-tab."
  (hippie-expand nil))

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
;;{{{  assembly language

(my/custom-set-variables
 '(asm-comment-char 35))                ; = '#'

;;}}}
;;{{{  GDB support

(eval-after-load "gud" '(progn
  (defun my/gud-cont-to-tbreak ()
    "Run to cursor"
    (interactive)
    (gud-tbreak)(gud-cont))

  (defun my/gud-stepi ()
    "Step one instruction then display the next instruction"
    (interactive)
    (gud-stepi 1)(gud-call "x/i $pc"))

  (defun my/gud-regs ()
    "Display register in tabular format."
    (interactive)
    (gud-call "regs"))

  (defun my/gud-keys ()
    "Display function key bindings in tabular format."
    (interactive)
    (gud-call "my/keys"))
  ))

;;}}}
;;{{{  Cedet, semantic, etc

;; (my/custom-set-variables
;;  '(semantic-mode t)
;;  '(semanticdb-default-save-directory "/home/jyates/.emacs.d/semanticdb")
;;  )

;;}}}

;;=== Uncatergorized ===================================================
;;{{{  emacsclient and server

(my/custom-set-variables
 '(server-done-hook '(delete-frame))
 '(server-window 'switch-to-buffer-other-frame)
 '(server-switch-hook 'my/pop-file-name-history) ; omit emacsclient temps
;; '(server-kill-new-buffers t) ; [DEF] if client is waiting kill its NEW buffer
;; '(server-raise-frame t)      ; [DEF]
 '(display-buffer-reuse-frames t)
 '(server-temp-file-regexp "^/tmp/\\|.*\\.tmp")
 )

(server-start)

(defun my/pop-file-name-history ()
  "Pop most recent filename from the history."
  (setq file-name-history (cdr file-name-history)))

;;}}}
;;{{{  Performance

(my/custom-set-variables
 '(gc-cons-threshold 50000000)
 '(message-log-max 10000)
 )

;;}}}
;;{{{  keydef

(add-to-list 'el-get-sources
             '(:name keydef
                     :type http
                     :description "A simpler way to define keys, with kbd syntax"
                     :url "file://localhost/home/jyates/.emacs.d/emacs-goodies-el/elisp/emacs-goodies-el/keydef.el"
                     :features (keydef)))

;;}}}

;;=== el-get (epilog) ==================================================
;;{{{  Sync and update

;; Use update all when first configuring a new machine or user
;; (el-get-update-all)

(el-get 'wait)

;;}}}

;;=== Commented out ====================================================
;;{{{  Customizations

;; (my/custom-set-variables
;;  '(align-to-tab-stop t) ; align.el

;;  '(ansi-color-faces-vector [default bold default italic underline bold bold-italic modeline])
;;  '(ansi-color-names-vector ["black" "IndianRed1" "medium spring green" "khaki1" "DodgerBlue1" "maroon1" "DarkSlateGray1" "white"])

;;  '(c-basic-offset 4)
;;  '(c-font-lock-extra-types (quote ("FILE" "\\sw+_[hpt]")))

;;  '(comint-highlight-input nil)

;;  '(dired-listing-switches "-alFv")

;;  '(dvc-prefix-key [(control c) 100])
;;  '(dvc-tips-enabled nil)

;;  '(ebnf-justify-sequence (quote left))
;;  '(ebnf-non-terminal-shape (quote miter))
;;  '(ebnf-production-font (quote (10 Helvetica "White" "White" bold)))
;;  '(ebnf-terminal-shape (quote round))

;;  '(ecb-compile-window-height 5)
;;  '(ecb-compile-window-width (quote edit-window))
;;  '(ecb-layout-name "left5")
;;  '(ecb-options-version "2.33beta2")
;;  '(ecb-scroll-other-window-scrolls-compile-window t)
;;  '(ecb-windows-width 0.2)

;;  '(flyspell-incorrect-hook (quote (flyspell-maybe-correct-doubling flyspell-maybe-correct-transposition)))

;;  '(gdb-many-windows t)
;;  '(gdb-use-separate-io-buffer t)
;;  '(gud-tooltip-mode t)
;;  '(tooltip-gud-tips-p t)

;;  '(help-at-pt-timer-delay 0.5)

;;  '(package-archives
;;    '(("elpa" . "http://tromey.com/elpa/")
;;      ("marmalade" . "http://marmalade-repo.org/packages/")
;;      ("sc" . "http://joseito.republika.pl/sunrise-commander/")
;;      ("gnu" . "http://elpa.gnu.org/packages/")
;;      ))
;;  '(package-user-dir "~/emacs/elpa")

;;  '(recentf-mode t nil (recentf))

;;  '(same-window-buffer-names
;;    '("*Buffer List*"
;;      "*ielm*"
;;      "*inferior-lisp*"
;;      "*mail*"
;;      "*scheme*"
;;      "*shell*"
;;      ))

;;  '(speedbar-frame-parameters
;;    '((minibuffer)
;;      (width . 20)
;;      (border-width . 0)
;;      (menu-bar-lines . 0)
;;      (tool-bar-lines . 0)
;;      (unsplittable . t)
;;      (set-background-color "black")
;;      ))

;;  '(widget-field ((((class grayscale color) (background light)) (:background "DarkBlue"))))
;; )

;; (my/custom-set-faces
;;  '(background "blue")

;;  '(makefile-space-face ((t (:background "wheat"))) t)

;;  '(paren-match ((t (:background "darkseagreen4"))))
;;  '(show-paren-match ((t (:foreground "black" :background "wheat"))))
;;  '(show-paren-mismatch ((((class color)) (:foreground "white" :background "red"))))

;;  '(speedbar-button-face ((((class color) (background dark)) (:foreground "green4"))))
;;  '(speedbar-directory-face ((((class color) (background dark)) (:foreground "khaki"))))
;;  '(speedbar-file-face ((((class color) (background dark)) (:foreground "cyan"))))
;;  '(speedbar-tag-face ((((class color) (background dark)) (:foreground "Springgreen"))))

;;  '(tool-bar ((t (:background "wheat1" :foreground "black" :box (:line-width 1 :style released-button)))))
;;  )

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

(require 'keydef)       ; simpler key definitions, autoloads for free


(keydef "C-c C-c M-x"   execute-extended-command) ; original M-x overridden by smex

(keydef "C-c C-k"       kill-compilation)
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
(keydef "C-c w f"       confluence-get-page) ; open page on confluence wiki


;; Additions to the help command
;;
(keydef "C-h A"         apropos-toc)       ; mnemonic: apropos All
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

(keydef "C-x ,"         am-find-file)
(keydef "C-x 4 ,"       am-find-file-other-window)


;; Additions to binding.el's goto-map; prior bindings:
;;  g   goto-line
;;  n   next-error
;;  p   previous-error
;;
(keydef "M-g b"         bookmark-jump)
(keydef "M-g M-b"       bookmark-jump)
(keydef "M-g l"         ilocate-library-find-source)
(keydef "M-g r"         jump-to-register)
(keydef "M-g M-r"       jump-to-register)

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

(keydef "M-x"           smex)
(keydef "M-X"           smex-major-mode-commands)

(keydef "<f1>"          bs-show)
(keydef "C-<f1>"        my/named-shell)
(keydef "M-<f1>"        menu-bar-mode)

(keydef "<f2>"          disk)


;; Strong similarity to MS Visual Studio's function keys
(keydef    "<f4>"       next-error)
(keydef  "C-<f4>"       first-error)

(eval-after-load "gud" '(progn
  (keydef   "<f5>"      gud-cont)          ; MS go / continue
  (keydef "C-<f5>"      my/gud-cont-to-tbreak) ; MS run to cursor
  (keydef "M-<f5>"      gud-run)           ; restart
  ))

(keydef   "C-<f7>"      compile)

(eval-after-load "gud" '(progn
  (keydef   "<f8>"      my/gud-regs)       ; show registers
  (keydef "C-<f8>"      my/gud-keys)       ; show key bindings

  (keydef   "<f9>"      gud-step)          ; MS step into
  (keydef "C-<f9>"      my/gud-stepi)
  (keydef "M-<f9>"      gud-down)

  (keydef   "<f10>"     gud-next)          ; MS step over
  (keydef "C-<f10>"     gud-finish)        ; MS step out
  (keydef "M-<f10>"     gud-up)

  (keydef   "<f11>"     gud-break)
  (keydef "C-<f11>"     gud-tbreak)
  (keydef "M-<f11>"     gud-remove)
  ))

(keydef "<f12>"         customize-option)
(keydef "C-<f12>"       customize-group)
(keydef "M-<f12>"       customize-apropos)


(eval-after-load "bs" '(keydef (bs "<f1>")  bs-kill))

;;}}}

;;======================================================================
;; Local Variables:
;; comment-column: 40
;; folded-file: t
;; End:

;;---------------------------------------------------------------------
;; Experimental trash... do not commit if there is anything here!
