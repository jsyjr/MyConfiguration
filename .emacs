;; Time-stamp: "2012-01-24 23:31:13 jyates"

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
;;{{{  Setup

;; Gnome system:  http://www.emacswiki.org/emacs/MovingTheCtrlKey#toc2
;;                I chose "Make Caps Lock an additional Ctrl"

;; Directories
;; ~/.emacs.d/auto-save-Confluence
;; ~/.emacs.d/backup

;;}}}
;;{{{  Missing

;; swap caps lock and control
;; Sanjay Dixit's am package - add autoload cookies, use el-get
;; filladapt
;; align
;; occur
;; grep
;; sort lines
;; collapse to a single space
;; dvc, magit
;; e/vtags

;;}}}

;;--

;;{{{  Augment LOAD-PATH

(defvar my/load-path-roots '("~/emacs" "~/.emacs.d")
  "Ordered list of root directory paths for augmenting load-path.
Each root will be searched recursively.  Any directory containing
at least one .el[c] or .el[c].gz file will be added to load-path.")

(defun my/collect-lisp-dirs (dir)
  "Return directories at or below DIR with .el[c][.gz] files."
  (if (not (file-accessible-directory-p dir))
      nil ; Silently ignore inaccessible or non-existent directories
    (let* ((home      (expand-file-name "~"))
           (add-this  nil)
	   (this-dir  (expand-file-name dir))
	   (all-files (directory-files this-dir t))
	   (file      nil)
	   (sub-dirs  nil))
      (dolist (file all-files)
        (cond
         ((file-directory-p file)
          (if (not (string-match "/\\.\\.?$" file))  ; Ignore "." and ".."
              (setq sub-dirs (append sub-dirs (my/collect-lisp-dirs file)))))
         ((and (not add-this) (string-match "\\.elc?\\(\\.gz\\)?$" file))
          (setq add-this (list this-dir)))))
      (append add-this sub-dirs))))

(dolist (root (nreverse my/load-path-roots))
  (dolist (dir (nreverse (my/collect-lisp-dirs root)))
    (add-to-list 'load-path (expand-file-name dir))))

;;}}}

;;=== Customization ====================================================
;;{{{  Auditing framework

(defvar my/custom-variables nil
 "List of customizations to be compared to those in the custom file.")

(defvar my/custom-faces nil
  "List of customizations to be compared to those in the custom file.")

(defun my/custom-set-variables (&rest args)
  "Accumulate ARGS on 'my/custom-variables."
  (dolist (entry args) (add-to-list
                        'my/custom-variables entry)))

(defun my/custom-set-faces (&rest args)
  "Accumulate ARGS on 'my/custom-faces."
  (dolist (entry args) (add-to-list
                        'my/custom-faces entry)))

;;}}}
;;{{{  Auditing functions

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

(defun my/check-custom-file ()
  "Audit customized variables and faces against the custom file."
  (let ((disk-lists
         (with-current-buffer (get-buffer-create "*Custom File*")
           (insert-file-contents-literally custom-file)
           (let ((lists (list (cdr (read (current-buffer)))
                              (cdr (read (current-buffer))))))
             (kill-buffer)
             lists))))
    (my/check-custom-list "variables" (car disk-lists) my/custom-variables)
    (my/check-custom-list "faces" (cadr disk-lists) my/custom-faces)))

;;}}}
;;{{{  Load customizations

(setq custom-file "~/emacs/custom-file.el")
(load custom-file)

;; Host specific initialization if it exists (my-rc-local-HOST.el[c])
(require (intern (concat "my-rc-local-"
                         (let* ((host (system-name))
                                (idx (string-match "\\." host)))
                           (if idx
                               (substring host 0 idx)
                             host))))
         nil t)
;;}}}

;;=== Package management ===============================================
;;{{{  el-get (booststrap and init)

;; Minimal bootstrap
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))

(setq el-get-recipe-path  '("~/.emacs.d/el-get/el-get/recipes/"))

(require 'inversion nil t) ; fix broken autoload in cedet/common/cedet-compat.el

(setq el-get-sources '(el-get)) ; built incrementally via add-to-list

;;}}}
;;{{{  Debian emacs-goodies package

(add-to-list 'el-get-sources 'emacs-goodies-el)

;;}}}

;;=== Coding convenience ===============================================
;;{{{  emacs-goodies/diminish - short minor modes in mode-line

(add-to-list 'el-get-sources 'diminish)

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
;;{{{  emacs-goodies/keydef - simpler definitions, autoloads for free

(require 'keydef)       ; simpler key definitions, autoloads for free

;;}}}

;;=== Protection =======================================================
;;{{{  Backup

(my/custom-set-variables
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backup"))))
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

;;=== Uncatergorized ===================================================
;;{{{  Always use y/n instead of yes/no

(fset 'yes-or-no-p 'y-or-n-p)

;;}}}
;;{{{  Performance

(my/custom-set-variables
 '(gc-cons-threshold 50000000)
 '(message-log-max 10000)
 )

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
 '(indicate-buffer-boundaries (quote right)) ; graphics in fringe
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
;;{{{  Cursor

(my/custom-set-variables
 '(blink-cursor-delay 0.1)
 '(cua-mode t nil (cua-base))
 '(cua-enable-cursor-indications t)
 '(cua-normal-cursor-color (quote (bar . "Orange")))
 '(cua-overwrite-cursor-color (quote (box . "HotPink1")))
 '(cua-read-only-cursor-color (quote (box . "SeaGreen1")))
 '(x-stretch-cursor t)
 )

(my/custom-set-faces
 '(cursor ((t (:background "gold"))))
 )

;;}}}
;;{{{  Basic faces

(my/custom-set-faces
 '(fixed-pitch ((t nil)))
 '(variable-pitch ((t (:height 0.9 :family "Sans Serif"))))
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
 '(highlight ((t (:background "CornflowerBlue"))))
 '(highlight-beyond-fill-column-face ((t (:inverse-video t))))
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
 '(which-func-format (quote ("[" (:propertize which-func-current local-map (keymap (mode-line keymap (mouse-3 . end-of-defun) (mouse-2 . #[nil "e\300=\203	 \301 \207~\207" [1 narrow-to-defun] 2 nil nil]) (mouse-1 . beginning-of-defun))) face which-func help-echo "Function (enclosing or preceding)
mouse-1: go to beginning
mouse-2: narrow to function
mouse-3: go to end") "]")))
 '(which-function-mode t nil (which-func))
 )

;;}}}
;;{{{  Uniquify buffer names

(require 'uniquify)

(my/custom-set-variables
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)) ; prefix '/'
 )

;;}}}
;;{{{  Pretty ^L

(add-to-list 'el-get-sources
             '(:name  pp-c-l
                      :after (lambda ()
                               (add-hook 'window-setup-hook
                                         'refresh-pretty-control-l)
                               (add-hook 'window-configuration-change-hook
                                         'refresh-pretty-control-l))))

;; Universally display ^L as a window-width horizontal rule
(my/custom-set-variables
 '(pretty-control-l-mode t)
 '(pp^L-^L-string-function (lambda (win) (make-string (1- (window-width win)) 32)))
                                        ;'(pp^L-^L-string-pre "")               ; eliminate preceding blank line
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
;;{{{  Other stuff

;; Display Windows extended glyphs (instead of things like \223)
(standard-display-8bit 128 255)

;;}}}

;;=== Searching and editing ============================================

(my/custom-set-variables
 '(cua-enable-cua-keys nil)
 '(delete-selection-mode t)
 '(fill-column 74)
 '(kill-whole-line t)
 )

;;{{{  Occur

;; (autoload 'occur "replace"
;;   "Show all lines in the current buffer containing a match for REGEXP.

;; \(fn REGEXP &optional NLINES)" t)

;;}}}
;;{{{  Filling

(my/custom-set-variables
 '(text-mode-hook
   '(text-mode-hook-identify turn-on-auto-fill))
 )

;;}}}
;;{{{  Whilte space hygine

(my/custom-set-variables
 '(indicate-empty-lines t)
 '(require-final-newline (quote query))
 '(show-trailing-whitespace t)
 '(whitespace-global-mode t)
 )

(my/custom-set-faces
 '(trailing-whitespace ((((class color) (background dark)) (:background "red3"))))
 )

;;}}}
;;{{{  Indent yanked text (cribbed from Alex Ott)

(defvar my/yank-indent-modes
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
 '(tab-stop-list '(  4   8  12  16  20  24  28  32  36  40
                         44  48  52  56  60  64  68  72  76  80
                         84  88  92  96 100 104 108 112 116 120
                         124 128 132 136 140 144 148 152 165 160
                         164 168 172 176 180 184 188 192 196 200
                         204 208 212 216 220 224 228 232 236 240
                         244 248 252 265 260 264 268 272 276 280
                         284 288 292 296 300 304 308 312 316 320))
 )

(defun canonicalize-tab4 (&rest ignored)
  (interactive "P")
  (let ((tab-width 4))
    (untabify (point-min) (point-max))
    (tabify (point-min) (point-max))))

;;}}}

;;=== Utilities ========================================================
;;{{{  Update timestamps before saving files

(add-hook 'before-save-hook 'time-stamp)

;;}}}
;;{{{  Always byte compile after saving elisp

(defun my/byte-compile-saved-elisp-buffer ()
  "Byte compile an elisp buffer anytime it is saved."
  (if (eq major-mode 'emacs-lisp-mode)
      (byte-compile-file (buffer-file-name))))

(add-hook 'after-save-hook 'my/byte-compile-saved-elisp-buffer)

;;}}}
;;{{{  Completion

(my/custom-set-variables
 '(completion-ignored-extensions 
   (quote (".a"
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
 '(ido-mode 'both nil (ido))
 )

;;}}}
;;{{{  emacsclient and server

(my/custom-set-variables
 '(server-done-hook (quote (delete-frame)))
 '(server-window (quote switch-to-buffer-other-frame))
 )

(server-start)

;;}}}
;;{{{  Help (apropos, info, etc)

(add-to-list 'el-get-sources
             '(:name        apropos-toc
                            :description "XEmacs-ish hyper-apropos for GNUEmacs"
                            :type        http
                            :url         "http://www.cbrunzema.de/download/apropos-toc/apropos-toc.el"
                            :localname   "apropos-toc.el"
                            :features    (apropos-toc)))

(my/custom-set-variables
 '(apropos-do-all t)                    ; invert sense of prefix arg
 )

;; This is serach order so we hope that function names are unique
(defvar my/elisp-manuals
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

;;}}}
;;{{{  ilocate-library

(add-to-list 'el-get-sources
             '(:name        ilocate-library
                            :description "Interactive locate-library (or source) with completion"
                            :type        emacsmirror
                            :url         "https://github.com/emacsmirror/ilocate-library.git"
                            :localname   "ilocate-library.el"
                            :features    (ilocate-library)))

;;}}}
;;{{{  Named shells

(defun my/named-shell (BUFFER)
  "Create or switch to a running shell process in BUFFER."
  (interactive "BShell buffer: ")
  (shell BUFFER))

;;}}}

;;=== Minor modes ======================================================
;;{{{  folding

(add-to-list 'el-get-sources
             '(:name  folding
                      :after folding-mode-add-find-file-hook))

(my/custom-set-variables
 '(folding-advice-instantiate nil)      ; not advising M-g g
 '(folding-goto-key "\M-gf")            ; Restore M-g's prefix behavior
 )

;;}}}

;;=== Major modes ======================================================
;;{{{  Org

(my/custom-set-variables
 `(org-hide-leading-stars t)
 '(org-default-notes-file "~/org/captute.org")
 '(org-modules
   '(org-docview                ; Links to doc-view buffers
     org-info                   ; Links to Info nodes
     org-habit                  ; Track your consistency with habits
     org-inlinetask             ; Tasks independent of outline hierarchy
     org-protocol               ; Intercept calls from emacsclient
     org-mouse                  ; Additional mouse support
     ))
 `(org-mobile-directory "~/Dropbox/MobileOrg")
 '(org-mobile-files '(org-agenda-files "~/org"))
 )

;;}}}
;;{{{  Confluence wiki

(add-to-list 'el-get-sources
             '(:name        confluence
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

(eval-after-load "confluence"
  '(progn
     (require 'longlines)
     (progn
       (add-hook 'confluence-mode-hook 'longlines-mode)
       (add-hook 'confluence-before-save-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-before-revert-hook 'longlines-before-revert-hook)
       (add-hook 'confluence-mode-hook '(lambda () (local-set-key "\C-j" 'confluence-newline-and-indent))))))

;; open confluence page
(global-set-key "\C-xwf" 'confluence-get-page)

;; setup confluence mode
(add-hook 'confluence-mode-hook
          '(lambda ()
             (local-set-key "\C-xw" confluence-prefix-map)))

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
               '(lambda ()
                  (dolist (tmp-buf (list ediff-buffer-A
                                         ediff-buffer-B
                                         ediff-buffer-C))
                    (if (buffer-live-p tmp-buf)
                        (with-current-buffer tmp-buf
                          (longlines-restore))))))))

;;}}}

;;=== Programming ======================================================
;;{{{  Sanjay Dixit's am package

(autoload 'am-scan "am" nil t)

(autoload 'am-find-file "am"
  "Find file known to amake."
  t)

(autoload 'am-find-file-other-window "am"
  "Find file known to amake, open it in another window."
  t)

;;}}}
;;{{{  C/C++ mode hooks

(defun my/activate-semantic-imenu ()
  ""
  (require 'semantic/imenu)
  (setq imenu-create-index-function 'semantic-create-imenu-index))

(my/custom-set-variables
 '(c-mode-hook    '(turn-on-auto-fill
;;                  turn-off-filladapt-mode
                    my/activate-semantic-imenu))
 '(c++-mode-hook  '(turn-on-auto-fill
;;                  turn-off-filladapt-mode
                    my/activate-semantic-imenu))
 '(java-mode-hook '(turn-on-auto-fill
;;                  turn-off-filladapt-mode
                    my/activate-semantic-imenu))
 )

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

;;=== el-get (epilog) ==================================================
;;{{{  Sync and update

(el-get 'sync)

;; Use update all when first configuring a new machine or user
;; (el-get-update-all)

;;}}}

;;=== Commented out ====================================================
;;{{{  Old whiz-bang

;;;; setnu (el-get)

;; Provide support for numbered lines via M-x setnu-mode
                                        ;(require 'setnu)

;;}}}
;;{{{  Customizations

;; (my/custom-set-variables
;;  '(align-to-tab-stop t) ; align.el

;;  '(ansi-color-faces-vector [default bold default italic underline bold bold-italic modeline])
;;  '(ansi-color-names-vector ["black" "IndianRed1" "medium spring green" "khaki1" "DodgerBlue1" "maroon1" "DarkSlateGray1" "white"])

;;  '(c-basic-offset 4)
;;  '(c-font-lock-extra-types (quote ("FILE" "\\sw+_[hpt]")))
;;  '(c-tab-always-indent nil)

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

(my/check-custom-file)

;;}}}
;;{{{  Key bindings

(keydef "C-c -"      replace-string)
(keydef "C-c C--"    query-replace-string)
(keydef "C-c ="      replace-regexp)
(keydef "C-c C-="    query-replace-regexp)
(keydef "C-c 4"      my/set-buffer-local-tab-width-to-4)
(keydef "C-c 8"      my/set-buffer-local-tab-width-to-8)
(keydef "C-c c"      org-capture)

;; Additions to the help command
;;
(keydef "C-h A"      apropos-toc)       ; mnemonic: apropos All
(keydef "C-h L"      (info "elisp"))    ; was describe-language-environment
(keydef "C-h R"      my/elisp-function-reference)


(keydef "C-x C-b"    bs-show)           ; same binding as <f1>

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
(keydef "C-x E"      apply-macro-to-region-lines)

(keydef "C-x ,"      am-find-file)
(keydef "C-x 4 ,"    am-find-file-other-window)

;; Additions to binding.el's goto-map; prior bindings:
;;  g   goto-line
;;  n   next-error
;;  p   previous-error
;;
(keydef "M-g b"      bookmark-jump)
(keydef "M-g M-b"    bookmark-jump)
(keydef "M-g l"      ilocate-library-find-source)
(keydef "M-g r"      jump-to-register)
(keydef "M-g M-r"    jump-to-register)

;; Additions to binding.el's search-map; prior bindings:
;;  h*  highlight-*/hi-lock-*
;;  o   occur
;;  w   isearch-forward-word
;;
(keydef "M-s g"      grep)
(keydef "M-s l"      lgrep)
(keydef "M-s m"      multi-occur-in-matching-buffers)
(keydef "M-s r"      rgrep)


(keydef "<f1>"       bs-show)
(keydef "C-<f1>"     my/named-shell)


;; Strong similarity to MS Visual Studio's function keys
(keydef    "<f4>" next-error)
(keydef  "C-<f4>" first-error)

(eval-after-load "gud" '(progn
  (keydef   "<f5>"   gud-cont)          ; MS go / continue
  (keydef "C-<f5>"   my/gud-cont-to-tbreak) ; MS run to cursor
  ))

(keydef   "C-<f7>"   compile)

(eval-after-load "gud" '(progn
  (keydef   "<f8>"   my/gud-regs)       ; show registers
  (keydef "C-<f8>"   my/gud-keys)       ; show key bindings

  (keydef   "<f9>"   gud-step)          ; MS step into
  (keydef "C-<f9>"   my/gud-stepi)
  (keydef "M-<f9>"   gud-down)

  (keydef   "<f10>"  gud-next)          ; MS step over
  (keydef "C-<f10>"  gud-finish)        ; MS step out
  (keydef "M-<f10>"  gud-up)

  (keydef   "<f11>"  gud-break)
  (keydef "C-<f11>"  gud-tbreak)
  (keydef "M-<f11>"  gud-remove)
  ))

(keydef "<f12>"      customize-apropos)
(keydef "C-<f12>"    customize-group)


(eval-after-load "bs" '(keydef (bs "<f1>")  bs-kill))

;;}}}

;;======================================================================
;; Local Variables:
;; comment-column: 40
;; folded-file: t
;; End:

;;---------------------------------------------------------------------
;; Experimental trash... do not commit if there is anything here!
