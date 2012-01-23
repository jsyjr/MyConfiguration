;; Time-stamp: "2012-01-20 09:52:32 jyates"

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty ofn
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;;== Customization ====================================================
;;{{{  Accumulate

(defvar my/customized-variables nil "List of customizations to be
  compared to those in the custom file.")

(defvar my/customized-faces nil "List of customizations to be compared
  to those in the custom file.")

(defun my/customize-variables (&rest args) "Accumulate ARGS on
'my/customized-variables."  (dolist (entry args) (add-to-list
'my/customized-variables entry)))

(defun my/customize-faces (&rest args) "Accumulate ARGS on
'my/customized-faces."  (dolist (entry args) (add-to-list
'my/customized-faces entry)))

;;}}}
;;{{{  Audit

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
    (my/check-custom-list "variables" (car disk-lists) my/customized-variables)
    (my/check-custom-list "faces" (cadr disk-lists) my/customized-faces)))

;;}}}

;;;== Package management ===============================================
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

;;;== Coding convenience ===============================================
;;{{{  diminish (from emacs-goodies)

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
;;{{{  keydef (from emacs-goodies)

(eval-when-compile
  (require 'keydef))      ; simpler key definitions, autoloads for free

;;}}}

;;;== Protection =======================================================
;;{{{  Backup

(my/customize-variables
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

(my/customize-variables
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

;;;== Visuals ==========================================================
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
        (buffer-file-name ("  [ " (:eval (file-name-directory (abbreviate-file-name (file-truename buffer-file-name)))) " ]") nil)))

;;}}}
;;{{{  Frame appearance

(my/customize-variables
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

(my/customize-faces
 '(fringe ((((class color) (background dark)) (:background "grey15"))))
 )

;;}}}
;;{{{  Menu bar

(my/customize-variables
 '(menu-bar-mode nil)
)

;;}}}
;;{{{  Minibuffer

(my/customize-variables
 '(minibuffer-frame-alist
   '((height . 2)
     (fullscreen . fullwidth)
     (user-size . t)
     (top - 0)
     (left . 0)
     (user-position . t)
     (name . "Emacs minibuffer    (CTRL + click to show/hide other Emacs frames)")))
 )

(my/customize-faces
 '(minibuffer-prompt ((t (:foreground "orange" :weight bold))))
 )

;;}}}
;;{{{  Cursor

(my/customize-variables
 '(blink-cursor-delay 0.1)
 '(cua-mode t nil (cua-base))
 '(cua-enable-cursor-indications t)
 '(cua-normal-cursor-color (quote (bar . "Gold")))
 '(cua-overwrite-cursor-color (quote (box . "HotPink1")))
 '(cua-read-only-cursor-color (quote (box . "SeaGreen1")))
 '(x-stretch-cursor t)
 )

(my/customize-faces
 '(cursor ((t (:background "gold"))))
 )

;;}}}
;;{{{  Basic faces

(my/customize-faces
 '(fixed-pitch ((t nil)))
 '(variable-pitch ((t (:height 0.9 :family "Sans Serif"))))
 )

;;}}}
;;{{{  Font lock faces

(my/customize-faces
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

(my/customize-faces
 '(highlight ((t (:background "CornflowerBlue"))))
 '(highlight-beyond-fill-column-face ((t (:inverse-video t))))
 '(region ((t (:background "DarkSlateBlue"))))
 )

;;}}}
;;{{{  mode-line basics

(my/customize-faces
 '(mode-line ((t (:background "tan2" :foreground "black" :box (:line-width -1 :style released-button) :height 1.2))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-highlight ((t (:background "wheat2" :box (:line-width -1 :style released-button)))))
 '(mode-line-inactive ((default (:inherit mode-line)) (((class color) (min-colors 88) (background dark)) (:background "grey70"))))
 )

(my/customize-variables
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

(my/customize-variables
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

(my/customize-variables
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

(my/customize-variables
 '(which-func-format (quote ("[" (:propertize which-func-current local-map (keymap (mode-line keymap (mouse-3 . end-of-defun) (mouse-2 . #[nil "e\300=\203	 \301 \207~\207" [1 narrow-to-defun] 2 nil nil]) (mouse-1 . beginning-of-defun))) face which-func help-echo "Function (enclosing or preceding)
mouse-1: go to beginning
mouse-2: narrow to function
mouse-3: go to end") "]")))
 '(which-function-mode t nil (which-func))
 )

;;}}}
;;{{{  Uniquify buffer names

(require 'uniquify)

(my/customize-variables
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
(my/customize-variables
 '(pretty-control-l-mode t)
 '(pp^L-^L-string-function (lambda (win) (make-string (1- (window-width win)) 32)))
;'(pp^L-^L-string-pre "")               ; eliminate preceding blank line
 )

(my/customize-faces
 '(pp^L-highlight ((((type x w32 mac graphic) (class color)) (:inherit shadow :strike-through t))))
 )

;;}}}
;;{{{  Random custom items

(my/customize-variables
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

(my/customize-variables
 '(tooltip-delay 0.3)
 )

(my/customize-faces
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

;;;== Editing ==========================================================

(my/customize-variables
 '(cua-enable-cua-keys nil)
 '(delete-selection-mode t)
 '(fill-column 79)
 '(kill-whole-line t)
 )

;;{{{  Whilte space hygine

(my/customize-variables
 '(indicate-empty-lines t)
 '(require-final-newline (quote query))
 '(show-trailing-whitespace t)
 '(whitespace-global-mode t)
 )

(my/customize-faces
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

(my/customize-variables
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

;;}}}

;;;== Utilities ========================================================
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

(my/customize-variables
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
 )

;;}}}
;;{{{  emacsclient and server

(my/customize-variables
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

(my/customize-variables
 '(apropos-do-all t)                    ; invert sense of prefix arg
 )

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

(my/customize-variables
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
               :description "interactive locate-library (or source) with completion"
               :type        emacsmirror
               :url         "https://github.com/emacsmirror/ilocate-library.git"
               :localname   "ilocate-library.el"
               :features    (ilocate-library)))

;;}}}

;;;== Minor modes ======================================================
;;{{{  folding

(add-to-list 'el-get-sources
             '(:name  folding
               :after folding-mode-add-find-file-hook))

(my/customize-variables
 '(folding-advice-instantiate nil)      ; not advising M-g g
 '(folding-goto-key "\M-gf")            ; Restore M-g's prefix behavior
 )

;;}}}

;;;== Major modes ======================================================
;;{{{  UNUSED


;;}}}

;;;== Uncatergorized ===================================================
;;{{{  Always use y/n instead of yes/no

(fset 'yes-or-no-p 'y-or-n-p)

;;}}}
;;{{{  Performance

(my/customize-variables
 '(gc-cons-threshold 50000000)
 '(message-log-max 10000)
 )

;;}}}

;;;== el-get (epilog) ==================================================
;;{{{  Sync and update

(el-get 'sync)

;; Use update all when first configuring a new machine or user
;; (el-get-update-all)

;;}}}

;;;== Commented out ====================================================
;;{{{  Old whiz-bang

;;;; setnu (el-get)

;; Provide support for numbered lines via M-x setnu-mode
;(require 'setnu)

;;}}}
;;{{{  Customizations

;; (my/customize-variables
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

;; (my/customize-faces
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

;;;== Now that all packages have been processed ========================
;;{{{  Apply customizations

(apply 'custom-set-variables my/customized-variables)
(apply 'custom-set-faces     my/customized-faces)

(my/check-custom-file)

;;}}}
;;{{{  Key bindings

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

(keydef "C-h A"      apropos-toc)       ; mnemonic: apropos All
(keydef "C-h L"      (info "elisp"))    ; was describe-language-environment

(keydef "C-x C-j"    ilocate-library-find-source)

(keydef "C-c -"      replace-string)
(keydef "C-c ="      replace-regexp)
(keydef "C-c l"      my/elisp-function-reference)
(keydef "C-c C-j"    grep)
(keydef "C-c 4"      my/set-buffer-local-tab-width-to-4)
(keydef "C-c 8"      my/set-buffer-local-tab-width-to-8)

(keydef "M-g b"      bookmark-jump)
(keydef "M-g M-b"    bookmark-jump)
(keydef "M-g r"      jump-to-register)
(keydef "M-g M-r"    jump-to-register)

(keydef "<f12>"      customize-apropos)
(keydef "C-<f12>"    customize-group)

(keydef "C-x C-b"    bs-show)
(keydef "<f1>"       bs-show)
(eval-after-load "bs" '(keydef (bs "<f1>")  bs-kill))

;;}}}

;;;=====================================================================
;; Local Variables:
;; comment-column: 40
;; folding-mode: t
;; folded-file: t
;; End:
