;;; init.el -*- lexical-binding: t; outshine-mode: 1; fill-column: 119 -*-

;(setq debug-on-error t)
;(debug-on-variable-change ??)
(setq native-comp-async-report-warnings-errors nil)
(setq warning-minimum-level :error)

;; Reference by graphviz-dot-mode
(defconst default-tab-width 4)

;; This program is free software; you can redistribute it and/or
;; modify it as you will.  I place it in the public domain.  I do
;; not need to be credited in anyway.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

;;; Commentary:
;; A bare-boned config template. Use "outshine-cycle-buffer" (<Tab> and <S-Tab>
;; in org style) to navigate through sections, and "imenu" to locate individual
;; use-package definition.

;;; === Early Init =====================================================

;; Delay garbage collection (likely for full startup process, reset in init.el)
;;(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-threshold 8000000)

;; Prevent unwanted runtime builds in gccemacs (native-comp); packages are
;; compiled ahead-of-time when they are installed and site files are compiled
;; when gccemacs is installed.
(setq comp-deferred-compilation nil)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
;; (advice-add #'x-apply-session-resources :override #'ignore)
(setq inhibit-x-resources nil)

;; See discussion of gnome shell bug
;; https://github.com/muffinmad/emacs-mini-frame#gnome-shell-does-not-resize-emacs-child-frames
(setq x-gtk-resize-child-frames 'resize-mode)

;;; === To investigate =================================================

;; My "Navigating an enormous code base" posting on Redit
;; https://www.reddit.com/r/emacs/comments/uc25wx/navigating_an_enormous_code_base

;; Extending project.el
;; https://manueluberti.eu/emacs/2020/11/14/extending-project/

;; Lots of ideas for speeding up startup
;; https://www.reddit.com/r/emacs/comments/u8o4vz/starting_fresh

;; Cursor blinking via fade-in / fade-out
;; https://www.reddit.com/r/emacs/comments/u9omcp/pulsingcursormode_ui_tweak_cursor_animation_fades

;; Electric cursor (better abstraction?, currently no readonly)
;; https://github.com/duckwork/electric-cursor

;;; === Missing and TODO  (NEEDS REVIEW) ===============================

;; TODO
;; - Work through https://github.com/bitspook/spookmax.d
;; - Work through https://codeberg.org/jao/elibs
;;
;; - Audit all :defer n
;; - Audit all :disabled
;; - Audit :requires and try to replace with :after
;; - Audit :demand t
;; - Highlight current error in compile buffer
;;   https://emacs.stackexchange.com/questions/13134/emphasise-the-current-error-in-the-compilation-window
;; - imerge
;; - include info-apropos in help menu
;; - mode-line:
;;   position-widget could change color if cursor exceeds limit column
;;   see http://www.emacswiki.org/emacs/ModeLinePosition
;;   consider https://github.com/Malabarba/smart-mode-line
;; - Searching:
;;   see https://github.com/nschum/highlight-symbol.el
;; - Window resizing:
;;   see https://github.com/roman/golden-ratio.el
;; - package management
;;   consider https://github.com/Malabarba/paradox
;; - face colors might be improved via an online gradient generator
;;   https://www.colorhexa.com/

;; IMO a much more handy UI metaphor is dragging the line at point
;; up or down through the surrounding lines while preserving the
;; point's position in the line. That is implemented in
;; org-drag-line-forward and org-drag-line-backward. Pretty
;; much nothing in their implementations depends on org-mode, so
;; they can be bound and used in any mode.


;; ;; See the Misc[ellaneous System] Events node of the Emacs Lisp manual:
;; (global-set-key [usr1-signal]
;;               (lambda ()
;;                 (interactive)
;;                 (save-buffers-kill-emacs t)))

;; A possible shell script for P4MERGE, HGMERGE, ...
;;
;; emacsclient -ucF "((delete-frame-on-ediff-quit . t))" \
;;   -e "(ediff-merge-with-ancestor \"${local}\" \"${other}\" \"${base}\"
;; nil \"${output}\")" \
;;   -e "(add-hook 'ediff-quit-hook (lambda () (when (frame-parameter nil
;; 'delete-frame-on-ediff-quit) (delete-frame))))"

;; See Emanuel Berg's error.el: http://user.it.uu.se/~embe8573/emacs-init/error.el

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

;; (add-to-list 'compilation-error-regexp-alist
;;             '("^\\(?:..\\[[ 0-9]+\\]+ \\)*\\([a-zA-Z0-9:\\/\\.-]+\.lp0\\)\(\\([0-9]+\\)\)"
;;               1 2 nil))



;;;; Goals

;; Package sanity:
;; - I fully concur with el-get author Dimitri Fontaine's sentiments.
;;   I too want a single file specifying all the packages I use, whence
;;   to obtain them and how they should be configured.

;; Custom file sanity:
;; - grouping
;; - commentary
;; - no repeating detault settings


;;;; Directory hygiene (NEEDS REVIEW)
;;
;; Apart from this file (~/.emacs.d/init.el) all state that I maintain
;; manually I place into my ~/emacs/ directory (e.g. custom-file,
;; spelling dictionaries, templates I write/modify for yasnippet,
;; packages I write or modify).  I then maintain the contents of that
;; directory in git and replicate to all sites where I use emacs.
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


;;;; Sharing and credits (NEEDS REVIEW)

;; "Everyone's .emacs  rips off someone else's"...

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


;;; === Key space ======================================================


;;; === Setup ==========================================================
;;;; ADD NOTES ON SETTING UP ON A NEW MACHINE


;;; === Package management via straight and use-package (not custom) ===
;;;; Straight: a next-generation package manager (Radon Rosborough)
;; https://github.com/raxod502/straight.el

;; Bootstrap `straight'
(defvar bootstrap-version)
(defvar straight-check-for-modifications
  '(find-when-checking watch-files)  ; no checking during startup
  "When to check for package modifications.")

(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Package `use-package' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features.
(straight-use-package 'use-package)


(use-package straight
  :straight (:host github :repo "raxod502/straight.el" :branch "master" :local-repo "straight.el" :files ("straight*.el"))
  :custom
  (straight-use-package-by-default t)      ; Make use-package install missing packages via straight
  (straight-vc-git-default-clone-depth 1)
  :config
; (straight-check-for-modifications '(find-when-checking watch-files))
  (push "lib" straight-default-files-directive))


(use-package straight-x
  :straight (:host github :repo "raxod502/straight.el" :branch "master" :local-repo "straight.el" :files ("straight*.el"))
  :after straight
  :commands (straight-x-fetch-all straight-remove-unused-repos straight-x-pull-all))


;;;; General: convenience wrappers for keybindings (Fox Kiester)
;; https://github.com/noctuid/general.el

(use-package general
  :straight (:host github :repo "noctuid/general.el")
  :custom
  (general-use-package-emit-autoloads t))


;;;; Use-package: a configuration macro to simplify .init.el (John Wiegley)
;; https://github.com/jwiegley/use-package
;; doc: https://github.com/jwiegley/use-package/blob/master/README.md
;; doc: https://jwiegley.github.io/use-package/keywords/
;; kbd: https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-Rebinding.html
;;      After loading, M-x describe-personal-keybindings show all such keybindings

;; This list comes primarily from examining use-package-keywords
;;
;; :defer is implied by :bind, :bind*, :commands, :hook, :interpreter, :magic[-fallback], :mode
;;
;; Order insensitive metadata (neither introduces nor references any symbols, nor executes any code):
;; :disabled         - turn off a troublesome or currently unused package
;; :straight
;; :pin              - pin the package to an archive
;; :ensure           - causes package(s) to be installed automatically if not already present on your system
;; :defer            - supress non-auto-deferred loading if you know that another package will load this one
;; :demand           - prevent automatic deferred loading
;; :no-require       - inhibit loading package at compile time (e.g. just need eval-after-load functionality)
;; :requires         - suppress if feature(s) not available at time of use-package decl (= :if (featurep 'foo))
;; :load             - :load keyword overrides :no-require
;; :after            - delay loading until (:all PKG...) (default) or (:any PKG...) are loaded
;; :catch            - override use-package-expand-minimally
;; :blackout         -
;; :diminish         - support for diminish.el (if installed).
;; :delight          - support for delight.el (if installed)
;;
;; Order sensitive (may introduce, references or modify symbols, and/or may execute code):
;; :load-path        - append to load-path before loading; relative paths are expanded within user-emacs-directory
;; :defines          - introduce dummy variable declarations solely for the sake of the byte-compiler
;; :functions        - introduce dummy function declarations solely for the sake of the byte-compiler
;; :preface          - definition to silence byte-compiler and to be used in :if tests
;; :if :when :unless - predicate initialization and loading
;; :custom           - custom-set or set-default each variable definition; does not modify custom-file
;; :custom-face      - customize-set-faces each face definition; does not modify custom-file
;; :bind(*)          - ( ( <global-key> . <command> )... :map <package-local-map> ( <local-key> . <command> )... )
;;                     * = overriding all minor mode bindings
;; :bind-keymap(*)   - ( <package-prefix-key> . <map-in-package> )
;;                     * = overriding all minor mode bindings
;; :interpreter      - extend interpreter-mode-alist; use cons if package name does not end with -mode
;; :mode             - extend auto-mode-alist; use cons if package name does not end with -mode
;; :magic            - extend magic-mode-alist; use cons if package name does not end with -mode
;; :magic-fallback   - extend magic-fallback-mode-alist; use cons if package name does not end with -mode
;; :hook             - see https://github.com/jwiegley/use-package/blob/master/README.md#hooks
;; :commands         - creates autoloads and defers loading of the module until commands are used
;; :init             - code executed unconditionally before package is loaded (and even when not loaded)
;; :config           - execute code after a package is loaded (perhaps lazily); could even be a use-package form
;; :hydra            -


(use-package use-package-core
  :straight (:type built-in)
  :requires (use-package-hydra)
  :custom
  (use-package-always-defer t)          ; Always defer load package to speed up startup
  (use-package-compute-statistics t)    ; Collect use-package statistics
  (use-package-enable-imenu-support t)  ; Let imenu finds use-package definitions
  (use-package-expand-minimally t)      ; Make the expanded code as minimal as possible
  (use-package-verbose t))              ; Report loading details


;;;; Use-package-hydra: add :hydra keyword to use-package macro (to1ne)
;; https://github.com/emacsmirror/use-package-hydra

(use-package use-package-hydra
  :straight (:host github :repo "emacsmirror/use-package-hydra")
  :defer ; Loaded via one or more hooks near the bottom of this file
  :requires (hydra))


;;;; Macro: use-feature

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))


;;;; Suppress use of the custom-file

(use-package cus-edit
  :straight (:type built-in)
  :custom
  ;(custom-theme-directory null-device)
  (custom-file null-device))            ; I use use-package's :custom instead


;;; === Init file helpers (Radian and others) ==========================
;;;; Macro: radian-defadvice

(defmacro radian-defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. PLACE should be sharp-quoted.
DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "Radian: advice `%S' not documented'" name))
  (unless (and (listp place)
               (= 2 (length place))
               (eq (nth 0 place) 'function)
               (symbolp (nth 1 place)))
    (error "Radian: advice `%S' does not sharp-quote place `%S'" name place))
  `(progn
     ;; You'd think I would put an `eval-and-compile' around this. It
     ;; turns out that doing so breaks the ability of
     ;; `elisp-completion-at-point' to complete on function arguments
     ;; to the advice. I know, right? Apparently this is because the
     ;; code that gets the list of lexically bound symbols at point
     ;; tries to `macroexpand-all', and apparently macroexpanding
     ;; `eval-and-compile' goes ahead and evals the thing and returns
     ;; only the function symbol. No good. But the compiler does still
     ;; want to know the function is defined (this is a Gilardi
     ;; scenario), so we pacify it by `eval-when-compile'ing something
     ;; similar (see below).
     (defun ,name ,arglist
       ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
                           "an"
                         "a")))
          (format "%s\n\nThis is %s `%S' advice for `%S'."
                  docstring article where
                  (if (and (listp place)
                           (memq (car place) ''function))
                      (cadr place)
                    place)))
       ,@body)
     (eval-when-compile
       (declare-function ,name nil))
     (advice-add ,place ',where #',name)
     ',name))


;;;; Macro: radian-defhook

(defmacro radian-defhook (name arglist hooks docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOKS is a list of hooks to which to
add the function, or just a single hook. DOCSTRING and BODY are
as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (dolist (hook hooks)
    (unless (string-match-p "-\\(hook\\|functions\\)$" (symbol-name hook))
      (error "Symbol `%S' is not a hook" hook)))
  (unless (stringp docstring)
    (error "Radian: no docstring provided for `radian-defhook'"))
  (let ((hooks-str (format "`%S'" (car hooks))))
    (dolist (hook (cdr hooks))
      (setq hooks-str (format "%s\nand `%S'" hooks-str hook)))
    `(progn
       (defun ,name ,arglist
         ,(format "%s\n\nThis function is for use in %s."
                  docstring hooks-str)
         ,@body)
       (dolist (hook ',hooks)
         (add-hook hook ',name)))))


;;;; Function: radian-clone-emacs-source-maybe

(defun radian-clone-emacs-source-maybe ()
  "Prompt user to clone Emacs source repository if needed."
  (when (and (not (file-directory-p source-directory))
             (not (get-buffer "*clone-emacs-src*"))
             (yes-or-no-p "Clone Emacs source repository? "))
    (make-directory (file-name-directory source-directory) 'parents)
    (let ((compilation-buffer-name-function
           (lambda (&rest _)
             "*clone-emacs-src*")))
      (save-current-buffer
        (compile
         (format
          "git clone https://github.com/emacs-mirror/emacs.git %s"
          (shell-quote-argument source-directory)))))))


;;; === Temporary configuration to optimize startup ====================

(use-package emacs ; optimize startup
  :straight (:type built-in)
  :custom
  ;; Resizing the Emacs frame can be a terribly expensive part of changing the
  ;; font. By inhibiting this, we easily halve startup times with fonts that are
  ;; larger than the system default.
  (frame-inhibit-implied-resize t)
  (gc-cons-threshold most-positive-fixnum)
  (ad-redefinition-action 'accept)
  :hook
  (after-init . (lambda ()
                  (setq frame-inhibit-implied-resize nil)
                  (setq gc-cons-threshold (* 100 1024 1024))
                  (setq gc-cons-percentage 0.1)
                  (setq ad-redefinition-action 'warn)
                  (setq load-prefer-newer t)
                  (garbage-collect)))
  :config
  (setq load-prefer-newer nil))

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
;; (advice-add #'x-apply-session-resources :override #'ignore)
(setq inhibit-x-resources nil)

;;; === Reserved to user keymaps for autoload bindings =================
;; Global-map:
;; C-c mode-specific prefix
;; C-x control-x-prefix
;; C-z suspend-frame
;; Free constrol-punctuation:
;;   C-!     C-%     C-*     C-=     C-}     C-'     C-"     C-,
;;   C-<     C-?     C-#     C-^     C-(     C-{     C-;     C-:
;;   C-|     C-.     C->     C-~     C-$     C-&     C-)

;; Ctl-c-map:

(use-package emacs ;-keymap
  :disabled
  :straight (:type built-in)
  :init
  (defvar ctl-c-r-map (make-sparse-keymap)
    "Keymap for subcommands of C-c r.")
  (fset 'ctl-c-r-prefix ctl-c-r-map)
  (global-unset-key "\C-cr")
  (global-set-key "\C-cr" 'ctl-c-r-prefix)

  (defvar ctl-x-t-map (make-sparse-keymap)
    "Keymap for subcommands of C-x t (toggle).")
  (fset 'ctl-x-t-prefix ctl-x-t-map)
  (global-unset-key "\C-xt")
  (global-set-key "\C-xt" 'ctl-x-t-prefix)
)


;;; === Common dependencies ============================================
;;;; Hydra: Make bindings that stick around (Oleh Krehel)
;; https://github.com/abo-abo/hydra

(use-package hydra
  :straight (:host github :repo "abo-abo/hydra")
  :defer ; Loaded via one or more hooks near the bottom of this file
  )

;; doc: https://emacs.stackexchange.com/a/46938/443
;;
;; (use-package hydra
;;   :config
;;   (use-package use-package-hydra)
;;   (use-package hydra-posframe
;;     :load-path hydra-posframe-p
;;     :config
;;     (require 'hydra-posframe)
;;     :custom
;;     (hydra-posframe-parameters
;;      '((left-fringe . 4) (right-fringe . 4) (top-fringe . 4) (bottom-fringe . 4) (height . 18) (width . 105) (min-height . 17) (max-height . 30) (top . 25)))
;;     :custom-face
;;     (hydra-posframe-border-face ((t (:background "#ffffff"))))
;;     (hydra-posframe-face ((t (:background-color "#6272a4"))))
;;     :hook
;;     (after-init . hydra-posframe-enable)
;;     )
;;   )


;;;; Transient: prefix cmd, infix args and suffix cmd menu (Jonas Bernoulli)
;; https://github.com/magit/transient

(use-package transient
  :custom
  (transient-enable-popup-navigation t))


;;;; # Posframe: pop a posframe (just a frame) at point (Feng Shu)
;; https://github.com/tumashu/posframe

(use-package posframe
  :disabled)


;;;; # Transient-posframe: use posframe to display a transient (yanghaoxie)
;; https://github.com/yanghaoxie/transient-posframe

(use-package transient-posframe
  :disabled
  :straight (:host github :repo "yanghaoxie/transient-posframe"))


;;;; # pos-tip: display a tooltip at mouse position (Shohei Yoshida)
;; https://github.com/syohex/pos-tip

(use-package pos-tip
  :disabled)


;;;; Imenu-list: Show imenu entries in a separate buffer (Bar Magal)
;; https://github.com/bmag/imenu-list

(use-package imenu-list
  :straight (:host github :repo "bmag/imenu-list"))


;;;; Popup: a visual popup user interface (Tomohiro Matsuyama)
;; https://github.com/auto-complete/popup-el

(use-package popup
  :straight  (:host github :repo "auto-complete/popup-el"))


;;;; Compat: a general COMPATibility library (Philip Kaludercic)
;; https://git.sr.ht/~pkal/compat

(use-package compat
  :straight (:host melpa :repro "gnu-elpa-mirror/compat")
  :defer)


;;; === Protection =====================================================
;;;; Bostr: Backup On Save To Rcs
;; Remember to install rcs

(use-package bostr
  :straight (:host github :repo "jsyjr/bostr")
  :hook after-save)


;;;; Byte-compile on save of a .el file

(defun my/compile-saved-elisp-buffer ()
  "Byte compile an elisp buffer anytime it is saved."
  (when (and (eq major-mode 'emacs-lisp-mode)
             (not (string-prefix-p "phw" (buffer-name))))
    (let ((f (buffer-file-name)))
      (byte-compile-file f)
;;      (native-compile-async f)
      )))

(setq byte+native-compile t)
(setq load-prefer-newer t)

(add-hook 'after-save-hook 'my/compile-saved-elisp-buffer)


;;;; Immortal buffers

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


;;;; Safe local variables

(use-package emacs ; safe-local-vars
  :straight (:type built-in)
  :custom
  (safe-local-variable-values
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
     (eval add-to-list 'auto-mode-alist
           '("\\.wiki\\'" . mediawiki-mode))
     (eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook 'write-contents-functions
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
     (outshine-mode . 1)
     )))


;;;; Less safe: y/n instead of yes/no

(fset 'yes-or-no-p 'y-or-n-p)


;;; === Visuals ========================================================
;;;; Random custom items
(use-package emacs ; random visuals
  :straight (:type built-in)
  :custom
  (inhibit-startup-message t)
  (inhibit-startup-screen t)           ; splash screens are for wimps
  (initial-scratch-message nil)        ; do not seed *scratch* w/ a msg
  (scalable-fonts-allowed t)           ; no restrictions (can be slow!)
  (scroll-conservatively 1)            ; scroll window a line at a line
  (tool-bar-mode nil)                  ; recover screen space, no toolbar
  (transient-mark-mode t)              ; hightlight region, etc.
  (truncate-lines t)                   ; no wrapped lines
  (use-dialog-box nil)                 ; dialog boxes are also for wimps
  (visible-bell t)                     ; subtle blink in response to ^G
  (ring-bell-function #'my/ring-bell)  ; avoid annoying dings and flashes
  :init
  ;; This function is especially important given the atypical mini-buffer
  ;; behavior in my use of mini-frame.  The flash did not happen in the
  ;; mini-buffer being destroyed, but rather in the header line of the
  ;; frame recovering focus.
  (defun my/ring-bell ()
    (message "command= %s" this-command)
    (unless (memq this-command
                  '(isearch-abort abort-recursive-edit exit-minibuffer abort-minibuffers
                                  keyboard-quit mwheel-scroll down up next-line previous-line
                                  backward-char forward-char))
      (ding)))
  :config
  ;; On Wed, Jan 19, 2022 at 6:52 AM Eli Zaretskii <eliz@gnu.org> wrote:
  ;; (https://lists.gnu.org/archive/html/emacs-devel/2022-01/msg01189.html)
  ;;
  ;; Users and readers of certain scripts cannot use such a simplistic
  ;; solution, which is basically only suitable for plain ASCII text.  (And
  ;; even there it is slowly becoming inappropriate, what with the growing
  ;; popularity of ligatures, let alone Emoji.) Emacs should be able to do
  ;; better.
  (set-face-background 'glyphless-char "red"))

(use-package simple ; column numbers in mode line
  :straight (:type built-in)
  :custom
  (column-number-mode t))               ; enable magic line/column format

(use-package cus-edit ; custom
  :straight (:type built-in)
  :custom
  (custom-buffer-done-kill t))          ; kill custom buffer on exit

(use-package font-core ; font locking
  :straight (:type built-in)
  :custom
  (global-font-lock-mode t))            ; font-lock in all buffers


;;;; # Good-scroll: good pixel line scrollin (Benjamin Levy)
;; https://github.com/io12/good-scroll.el

(use-package good-scroll
  :disabled
  :straight (:host github :repo "io12/good-scroll.el")
  :defer 3
  :config
  (good-scroll-mode +1))

;;;; # Emacs-svg-icon: greyscale icons from remote collections (Nicolas P. Rougier)
;; https://github.com/rougier/emacs-svg-icon

(use-package svg-icon
  :disabled
  :straight (:host github :repo "rougier/emacs-svg-icon"))


;;;; # All-the-icons: expose various colored icon fonts fonts (Dom Charlesworth)
;; https://github.com/domtronn/all-the-icons.el

(use-package all-the-icons
  :disabled
  :straight (:host github :repo "domtronn/all-the-icons.el"))


;;;; # All-the-icons-completion: Add icons completion candidates (Itai Y. Efrat)
;; https://github.com/iyefrat/all-the-icons-completion

(use-package all-the-icons-completion
  :disabled
  :straight (:host github :repo "iyefrat/all-the-icons-completion")
  :after (marginalia all-the-icons svg-icon)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode))


;;;; # All-the-icons-dired: shows icons for each file in dired mode (jtbm37)
;; https://github.com/jtbm37/all-the-icons-dired

(use-package all-the-icons-dired.el
  :disabled
  :straight (:host github :repo "jtbm37/all-the-icons-dired")
  :after (:any all-the-icons svg-icon)
  :hook (dired-mode-hook . all-the-icons-dired-mode)
  :config
  (all-the-icons-dired-mode))


;;;; Modus: theming (Protesilaos Stavrou) + cursor colors, shape and blinking
;; doc: https://protesilaos.com/modus-themes/

(use-package emacs ; modus-vivendi
  :straight (:type built-in)
  :demand t
  :custom
  (modus-themes-bold-constructs nil)
  (modus-themes-slanted-constructs t)
  (modus-themes-hl-line nil)
  (modus-themes-region '(bg-only))
  (modus-themes-fringes 'intense)
  (modus-themes-completions nil)
  (modus-themes-diffs 'bg-only)
  (modus-themes-mode-line '(borderless accented))
  (modus-themes-paren-match '(intense-bold))
  (modus-themes-syntax nil)

  :config

;; InputMono-BlackItalic
;; InputMono-Black
;; InputMono-BoldItalic
;; InputMono-Bold
;; InputMonoCompressed-BlackItalic
;; InputMonoCompressed-Black
;; InputMonoCompressed-BoldItalic
;; InputMonoCompressed-Bold
;; InputMonoCompressed-ExtraLightItalic
;; InputMonoCompressed-ExtraLight
;; InputMonoCompressed-Italic
;; InputMonoCompressed-LightItalic
;; InputMonoCompressed-Light
;; InputMonoCompressed-MediumItalic
;; InputMonoCompressed-Medium
;; InputMonoCompressed-Regular
;; InputMonoCompressed-ThinItalic
;; InputMonoCompressed-Thin
;; InputMonoCondensed-BlackItalic
;; InputMonoCondensed-Black
;; InputMonoCondensed-BoldItalic
;; InputMonoCondensed-Bold
;; InputMonoCondensed-ExtraLightItalic
;; InputMonoCondensed-ExtraLight
;; InputMonoCondensed-Italic
;; InputMonoCondensed-LightItalic
;; InputMonoCondensed-Light
;; InputMonoCondensed-MediumItalic
;; InputMonoCondensed-Medium
;; InputMonoCondensed-Regular
;; InputMonoCondensed-ThinItalic
;; InputMonoCondensed-Thin
;; InputMono-ExtraLightItalic
;; InputMono-ExtraLight
;; InputMono-Italic
;; InputMono-LightItalic
;; InputMono-Light
;; InputMono-MediumItalic
;; InputMono-Medium
;; InputMonoNarrow-BlackItalic
;; InputMonoNarrow-Black
;; InputMonoNarrow-BoldItalic
;; InputMonoNarrow-Bold
;; InputMonoNarrow-ExtraLightItalic
;; InputMonoNarrow-ExtraLight
;; InputMonoNarrow-Italic
;; InputMonoNarrow-LightItalic
;; InputMonoNarrow-Light
;; InputMonoNarrow-MediumItalic
;; InputMonoNarrow-Medium
;; InputMonoNarrow-Regular
;; InputMonoNarrow-ThinItalic
;; InputMonoNarrow-Thin
;; InputMono-Regular
;; InputMono-ThinItalic
;; InputMono-Thin

  (let
;;    ((fixed-family "Monoid") (variable-family "Source Sans Pro"))
    ((fixed-family "Source Code Pro") (variable-family "Source Sans Pro"))
;;    ((fixed-family "Terminus") (variable-family "Source Sans Pro"))
;;    ((fixed-family "Input Mono") (variable-family "Input Sans"))
;;    ((fixed-family "Dina") (variable-family "Input Sans"))
    ;; Main typeface
    (set-face-attribute 'default nil :family fixed-family :height 140)
    ;; Proportionately spaced typeface
    (set-face-attribute 'variable-pitch nil :family variable-family :height 1.0)
    ;; Monospaced typeface
    (set-face-attribute 'fixed-pitch nil :family fixed-family :height 1.0))
  (load-theme 'modus-vivendi))

(use-package cua-base ; cursor style: insert (bar), overwrite (box), or read-only
  :straight (:type built-in)
  :demand t
  :custom
  (cua-enable-cua-keys nil)
  (cua-overwrite-cursor-color '(box . "#ff69b4")) ; HotPink
  (cua-read-only-cursor-color '(box . "#54ff9f")) ; SeaGreen1
  (cua-normal-cursor-color    '(bar . "#ffd700")) ; Gold
  (cua-enable-cursor-indications t)
  :config
  (cua-mode t))

(use-package frame ; blinking cursor timing
  :straight (:type built-in)
  :custom
  (blink-cursor-blinks 0)
  (blink-cursor-delay 0))

(use-package emacs ; stretch cursor
  :straight (:type built-in)
  :custom
  (x-stretch-cursor t))


;;;; Dimmer: dim non-active windows (Neil Okamoto)
;; https://github.com/gonewest818/dimmer.el

(use-package dimmer
  :demand t
  :custom
  (dimmer-adjustment-mode :both)        ; both foreground and background
  (dimmer-fraction 0.1)                 ; subtle, slightly less contrast
  :config
  (setq dimmer-buffer-exclusion-regexps
        '("^ \\*Minibuf-[0-9]+\\*$"     ; dimmer default
          "^ \\*Echo.*\\*$"             ; dimmer default
          "^ \\*LV\\*$"                 ; hydras
          "^ \\*transient\\*$"          ; magit et al
          "^\\*Org Select\\*$"          ; org-mode
          "^ \\*Agenda Commands\\*$"    ; org-mode
          "^ \\*.*posframe.*buffer.*\\*$"
          "^ \\*which-key\\*$"))
  (dimmer-mode 1))


;;;; Minions: minions minor modes menu (Jonas Bernoulli)
;; https://github.com/tarsius/minions

(use-package minions
  ;; :straight (:host github :repo "tarsius/minions")
  :demand t
  :config
  (minions-mode 1)
  (global-set-key [S-down-mouse-3] 'minions-minor-modes-menu))


;;;; # Some modes look better with proportional fonts

;; (mapc
;;  (lambda (hook)
;;    (add-hook hook (lambda () (variable-pitch-mode t))))
;;  '(erc-mode-hook
;;    edit-server-start-hook
;;    markdown-mode-hook
;;    twittering-mode
;;    text-mode
;;    ))


;;;; Icon label when minimized

(use-package emacs ; icon title
  :straight (:type built-in)
  :custom
  (icon-title-format
   '((((winring-show-names
        (" " winring-name))))
     "Emacs:  %b")))


;;;; Filename [ path ] in title bar

;; Suggstion from https://blog.lambda.cx/posts/eamacs-improved-frame-title
;; (setq frame-title-format '("%b@" (:eval (or (file-remote-p default-directory 'host) system-name)) " — Emacs"))

(use-package emacs ; contents of title bar
  :straight (:type built-in)
  :custom
  (frame-title-format
   '((((winring-show-names
        (" " winring-name))))
     ("Emacs:  ")
     ((:eval (buffer-name
              (if (minibufferp)
                  (window-buffer (minibuffer-selected-window))
                (current-buffer)))))
     ((:eval (if buffer-file-name
                 (concat
                  "  [ "
                  (file-name-directory
                   (abbreviate-file-name
                    (file-truename buffer-file-name)))
                  " ]")
               ""))))))

;;;; Frame appearance

(use-package emacs ; appearance of subsequent frames
  :straight (:type built-in)
  :custom
  (default-frame-alist
   '((minibuffer . nil)
     (frame-minibuffer-at-top . t)
     (window-mode-line-at-top . t)
     (menu-bar-lines . 0)
     (tool-bar-lines . 0)
     (tab-bar-lines . 1)
     (icon-type . nil)
     (background-mode . dark)
     (background-color . "black")
     (foreground-color . "white")
     (vertical-scroll-bars . nil)
     (cursor-type . bar)
     (x-stretch-cursor . t)))
  (indicate-buffer-boundaries 'right))  ; graphics in fringe

;; unspecified values come from default-frame-alist (above)
(use-package frame
  :straight (:type built-in)
  :custom
  (initial-frame-alist
   '((fullscreen . maximized))))


;;;; MBTB: separate minibuffer frame overlaying the tab bar
;; https://github.com/jsyjr/mbtb

(use-package mbtb
  :straight (:host github :repo "jsyjr/mbtb" :branch "main")
  :demand t)

;; Usurp global tab bar as location of persistent collapsed minibuffer
(use-package tab-bar
  :straight (:type built-in)
  :demand t
  :custom
  (tab-bar-format '((lambda () " ")))
  :config
  (tab-bar-mode +1))



;;;; Visualize current line

(use-package emacs ; current line
  :straight (:type built-in)
  :custom
  (indicate-empty-lines t)
  (show-trailing-whitespace t))


;; Highlight current line except when region is active
(use-package hl-line
  :straight (:type built-in)
  :defer ; Loaded via one or more hooks near the bottom of this file
  :custom
  (global-hl-line-sticky-flag t "maintian highlight in inactive windows")
  :custom-face
  (hl-line ((t (:underline (:color "gray30" :style line) :extend t :inherit (modus-themes-hl-line)))))
  :config
  (defun my/global-hl-line-hide-on-activate-mark()
    (hl-line-unhighlight)
    (when global-hl-line-mode
      (setq-local global-hl-line-mode nil)
      (global-hl-line-unhighlight)))
  (defun my/global-hl-line-show-on-deactivate-mark()
    (unless global-hl-line-mode
      (setq-local global-hl-line-mode t)
      (global-hl-line-highlight)))
  (add-hook   'activate-mark-hook #'my/global-hl-line-hide-on-activate-mark)
  (add-hook 'deactivate-mark-hook #'my/global-hl-line-show-on-deactivate-mark)
  (global-hl-line-mode +1))

;; Indicators in fringe when line is wrapped
(use-package simple
  :straight (:type built-in)
  :custom
  (visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))


;;;; Display Windows extended glyphs (instead of things like \223)

(use-package disp-table
  :straight (:type built-in)
  :config
  (standard-display-8bit 128 255))


;;;; Pretty ^L (display as a full-width horizontal rule)
;; https://github.com/jsyjr/pp-c-l

;; Universally display ^L as a full window-width horizontal rule
(use-package pp-c-l
  :straight (:host github :repo "jsyjr/pp-c-l" :branch "main")
  :custom
  (pretty-control-l-mode t)
  (pp^L-^L-string-function (lambda (win) (make-string (1- (window-width win)) 32)))
  :custom-face
  (pp^L-highlight ((((type x w32 mac graphic) (class color)) (:inherit shadow :strike-through t))))
  :hook ((window-setup window-configuration-change) . refresh-pretty-control-l))


;; (my/custom-set-faces
;;  '(cursor ((t (:background "gold"))))
;;  '(show-paren-match ((t (:background "light green" :foreground "black" :weight bold))))
;;  '(show-paren-mismatch ((t (:background "firebrick" :foreground "white"))))
;;  )


;;;; Tool tips

(use-package tooltip
  :straight (:type built-in)
  :custom
  (tooltip-delay 0.3))

;; (my/custom-set-faces
;;  '(tooltip ((((class color)) (:inherit variable-pitch :background "#f5f5b5" :foreground "black"))))
;;  )


;;;; Width to use for hard tabs

(defun my/set-buffer-local-tab-width-to-4 ()
  "Make display engine's tab-width buffer local and set it to 4."
  (interactive)
  (setq tab-width 4))

(defun my/set-buffer-local-tab-width-to-8 ()
  "Make display engine's tab-width buffer local and set it to 8."
  (interactive)
  (setq tab-width 8))


;;;; Kurecolor: color editing goodies (Jason Milkins)
;; https://github.com/emacsfodder/kurecolor
;; Demo: https://github.com/emacsfodder/kurecolor/blob/master/kurecolor.gif

(use-package kurecolor
  :disabled)


;;;; Extra color themes and colors

(use-package custom
  :straight (:type built-in)
  :custom
  (custom-theme-directory "~/emacs/themes/"))


;;;; Rainbow-mode: colorize color names in buffers (Julien Danjou)
;; https://github.com/ruediger/rainbow-mode

(use-package rainbow-mode)


;;;; Rainbow-delimiters: highlight brackets according to their depth (Jeremy Rayman)
;; https://github.com/Fanael/rainbow-delimiters

(use-package rainbow-delimiters
  :defer 2
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable))


;;;; Paren: highlight matching paren (Richard Stallman)

(use-package paren
  :straight (:type built-in)
  :defer ; Loaded via one or more hooks near the bottom of this file
  :custom
  (show-paren-delay 0)                  ; do it immediately
  ;; Appers no longer to be part of paren.el
  ;; :custom-face
  ;; (show-paren-match ((t (:background "light green" :foreground "black" :weight bold))))
  ;; (show-paren-mismatch ((t (:background "firebrick" :foreground "white"))))
  :config
  (show-paren-mode t))


;;;; Highlight-doxygen: highlight doxy cmds and args (Anders Lindgren)
;; https://github.com/Lindydancer/highlight-doxygen

;; Hook for c-mode and c++-mode
;; (use-package highlight-doxygen)


;;; === Mode-line ======================================================
;;;; mode-line basics

(defface mode-line-highlight-bold
  '((t (:inherit mode-line-highlight :weight bold)))
  "Basic mode line face for bolding in highlights."
  :version "22.1"
  :group 'mode-line-faces
  :group 'basic-faces)


(use-package emacs ; mode-line-format
  :straight (:type built-in)
  :custom
  (mode-line-format
   '("%e" mode-line-window-id
     #("-" 0 1 (help-echo
                "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy enitre frame
mouse-3: Remove current window from display"))
     mode-line-mule-info
     mode-line-client
     mode-line-modified
     mode-line-remote
     #(" " 0 1 (help-echo
                "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy enitre frame
mouse-3: Remove current window from display"))
     mode-line-position
     mode-line-frame-identification
     mode-line-buffer-identification
     #(" " 0 1 (help-echo
                "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy enitre frame
mouse-3: Remove current window from display"))
     (which-func-mode
      ("" which-func-format
       #(" " 0 1 (help-echo
                  "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy enitre frame
mouse-3: Remove current window from display"))))
     mode-line-modes
     ;;      (vc-mode ("" vc-mode #(" " 0 1 (help-echo
     ;; "mouse-1: Select (drag to resize)
     ;; mouse-2: Make current window occupy enitre frame
     ;; mouse-3: Remove current window from display"))))
     (global-mode-string (#("-" 0 1 (help-echo
                                     "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy enitre frame
mouse-3: Remove current window from display")) global-mode-string))
     #("-%-" 0 3 (help-echo
                  "mouse-1: Select (drag to resize)
mouse-2: Make current window occupy enitre frame
mouse-3: Remove current window from display")))))

;; If vc-mode ever gets turned on try to minize its impact.
(use-package vc-hooks
  :straight (:type built-in)
  :custom
  (vc-ignore-dir-regexp
   "\"\\\\`\\\\(?:/mathworks\\\\|/mathworks/[A-Z]+\\\\|/mathworks/[^/]+\\\\|/mathworks/[A-Z]+/[^/]+\\\\|/mathworks/devel/[^/]+\\\\|/mathworks/[A-Z]+/devel/[^/]+\\\\|/mathworks/devel/bat/[^/]+\\\\|/mathworks/[A-Z]+/devel/bat/[^/]+\\\\|/mathworks/devel/jobarchive\\\\|/mathworks/[A-Z]+/devel/jobarchive\\\\|/mathworks/hub/[^/]+\\\\|/mathworks/[A-Z]+/hub/[^/]+\\\\|/mathworks/hub/scratch\\\\|/mathworks/[A-Z]+/hub/scratch\\\\|/mathworks/hub/site-local\\\\|/mathworks/[A-Z]+/hub/site-local\\\\|/sandbox\\\\|/home\\\\|/hub\\\\|/public\\\\|/src\\\\|/scratch\\\\|\\\\)/\\\\'\"\n"))


;;;; mode-line eol-mnemonic

(use-package emacs ; mode-line: eol
  :straight (:type built-in)
  :custom
  (eol-mnemonic-mac ":")                ; defaults to '/'
  (eol-mnemonic-undecided "?")          ; defaults to ':'
  (eol-mnemonic-unix "/"))              ; defaults to ':'


;;;; mode-line window-id

(defconst mode-line-window-id
  '(:eval (mode-line-window-id)))

(defun mode-line-window-id ()
  "If possible return a phw-mode window ID."
   (if (fboundp 'phw-window-ordinal)
       (phw-window-ordinal (selected-window))
     nil))


;;;; mode-line position-widget

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
;;
;; TODO: Change color if cursor exceeds limit column

(defvar buffer-max-column-visited 1
  "Accumulate max column visited to prevent mode-line jitter.")
(make-variable-buffer-local 'buffer-max-column-visited)

(copy-face 'modus-themes-subtle-blue 'my/position-widget-bold)
(set-face-attribute 'my/position-widget-bold nil :weight 'bold)

(defun my/position-widget ()
  ""
  (let*
      ((c-n-m column-number-mode)
       (l-n-m line-number-mode)
       (wbeg (window-start))
       (wend (window-end)))

    (save-restriction
      (widen)

      (let*
          (
           (eob-line  (line-number-at-pos (point-max)))
           (wbeg-line (line-number-at-pos wbeg))
           (wend-line (line-number-at-pos wend))

           (widget
            (concat
             ;; Leading [
             (if (= wbeg-line 1)
                 #("[" 0 1 (face my/position-widget-bold))
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
                        (c-n-m "%6l")
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
                               ((max-col (max (if l-n-m 999999 0)
                                              (current-column)
                                              buffer-max-column-visited))
                                (field   (cond
                                          ((> 10     max-col) 3)
                                          ((> 100    max-col) 4)
                                          ((> 1000   max-col) 5)
                                          ((> 10000  max-col) 6)
                                          ((> 100000 max-col) 7)
                                          (t                  8)))
                                (cur-col (current-column))
                                (digits  (cond
                                          ((> 10     cur-col) 1)
                                          ((> 100    cur-col) 2)
                                          ((> 1000   cur-col) 3)
                                          ((> 10000  cur-col) 4)
                                          ((> 100000 cur-col) 5)
                                          (t                  6))))
                             (setq buffer-max-column-visited max-col)
                             (substring "%c       " 0 (- field digits))))))))

                    (len (length expanded))
                    (hilen (max 1           ; at least one column
                                (min len    ; no more than full string
                                     (round (/ (* wlines len)
                                               (float eob-line))))))
                    (lpad (round (/ (* wbeg-line (- len hilen))
                                    (float (- eob-line wlines -2)))))
                    (rpad (+ lpad hilen)))

                 (put-text-property lpad rpad 'face 'modus-themes-subtle-blue expanded)
                 expanded))
             ;; Trailing ]
             (if (= wend-line eob-line)
                 #("]" 0 1 (face my/position-widget-bold))
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

(setq mode-line-position '(:eval (my/position-widget)))

;; defer which-func until we initialize a buffer in one of the following
;;   modes: emacs-lisp-mode c-mode c++-mode  python-mode makefile-mode
;;          sh-mode perl-mode or cperl-mode

;;;; mode-line which-func

(use-package which-func
  :straight (:type built-in)
  :custom
  ;; Don't set `which-function-mode' to be enabled by default for all modes
  ;; For which-func support: (add-to-list 'which-func-mode 'MAJOR-MODE)
  (which-func-modes '(c++-mode
                      c-mode
                      cperl-mode
                      emacs-lisp-mode
                      makefile-mode
                      perl-mode
                      python-mode
                      sh-mode))
  (which-func-format
   '("["
     (:propertize which-func-current local-map
                  (keymap
                   (mode-line keymap
                              (mouse-1 . beginning-of-defun)
                              (mouse-2 . my/toggle-function-narrowing)
                              (mouse-3 . end-of-defun)))
                  face which-func help-echo
                  "Function (enclosing or preceding)
mouse-1: go to beginning
mouse-2: toggle function narrowing
mouse-3: go to end")
     "]"))
  :init
  (defun my/toggle-function-narrowing ()
    "Alternate between narrowing and windening current function"
    (interactive)
    (if (eq (point-min) 1)
        (narrow-to-defun)
        (widen))))

;; (my/custom-set-faces
;;  '(which-func ((t (:foreground "dark blue" :weight bold))))
;;  )

;; When config gets stable, using emacs server may be more convenient
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))


;;;; Uniquify buffer names styling

(use-package uniquify
  :straight (:type built-in)
  :custom
  (uniquify-buffer-name-style 'forward)) ; prefix '/'


;;; === Help and introspection =========================================
;;;; TODO

;; something to access use-package's describe-personal-keybindings

;;;; Radian's customization of builtin help

;; Feature `help' powers the *Help* buffer and related functionality.
(use-feature help
  :bind (:map help-map
              ("M-k" . #'radian-describe-keymap))
  :config

  (radian-defadvice radian--advice-help-inhibit-hints (&rest _)
    :override #'help-window-display-message
    "Inhibit the \"Type q in help window to delete it\" hints.
Normally these are printed in the echo area whenever you open a
help buffer.")

  (radian-defadvice radian--advice-help-disable-revert-prompt
      (help-mode-revert-buffer ignore-auto _noconfirm)
    :around #'help-mode-revert-buffer
    "Don't ask for confirmation before reverting help buffers.
\(Reverting is done by pressing \\<help-mode-map>\\[revert-buffer].)"
    (funcall help-mode-revert-buffer ignore-auto 'noconfirm))

  (defun radian-describe-keymap (keymap)
    "Display the bindings defined by KEYMAP, a symbol or keymap.
Interactively, select a keymap from the list of all defined
keymaps."
    (interactive
     (list
      (intern
       (completing-read
        "Keymap: " obarray
        (lambda (m)
          (and (boundp m)
               (keymapp (symbol-value m))))
        'require-match
        nil nil (thing-at-point 'symbol)))))
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        (insert (format "Keymap `%S' defines the following bindings:" keymap)
                "\n\n"
                (substitute-command-keys (format "\\{%S}" keymap))))))

  (radian-defhook radian--xref-help-setup ()
    help-mode-hook
    "Make xref look up Elisp symbols in help buffers.
Otherwise, it will try to find a TAGS file using etags, which is
unhelpful."
    (add-hook 'xref-backend-functions #'elisp--xref-backend nil 'local)))


;;;; # Help-fns+ - extensions to `help-fns.el (Drew Adams)
;; https://www.emacswiki.org/emacs/help-fns%2B.el

(use-package help-fns
  :disabled
  :straight (:type built-in)
  :bind (:map help-map
         ("C-c" . describe-key-briefly)
         ("M-c" . describe-copying)
         ("M-l" . find-function-on-key)))


(use-package help-fns+
  :disabled
  :after help-fns
  :bind (:map help-map
         ("B"	. describe-buffer)
         ("c"	. describe-command)
         ("o"	. describe-option)
         ("C-o" . describe-option-of-type)
         ("M-f" . describe-file)
         ("M-k" . describe-keymap)))


;;;; Helpful - a better *help* buffer (Wilfred Hughes)
;; https://github.com/Wilfred/helpful

;; Package `helpful' provides a complete replacement for the built-in
;; Emacs help facility which provides much more contextual information
;; in a better format.
(use-package helpful
  :disabled
  ;; :after help-fns+
  :bind (;; Remap standard commands.
         ([remap describe-command]  . helpful-command)
         ([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol]   . helpful-symbol)
         ([remap describe-key]      . helpful-key)

         ;; Suggested bindings from the documentation at
         ;; https://github.com/Wilfred/helpful.

         :map help-map
         ("F"   . helpful-function)
         ("M-m" . helpful-macro)
         ("C"   . helpful-command)

         :map global-map
         ("C-c C-d" . helpful-at-point))

  :config

  ;; Make it so you can quit out of `helpful-key' with C-g, like for
  ;; every other command. Put this in a minor mode so it can be
  ;; disabled.
  (define-minor-mode radian-universal-keyboard-quit-mode
    "Minor mode for making C-g work in `helpful-key'."
    :global t
    (if radian-universal-keyboard-quit-mode
        (radian-defadvice radian--advice-helpful-key-allow-keyboard-quit
            (&rest _)
          :before #'helpful-key
          "Make C-g work in `helpful-key'."
          ;; The docstring of `add-function' says that if we make our
          ;; advice interactive and the interactive spec is *not* a
          ;; function, then it overrides the original function's
          ;; interactive spec.
          (interactive
           (list
            (let ((ret (read-key-sequence "Press key: ")))
              (when (equal ret "\^G")
                (signal 'quit nil))
              ret))))
      (advice-remove
       #'helpful-key #'radian--advice-helpful-key-allow-keyboard-quit)))

  (radian-universal-keyboard-quit-mode +1)

  (radian-defadvice radian--advice-helpful-clone-emacs-source (library-name)
    :before #'helpful--library-path
    "Prompt user to clone Emacs source code when looking up functions.
Otherwise, it only happens when looking up variables, for some
bizarre reason."
    (when (member (file-name-extension library-name) '("c" "rs"))
      (radian-clone-emacs-source-maybe))))


;;;; Free-keys: show free keybindings for modkeys or prefixes (Matus Goljer)
;; https://github.com/Fuco1/free-keys
;; doc: https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html

(use-package free-keys
  :straight (:host github :repo "Fuco1/free-keys")
  :custom
  (free-keys-modifiers '("" "C" "A" "M" "s" "H" "C-M" "A-C" "C-s" "A-M"))
  :commands free-keys
  :config
  (radian-defadvice my--advice-free-keys-disable-whitespace-mode
      (&optional _prefix _buffer)
    :after #'free-keys
    "Locally disable showing trailing whitespace"
    (with-current-buffer (get-buffer-create "*Free keys*")
      (setq show-trailing-whitespace nil))))


;;;; # Unbound: find convenient unbound keystrokes (Davis Herring)
;; https://github.com/emacsmirror/emacswiki.org/blob/master/unbound.el

(use-package unbound
  :disabled
  :straight (:host github :repo "emacsmirror/emacswiki.org" :files ("unbound.el")))


;;;; Which-key: display key bindings after typing a prefix (Justin Burkett)
;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :straight (:host github :repo "justbur/emacs-which-key")
  :demand t
  :custom
  (which-key-popup-type 'side-window)
  (which-key-side-window-location 'left)
  (which-key-show-prefix 'top)
  (which-key-sort-order #'which-key-prefix-then-key-order)
  :config
  (which-key-mode 1))


;;; === Window management ==============================================
;;;; Edm: an emacs display manager (forked from Alex Griffin's edwina)
;; https://github.com/jsyjr/edm

;; Possible easy prefixes: C-. C-, C-' C-; C-= C-`

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Controlling-Active-Maps.html
;; set-transient-map keymap &optional keep-pred on-exit

;; edm-redraw
;; edm-clone-window
;; edm-dec-mfact
;; edm-dec-nmaster
;; edm-delete-window
;; edm-inc-mfact
;; edm-inc-nmaster
;; edm-select-next-window
;; edm-select-previous-window
;; edm-swap-next-window
;; edm-swap-previous-window
;; edm-zoom

(use-package edm
  :disabled
  :straight (:host github :repo "jsyjr/edm")
  :custom
  (edm-mfact 0.66)
  (edm-nmaster 1)
  :bind-keymap ("C-," . edm-mode-map)
  :bind (:map edm-mode-map
              ("r"        . 'edm-redraw)
              ("C-r"      . 'edm-redraw)
              ("n"        . 'edm-select-next-window)
              ("C-n"      . 'edm-select-next-window)
              ("SPC"      . 'edm-select-next-window)
              ("p"        . 'edm-select-previous-window)
              ("C-p"      . 'edm-select-previous-window)
              ("N"        . 'edm-swap-next-window)
              ("C-S-n"    . 'edm-swap-next-window)
              ("P"        . 'edm-swap-previous-window)
              ("C-S-p"    . 'edm-swap-previous-window)
              ("%"        . 'edm-dec-mfact)
              ("{"        . 'edm-dec-mfact)
              ("["        . 'edm-dec-mfact)
              ("^"        . 'edm-inc-mfact)
              ("}"        . 'edm-inc-mfact)
              ("]"        . 'edm-inc-mfact)
              ("d"        . 'edm-dec-nmaster)
              ("C-d"      . 'edm-dec-nmaster)
              ("i"        . 'edm-inc-nmaster)
              ("k"        . 'edm-delete-window)
              ("C-k"      . 'edm-delete-window)
              ("RET"      . 'edm-zoom)
              ("<return>" . 'edm-zoom)
              ("c"        . 'edm-clone-window)
              ("C-c"      . 'edm-clone-window))
  :config
;  (setq display-buffer-alist '((".*" . display-buffer-reuse-window)))
;  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edm-mode +1)
)


;;;; Window splitting and deletion

(use-package window
  :straight (:type built-in)
  :custom
  (split-height-threshold nil))

;; Cribbed from http://www.emacswiki.org/emacs/HorizontalSplitting

;; (defun split-horizontally-for-temp-buffers ()
;;   "Split the window horizontally for temp buffers."
;;   (when (and (one-window-p t)
;;                   (not (active-minibuffer-window)))
;;     (split-window-horizontally t)))

; (add-hook 'temp-buffer-setup-hook 'split-horizontally-for-temp-buffers)


;;;; # Mini-frame: show minibuffer reads in a child frame (Andrii Kolomoiets)
;; https://github.com/muffinmad/emacs-mini-frame

(use-package mini-frame
  :disabled
  :straight (:host github :repo "muffinmad/emacs-mini-frame")
  :custom
  (mini-frame-color-shift-step 0)
  (mini-frame-create-lazy nil)
  (mini-frame-detach-on-hide nil)
  (mini-frame-internal-border-color "white")
  (mini-frame-standalone t)
;;(x-gtk-resize-child-frames 'resize-mode)
  (mini-frame-show-parameters (lambda ()
                                (let* ((sf (selected-frame))
                                       (fg (frame-geometry sf)))
                                  ;; `((top   . ,(alist-get 'top   fg))
                                  ;;   (left  . ,(alist-get 'left  fg))
                                  ;;   (width . ,(alist-get 'width fg))
                                  `((top   . ,(frame-parameter sf 'top))
                                    (left  . ,(+ (frame-parameter sf 'left)
                                                 (car (cdr (assq 'external-border-size fg)))))
                                    (width . ,(+ (frame-parameter sf 'width)
                                                 2))
                                    (height . 1)
                                    (fullscreen . nil)
                                    (user-position . t)
                                    (user-size . t)
                                    (keep-ratio . t)
                                    (child-frame-border-width . 1)
                                    (internal-border-width . 1)
                                    )))))


;; (use-package mini-frame
;;   :disabled
;;   :straight (:host github :repo "muffinmad/emacs-mini-frame" :files("mini-frame.el"))
;;   :custom
;;   (mini-frame-color-shift-step 0)
;;   (mini-frame-create-lazy nil)
;;   (mini-frame-internal-border-color "white")
;;   ;; Completions overlay "parent" frame's title bar; 2/3 width, starting at left edge
;;   (mini-frame-show-parameters (lambda ()
;;                                 (let ((sf (selected-frame)))
;;                                   `((top   . ,(frame-parameter sf 'top))
;;                                     (left . ,(+ (frame-parameter sf 'left)
;;                                                 (car (cdr (assq 'external-border-size (frame-geometry sf))))))
;;                                     ;; (width . ,(frame-parameter sf 'width))
;;                                     (width . 0.66)
;;                                     (fullscreen . nil)
;;                                     (z-group . above)
;;                                     (user-position . t)
;;                                     (user-size . t)
;;                                     (keep-ratio . t)
;;                                     (child-frame-border-width . 1)
;;                                     (internal-border-width . 1)
;;                                     ))))
;;   (mini-frame-standalone t)) ; Escape parent confines to allow above positioning

;; (defun my/experiment ()
;;   ""
;;   (interactive)
;;   (setq mini-frame-internal-border-color "white")
;;   (setq mini-frame-show-parameters (lambda ()
;;                                 (let ((sf (selected-frame)))
;;                                   (my/echo-area-track-selected-frame)
;;                                   `((top   . ,(+ (frame-parameter sf 'top) 41))
;;                                     (left . ,(+ (frame-parameter sf 'left)
;;                                                 (car (cdr (assq 'external-border-size (frame-geometry sf))))))
;;                                     (width . ,(frame-parameter sf 'width))
;;                                     ;; Remaining parameters mimic minibuffer-frame-alist
;;                                     (user-position . t)
;;                                     (user-size . t)
;;                                     (keep-ratio . t)
;;                                     (child-frame-border-width . 1)
;;                                     (internal-border-width . 1)
;;                                     (fullscreen . nil)))))
;;   (setq mini-frame-standalone t)
;;
;;   ;; (setq mini-frame-show-parameters (lambda ()
;;   ;;                               (let ((sf (selected-frame)))
;;   ;;                                 `((user-position . t)
;;   ;;                                   (user-size . t)
;;   ;;                                   (keep-ratio . t)
;;   ;;                                   (child-frame-border-width . 1)
;;   ;;                                   (internal-border-width . 1)
;;   ;;                                   (fullscreen . nil)
;;   ;;                                   (z-group . nil)
;;   ;;                                   (left . 0)
;;   ;;                                   (width . 1.0)
;;   ;;                                   (top   .  0)))))
;;   ;; (setq mini-frame-standalone nil)
;;
;;   (setq-default header-line-format mode-line-format)
;;   (setq-default mode-line-format nil)
;;   (setq mode-line-format nil))


;;;; # Purpose: purpose-based window management (Bar Magal)
;; https://github.com/bmag/emacs-purpose

(use-package window-purpose
  :disabled
  :straight (:host github :repo "bmag/emacs-purpose")
  :after imenu-list
  :config
  (setq purpose-default-action-order 'prefer-same-window)
  ;; (add-to-list 'purpose-user-mode-purposes`gc50075

  ;;              '((bazel-mode . edit)
  ;;                (terraform-mode . edit))
  ;; (purpose-compile-user-configuration)
  (purpose-mode)
  (require 'window-purpose-x)
  (purpose-x-golden-ratio-setup)
  (purpose-x-popwin-setup)
  (add-to-list 'purpose-special-action-sequences
               '(edit purpose-display-maybe-same-window
                      purpose-display-reuse-window-buffer
                      purpose-display-reuse-window-purpose
                      purpose-display-maybe-other-window
                      purpose-display-maybe-other-frame
                      purpose-display-maybe-pop-up-window
                      purpose-display-maybe-pop-up-frame)))


;;;; # Popper: summon and disimss ephemeral buffer and windows (karthink)
;; https://github.com/karthink/popper

(use-package popper
  :disabled
  :straight (:host github :repo "karthink/popper")
  :after edm
  :custom
  (popper-reference-buffers '("^magit[-:].*$"
                              "\\*Messages\\*"
                              "Output\\*$"
                              help-mode
                              helpful-mode
                              compilation-mode))
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (popper-mode +1))


;;;; # Popwin: force better behavior on popup windows (Tomohiro Matsuyama)
;; https://github.com/emacsorphanage/popwin

(use-package popwin
  :disabled)


;;; === Minibuffer selection ===========================================
;;;; Marginalia: rich annotations for minibuffer completions (Daniel Mendler)
;; https://github.com/minad/marginalia

(use-package marginalia
  :straight (:host github :repo "minad/marginalia")
  :demand t
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :general
  ("M-A" 'marginalia-cycle)             ; cycle annotations: heavy, light, none
  (:keymaps 'minibuffer-local-map
            "M-A" 'marginalia-cycle)
  :config
  (marginalia-mode 1))


;;;; Orderless: orderless completion style (Omar Antolín Camarena)
;; https://github.com/oantolin/orderless

(use-package orderless
  :straight (:host github :repo "oantolin/orderless")
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))



(use-package orderless
  :straight (:host github :repo "oantolin/orderless")
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)    ; I want to be in control!
  (completion-category-overrides
   '((file (styles basic-remote         ; For `tramp' hostname completion with `vertico'
                   orderless
                   ))
     ))

  ;; The following is taken directly from Protesilaos's
  ;; Emacs configuration, with very minor changes. See
  ;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-emacs.el
  :custom
  (orderless-component-separator 'orderless-escapable-split-on-space)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp
     ;; orderless-flex
     ;; orderless-strict-leading-initialism
     ;; orderless-strict-initialism
     ;; orderless-strict-full-initialism
     ;; orderless-without-literal       ; Recommended for dispatches instead
     ))
  (orderless-style-dispatchers
   '(prot-orderless-literal-dispatcher
     prot-orderless-strict-initialism-dispatcher
     prot-orderless-flex-dispatcher
     ))
  :init
  (defun orderless--strict-*-initialism (component &optional anchored)
    "Match a COMPONENT as a strict initialism, optionally ANCHORED.
The characters in COMPONENT must occur in the candidate in that
order at the beginning of subsequent words comprised of letters.
Only non-letters can be in between the words that start with the
initials.

If ANCHORED is `start' require that the first initial appear in
the first word of the candidate.  If ANCHORED is `both' require
that the first and last initials appear in the first and last
words of the candidate, respectively."
    (orderless--separated-by
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)))
      (cl-loop for char across component collect `(seq word-start ,char))
      (when anchored '(seq (group buffer-start) (zero-or-more (not alpha))))
      (when (eq anchored 'both)
        '(seq (zero-or-more alpha) word-end (zero-or-more (not alpha)) eol))))

  (defun orderless-strict-initialism (component)
    "Match a COMPONENT as a strict initialism.
This means the characters in COMPONENT must occur in the
candidate in that order at the beginning of subsequent words
comprised of letters.  Only non-letters can be in between the
words that start with the initials."
    (orderless--strict-*-initialism component))

  (defun prot-orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun prot-orderless-strict-initialism-dispatcher (pattern _index _total)
    "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "," pattern)
      `(orderless-strict-initialism . ,(substring pattern 0 -1))))

  (defun prot-orderless-flex-dispatcher (pattern _index _total)
    "Flex  dispatcher using the tilde suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "." pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  )


;;;; Vertico: vertical interactive completion (Daniel Mendler)
;; https://github.com/minad/vertico

(use-package vertico
  ;; Special recipe to load extensions conveniently
  :straight (vertico :host github :repo "minad/vertico"
                     :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive
                                ))
  :defer ; Loaded via one or more hooks near the bottom of this file
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle nil)
  ;; Extensions
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 50)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  (vertico-multiform-categories
   '((file reverse)
     (consult-grep buffer)
     (consult-location)
     (imenu buffer)
     (library reverse indexed)
     (org-roam-node reverse indexed)
     (t reverse)
     ))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)
     ))
  :general
  (:keymaps '(normal insert visual motion)
            "M-." #'vertico-repeat
            )
  (:keymaps 'vertico-map
            "<tab>" #'vertico-insert ; Set manually otherwise setting `vertico-quick-insert' overrides this
            "<escape>" #'minibuffer-keyboard-quit
            "?" #'minibuffer-completion-help
            "C-M-n" #'vertico-next-group
            "C-M-p" #'vertico-previous-group
            ;; Multiform toggles
            "<backspace>" #'vertico-directory-delete-char
            "C-w" #'vertico-directory-delete-word
            "C-<backspace>" #'vertico-directory-delete-word
            "RET" #'vertico-directory-enter
            "C-i" #'vertico-quick-insert
            "C-o" #'vertico-quick-exit
            "M-o" #'kb/vertico-quick-embark
            "M-G" #'vertico-multiform-grid
            "M-F" #'vertico-multiform-flat
            "M-R" #'vertico-multiform-reverse
            "M-U" #'vertico-multiform-unobtrusive
            "C-l" #'kb/vertico-multiform-flat-toggle
            )
  :hook ((rfn-eshadow-update-overlay . vertico-directory-tidy) ; Clean up file path when typing
         (minibuffer-setup . vertico-repeat-save) ; Make sure vertico state is saved
         )
  :init
  (defun kb/vertico-multiform-flat-toggle ()
    "Toggle between flat and reverse."
    (interactive)
    (vertico-multiform--display-toggle 'vertico-flat-mode)
    (if vertico-flat-mode
        (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
      (vertico-multiform--temporary-mode 'vertico-reverse-mode 1)))
  (defun kb/vertico-quick-embark (&optional arg)
    "Embark on candidate using quick keys."
    (interactive)
    (when (vertico-quick-jump)
      (embark-act arg)))

  ;; Workaround for problem with `tramp' hostname completions. This overrides
  ;; the completion style specifically for remote files! See
  ;; https://github.com/minad/vertico#tramp-hostname-completion
  (defun kb/basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun kb/basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list 'completion-styles-alist
               '(basic-remote           ; Name of `completion-style'
                 kb/basic-remote-try-completion kb/basic-remote-all-completions nil))
  :config
  (vertico-mode)
  ;; Extensions
  (vertico-multiform-mode)

  ;; https://github.com/minad/vertico/wiki#move-annotations-in-front-of-the-candidate
  (defun vertico--swap-annotations (result)
  ;; Move annotations only for files
  (if minibuffer-completing-file-name
      (mapcar (lambda (x)
                ;; Swap prefix/suffix annotations
                (list (car x) (concat (string-trim-left (caddr x)) " ") (cadr x)))
              result)
    result))
  (advice-add #'vertico--affixate :filter-return #'vertico--swap-annotations)
  )

;;;; Consult: commands based on the completing-read function  (Daniel Mendler)
;; https://github.com/minad/consult

;; Make sure that these additional integrations are implemented:
;; consult-compile.el
;; consult-flycheck.el
;; consult-flymake.el
;; consult-icomplete.el
;; consult-xref.el


(use-package consult
  :straight (:host github :repo "minad/consult" :local-repo "consult" :files (:defaults))
  ;; :init
  ;; Optionally configure a function which returns the project root directory
  ;; (autoload 'projectile-project-root "projectile")

  ;; Custom command wrappers. It is generally encouraged to write your own
  ;; commands based on the Consult commands. Some commands have arguments which
  ;; allow tweaking. Furthermore global configuration variables can be set
  ;; locally in a let-binding.
  ;; (defun my-fdfind (&optional dir)
  ;;   (interactive "P")
  ;;   (let ((consult-find-command '("fdfind" "--color=never" "--full-path")))
  ;;     (consult-find dir)))

  :custom
  (consult-project-root-function (lambda () (locate-dominating-file "." ".git")))

  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)

  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (multi-occur #'consult-multi-occur)

  ;; Optionally configure narrowing key.
  ;; Both < and C-+ work reasonably well.
  (consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (register-preview-delay 0)
  (register-preview-function #'consult-register-format)

  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind
  (;; C-c bindings (mode-specific-map)
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-c b" . consult-bookmark)
   ("C-c k" . consult-kmacro)
   ;; C-x bindings (ctl-x-map)
   ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complet-command
   ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)
   ;; Other custom bindings
   ("M-y" . consult-yank-pop)                ;; orig. yank-pop
   ("<help> a" . consult-apropos)            ;; orig. apropos-command
   ;; M-g bindings (goto-map)
   ("M-g e" . consult-compile-error)
   ("M-g g" . consult-goto-line)             ;; orig. goto-line
   ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
   ("M-g o" . consult-outline)
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-project-imenu)
   ;; M-s bindings (search-map)
   ("M-s f" . consult-find)
   ("M-s L" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s m" . consult-multi-occur)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
   ;; Isearch integration
   ("M-s e" . consult-isearch)
   :map isearch-mode-map
   ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
   ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
   ("M-s l" . consult-line))                 ;; required by consult-line to detect isearch
  ;; :bind
  ;; (:map flycheck-command-map
  ;;       ("!" . consult-flycheck))

  :config
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally configure preview. Note that the preview-key can also be
  ;; configured on a per-command basis via `consult-config'. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-p"))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; Probably not needed if you are using which-key.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  )

;; Optionally add the `consult-flycheck' command.
(use-package consult-flycheck
  :disabled
  :bind (:map flycheck-command-map
              ("!" . consult-flycheck)))


;;;; Embark: minibuffer actions rooted in keymaps (Omar Antolín Camarena)
;; https://github.com/oantolin/embark

(use-package embark
  :straight (:host github :repo "oantolin/embark" :local-repo "embark" :files (:defaults))
  :bind
  ("C-S-a" . embark-act)               ; pick some comfortable binding
  :config
  (add-hook 'embark-target-finders #'current-candidate+category)
  (add-hook 'embark-candidate-collectors #'current-candidates+category))

(use-package embark-consult
  :straight (:host github :repo "oantolin/embark" :local-repo "embark" :files (:defaults))
  :after (consult embark)
  :config
  (add-hook 'embark-collect-mode #'embark-consult-preview-minor-mode))

;;; === Buffers: selection, display and navigation =====================
;;;; Ibuffer: operate on buffers like dired (Colin Walters)

(use-package ibuffer
  :straight (:type built-in)
  :custom
  (ibuffer-default-shrink-to-minimum-size t)
  :bind
  (("<f1>" . ibuffer-list-buffers)
   :map ibuffer-mode-map
   ("<f1>" . quit-window))
  :config
  (my/beginning-of-buffer
    ibuffer (ibuffer-forward-line 1))
  (my/end-of-buffer
    ibuffer (ibuffer-backward-line 1)))

;;;; Ibuffer-vc: group buffers in ibuffer list by VC project (Steve Purcell)
;; https://github.com/purcell/ibuffer-vc

(use-package ibuffer-vc
  :straight (:host github :repo "purcell/ibuffer-vc"))


;;;; Improved beginning-of-buffer and end-of-buffer
;; https://fuco1.github.io/2017-05-06-Enhanced-beginning--and-end-of-buffer-in-special-mode-buffers-(dired-etc.).html
;; Web page has support for
;;   Dired (M-x dired)
;;   Occur (M-x occur)
;;   Ibuffer (M-x ibuffer)
;;   vc directory view (M-x vc-dir or C-x v d)
;;   bs (M-x bs-show)
;;   Recentf (M-x recentf-open-files)
;;   Org Agenda (M-x org-agenda)
;;   ag (from ag.el package, M-x ag)
;;   Notmuch (from notmuch package, M-x notmuch-search)
;;   Elfeed (from elfeed package, M-x elfeed)

(defmacro my/beginning-of-buffer (mode &rest forms)
  "Define a special version of `beginning-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-min' and then FORMS are evaluated.  If the point did
not change because of the evaluation of FORMS, jump
unconditionally to `point-min'.  This way repeated invocations
toggle between real beginning and logical beginning of the
buffer."
  (declare (indent 1))
  (let ((fname (intern (concat "my/" (symbol-name mode) "-beginning-of-buffer")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-min))
           ,@forms
           (when (= p (point))
             (goto-char (point-min)))))
       (add-hook ',mode-hook
                 (lambda ()
                   (define-key ,mode-map
                     [remap beginning-of-buffer] ',fname))))))


(defmacro my/end-of-buffer (mode &rest forms)
  "Define a special version of `end-of-buffer' in MODE.

The special function is defined such that the point first moves
to `point-max' and then FORMS are evaluated.  If the point did
not change because of the evaluation of FORMS, jump
unconditionally to `point-max'.  This way repeated invocations
toggle between real end and logical end of the buffer."
  (declare (indent 1))
  (let ((fname (intern (concat "my/" (symbol-name mode) "-end-of-buffer")))
        (mode-map (intern (concat (symbol-name mode) "-mode-map")))
        (mode-hook (intern (concat (symbol-name mode) "-mode-hook"))))
    `(progn
       (defun ,fname ()
         (interactive)
         (let ((p (point)))
           (goto-char (point-max))
           ,@forms
           (when (= p (point))
             (goto-char (point-max)))))
       (add-hook ',mode-hook
                 (lambda ()
                   (define-key ,mode-map
                               [remap end-of-buffer] ',fname))))))


;;; === Completion =====================================================
;;;; Completion: General completion configuration (jao)
;; https://codeberg.org/jao/elibs/src/branch/main/completion.org

(use-package minibuffer
  :straight (:type built-in)
  :custom
  (tab-always-indent 'complete) ; indent.el
  (read-extended-command-predicate #'command-completion-default-include-p) ; simple.el
  (completion-auto-select nil)
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion))
     (command (styles initials substring partial-completion))
     (symbol (styles initials substring partial-completion))
     (variable (styles initials substring partial-completion))))
  (completion-cycle-threshold nil)
  (completion-ignore-case t)
  (completion-show-help nil)
  (completion-show-inline-help nil)
  (completion-styles '(basic substring partial-completion emacs22))
  (completion-wrap-movement t)
  (completions-detailed t)
  (completions-format 'one-column)
  (completions-sort #'jao-completion--sort-by-history)
  :config
  (defun jao-completion--sort-by-alpha-length (elems)
    (sort elems (lambda (c1 c2)
                  (or (string-version-lessp c1 c2)
                      (< (length c1) (length c2))))))

  (defun jao-completion--sort-by-history (elems)
    (let ((hist (and (not (eq minibuffer-history-variable t))
                     (symbol-value minibuffer-history-variable))))
      (if hist
          (minibuffer--sort-by-position hist elems)
        (jao-completion--sort-by-alpha-length elems))))
  )


;;;; Corfu: Completion Overlay Region FUnction (Daniel Mendler)
;; https://github.com/minad/corfu

(use-package corfu
  :straight (corfu :host github :repo "minad/corfu")
  :defer ; Loaded via one or more hooks near the bottom of this file
  :custom
  (corfu-auto t)                        ; Enable auto completion
  (corfu-auto-delay 0.25)               ; Delay for auto completion
  (corfu-auto-prefix 3)                 ; Min prefix length for auto completion
  (corfu-count 10)                      ; Max # of candidates to show
  (corfu-cycle nil)                     ; Enable corfu-next / previous cycling
  (corfu-echo-documentation '(1.0 . 0.2))
  (corfu-min-width 30)
  (corfu-min-width 99)
  (corfu-preselect-first t)             ; Preselect first candidate
  (corfu-preview-current 'insert)       ; First candidate as overlay. Insert on input if only one
  (corfu-quit-at-boundary 'separator)   ; Boundary: stay alive if separator inserted
  (corfu-quit-no-match 'separator)      ; No match: stay alive if separator inserted
  (corfu-scroll-margin 4)               ; Top and bottom lines when scrolling
  (corfu-separator ?\s)                 ; Component separator character

  :config
  ;; show eldoc string immediately after accepted completion too
  (with-eval-after-load "eldoc"
    (eldoc-add-command-completions "corfu-"))

  (defun jao-corfu--active-p ()
    (and (>= corfu--index 0) (/= corfu--index corfu--preselect)))

  (defun jao-corfu-quit-or-insert ()
    (interactive)
    (if (jao-corfu--active-p) (corfu-insert) (corfu-quit)))

  (defun jao-corfu-quit-or-previous ()
    (interactive)
    (if (jao-corfu--active-p)
        (corfu-previous)
      (corfu-quit)
      (previous-line)))

  :general
  (:keymaps 'corfu-map
            "C-n"        #'corfu-next
            "C-p"        #'corfu-previous
            "<escape>"   #'corfu-quit
            "C-<return>" #'corfu-insert
            "\r"         #'jao-corfu-quit-or-insert
;           "<return>"   #'corfu-insert
            "H-SPC"      #'corfu-insert-separator
            ;; "SPC" #'corfu-insert-separator ; Use when `corfu-quit-at-boundary' is non-nil
            "M-d"        #'corfu-show-documentation
            "C-g"        #'corfu-quit
            "M-l"        #'corfu-show-location))

;; For lsp-mode buffers:
;;
;; :hook (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
;; :custom
;; (lsp-completion-provider :none) ; Use corfu instead the default for lsp completions
;; :config
;; ;; Setup lsp to use corfu for lsp completion
;; (defun kb/corfu-setup-lsp ()
;;   "Use orderless completion style with lsp-capf instead of the
;;   default lsp-passthrough."
;;   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;         '(orderless)))


;;; === Editing ========================================================
;;;; Whitepace and general hygiene

;; TO CONSIDER: ws-butler (https://github.com/lewang/ws-butler)
;; (use-package whitespace
;;   :straight (:type built-in)
;;   :custom
;;   (whitespace-global-mode t))


(use-package files
  :straight (:type built-in)
  :custom
  (require-final-newline 'query))


;;;; DWIM insert closer (')', ']' or '}')

(defun paren-close-dwim ()
  "Insert closing parenthesis from syntax table.
Use a normal parenthesis if not inside any."
  (interactive "*")
  (insert (or (ignore-errors
                (save-excursion (backward-up-list)
                                (cdr (syntax-after (point)))))
              ?\))))


;;; === Autotype: completion and expansion =============================
;;;; # Company:


;;;; # Company-quickhelp: completion popup documentation (Lars Andersen)
;; https://github.com/company-mode/company-quickhelp

(use-package company-quickhelp
  :disabled
  :requires (company pos-tip))


;;;; # Auto-insert-mode: (builtin: Charlie Martin)

(use-package autoinsert
  :disabled
  :custom
  (auto-insert-mode t))


;;;; # Yatemplate: populate auto-insert-alist (Wieland Hoffmann)
;; https://github.com/mineo/yatemplate

(use-package yatemplate
  :disabled)


;;;; # Yasnippet: a TextMate inspired template system  (João Távora)
;; https://github.com/joaotavora/yasnippet

(use-package yasnippet
  :disabled
  :custom
  (yas-snippet-dirs "~/emacs/yasnippet")
  :mode ("emacs/yasnippet/"         . snippet-mode)
  :mode ("emacs/yasnippet/.+\\.el$" . emacs-lisp-mode)
  :config
  (yas-global-mode t))


;; reload modified snippets
;; (defun my/yasnippet-reload-on-save ()
;;   "Reload the entire collection of snippets when one gets modified."
;;   (if (eq major-mode 'snippet-mode)
;;       ;;  (mapc 'yas/load-directory yas/snippet-dirs)))
;;       (yas-reload-all))) ; no mapc with just a single directory root


;;;; # Gendoxy: generate doxygen from C source (Michele Pes)
;; https://github.com/mp81ss/gendoxy

(use-package gendoxy
  :disabled
  :straight (:host github :repo "mp81ss/gendoxy")
  :custom
  (gendoxy-backslash t))


;;; === File, directories, URLs and FFAP ===============================
;;;; FFAP

(use-package ffap
  :custom
  (ffap-require-prefix t)       ; so that neophytes notice ffap
  :config
  (ffap-bindings))


;;;; Ilocate-library: interactive locate-library (Michael Slass)
;; https://github.com/jsyjr/ilocate-library

(use-package ilocate-library
  :straight (:host github :repo "jsyjr/ilocate-library")
  :bind
  ("M-g  l" . ilocate-library-find-source))

;;; === Diff, ediff, vdiff, merge, patch ===============================
;;;; Ediff: (builtin: Michael Kifer)

(use-package ediff
  :straight (:type built-in)
  :custom
  (ediff-cmp-program "git")
  (ediff-cmp-options '("diff --histogram"))
  (ediff-keep-variants nil)
  (ediff-make-buffers-readonly-at-startup t)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-use-last-dir t)
  (ediff-window-setup-function 'ediff-setup-windows-plain))


;;;; Vdiff: vimdiff-inspired diffing (Justin Burkett)
;; https://github.com/justbur/emacs-vdiff

(use-package vdiff
  :straight (:host github :repo "justbur/emacs-vdiff"))


;;;; Smerge-mode: (builtin: Stefan Monnier)

(use-package smerge-mode
  :straight (:type built-in)
  :custom
  (smerge-auto-leave nil)
  (smerge-command-prefix ""))


;;; === Symbol navigation ==============================================
;;;; Xref: cross-referencing commands (Dmity Gutov)

(use-package xref
  :straight (:type built-in)
  :custom
  (xref-search-program: 'ripgrep)
  (xref-file-name-display: 'project-relative))


;;;; Dumb-jump: use ripgrep to goto definition (Jack Angers)
;; https://github.com/jacktasia/dumb-jump

;; Default bindings:
;; C-M-g  dumb-jump-go
;; C-M-p  dumb-jump-back
;; C-M-q  dumb-jump-quick-look

(use-package dumb-jump
  :disabled
  :straight (:host github :repo "jacktasia/dumb-jump")
  :custom
  (dumb-jump-prefer-searcher 'rg)
  :config
  (setq xref-backend-functions (remq 'etags--xref-backend xref-backend-functions))
  (add-to-list 'xref-backend-functions #'dumb-jump-xref-activate t))

  ;; Obsolete function (as of 2020-06-26); see github page (preserve as :hydra example)
  ;;
  ;; :hydra
  ;; (hydra-dumb-jump (:color blue :columns 3)
  ;;   "Dumb Jump"
  ;;   ("j" dumb-jump-go "Go")
  ;;   ("o" dumb-jump-go-other-window "Other window")
  ;;   ("e" dumb-jump-go-prefer-external "Go external")
  ;;   ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
  ;;   ("i" dumb-jump-go-prompt "Prompt")
  ;;   ("l" dumb-jump-quick-look "Quick look")
  ;;   ("b" dumb-jump-back "Back")))


;;; === Projects and project navigation ================================
;;;; Project: operations on a current project

(use-package project
  :straight (::type built-in))

;;;; # Projectile: project interaction library (Bozhidar Batsov)
;; https://github.com/bbatsov/projectile

(use-package projectile
  :disabled)


;;; === VC, git, magit, etc ============================================
;;;; Vc: a version control system driver (Eric S. Raymond)

(use-package vc
  :disabled
  :straight (:type built-in)
  ;; :custom
  ;; (vc-follow-symlinks true)
  :init
  (defun my/vc-follow-some-symbolic-links ()
    (message "my/vc-follow-some-symbolic-links: %s" buffer-file-name)
    (when (and
           (file-exists-p buffer-file-name)
           (stringp buffer-file-name)
           (or (string-prefix-p "/home/jyates/.emacs.d/straight/repos/" buffer-file-name)
               (string-equal "" buffer-file-name)))
         (setq-local vc-follow-symlinks t)))
  :hook
  (find-file-hook . my/vc-follow-some-symbolic-links)
  )


;;;; Magit: a git porcelain inside Emacs (Jonas Bernoulli)
;; https://github.com/magit/magit

(use-package magit
  :straight (:host github :repo "magit/magit")
  :requires (magit-process compat) ;; not sure why this is necessary
  :custom
  (magit-auto-revert-mode t)
  (magit-backup-mode nil)
;  (magit-diff-refine-hunk t)   ;; why is this disabled? ugly faces?
  (magit-display-buffer-function 'magit-display-buffer-fullcolumn-most-v1)
  (magit-refs-show-commit-count 'all)
  (magit-repository-directories-depth 0 t)
  (magit-save-repository-buffers nil)
  (git-commit-setup-hook
   '(git-commit-save-message
     git-commit-setup-changelog-support
     git-commit-turn-on-auto-fill
     git-commit-propertize-diff
     with-editor-usage-message))
  :config
  (defun my/magit-status ()
    "Switch to (or create) the magit status buffer for current context"
    (interactive)
    (let ((dir (locate-dominating-file "." ".git")))
      (if (not dir)
          (error "Directory not within a git workspace (%s)" default-directory)
        (let ((mbuf (get-buffer (concat "*magit: " (file-name-as-directory (substring dir 0 -1)) "*"))))
          (if mbuf
              (switch-to-buffer mbuf)
            (magit-status-setup-buffer dir)))))))


;;;; Vdiff-magit: integrate vdiff into magit (Justin Burkett)
;; https://github.com/justbur/emacs-vdiff-magit

(use-package vdiff-magit
  :straight (:host github :repo "justbur/emacs-vdiff-magit")
  :requires (vdiff magit))


;;;; Git-timemachine: forward and back through file revisions (Peter Stiernström)
;; https://github.com/emacsmirror/git-timemachine

(use-package git-timemachine)


;;; === Searching the file system ======================================
;;;; Deadgrep: fast, beautiful text search (Wilfred Hughes)
;; https://github.com/Wilfred/deadgrep

(use-package deadgrep
  :commands deadgrep
  :bind
  ("C-c r ." . deadgrep))


;;;; Wgrep: apply changes by editting a grep buffer (Masahiro Hayashi)
;; https://github.com/mhayashi1120/Emacs-wgrep

(use-package wgrep
  :straight (:host github :repo "mhayashi1120/Emacs-wgrep")
  )


;;;; Rg: a search tool based on ripgrep (David Landell)
;; https://github.com/dajva/rg.el
;; https://rgel.readthedocs.io/en/2.0.3/

(use-package rg
  :straight (:host github :repo "dajva/rg.el")
  :requires (rg-menu transient wgrep)
  :bind
  (("C-c r d" . rg-dwim)
   ("C-c r k" . rg-kill-saved-searches)
   ("C-c r l" . rg-list-searches)
   ("C-c r m" . rg-menu)
   ("C-c r p" . rg-project)
   ("C-c r r" . rg)
   ("C-c r s" . rg-save-search)
   ("C-c r S" . rg-save-search-as-name)
   ("C-c r t" . rg-literal)))


;;; === Moving, in file searching and editing ==========================
;;;; Update copyright and timestamp before saving files

; (use-package copyright
;   :straight (:type built-in)
;   :hook (before-save copyright-update)
;   :config
;   (put 'copyright-owner 'safe-local-variable 'stringp)
;   (defconst copyright-owner "John S Yates Jr"))

(use-package time-stamp
  :straight (:type built-in)
  :hook (before-save time-stamp))


;;;; Kill kills end-of-line

(use-package simple
  :straight (:type built-in)
  :custom
  (kill-whole-line t))


;;;; Use view-mode if file is or opened read-only
(use-package files
  :straight (:type built-in)
  :custom
  (view-read-only t))


;;;; Avy: (Oleh Krehel)
;; https://github.com/abo-abo/avy

(use-package avy
  :config
  (avy-setup-default)
; TODO: assign a key
;  :bind ("M-s" . avy-goto-char)
  )


;;;; Smarter move-beginning-of-line, move-end-of-line

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


;;;; Improved beginning-of-buffer and end-of-buffer

(eval-after-load "dired"
  '(progn
     (my/beginning-of-buffer
      dired (while (not (ignore-errors (dired-get-filename)))
              (dired-next-line 1)))
     (my/end-of-buffer
      dired (dired-previous-line 1))))

(eval-after-load "occur"
  '(progn
     (my/beginning-of-buffer
      occur (occur-next 1))
     (my/end-of-buffer
      occur (occur-prev 1))))

(eval-after-load "ibuffer"
  '(progn
     (my/beginning-of-buffer
      ibuffer (ibuffer-forward-line 1))
     (my/end-of-buffer
      ibuffer (ibuffer-backward-line 1))))

(eval-after-load "vc-dir"
  '(progn
     (my/beginning-of-buffer
      vc-dir (vc-dir-next-line 1))
     (my/end-of-buffer
      vc-dir (vc-dir-previous-line 1))))

(eval-after-load "bs"
  '(progn
     (my/beginning-of-buffer
      bs (bs-down 2))
     (my/end-of-buffer
      bs (bs-up 1) (bs-down 1))))

(eval-after-load "org-agenda"
  '(progn
     (my/beginning-of-buffer
      org-agenda (org-agenda-next-item 1))
     (my/end-of-buffer org-agenda (org-agenda-previous-item 1))))

(eval-after-load "ag"
  '(progn
     (my/beginning-of-buffer
      ag (compilation-next-error 1))
     (my/end-of-buffer
      ag (compilation-previous-error 1))))


;;;; Delete selection mode


(use-package delsel
  :straight (:type built-in)
  :custom
  (activate-mark #'my/turn-on-delete-selection-mode)
  :config
  (defun my/turn-on-delete-selection-mode ()
    (remove-hook activate-mark-hook #'my/turn-on-delete-selection-mode)
    (delete-selection-mode 1)))


;;;; Simple editing operations

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

(put 'narrow-to-region 'disabled nil)


;;;; White space hygiene

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

;; ONLY REENABLE THIS
;; (use-package whitespace
;;   :straight (:type built-in)
;;   :custom
;;  (whitespace-global-mode t))

;; (my/custom-set-faces
;;  '(trailing-whitespace ((((class color) (background dark)) (:background "red3"))))
;;  )


;;;; Indent yanked text (cribbed from Alex Ott)

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


;;;; Tabs

(use-package emacs ; tabbing and tab stops
  :straight (:type built-in)
  :custom
  (indent-tabs-mode nil)        ; no hard tabs
  (tab-always-indent nil)       ; indent, complete else insert whitespace
  (tab-stop-list '(  4   8  12  16  20  24  28  32  36  40
                    44  48  52  56  60  64  68  72  76  80
                    84  88  92  96 100 104 108 112 116 120
                   124 128 132 136 140 144 148 152 165 160
                   164 168 172 176 180 184 188 192 196 200
                   204 208 212 216 220 224 228 232 236 240
                   244 248 252 265 260 264 268 272 276 280
                   284 288 292 296 300 304 308 312 316 320)))



(defun my/canonicalize-tab4 ()
  (interactive)
  (let ((tab-width 4))
    (untabify (point-min) (point-max))
    (tabify (point-min) (point-max))))

;;;; Buffer life cycle: find, save if editted, revert if modified, toggle R/O

;; Inspired by:
;; disk: simplified find-file, revert-file, save-buffer (Alex Schroeder)
;; http://www.emacswiki.org/cgi-bin/wiki.pl?DiskKey

(defvar disk-access nil)
(make-variable-buffer-local 'disk-access)

(add-hook 'find-file-hook  #'disk-notice)
(add-hook 'view-mode-hooks #'disk-notice)
(add-hook 'after-save-hook #'disk-notice)

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

(defun my/disk ()
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


;;;; Undo-tree: (Toby Cubitt)
;; https://gitlab.com/tsc25/undo-tree

(use-package undo-tree
  :demand t)


;;;; Iedit: edit multiple regions in the same way simultaneously (Victor Ren)
;; https://github.com/victorhge/iedit

(use-package iedit
  :straight (:host github :repo "victorhge/iedit")
  :general ("C-c ;" 'iedit-mode))


;;;; Smartscan: jumps between other symbols found at point (Mickey Petersen)
;; https://github.com/mickeynp/smart-scan

;; Introduces bindings:
;;
;; M-n  smartscan-symbol-go-forward
;; M-p  smartscan-symbol-go-backward
;; M-'  smartscan-symbol-replace         [conflicts with abbrev-prefix-mark]

(use-package smartscan)


;;; === Minor modes ====================================================
;;;; Folding: folding-editor-like minor mode (Jari Aalto)
;; https://github.com/jaalto/project-emacs--folding-mode
;; Used in my older .emacs configuration

(use-package folding
  :straight (:host github :repo "jaalto/project-emacs--folding-mode"))

(autoload 'folding-mode "folding" "Folding mode" t)


;;;; Hideshowvis: clickable fringe +/- for hideable regions (sheijk: Jan R)

;; https://github.com/sheijk/hideshowvis

(use-package hideshowvis)


;;;; Hideshow:

(use-package hideshow
  :requires (hideshowvis)
  ;; :custom
  ;; (hs-set-up-overlay #'my/display-code-line-counts)
  :config
  (hideshowvis-enable))

;; https://www.reddit.com/r/emacs/comments/746cd0/comment/dnwi2x1/?utm_source=share&utm_medium=web2x&context=3
;;
;; (defun toggle-fold ()
;;   (interactive)
;;   (save-excursion
;;     (end-of-line)
;;     (hs-toggle-hiding))

;; Display the size of a collapsed function body  (obsolete?)
;; (defun my/display-code-line-counts (ov)
;;   (when (eq 'code (overlay-get ov 'hs))
;;     (let ((str (format " %d " (count-lines (overlay-start ov)
;;                                            (overlay-end ov)))))
;;       (put-text-property 0 (length str) 'face 'glyphless-char str)
;;       (overlay-put ov 'display str))))




;;;; Outshine

(use-package outshine
  ;; Easier navigation for source files, especially this one.
  :custom
  (outshine-startup-folded-p nil)
  :bind (:map outshine-mode-map
              ("<S-iso-lefttab>" . outshine-cycle-buffer))
  :hook (emacs-lisp-mode . outshine-mode))


;;;; Filladapt: enhance default auto-fill behavior (Kyle E. Jones)
;; http://elpa.gnu.org/packages/filladapt-2.12.2.el

(use-package filladapt
  :config
  (defun my/turn-on-filling ()
    (text-mode-hook-identify)  ; mark buffer for toggle-text-mode-auto-fill
    (turn-on-auto-fill)
    (ignore-errors
      (require 'filladapt)
      (turn-on-filladapt-mode)))

  (defun my/text-mode ()
    (my/turn-on-filling)
    (setq fill-column 80))
  :hook (text-mode . my/text-mode))


;;; === Abbreviation and expansion =====================================


;;; === Terminal and shell support =====================================
;;;; libvterm: a fully-fledged terminal emulator within Emacs (Lukas Fürmetz)
;; https://github.com/akermu/emacs-libvterm

;; (use-package vterm
;;   :straight (:host github :repo "akermu/emacs-libvterm"))

;;; === Utilities ======================================================
;;;; Recentf: a menu of recently opened files (David Ponce)

(use-package recentf
  :straight (:type built-in)
  :defer ; Loaded via one or more hooks near the bottom of this file
  :custom
  (recentf-save-file "~/.emacs.d/recentf")
  :config
  (my/beginning-of-buffer
    recentf-dialog (when (re-search-forward "^  \\[" nil t)
                     (goto-char (match-beginning 0))))
  (my/end-of-buffer
    recentf-dialog (re-search-backward "^  \\[" nil t)))


;;; === Major modes ====================================================
;;;; sgml-mode, includeing html-mode (James Clark, ESR)

;; (defun my/html-hook ()
;;   (set-indentation-common-hook)
;;   (cond (have-dump-jump
;;          (define-key html-mode-map "\M-." #'dumb-jump-go)
;;          (define-key html-mode-map "\M-," #'dumb-jump-back)
;;          (define-key html-mode-map "\M-/," #'dumb-jump-quick-look)
;;          (define-key html-mode-map "\M-/." #'dumb-jump-go-prompt)
;;          (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))))

;;; === Programming ====================================================
;;;; Elisp

(use-package eldoc
  :straight (:type built-in)
  :custom
  (global-eldoc-mode -1))




;;;; gdb-mi

(use-package gdb-mi
  :straight (:type built-in)
  :custom
  (gdb-create-source-file-list nil)
  (gdb-many-windows t)
  (gdb-stack-buffer-addresses t)
  (gdb-thread-buffer-arguments nil)
  (gud-gdb-command-name "gdb -i=mi")
  (split-width-threshold 1023)         ;; Crummy window splitting rules for wide screens
  :config

  (defun my/gdb-new-frame ()
    (interactive)
    (select-frame (make-frame))
    (call-interactively 'gdb))

  (defvar gdb-source-window nil)
  (defvar gdb-comint-window nil)

  ;; Mathworks trashes the program to run when requesting gdb
  ;; (when (file-exists-p "/mathworks/hub/share/sbtools/emacs_setup.el")
  ;;   (setq gud-gdb-command-name "~/bin/gdb"))

  (defun my/gud-eob ()
    "Jump to EOB in GUD's interaction buffer."
    (select-window gdb-comint-window)
    (goto-char (point-max)))

  (defmacro my/gud-def (func cmd stay-put &optional doc)
    "Define FUNC as sending CMD. See gud.el's gud-def for more details."
    `(defalias ',func (lambda (arg)
                        ,@(if doc (list doc))
                        (interactive "p")
                        (when (not gud-running)
                          ,(unless (zerop stay-put)
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
  (my/gud-def my/gud-cont   "cont"         1 "Continue with display.")
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
  (my/gud-def my/gud-run    "run"          1 "Run/restart program.")
  (my/gud-def my/gud-frame0 "frame 0"      0 "Return to stack frame 0 in source window.")
  (my/gud-def my/gud-prompt "frame"        1 "Jump to EOB in GUD's interaction buffer.")


  (defun my/dedicate-gdb-comint-window()
    "Mark gud-comint-buffer's window as dedicated for gdb-many-windows"
    (when (and gud-comint-buffer gdb-many-windows)
      (set-window-dedicated-p (setq gdb-comint-window (get-buffer-window gud-comint-buffer)) t)))

  (eval-after-load "gdb-mi"
    '(advice-add 'gdb-setup-windows :after #'my/dedicate-gdb-comint-window))

  (defun my/gud-help ()
    "Toggle display of a table of GDB key bindings."
    (interactive)
    (let ((gdb-keys (get-buffer "*GDB key bindings*")))
      (unless gdb-keys
        (setq gdb-keys (get-buffer-create "*GDB key bindings*"))
        (with-current-buffer gdb-keys
          (buffer-disable-undo)
          (read-only-mode -1)
          (erase-buffer)
          (insert
           "
Modi |
fier | <F5>     <F6>     <F7>     <F8>     <F9>     <F10>    <F11>    <F12>
---- | ------   ------   ------   ------   ------   ------   ------   ------
     | run      print    print    goto     step     next     set      customize
     |                   via pp   prompt   into     stmt     break    option
     |
Ctrl | run to   unit     compile  toggle   step     frame    temp     customize
     | cursor   test              help     instr    return   break    group
     |
Shft | rerun    print*   print*   frame    frame    frame    remove   customize
     |                   via pp   zero     down     up       break    apropos
")
          (set-buffer-modified-p nil)
          (view-mode 1)
          ))
      (with-current-buffer gdb-keys
        (let ((win (get-buffer-window)))
          (if (not win)
              (set-window-buffer gdb-source-window gdb-keys)
            (select-window win)
            (bury-buffer))))
      (my/gud-eob)))


;;   ;; From http://markshroyer.com/2012/11/emacs-gdb-keyboard-navigation/
;;
;;   ;; For the consistency of gdb-select-window's calling convention...
;;   (defun gdb-comint-buffer-name ()
;;     (buffer-name gud-comint-buffer))
;;   (defun gdb-source-buffer-name ()
;;     (buffer-name (window-buffer gdb-source-window)))
;;
;;   (defun gdb-select-window (header)
;;     "Switch directly to the specified GDB window.
;; Moves the cursor to the requested window, switching between
;; `gdb-many-windows' \"tabs\" if necessary in order to get there.
;;
;; Recognized window header names are: 'comint, 'locals, 'registers,
;; 'stack, 'breakpoints, 'threads, and 'source."
;;
;;     (interactive "Sheader: ")
;;
;;     (let* ((header-alternate (case header
;;				  ('locals	'registers)
;;				  ('registers	'locals)
;;				  ('breakpoints 'threads)
;;				  ('threads	'breakpoints)))
;;	      (buffer (intern (concat "gdb-" (symbol-name header) "-buffer")))
;;	      (buffer-names (mapcar (lambda (header)
;;				      (funcall (intern (concat "gdb-"
;;							       (symbol-name header)
;;							       "-buffer-name"))))
;;				    (if (null header-alternate)
;;					(list header)
;;				      (list header header-alternate))))
;;	      (window (if (eql header 'source)
;;			  gdb-source-window
;;			(or (get-buffer-window (car buffer-names))
;;			    (when (not (null (cadr buffer-names)))
;;			      (get-buffer-window (cadr buffer-names)))))))
;;
;;	 (when (not (null window))
;;	   (let ((was-dedicated (window-dedicated-p window)))
;;	     (select-window window)
;;	     (set-window-dedicated-p window nil)
;;	     (when (member header '(locals registers breakpoints threads))
;;	       (switch-to-buffer (gdb-get-buffer-create buffer))
;;	       (setq header-line-format (gdb-set-header buffer)))
;;	     (set-window-dedicated-p window was-dedicated))
;;	   t)))
;;
;;   ;; Use global keybindings for the window selection functions so that they
;;   ;; work from the source window too...
;;   (mapcar (lambda (setting)
;;	       (lexical-let ((key    (car setting))
;;			     (header (cdr setting)))
;;		 (global-set-key (concat "\M-g" key) #'(lambda ()
;;							     (interactive)
;;							     (gdb-select-window header)))))
;;	     '(("A" . disassembly)	   ; assembly language
;;	       ("B" . breakpoints)
;;	       ("D" . disassembly)
;;	       ("F" . stack)
;;	       ("G" . comint)
;;	       ("I" . disassembly)	   ; instructions
;;	       ("L" . locals)
;;	       ("O" . input/output)
;;	       ("R" . registers)
;;	       ("S" . source)
;;	       ("T" . threads)))

;; Make sure the file named TRUE-FILE is in a buffer that appears on the screen
;; and that its line LINE is visible.
;; Put the overlay-arrow on the line LINE in that buffer.
;; Most of the trickiness in here comes from wanting to preserve the current
;; region-restriction if that's possible.  We use an explicit display-buffer
;; to get around the fact that this is called inside a save-excursion.

;; (defadvice gud-display-line (around my/gud-display-line activate)
;;   (let* ((last-nonmenu-event t)	 ; Prevent use of dialog box for questions.
;;	  (buffer
;;	   (with-current-buffer gud-comint-buffer
;;	     (gud-find-file true-file)))
;;	    ;;================
;;	  ;; (window (and buffer
;;	  ;;		  (or (get-buffer-window buffer)
;;	  ;;		   (display-buffer buffer))))
;;	    ;;================
;;	    (window (and buffer
;;			 (or (if (eq gud-minor-mode 'gdbmi)
;;				 (unless (gdb-display-source-buffer buffer)
;;				   (gdb-display-buffer buffer nil 'visible)))
;;			     (get-buffer-window buffer)
;;			     (display-buffer buffer))))
;;	    ;;================
;;	  (pos))
;;     (when buffer
;;	 (with-current-buffer buffer
;;	 (unless (or (verify-visited-file-modtime buffer) gud-keep-buffer)
;;	   (if (yes-or-no-p
;;		(format "File %s changed on disk.  Reread from disk? "
;;			(buffer-name)))
;;	       (revert-buffer t t)
;;	     (setq gud-keep-buffer t)))
;;	 (save-restriction
;;	   (widen)
;;	   (goto-char (point-min))
;;	   (forward-line (1- line))
;;	   (setq pos (point))
;;	   (or gud-overlay-arrow-position
;;	       (setq gud-overlay-arrow-position (make-marker)))
;;	   (set-marker gud-overlay-arrow-position (point) (current-buffer))
;;	   ;; If they turned on hl-line, move the hl-line highlight to
;;	   ;; the arrow's line.
;;	   (when (featurep 'hl-line)
;;	     (cond
;;	      (global-hl-line-mode
;;	       (global-hl-line-highlight))
;;	      ((and hl-line-mode hl-line-sticky-flag)
;;	       (hl-line-highlight)))))
;;	 (cond ((or (< pos (point-min)) (> pos (point-max)))
;;		(widen)
;;		(goto-char pos))))
;;	 (when window
;;	 (set-window-point window gud-overlay-arrow-position)
;;	 (if (eq gud-minor-mode 'gdbmi)
;;	     (setq gdb-source-window window))))))
  :general
  (:keymaps 'global
              "<f5>" #'my/gud-cont      ; MS go / continue
            "C-<f5>" #'my/gud-until     ; MS run to cursor
            "S-<f5>" #'my/gud-run       ; restart

              "<f6>" #'my/gud-print     ; print  %e
            "C-<f6>" #'my/gud-run-ut    ; run unit test
            "S-<f6>" #'my/gud-pstar     ; print* %e

              "<f7>" #'my/gud-pprint    ; print  %e via pp
            "C-<f7>" #'my/compile
            "S-<f7>" #'my/gud-ppstar    ; print* %e via pp

              "<f8>" #'my/gud-prompt    ; focus GUD prompt
            "C-<f8>" #'my/gud-help
            "S-<f8>" #'my/gud-frame0

              "<f9>" #'my/gud-step      ; MS step into
            "C-<f9>" #'my/gud-stepi     ; step by instructi
            "S-<f9>" #'my/gud-down

              "<f10>" #'my/gud-next     ; MS step over
            "C-<f10>" #'my/gud-finish   ; MS step out
            "S-<f10>" #'my/gud-up

              "<f11>" #'my/gud-break
            "C-<f11>" #'my/gud-tbreak
            "S-<f11>" #'my/gud-remove)
)

;;;; compile: run inferior compiler, parse error messages

(use-package compile
  :straight (:type built-in)
  :general
  (keymaps 'global
             "<f4>" #'next-error
           "C-<f4>" #'first-error
           "S-<f4>" #'kill-compilation
           "C-<f7>" #'my/compile)
  :custom
; (compile-command "/usr/bin/make -k")
  (compilation-scroll-output 'first-error) ; follow compilation output
; (compilation-skip-threshold 2)           ; next-errormpil should only stop at errors
  (next-error-hook 'compile-goto-error)
  (next-error-recenter '(4))
; :custom-face
; (compilation-column-number ((t (:inherit file-name-shadow           ))))
; (compilation-info          ((t (:inherit font-lock-comment-face     ))))
; (compilation-line-number   ((t (:inherit font-lock-preprocessor-face))))
  :init
  ;; File is in a test directory if it contains a suite_registration.cpp
  ;; Should locate dominating module directory by looking for Makefile
  ;; Name of image is path from module directory to test directory with '/' replaced by '_'
  ;;
  ;;   cd /ws/WARP/matlab/src/<MODULE>/
  ;;   rm -f /ws/WARP/matlab/derived/glnxa64/testbin/src/<MODULE>/<TEST_IMAGE>
  ;;   cgmake -no-distcc DEBUG=1
  ;;   /ws/WARP/matlab/derived/glnxa64/testbin/src/<MODULE>/<TEST_IMAGE>

  (defun my/compile-command ()
    "Return a compile command for the current (.cpp) buffer.

If not in a unittest directory compile only the single file.
If in a unittest directory locate the module directory, run
cgmake -no-distcc there and if successful run the test image."
    (let* ((cwd (file-name-directory (buffer-file-name)))
           (exe nil)
           (cmd (if (not (locate-dominating-file "." "unittest"))
                    (concat "sbcc -mc " (buffer-name))
                  (while (cond
                          ((string-suffix-p "unittest/" cwd)
                           (setq exe "unittest")
                           (setq cwd (substring cwd 0 -9)))
                          ((string-suffix-p "my/" cwd)
                           (setq exe "my_unittest")
                           (setq cwd (substring cwd 0 -3)))
                          (t
                           nil)))
                  (let* ((path (split-string cwd "matlab/src"))
                         (target (concat (nth 0 path) "matlab/derived/glnxa64/testbin/src" (nth 1 path) exe)))
                    (concat
                     "rm -f " target "\n"
                     "cgmake -no-distcc DEBUG=1\n"
                     target)))))
      (concat "cd " cwd "\n" cmd)))

  (defun my/compile (command &optional comint)
    "If within a Mathworks sandbox recompile most recent .cpp.
A minor variant of the compile function shipped in compile.el.
Here is the doc-string for that original function:

Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*compilation*'.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

If optional second arg COMINT is t the buffer will be in Comint mode with
`compilation-shell-minor-mode'.

Interactively, prompts for the command if the variable
`compilation-read-command' is non-nil; otherwise uses `compile-command'.
With prefix arg, always prompts.
Additionally, with universal prefix arg, compilation buffer will be in
comint mode, i.e. interactive.

To run more than one compilation at once, start one then rename
the `*compilation*' buffer to some other name with
\\[rename-buffer].  Then _switch buffers_ and start the new compilation.
It will create a new `*compilation*' buffer.

On most systems, termination of the main compilation process
kills its subprocesses.

The name used for the buffer is actually whatever is returned by
the function in `compilation-buffer-name-function', so you can set that
to a function that generates a unique name."
    (interactive
     (list
      (let ((command (progn
                       (if (string-suffix-p ".cpp" (buffer-name))
                           (setq compile-command (my/compile-command)))
                       (eval compile-command))))
        (if (or compilation-read-command current-prefix-arg)
	    (compilation-read-command command)
	  command))
      (consp current-prefix-arg)))
    (unless (equal command (eval compile-command))
      (setq compile-command command))
    (save-some-buffers (not compilation-ask-about-save)
                       compilation-save-buffers-predicate)
    (setq-default compilation-directory default-directory)
    (compilation-start command comint))
  :config
  ;; Fix next error so that it does not visit every file in the include stack
  ;; leading up the the error site.  Lifted directly from stackoverflow
  ;; how-can-i-skip-in-file-included-from-in-emacs-c-compilation-mode
  (setf (nth 5 (assoc 'gcc-include compilation-error-regexp-alist-alist)) 0)
  )


;;; === Mathworks stuff ================================================
;;;; mw-jsy: my personal Mathworks stuff
;; https://github.com/jsyjr/my-jsy

(use-package mw-jsy
  :straight (:host github :repo "jsyjr/mw-jsy")
  :defer
  :custom
  (sb-separate-kill-ring-from-clipboard nil)
  )


;;;; wsf: find a file in an indexed Mathworks workspace
;; https://github.com/jsyjr/wsf

(use-package wsf
  :straight (:host github :repo "jsyjr/wsf")
  :after mw-jsy
  :general
  (:keymaps 'sb-prefix-map
            "f" #'wsf-find-file
            "R" #'wsf-force-from-scratch)
  )


;;;; my/workspace-shell

(defun my/workspace-shell (WORKSPACE)
  "Create or switch to a running shell process in WORKSPACE."
  (interactive "BWorkspace: ")
  (let ((default-directory (concat "/ws/" WORKSPACE "/")))
    (shell WORKSPACE))
  (load-library "mw-jsy"))


;;; === Unclassified ===================================================

;;{{{  Less safe: y/n instead of yes/no

(fset 'yes-or-no-p 'y-or-n-p)


;;; === Key bindings ===================================================
;;;; Keydef: simpler way to define keys, with kbd syntax (Michael John Downes)
;; https://github.com/emacsorphanage/keydef

(use-package keydef
  :demand t)


;;;; My key bindings

;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;(keydef (projectile "C-c p") projectile-command-map)


(keydef "C-c d"         my/delete-whitespace-forward)
; (keydef "C-c C-c M-x"   execute-extended-command) ; original M-x overridden by smex

; (keydef "M-o"           other-window) ; simple repetition

(keydef "C-c C-k"       kill-compilation)

(keydef "C-c j"         avy-goto-word-or-subword-1)

; (keydef "C-c w f"       wsf-ivy-find-file)

(keydef "S-<mouse-3>"   browse-url-at-mouse)
(keydef "C-c z ."       browse-url-at-point)
(keydef "C-c z b"       browse-url-of-buffer)
(keydef "C-c z r"       browse-url-of-region)
(keydef "C-c z u"       browse-url)
(keydef "C-c z v"       browse-url-of-file)
;(keydef (dired "z"      browse-url-of-dired-file))

; (keydef "C-c b"         phw-mode)
; (keydef "C-c d"         phw-deactivate)


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
(keydef "C-c c"         org-capture)
;(keydef "C-c l"         org-store-link)


;; Additions to the help command
;;
;;(keydef "C-h A"         apropos-toc)       ; mnemonic: apropos All
(keydef "C-h B"         describe-bindings)
(keydef "C-h L"         (info "elisp"))    ; was describe-language-environment
(keydef "C-h R"         'my/elisp-function-reference)

(keydef "C-x C-b"       bs-show)           ; same binding as S-<f1>

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

;; (keydef "C-x , f"       (sbf-find-file))
;; (keydef "C-x , f"       am-find-file)
;; (keydef "C-x , 4 f"     am-find-file-other-window)
;; (keydef "C-x , g"       am-grep)
;; (keydef "C-x , r"       am-force-recache)
;; (keydef "C-x , ."       am-grep-tag)

;; The manual recommends C-c for user keys, but C-x t is
;; always free, whereas C-c t is used by some modes.
;; (keydef "C-x t ." dired-hide-dotfiles-mode)
;; (keydef "C-x t c" column-number-mode)
;; (keydef "C-x t d" toggle-debug-on-error)
;; (keydef "C-x t e" toggle-debug-on-error)
;; (keydef "C-x t f" auto-fill-mode)
;; (keydef "C-x t l" toggle-truncate-lines)
;; (keydef "C-x t m" menu-bar-mode)
;; (keydef "C-x t q" toggle-debug-on-quit)
;; (keydef "C-x t r" read-only-mode)
;; (keydef "C-x t w" whitespace-mode)
;; (keydef "C-x t D" dired-toggle-read-only) (autoload 'dired-toggle-read-only "dired" nil t)
;; (keydef "C-x t 4" my/set-buffer-local-tab-width-to-4)
;; (keydef "C-x t 8" my/set-buffer-local-tab-width-to-8)


;; GUD navigation claims the capital letters in the "goto" map:
;;  "M-g A"     *disassembly            ; assembly language
;;  "M-g B"     *breakpoints
;;  "M-g D"     *disassembly
;;  "M-g F"     *stack (Frames)
;;  "M-g G"     *gud*
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
(keydef "M-g j"         jump-to-register)
(keydef "M-g y"         ilocate-library-find-source)
(keydef "M-g s"         'my/magit-status)

(keydef "M-["           align)


;; (keydef "M-x"           smex)
;; (keydef "M-X"           smex-major-mode-commands)
;(keydef "<f1>"          bs-show)
;(eval-after-load "bs"  '(keydef (bs "<f1>") (bs-kill)))
(keydef "C-<f1>"        'my/workspace-shell)
(keydef "S-<f1>"        'my/named-shell)
(keydef "M-<f1>"        menu-bar-mode)

(keydef "<f2>"          'my/disk)


;; Strong similarity to MS Visual Studio's function keys
(keydef   "<f4>"        next-error)
(keydef "C-<f4>"        first-error)
(keydef "S-<f4>"        kill-compilation)

;; (keydef "C-."           vtags-find)
;; (keydef "<kp-begin>"    vtags-point-to-placeholder)
;; (keydef "<kp-right>"    vtags-next-placeholder)
;; (keydef "<kp-left>"     vtags-prev-placeholder)

(keydef "<f12>"         customize-option)
(keydef "C-<f12>"       customize-group)
(keydef "M-<f12>"       customize-apropos)

;; (eval-after-load 'helm '(progn
;;   (keydef "M-x"         helm-M-x)
;;   (keydef "C-x C-f"     helm-find-files)
;;   ;; Swap TAB and C-j
;;   (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
;;   (define-key helm-map (kbd "C-j") 'helm-select-action)
;;   ))


;(eval-after-load "bs"  '(keydef (bs "<f1>") (bs-kill)))

;;; === Load certain packages via oneshot hooks rather than autoload ===

;; The traditional technique to delay the cost of loading a package
;; is emacs' autoload feature.  It depend on a package having a set
;; of entry functions which, if intercepted, allow that package to
;; be loaded transparently.  Unfortunately, many packages fail to
;; satisfy this pre-condtion.
;;
;; But all is not lost.  Very often, for such packages, there exists
;; some hook which can be gauranteed to be invoked just before that
;; package needs to initialize.  Implementing oneshot initialization
;; within a self-removing function on that hook suffices.  Upon load
;; all traditional use-package mechasims kick in, completing package
;; customization and configuration.

(use-package emacs ; force load stuff via pre-command-hook
  :init

  ;; Pre-command
  (defun my/pre-command-oneshot()
    (remove-hook 'pre-command-hook #'my/pre-command-oneshot)

    (recentf-mode 1)
;   (when (display-graphic-p)
;     (load-library "all-the-icons")
;     (load-library "svg-icon"))
;   (mini-frame-mode +1)
;   (load-library "popup")
    (load-library "corfu")
    (global-corfu-mode 1)
    (load-library "hydra")
;   (load-library "orderless")
    (load-library "vertico")
;   (load-library "prescient")
;   (load-library "marginalia")
;   (load-library "consult")           ; loads recentf
;   (load-library "embark")
;   (load-library "embark-consult")
;   (load-library "dumb-jump")
    )
  (add-hook 'pre-command-hook #'my/pre-command-oneshot)

  ;; Post-command
  (defun my/post-command-oneshot()
    (remove-hook 'post-command-hook #'my/post-command-oneshot)

    (load-library "hl-line")
    (load-library "paren")
    (load-library "recentf")
    )
  (add-hook 'post-command-hook #'my/post-command-oneshot)

  ;; Minibuffer-setup
  (defun my/minibuffer-setup-oneshot()
    (remove-hook 'minibuffer-setup-hook #'my/minibuffer-setup-oneshot)

    (setq enable-recursive-minibuffers t)
    (minibuffer-depth-indicate-mode 1)  ; show recursion depth in promp
    (setq read-extended-command-predicate       ; Hide M-x commands which do
        #'command-completion-default-include-p) ; not work in current mode
    (setq minibuffer-prompt-properties  ; No cursor within minibuffer prompt
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

    ;; Add prompt indicator to `completing-read-multiple'.
    ;; Alternatively try `consult-completing-read-multiple'.
    ;; (defun crm-indicator (args)
    ;;   (cons (concat "[CRM] " (car args)) (cdr args)))
    ;; (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
    )
  (add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-oneshot)

  ;; (defun my/find-file()
  ;;   (remove-hook 'find-file-hook #'my/find-file)
  ;;   )
  ;; (add-hook 'find-file-hook #'my/find-file)

  ;; Find-file-not-found
  (defun my/find-file-not-found-functions-oneshot()
    (remove-hook 'find-file-not-found-functions #'my/find-file-not-found-functions-oneshot)
;;    (load-library "yasnippet")
    )
  (add-hook 'find-file-not-found-functions #'my/find-file-not-found-functions-oneshot)

)


;; Display collected use-package statistics in a tabulated-list buffer
;;   Declared          the use-package declaration was seen
;;   Prefaced          :preface has been processed
;;   Initialized       :init has been processed (load status unknown)
;;   Configured        :config has been processed (the package is loaded!)

(use-package-report)


;;; init.el ends here
