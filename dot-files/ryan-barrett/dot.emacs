; Ryan Barrett's .emacs  -*- emacs-lisp -*-
; dotemacs@ryanb.org

; tell me if there's something wrong
;; (setq debug-on-error t)

; load extra elisp packages from ~/bin
(dolist (dir '("~/bin")); "~/bin/google-el"))
  (if (not (member dir load-path))
      (setq load-path (cons dir load-path))))

; avoid garbage collection up to 10M (default is only 400k)
(setq-default gc-cons-threshold 10000000)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; frames and server
; (note that .emacs.google is loaded at the *end* of this file!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (and (getenv "DISPLAY") (not (string-match "^:0" (getenv "DISPLAY"))))
  ; this is a X frame on a remote emacs. do remote setup (turn off
  ; interprogram-cut/paste, etc.)
  (load-file (concat (getenv "HOME") "/.emacs.remote")))

;; TODO: try this. turn of interprogram-*-function and write custom fns for M-w,
;; C-w, C-y to pull over remote X.
; this turns them back on:
;; (setq interprogram-cut-function 'x-select-text)
;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; ;; ...however, whenever i explicitly cut, copy, or paste, i *do* want to
;; ;; synchronize with X. this adds hooks to do that.
;; (global-set-key [(meta w)]
;;   (lambda () (interactive)
;;     (kill-ring-save (region-beginning) (region-end))
;;     (x-select-text (buffer-substring (region-beginning) (region-end)))))

;; (global-set-key [(control w)] 'kill-region)
;; (global-set-key [(control y)] 'yank)

(defun make-my-frames ()
  (interactive)
  (make-frame '((name . "emacs main")))
  (make-frame '((name . "emacs main secondary")))
  (make-frame '((name . "emacs work left")))
  (make-frame '((name . "emacs work right")))
  (make-frame '((name . "emacs fullscreen")))
  (let ((at-work (string-match "\.corp\.google\.com$" (getenv "HOST")))
        (extra-monitor
         (and (equal (getenv "HOST") "laptop")
              (eq 0 (shell-command "xrandr --current | grep -qE '(DP1|VGA1|HDMI1) connected'" nil nil)))))
    (when (or at-work extra-monitor)
      (make-frame '((name . "emacs upper left")))
      (make-frame '((name . "emacs lower left")))
      (make-frame '((name . "emacs upper right")))
      (make-frame '((name . "emacs lower right")))
      (when at-work
        (make-frame '((name . "emacs fullscreen 2")))))))

(defun make-remote-frames (display)
  (make-frame-on-display display '((name . "emacs google left")))
  (make-frame-on-display display '((name . "emacs google right"))))

(if (or window-system (and (>= emacs-major-version 23) (daemonp)))
  ;; we're a server
  (progn (global-set-key [(control \4)] 'server-edit)
         (global-set-key [(meta \4)] 'server-edit)
         (global-set-key [(control \\)] 'server-edit)
         ;; use TCP for the server so that when I'm sshed into remote machines,
         ;; i can set my EDITOR to an emacsclient wrapper that tells my local
         ;; emacs to open the remote files via tramp.
         (custom-set-variables '(server-use-tcp t))
         (server-start))
  ;; we're emacs -nw
  (global-set-key [(control \\)]
   (lambda () (interactive) (save-buffers-kill-emacs t))))

;; used in my ion C-F2 key binding. run before shutting down X!
(defun delete-all-x-frames ()
  (mapcar (lambda (frame) (if (eq 'x (frame-live-p frame))
                              (delete-frame frame)))
          (frame-list)))

;; Also make C-4 close and continue vc checkin message (*VC-log*) buffers
(require 'log-edit)
(define-key log-edit-mode-map [(control \4)] 'log-edit-done)


(auto-raise-mode nil)

; if another frame is visible, raise some buffers in that frame, *not* the
; current one!
(defvar non-special-display-buffer-regexps
  '("\\*Completions\\*" "\\*TAGS: ")
  "Buffers that should *not* be handled by `prefer-other-visible-frame'.
   (I'd rather just exclude them in `special-display-regexps', but elisp regex
   doesn't support negative lookahead, ie (?!...), so i can't.)")

(defun prefer-other-visible-frame (buffer &optional buffer-data)
  "If other frames are visible, display the buffer in one of them.
Otherwise, display the buffer in this frame in another window. If
there's only one window, split to create another. Also hide the
buffer in all windows other than the window it gets displayed in.

Intended to be set as `special-display-function'.

Ignores buffers in `non-special-display-buffer-names'.

I can *almost* do this with frame parameters alone, e.g. visibility, except for
forcing it to use a different frame if possible. (same-frame . t) lets you force
it to use the *same* frame, but (same-frame . nil) doesn't force a different
one.

http://www.gnu.org/software/emacs/manual/html_node/emacs/Special-Buffer-Frames.html
http://www.gnu.org/software/emacs/manual/html_node/emacs/Frame-Parameters.html

If this function isn't reusing buffers when they're already visible in a frame,
that's usually because the visible frames have more than one display value.
Run (frame-parameters) in each of them and make sure they're on the same display.

I wish i could just use `switch-to-buffer-other-window' here, but it calls
`display-buffer', which ends up calling this again. Sigh."
   ;; i'd like to use `other-frame', but it raises the frame :/
  (let ((window
    (cond
     ;; is this buffer a special case that we want to use the default handling?
     ((eval (cons 'or (mapcar
                       (lambda (regex) (string-match regex (buffer-name buffer)))
                       non-special-display-buffer-regexps)))
      nil)
     ;; is the buffer already displayed in another visible window?
     ((get-buffer-window buffer 'visible))
     ;; find another window
     (t (let* ((existing (next-window (selected-window) 'never-minibuf 'visible))
               (new (if (and existing (not (eq existing (selected-window))))
                      ; found another existing one
                      existing
                      ; couldn't find an existing one; splitting the current one
                      (split-window))))
          (set-window-buffer new buffer)
          new)))))
    ;; (when (string-match special-display-switch-to-regexp (buffer-name buffer))
    ;;   (select-frame-set-input-focus (window-frame window))
    ;;   (select-window window))
    window))

(custom-set-variables
 '(special-display-function 'prefer-other-visible-frame)
 '(special-display-regexps '(".*"))  ; modulo non-special-display-buffer-names
 '(special-display-buffer-names nil)
 '(focus-follows-mouse nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; indent two spaces, not four
(setq-default c-basic-offset 2
              python-indent 2)

; extra major modes!
(require 'css-mode)
(require 'lua-mode)
(require 'markdown-mode)
(require 'sgml-mode) ; (for html-mode)
(require 'sh-script)
(require 'shell)

(require 'alist)
;; set-alist, used below, is defined in alist, which is part of APEL:
;; http://kanji.zinbun.kyoto-u.ac.jp/~tomo/elisp/APEL/index.en.html"
(set-alist 'auto-mode-alist "\\.c$" 'c++-mode)  ; use c++ comments for .h and .c files!
(set-alist 'auto-mode-alist "\\.json$" 'javascript-mode)
(set-alist 'auto-mode-alist "\\.h$" 'c++-mode)  ; (i use gcc...it can deal)
(set-alist 'auto-mode-alist "\\.htmlt$" 'xml-mode)     ; apiary atom templates
(set-alist 'auto-mode-alist "\\.ezt$" 'sgml-mode)
(set-alist 'auto-mode-alist "\\.jsont$" 'javascript-mode) ; apiary json templates
(set-alist 'auto-mode-alist "\\.lua$" 'lua-mode)
(set-alist 'auto-mode-alist "\\.snarfed$" 'html-mode)  ; pyblosxom snarfed flavour files
(set-alist 'auto-mode-alist "\\.txt$" 'markdown-mode)  ; blog posts
(set-alist 'auto-mode-alist "\\.xmlt$" 'xml-mode)      ; apiary atom templates
(set-alist 'auto-mode-alist "^/tmp/\\*scratch\\*$" 'emacs-lisp-mode)
(set-alist 'auto-mode-alist "BUILD$" 'python-mode)

; C-F11 to compile
(global-set-key [(control f11)] 'compile)

; C-c C-k to kill the current compilation
(global-set-key [(control c) (control k)] 'kill-compilation)

;; python-mode steals C-c C-k for python-mark-block. steal it back.
(require 'python)
(define-key python-mode-map [(control c) (control k)] 'kill-compilation)

(defun bury-then-switch-to-buffer (buffer)
  "Bury buffer in all windows, then switch to it in the current window."
  (replace-buffer-in-windows buffer)
  (switch-to-buffer buffer t))  ; t: don't add it to the recent buffer list

;; C-8 for *scratch*, C-9 for *compilation*.
;; (use M-8, etc as alternates since C-number keys don't have ascii control
;; codes, so they can't be used in terminal frames.)
(defun switch-to-scratch ()
  (interactive) (bury-then-switch-to-buffer "*scratch*"))
(global-set-key [(control \8)] 'switch-to-scratch)
(global-set-key [(control x) (\8)] 'switch-to-scratch)
(global-set-key [(meta \8)] 'switch-to-scratch)

(defun switch-to-compilation ()
  (interactive) (bury-then-switch-to-buffer "*compilation*"))
(global-set-key [(control \9)] 'switch-to-compilation)
(global-set-key [(control x) (\9)] 'switch-to-compilation)
(global-set-key [(meta \9)] 'switch-to-compilation)

;; TODO(ryanb): this is shift-tab when running in terminal. it needs to be set
;; only for certain modes though, e.g. compilation-mode.
;; (global-set-key [(meta \[) (Z)] ')

(custom-set-variables
 '(x-select-enable-clipboard nil) ; these two *might* make remote X faster
 '(x-select-enable-primary t)
 )

(add-hook 'c-mode-hook
; for some c code (e.g. kernel, tcsh), use 8-char tabs, indent 4 columns
;;     (lambda () (setq indent-tabs-mode 'p tab-width 8 c-basic-offset 4)))
; for other c code (e.g. GNU readline), use 8-char tabs, indent 2 columns
;;     (lambda () (setq indent-tabs-mode 'p tab-width 8 c-basic-offset 2)))
; for other c code (e.g. other GNU), don't use tabs, indent 2 columns
    (lambda () (setq indent-tabs-mode nil c-basic-offset 2)))
;; ; for other c code (e.g. gaim), use 4-char tabs, indent 4 columns
;;     (lambda () (setq indent-tabs-mode t c-basic-offset 4)))

(add-hook 'java-mode-hook
    (lambda () (setq indent-tabs-mode nil c-basic-offset 4)))

(add-hook 'php-mode-hook
    (lambda () (setq indent-tabs-mode nil c-basic-offset 2)))

; fix word-constituent chars in a few modes. these for word motion...
(modify-syntax-entry ?\" "\"" markdown-mode-syntax-table)
(modify-syntax-entry ?. "." css-mode-syntax-table)
(modify-syntax-entry ?- "_" css-mode-syntax-table)
; ...and these so that M-/ dabbrev-expand treats . and / as delimiters.
(modify-syntax-entry ?. "." html-mode-syntax-table)
(modify-syntax-entry ?. "." sh-mode-default-syntax-table)
(modify-syntax-entry ?/ "." shell-mode-syntax-table)

; have to use a hook for lua mode since it doesn't have a variable for its
; syntax table.
; this causes a bug, but i do it anyway. on lua-mode 20071122 line 332:
;;_ needs to be part of a word, or the regular expressions will
;; incorrectly regognize end_ to be matched by "\\<end\\>"!
(defun fix-lua-mode-underscore-syntax () (modify-syntax-entry ?_ "_"))
(add-hook 'lua-mode-hook 'fix-lua-mode-underscore-syntax)

(defun rotate-test-files ()
  "Rotate between code and unit test files."
  (interactive)
  (let ((basename (file-name-sans-extension buffer-file-name))
        (ext (file-name-extension buffer-file-name t)))
    (find-file-existing
     (concat (if (string-match "^\\(.+\\)_test$" basename)
                 (match-string 1 basename)
               (concat basename "_test"))
             ext))))
(global-set-key [(control \')] 'rotate-test-files)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; shells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; try to see ssh verbose messages in *tramp ...* buffer, but didn't work
;; (setcdr (assoc 'tramp-login-args (assoc "ssh" tramp-methods))
;;         '((("%h") ("-l" "%u") ("-p" "%p") ("-v") ("-e" "none"))))

;; XXX begin why_i_run_shells_inside_emacs.txt XXX

(defvar my-local-shells
  '("*shell0*" "*shell1*" "*shell2*" "*shell3*" "*music*"))
(defvar heaven-shells '("*heaven0*" "*heaven1*" "*heaven2*"))
(defvar purgatory-shells '("*purgatory0*" "*purgatory1*" "*purgatory2*"))
(defvar my-remote-shells (append '("*snarfed*") heaven-shells purgatory-shells))
    
(defvar my-shells (append my-local-shells my-remote-shells))
;; (defvar my-sshfs-mountpoints
;;   '(("heaven:/google" "/google")
;;     ("heaven:/home/build" "/home/build")
;;     ("heaven:/home/ryanb" "~/w")))

(require 'tramp)

(custom-set-variables
 '(tramp-default-method "ssh")          ; uses ControlMaster
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 ;; '(comint-completion-autolist t)     ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 '(comint-buffer-maximum-size 20000)    ; max length of the buffer in lines
 '(comint-prompt-read-only nil)         ; if this is t, it breaks shell-command
 '(comint-get-old-input (lambda () "")) ; what to run when i press enter on a
                                        ; line above the current prompt
 '(comint-input-ring-size 5000)         ; max shell history size
 '(protect-buffer-bury-p nil)
)

(setenv "PAGER" "cat")

;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(defun make-my-shell-output-read-only (text)
  "Add to comint-output-filter-functions to make stdout read only in my shells."
  (if (member (buffer-name) my-shells)
      (let ((inhibit-read-only t)
            (output-end (process-mark (get-buffer-process (current-buffer)))))
        (put-text-property comint-last-output-start output-end 'read-only t))))
(add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

(defun my-dirtrack-mode ()
  "Add to shell-mode-hook to use dirtrack mode in my shell buffers."
  (when (member (buffer-name) my-shells)
    (shell-dirtrack-mode 0)
    (set-variable 'dirtrack-list '(":\\([^ :>]*\\)> *$" 1 nil))
    (dirtrack-mode 1)))
(add-hook 'shell-mode-hook 'my-dirtrack-mode)

; interpret and use ansi color codes in shell output windows
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun set-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10))
(add-hook 'shell-mode-hook 'set-scroll-conservatively)

;; i think this is wrong, and it buries the shell when you run emacsclient from
;; it. temporarily removing.
;; (defun unset-display-buffer-reuse-frames ()
;;   "Add to shell-mode-hook to prevent switching away from the shell buffer
;; when emacsclient opens a new buffer."
;;   (set (make-local-variable 'display-buffer-reuse-frames) t))
;; (add-hook 'shell-mode-hook 'unset-display-buffer-reuse-frames)

;; make it harder to kill my shell buffers
(require 'protbuf)
(add-hook 'shell-mode-hook 'protect-process-buffer-from-kill-mode)

(defun make-comint-directory-tracking-work-remotely ()
  "Add this to comint-mode-hook to make directory tracking work
while sshed into a remote host, e.g. for remote shell buffers
started in tramp. (This is a bug fix backported from Emacs 24:
http://comments.gmane.org/gmane.emacs.bugs/39082"
  (set (make-local-variable 'comint-file-name-prefix)
       (or (file-remote-p default-directory) "")))
(add-hook 'comint-mode-hook 'make-comint-directory-tracking-work-remotely)

(defun enter-again-if-enter ()
  "Make the return key select the current item in minibuf and shell history isearch.
An alternate approach would be after-advice on isearch-other-meta-char."
  (when (and (not isearch-mode-end-hook-quit)
             (equal (this-command-keys-vector) [13])) ; == return
    (cond ((active-minibuffer-window) (minibuffer-complete-and-exit))
          ((member (buffer-name) my-shells) (comint-send-input)))))
(add-hook 'isearch-mode-end-hook 'enter-again-if-enter)

(defadvice comint-previous-matching-input
    (around suppress-history-item-messages activate)
  "Suppress the annoying 'History item : NNN' messages from shell history isearch.
If this isn't enough, try the same thing with
comint-replace-by-expanded-history-before-point."
  (let ((old-message (symbol-function 'message)))
    (unwind-protect
      (progn (fset 'message 'ignore) ad-do-it)
    (fset 'message old-message))))

(defadvice comint-send-input (around go-to-end-of-multiline activate)
  "When I press enter, jump to the end of the *buffer*, instead of the end of
the line, to capture multiline input. (This only has effect if
`comint-eol-on-send' is non-nil."
  (flet ((end-of-line () (end-of-buffer)))
    ad-do-it))

;; not sure why, but comint needs to be reloaded from the source (*not*
;; compiled) elisp to make the above advise stick.
(load "comint.el.gz")

;; XXX end why_i_run_shells_inside_emacs.txt XXX


;; XXX begin automatically_close_completions_in_emacs_shell_comint_mode.txt XXX

(defun comint-close-completions ()
  "Close the comint completions buffer.
Used in advice to various comint functions to automatically close
the completions buffer as soon as I'm done with it. Based on
Dmitriy Igrishin's patched version of comint.el."
  (if comint-dynamic-list-completions-config
      (progn
        (set-window-configuration comint-dynamic-list-completions-config)
        (setq comint-dynamic-list-completions-config nil))))

(defadvice comint-send-input (after close-completions activate)
  (comint-close-completions))

(defadvice comint-dynamic-complete-as-filename (after close-completions activate)
  (if ad-return-value (comint-close-completions)))

(defadvice comint-dynamic-simple-complete (after close-completions activate)
  (if (member ad-return-value '('sole 'shortest 'partial))
      (comint-close-completions)))

(defadvice comint-dynamic-list-completions (after close-completions activate)
    (comint-close-completions)
    (if (not unread-command-events)
        ;; comint's "Type space to flush" swallows space. put it back in.
        (setq unread-command-events (listify-key-sequence " "))))

;; XXX end automatically_close_completions_in_emacs_shell_comint_mode.txt XXX


;; XXX begin emacsclient_in_tramp_remote_shells.txt XXX

(defadvice make-network-process (before force-tcp-server-ipv4 activate)
  "Monkey patch the server to force it to use ipv4. This is a bug fix that will
hopefully be in emacs 24: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=6781"
  (if (eq nil (plist-get (ad-get-args 0) :family))
      (ad-set-args 0 (plist-put (ad-get-args 0) :family 'ipv4))))

;; now that the ipv4 advice is in place, restart the server.
(custom-set-variables '(server-use-tcp t))
(if (server-running-p) (server-start))

(require 'alist)
(defun update-tramp-emacs-server-ssh-port-forward ()
  "Update TRAMP's ssh method to forward the Emacs server port to the local host.
This lets emacsclient on the remote host open files in the local Emacs server.

put-alist, used below, is defined in alist, which is part of the APEL library:
http://kanji.zinbun.kyoto-u.ac.jp/~tomo/elisp/APEL/index.en.html"
  (let* ((ssh-method (assoc "ssh" tramp-methods))
         (ssh-args (cadr (assoc 'tramp-login-args ssh-method))))
    (put-alist 'tramp-login-args
      (list (put-alist "-R" (let ((port (process-contact server-process :service)))
        ;; put-alist makes a dotted pair for the key/value, but tramp-methods
        ;; needs a normal list, so put the value inside a list so that the
        ;; second part of the dotted pair (ie the cdr) is a list, which converts
        ;; it from a dotted pair into a normal list.
                              (list (format "%d:127.0.0.1:%d" port port)))
                       ssh-args))
      ssh-method)))

(defadvice server-process-filter (before handle-remote-emacsclient-file activate)
  "Detect remote emacsclient and inject the tramp '/host:' prefix.

  Note the hack here that assumes remote emacsclient invocations
  have the regex '-tty /dev/(pts/[0-9]|ttype)[0-9]+)' in their
  command sequence string, and all others have either no -tty or
  a one-digit /dev/pts/.... I haven't yet found a better way to
  distinguish local and remote clients."
  (if (string-match "-tty /dev/\\(pts/[0-9]\\|ttyp\\)[0-9]+" (ad-get-arg 1))
      (with-parsed-tramp-file-name default-directory parsed
        (let* ((message (ad-get-arg 1))
               (absolute (and (string-match "-file \\([^ ]+\\)" message)
                              (file-name-absolute-p (match-string 1 message))))
               (tramp-prefix (tramp-make-tramp-file-name parsed-method
                                                         parsed-user
                                                         parsed-host
                                                         nil))
               (dir (if absolute nil parsed-localname)))
          (ad-set-arg 1 (replace-regexp-in-string "-file "
                          (concat "-file " tramp-prefix dir) message))))))

(defun ssh-shell (host bufname)
  "SSH to a remote host in a shell-mode buffer using TRAMP."
  (update-tramp-emacs-server-ssh-port-forward)
  (let ((default-directory (format "/%s:" host))
        (tramp-remote-process-environment
         (cons (format "EDITOR='emacsclient -f ~/.emacs.d/%s_server'" (getenv "HOST"))
                 tramp-remote-process-environment)))
    (shell bufname))
  ;; copy emacs server file so remote emacsclient can connect to this emacs
  (let ((default-directory "/tmp")
        (local-server-file (process-get server-process :server-file))
        (remote-server-file (format "~/.emacs.d/%s_server" (getenv "HOST"))))
    (async-shell-command
     (format "scp -v %s %s:%s" local-server-file host remote-server-file))))

;; XXX end emacsclient_in_tramp_remote_shells.txt XXX


;; run a few shells.
(defun start-my-shells ()
  (interactive)
  (let ((default-directory "~")
        ;; trick comint into thinking the current window is 82 columns, since it
        ;; uses that to set the COLUMNS env var. otherwise it uses whatever the
        ;; current window's width is, which could be anything.
        (window-width (lambda () 82)))
    (mapcar 'shell my-local-shells)
    (python-shell)
    (protect-process-buffer-from-kill-mode 1 "*Python*")))
  ;; (sn))

;; TRAMP shells don't get the right path, so set it manually. in ubuntu lucid,
;; it looks like the extra path elements come from /etc/environment and/or
;; /etc/default/locale. in FreeBSD 7.3 (ie pair), i don't know. :/
(defun sn () (interactive) (ssh-shell "snarfed.org" "*snarfed*"))
;; TODO: make generic so it works with purgatory too
(defun h () (interactive)
 (let ((tramp-remote-process-environment
        (cons "PATH=/home/ryanb/bin:/usr/local/symlinks:/usr/local/scripts:/usr/local/buildtools/java/jdk/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
              tramp-remote-process-environment)))
   (mapcar (lambda (buf) (ssh-shell "heaven" buf))
           heaven-shells)))

(defun kt ()
  "Kill vpn and ssh sessions."
  (interactive)
  (mapcar (lambda (buffer) (let ((proc (get-buffer-process buffer)))
                             (if proc (interrupt-process proc))))
          (cons "*vpn*" my-remote-shells))
  (tramp-cleanup-all-connections)
  (async-shell-command "pkill -f controlmaster" "*kt pkill ssh*")
  (async-shell-command "~/src/misc/unmount_sshfs.sh" "*kt unmount sshfs*"))

(defun vpn ()
  (interactive)
  (let ((default-directory "~"))
    (async-shell-command "ovpn -u -k -c ~/src/misc/mount_sshfs.sh" "*vpn*")))

; F1-* to switch to shells
(global-set-key [(f1) (\0)]
  (lambda () (interactive) (bury-then-switch-to-buffer "*shell0*")))
(global-set-key [(f1) (\1)]
  (lambda () (interactive) (bury-then-switch-to-buffer "*shell1*")))
(global-set-key [(f1) (\2)]
  (lambda () (interactive) (bury-then-switch-to-buffer "*shell2*")))
(global-set-key [(f1) (\3)]
  (lambda () (interactive) (bury-then-switch-to-buffer "*shell3*")))
(global-set-key [(f1) (s)]
  (lambda () (interactive) (bury-then-switch-to-buffer "*snarfed*")))
(global-set-key [(f1) (h)]
  (lambda () (interactive) (bury-then-switch-to-buffer "*heaven0*")))
(global-set-key [(f1) (i)]
  (lambda () (interactive) (bury-then-switch-to-buffer "*heaven1*")))
(global-set-key [(f1) (j)]
  (lambda () (interactive) (bury-then-switch-to-buffer "*heaven2*")))
(global-set-key [(f1) (g)]
  (lambda () (interactive) (bury-then-switch-to-buffer "*heaven3*")))
(global-set-key [(f1) (m)]
  (lambda () (interactive) (bury-then-switch-to-buffer "*music*")))
(global-set-key [(f1) (p)]
  (lambda () (interactive) (bury-then-switch-to-buffer "*Python*")))

(defun fix-shell ()
  "Sometimes the input area of a shell buffer goes read only. This fixes that."
  (interactive)
  (let ((inhibit-read-only t))
    (comint-send-input)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; turn on random customization options
(custom-set-variables
  '(column-number-mode t)
  '(completion-ignore-case t)
  '(read-file-name-completion-ignore-case t)
  '(default-major-mode 'text-mode)  ; default to text mode, not fundamental
  '(diff-default-read-only nil)  ; don't make diff mode (for patches) read-only
  '(diff-switches "-u --ignore-all-space")
  '(inhibit-default-init t)  ; don't load /usr/share/emacs/site-lisp/default.el
                             ; (it loads *after* ~/.emacs for some reason!)
  '(line-number-mode t)
  '(normal-erase-is-backspace nil)  ; makes backspace work in emacs -nw
                                    ; (...but seems unnecessary)
  '(query-user-mail-address nil)
  '(require-final-newline t)
  '(sentence-end-double-space nil)
  '(truncate-partial-width-windows nil)
  '(vc-follow-symlinks t)  ; don't complain when following symlinks to files
                           ; under source control
  '(vc-log-show-limit 20)
  '(visible-bell t)
  '(history-length t)      ; no max length for minibuffer history; store it all
  '(history-delete-duplicates t)
  '(dired-listing-switches "-l") ; default dired mode to hide dotfiles
  '(grep-command "grep --color=never --binary-files=without-match -niR ")
  '(grep-highlight-matches t)      ; highlight just the grep pattern
  '(isearch-allow-scroll t)
  '(isearch-case-fold-search t)
  '(set-mark-command-repeat-pop t) ; allow repeating C-space after C-u
  '(inhibit-startup-message t)
  '(use-dialog-box nil)

  '(confirm-nonexistent-file-or-buffer t) ; don't let switch-to-buffer create new buffers
  '(ibuffer-default-sorting-mode 'filename/process)
  '(setq save-interprogram-paste-before-kill t)

 ; this breaks M-x grep-find
;;   '(null-device nil)               ; don't append /dev/null to grep command
)

;; browse-url
;; don't let browse-url-at-point end with a paren or brace
;; test: file:///home/ryanb/docs/emacs_manual/index.html)
(require 'thingatpt)
(let ((dont-end-with "[^][()<>{}.,;!? \t\n\"]"))
  (if (not (string-match (concat (regexp-quote dont-end-with) "$")
                         thing-at-point-url-regexp))
      (setq thing-at-point-url-regexp
        (concat thing-at-point-url-regexp dont-end-with)))
  (if (not (string-match (concat (regexp-quote dont-end-with) "$")
                         thing-at-point-short-url-regexp))
      (setq thing-at-point-short-url-regexp
        (concat thing-at-point-short-url-regexp dont-end-with))))


(defmacro with-firefox-profile (profile &rest body)
  "Runs body with the given firefox profile."
  `(let ((browse-url-firefox-arguments (list "-P" ,profile)))
     (progn . ,body)))

(defun browse-snarfed-url-at-host (host)
  "Run browse-url for the snarfed file in the current buffer at the given host."
  (with-firefox-profile "personal"
    (let ((page (file-name-sans-extension
                 (file-name-nondirectory buffer-file-name))))
      (browse-url (format "http://%s/%s" host page)))))

(define-prefix-command 'my-browse-url-keymap)
(global-set-key [(control x) (u)] 'my-browse-url-keymap)
(define-key my-browse-url-keymap "p"
  (lambda () (interactive) (with-firefox-profile "personal" (browse-url-at-point))))
(define-key my-browse-url-keymap "w"
  (lambda () (interactive) (with-firefox-profile "work" (browse-url-at-point))))
(define-key my-browse-url-keymap "l"
  (lambda () (interactive) (browse-snarfed-url-at-host "localhost")))
(define-key my-browse-url-keymap "s"
  (lambda () (interactive) (browse-snarfed-url-at-host "snarfed.org")))

; use vc-svn, a vc interface to subversion. (e.g. C-x v = diffs.)
(require 'vc-svn)
(add-to-list 'vc-handled-backends 'SVN)
(custom-set-variables '(vc-svn-program-name "svn"))

; turn on syntax highlighting
(if (not (featurep 'xemacs))
    (global-font-lock-mode t))

; stop that damn blinking cursor!
(blink-cursor-mode 0)

; turn on pending delete (when a region is selected, typing replaces it)
(delete-selection-mode t)

; when on a tab, make the cursor the tab length
(setq-default x-stretch-cursor t)

; turn off stupid "yes" / "no" full word prompts
(fset 'yes-or-no-p 'y-or-n-p)

; by default, emacs only adds to its undo history. Kyle Jones' wonderful redo
; package (forked as redo+) makes undo sane; you can walk the undo history
; forward and backward.
(require 'redo+)

; use ido for switching and choosing buffers, but *not* files.
;
; note: emacs 23's ido.el is *old*: version 1.56 from 2002. it has a bug in
; ido-switch-buffer that throws the error "Error in post-command-hook:
; (wrong-type-argument sequencep t)" when you type in a complete suffix,
; e.g. .h when you're looking for file.h. details in
; http://search.gmane.org/?query=%22Issue+with+ido.el+%28post-command-hook%29%22&author=&group=gmane.emacs.bugs&sort=revdate
;
; i downloaded a more recent 2009 version of ido.el from
; http://cvs.savannah.gnu.org/viewvc/*checkout*/emacs/emacs/lisp/ido.el ,
; which fixed that bug, and put it in ~/bin. i was inspired to do that by
; http://www.emacswiki.org/emacs/InteractivelyDoThings .
;
; evidently ubuntu lucid's emacs 23 package puts the 2009 ido.elc into
; /usr/share/emacs/23.1/site-lisp, but hardy's package symlinks that directory
; to ../../emacs23/site-lisp, so ido gets pulled from emacs-goodies-el, which
; has the old version. sigh. just as well i'm fully on lucid now.
(ido-mode 'buffers)
(global-set-key [(control b)] 'ido-switch-buffer)

(custom-set-variables
 '(ido-everywhere nil)
 '(ido-case-fold t)
 '(ido-create-new-buffer 'never)
 '(ido-default-file-method 'samewindow)
 '(ido-default-buffer-method 'samewindow)
 '(ido-enable-flex-matching t))

; reset this because ido steals it
(global-set-key [(control x) (k)] 'kill-this-buffer)

; use utf-8!
(custom-set-variables
 '(default-buffer-file-coding-system 'utf-8-unix))

(prefer-coding-system coding-category-utf-8)

; erin-mode does fontifying for TWiki markup.
;
; note that i've hacked erin.el to work around an emacs bug:
; http://thread.gmane.org/gmane.emacs.devel/103415/focus=103446
;
; specifically, i commented out line 381 of erin.el, version 0.4.
(require 'erin)

; on duplicate filenames, show path names, not foo.x<2>, foo.x<3>, etc.
(require 'uniquify)
(custom-set-variables
 '(uniquify-buffer-name-style 'reverse)
 '(uniquify-after-kill-buffer-p t))

; turn on random customization options
(custom-set-variables
 '(font-lock-use-colors t)
 '(font-lock-use-fonts nil))

; font selections are globally saved (not specific to a buffer)
(setq options-save-faces t)

; default tab width (for tabbing to tab stops, *not* for c-mode indent command)
(setq-default tab-width 4)

; show the matching paren immediately, we are in a hurry
(setq show-paren-delay 0)
(show-paren-mode t)

; revert *scratch* from /tmp and make it save there. this makes emacs ask to
; save it before exiting if necessary.
(with-current-buffer "*scratch*"
  (setq buffer-file-name "/tmp/*scratch*"))

(defun fix-scratch ()
  (interactive)
  (find-file "/tmp/*scratch*")
  (if (not (buffer-modified-p))
      (revert-buffer t t)))

; use the savehist package, yanked from xemacs. (this saves minibuffer,
; filename, regexp, query-replace, etc. history across emacs sessions.)
(require 'savehist)
(savehist-mode 1)
(custom-set-variables
  '(savehist-length nil)
  '(savehist-history-variables
    (cons 'gud-gdb-history savehist-history-variables)))

; only use desktop mode and timers on server
(when (and (>= emacs-major-version 23) (daemonp))
  ; use desktop save mode. state is king!
  (custom-set-variables
   '(desktop-save t)
   '(desktop-restore-eager 0)
   '(desktop-lazy-idle-delay 0)
   '(desktop-lazy-verbose nil)
   '(desktop-save-buffer t)  ; saves buffer "status" (point, mark, etc) too
   '(desktop-load-locked-desktop t)
   )
  (desktop-save-mode 1)
  
  ; save history and desktop periodically, since emacs is often killed, not quite
  ; nicely.
  (run-with-timer 300 300
   (lambda () (desktop-save-in-desktop-dir)
              (savehist-save)
              (message nil))))  ; clear the "Desktop saved in..." message

; allow narrowing
(put 'narrow-to-region 'disabled nil)

;; i used to use global-auto-revert-mode, but not any more. it can cause hangs
;; if it tries to revert e.g. TRAMP files without a network connection.
(global-auto-revert-mode 0)
;; (custom-set-variables
;;   '(revert-without-query '(".*")))

;; the thinkpad "forward" button, above the right arrow key and to the right
;; of the up arrow key, sends f20.
(global-unset-key [(f20)])

;; C-page up and C-page down, bound to scroll-left and -right respectively. i
;; never use them, and the "disabled command" prompt is annoying.
(global-unset-key [(control next)])
(global-unset-key [(control prior)])

; enable downcase-region
(put 'downcase-region 'disabled nil)

; enable 'a' to open directory or file in the same buffer in Dired mode
; NOTE: if i want to expand this to reuse Dired buffers for all motion, e.g. ^
; to move up to the parent directory, see
; http://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
(put 'dired-find-alternate-file 'disabled nil)

;; Scroll up/down ARG lines, or near full screen if no ARG. A near full screen
;; is `next-screen-context-lines' less than a full screen. In practice, this
;; means that when the window is at the beginning or end of the buffer, page-up
;; and page-down will actually move the point to the beginning or end of the
;; buffer, respectively.
;;
;; Based on scroll-down-nomark from pc-select.el.
(global-set-key [next]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
  (lambda () (interactive)
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))

; control-up and control-down scroll one line in either direction
(defun scroll-up-one-line ()
  (interactive) (scroll-up 1))
(defun scroll-down-one-line ()
  (interactive) (scroll-down 1))
(global-set-key [(control up)] 'scroll-down-one-line)
(global-set-key [(control down)] 'scroll-up-one-line)

; only let C-w kill if mark/region is active
(global-set-key [(control w)]
  (lambda (beg end &optional yank-handler)
    (interactive (list (point) (mark)))
    (if mark-active (kill-region beg end yank-handler))))

;; automatically close buffers that haven't been displayed in more than a month
(require 'midnight)
(custom-set-variables '(clean-buffer-list-delay-general 30))

(defmacro replace-match-num (replacement num)
  "Convert shorthand for an entry in one of my 'list' pages into html/markdown."
  (replace-match replacement nil nil nil num))

;; TEMPORARY! until ff9 supports -no-remote and remote command line again.
;; http://superuser.com/questions/372573/cant-aim-command-line-at-one-of-multiple-running-instances-since-upgrading-to-f
;; https://bugzilla.mozilla.org/show_bug.cgi?id=650078
;; (defadvice browse-url (around ff9-hack-workaround activate)
;;   "Firefox 9 broke command line remote, so don't try to open URLs with it.
;; Instead, just copy the URL into the kill ring."
;;   (kill-new url))

(defun listify ()
  (interactive)
  (let* ((before-regexp "\\(^\\)\\(.*\\)")
         (after-regexp "\\( \\)?\\(.+\\)\\( [ogb]\\)\\($\\)")
         (filename (file-name-nondirectory buffer-file-name))
         (span (not (member filename '("games.txt" "books.txt"))))
         (title (member filename '("games.txt" "classical_music.txt"))))
    (when (looking-at after-regexp)
      ;; expand two digit vintage years in wine.txt
      (if (equal filename "wine.txt")
          (save-excursion
            (if (re-search-forward " \\([0-9][0-9]\\) " (point-at-eol) t)
                (replace-match-num "20\\1" 1))))

      (kill-ring-save (line-beginning-position) (- (line-end-position) 2))
      (capitalize-region (line-beginning-position) (line-end-position))
  
      (looking-back before-regexp)
      (if span (replace-match-num "<span title=\"\">\n" 1)
        (replace-match-num "  * " 1))
      (if (not (equal "" (match-string 2)))
          (replace-match-num "[\\2]()\n" 2))
  
      (looking-at after-regexp)
      (if (match-string 1)
          (replace-match-num "" 1))
      (replace-match (if title "_[\\2]()_\n" "[\\2]()\n") nil nil nil 2)
      (let ((r (match-string 3)))
        (cond ((equal r " O") (replace-match-num "![ok](/ok.png)" 3))
              ((equal r " G") (replace-match-num "![good](/good.png)" 3))
              ((equal r " B") (replace-match-num "![bad](/bad.png)" 3))))
      (if span (replace-match-num "\n</span>" 4))

      (let ((url (cond
  ((equal filename "wine.txt")
   "http://www.cellartracker.com/list.asp?fInStock=0&Table=List&szSearch=%s")
  ((equal filename "beer.txt")
   "http://beeradvocate.com/search?q=%s")
  ((equal filename "chocolate.txt")
   "http://www.google.com/search?&q=site:seventypercent.com+%s")
  ((equal filename "movies.txt")
   "http://www.imdb.com/find?q=%s")
  ((equal filename "restaurants.txt")
   "http://www.sfgate.com/cgi-bin/listings/restaurants/basic?cuisine=&loc=&Submit=1&Search=1&term=%%22%s%%22")
  ((equal filename "games.txt")
   "http://www.gamespot.com/search.html?qs=%s")
  ((equal filename "books.txt")
   "http://www.google.com/search?q=%s&tbm=bks")
  ((equal filename "whiskey.txt")
   "http://www.whiskymag.com/whisky/find.php?q=%s&Search=Find+a+Whisky")
  (t "http://www.google.com/search?q=%s")))) ;; default to google search
        (if url (with-firefox-profile "personal"
                                      (browse-url (format url (current-kill 0 t)))))))))

(global-set-key [(control x) (control l)] 'listify)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; menu is the right-click windows key on the right side, btw alt and ctrl
(global-unset-key [(menu)])

; C-- is undo (removes the shift), C-= is redo
(global-set-key [(control -)] 'undo)
(global-set-key [(control =)] 'redo)
(global-set-key [(meta =)] 'redo)
(global-unset-key [(control /)])
(global-unset-key [(control h) (g)])  ; describe-gnu-project
(global-unset-key [(control h) (r)])  ; info-emacs-manual

; M-$ is spell-check by default...but i never use it, and i often hit it by
; accident when i mean M-% for query-replace. so, make M-$ query-replace too.
(global-set-key [(meta $)] 'query-replace)

; M-g prompts for a line to go to
(global-set-key [(meta g)] 'goto-line)

; M-r reverts the current buffer
(global-set-key [(meta r)] (lambda () (interactive) (revert-buffer nil t)))

; C-x C-d deletes this file
(global-set-key [(control x) (control d)] 'delete-file)

; C-v keybinding for yank, to make it consistent with GTK apps
(global-set-key [(control v)] 'yank)

; M-C-f for find-file-at-point
(global-set-key [(meta control f)] 'find-file-at-point)

; vc-mode
(global-set-key [(control x) (v) (r)] 'vc-revert-buffer)
(global-set-key [(control x) (v) (u)] 'vc-update)

; ergonomic keybindings - shorten C-x 0, C-x 1, C-x o, C-x b, C-x f, etc. (i
; used to have C-k kill the buffer, but i switched it back to kill line, for
; compatibility w/tcsh, readline, etc.)
(global-set-key [(control f)] 'find-file)
(global-set-key [(control b)] 'switch-to-buffer)
(global-set-key [(control o)] 'other-window)
(global-set-key [(control \0)] 'delete-window)
(global-set-key [(control \1)] 'delete-other-windows)
(global-set-key [(control \`)] 'next-error)
(global-set-key [(meta control \`)] 'previous-error)

;; C-number keys don't have ascii control codes, so they can't be used in
;; terminal frames. C-` is translated to C-@ in terminal, which is C-[space],
;; which we need to set mark, so we can't set that. :( use the M-` workaround
;; below instead. use M-* instead as alternates.
(global-set-key [(meta \0)] 'delete-window)
(global-set-key [(meta \1)] 'delete-other-windows)
(global-set-key [(meta \`)] 'next-error)
;; TODO: i need esc-esc-meta-` :P
;; (global-set-key [(esc meta \`)] 'previous-error)

(global-set-key [(control x) (k)] 'kill-this-buffer)

; more ergonomics - shortcuts for common commands.
(defalias 'sv 'set-variable)

; rxvt's keycodes for C-up/down/right/left are M-O a/b/c/d. C-F11 is M-[ 2 3 ^.
; this makes those work while running emacs -nw.
;
; this *should* be solved with rxvt.el, but it doesn't seem to work very well.
; http://www.emacswiki.org/cgi-bin/wiki/rxvt.el
(global-set-key [(meta O) (a)] 'scroll-down-one-line)
(global-set-key [(meta O) (b)] 'scroll-up-one-line)
(global-set-key [(meta O) (c)] 'forward-word)
(global-set-key [(meta O) (d)] 'backward-word)
; yes, the extra parens are necessary. no, i don't know why.
(global-set-key [(meta \[) (\2) (\3) (\^)] 'compile)
(global-set-key [(meta \[) (\7) (\^)] 'beginning-of-buffer)
(global-set-key [(meta \[) (\8) (\^)] 'end-of-buffer)
(global-set-key [(control @)] 'set-mark-command)

; dired-mode keybindings. ! toggles showing dotfiles, C-o switches buffers
(require 'dired)

(define-key dired-mode-map "!"
  (lambda ()
    (interactive)
    (dired-sort-other
     (if (equal dired-actual-switches "-l") "-al" "-l"))))

(define-key dired-mode-map [(control o)] 'other-window)

; use ibuffer, not buffer-menu-mode
(global-set-key [(control x) (control b)] 'ibuffer)

(global-set-key [(control h) (a)] 'apropos)

; translate some C-x C-* bindings to C-x * for easier use with sticky keys
(global-set-key [(control x) (s)] 'save-buffer)
(global-set-key [(control x) (S)] 'save-some-buffers)
(global-set-key [(control x) (w)] 'write-file)
(global-set-key [(control x) (l)] 'listify)
(global-set-key [(control x) (q)] 'toggle-read-only)

;; mpg123 commands in the *music* buffer
(defun music-buffer-cmd (cmd)
  (with-current-buffer "*music*"
    (end-of-buffer)
    (insert cmd)
    (comint-send-input)))

(define-prefix-command 'music-buffer-keymap)
(global-set-key [(meta m)] 'music-buffer-keymap)
(define-key music-buffer-keymap "f"
  (lambda () (interactive) (music-buffer-cmd "f")))
(define-key music-buffer-keymap "q"
  (lambda () (interactive) (music-buffer-cmd "q")))
(define-key music-buffer-keymap " "
  (lambda () (interactive) (music-buffer-cmd " ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; filling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables '(fill-column 80))

; F12 toggles auto-fill-mode
(global-set-key [f12] 'auto-fill-mode)

(defun turn-on-auto-fill-mode ()
  "Add to a major mode hook to turn on auto-fill mode."
  (if (not auto-fill-function)
      (auto-fill-mode)))

(add-hook 'fundamental-mode-hook 'turn-on-auto-fill-mode)
(add-hook 'c-mode-common-hook 'turn-on-auto-fill-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill-mode)
(add-hook 'lisp-mode-hook 'turn-on-auto-fill-mode)
(add-hook 'python-mode-hook 'turn-on-auto-fill-mode)
(add-hook 'perl-mode-hook 'turn-on-auto-fill-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill-mode)
(add-hook 'sql-mode-hook 'turn-on-auto-fill-mode)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill-mode)

;; TODO: consider switching to the built-in adaptive-fill-mode instead?
;; file:///home/ryanb/docs/emacs_manual/Adaptive-Fill.html
(require 'filladapt)
(add-hook 'text-mode-hook 'filladapt-mode)

; use my very own fillcode automatically in most programming major modes.
(require 'fillcode)
(add-hook 'c-mode-common-hook 'fillcode-mode)
(add-hook 'perl-mode-hook 'fillcode-mode)
(add-hook 'python-mode-hook 'fillcode-mode)
(add-hook 'shell-script-mode-hook 'fillcode-mode)
(add-hook 'sql-mode-hook 'fillcode-mode)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; emacs will create the backup dir automatically, but not the autosave dir.
(make-directory "~/.emacs.d/autosaves/" t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; reopen and rename-this-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; reopen marked buffers in *Buffer-List* or *Ibuffer*
(require 'ibuffer)
(defun Buffer-menu-reopen (regexp replacement)
  "Reopens marked files in a different place by replacing part of their path."
  (interactive "sReplace: \nsWith: ")
  (goto-char (point-min))
  (while (search-forward "\n>" nil t)
    (let* ((original-buffer
             (cond ((eq major-mode 'Buffer-menu-mode) (Buffer-menu-buffer nil))
                   ((eq major-mode 'ibuffer-mode) (ibuffer-current-buffer))))
           (original-file (buffer-file-name original-buffer)))
      (when (and original-buffer
                 original-file
                 (string-match regexp original-file))
        (let ((new-file (replace-match replacement nil nil original-file)))
          (when (file-exists-p new-file)
            (kill-buffer original-buffer)
            (find-file-noselect new-file))))))
  (cond ((eq major-mode 'Buffer-menu-mode) (list-buffers))
        ((eq major-mode 'ibuffer-mode) (ibuffer-update nil))))

; add keybinding
(define-key Buffer-menu-mode-map "r" 'Buffer-menu-reopen)
(define-key ibuffer-mode-map "r" 'Buffer-menu-reopen)

; add to docstring
(progn
  (put 'Buffer-menu-mode 'function-documentation nil)
  (put 'Buffer-menu-mode 'function-documentation
       (concat (documentation 'Buffer-menu-mode t)
               "\n\\[Buffer-menu-reopen] -- reopen the files marked with m "
               "in a new place in the filesystem.")))

(defun reopen-elsewhere (regexp replacement)
  "Reopens the file visited by the current buffer in a different location."
  (interactive "sReplace: \nsWith: ")
  (let ((orig-buffer (current-buffer)))
    (if (string-match regexp buffer-file-name)
        (progn
          (find-file (replace-match replacement nil nil buffer-file-name))
          (kill-buffer orig-buffer))
      (error "No match for %s in %s" regexp orig-file))))

(global-set-key [(meta control r)] 'reopen-elsewhere)

(defun rename-this-file (new-file-name)
  "Renames this file and switches the buffer to point to the new file."
  (interactive "FRename to: ")
  (let ((orig-buffer (current-buffer)))
    (rename-file buffer-file-name new-file-name)
    (find-file new-file-name)
    (kill-buffer orig-buffer)))

(global-set-key [(control x) (control r)] 'rename-this-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; highlight tab characters, TODOs, and column 80
(make-face 'ugly-red-face)
(set-face-attribute 'ugly-red-face nil :foreground "red" :background "yellow")

(make-face 'ugly-yellow-face)
(set-face-attribute 'ugly-yellow-face nil
                    :foreground "yellow" :background "red")

(defun my-faces ()
   (font-lock-add-keywords nil
    '(("^.\\{80\\}\\(.\\)" 1 'ugly-red-face t) ; highlight column 80
      ("TODO.*" 0 'ugly-red-face 'prepend)     ; highlight TODOs in code
       ("\t" 0 'ugly-yellow-face t)             ; highlight tab characters
      ; highlight out-of-date copyright notices
      ("^.*opyright.*\\(19[0-9][0-9]\\|200[01234]\\)[^-,]"
       0 'ugly-yellow-face 'prepend))))
(add-hook 'c-mode-common-hook 'my-faces)
(add-hook 'python-mode-hook 'my-faces)
(add-hook 'shell-script-mode-hook 'my-faces)
(add-hook 'sgml-mode-hook 'my-faces)
(add-hook 'emacs-lisp-mode-hook 'my-faces)
(add-hook 'lisp-mode-hook 'my-faces)
(add-hook 'sql-mode-hook 'my-faces)
(add-hook 'xml-mode-hook 'my-faces)
(add-hook 'text-mode-hook 'my-faces)

; color themes, from color-themes.el. i've tweaked them from the originals.
;   http://www.emacswiki.org/cgi-bin/wiki?ColorTheme
;
; this is a good color theme comparator:
;   http://www-2.cs.cmu.edu/~maverick/GNUEmacsColorThemeTest/
;
; run M-x list-colors-display to show available colors

(require 'color-theme nil t)  ; the last t means don't error if it's not found

(defun clarity-color-theme ()
  "Tweaked version of Clarity and Beauty theme. Black background; my default."
  (interactive)
  (color-theme-install
   ;; frame parameters
   '(((background-color . "black")
      (foreground-color . "lightgray")
      (border-color . "lightgray")
      (cursor-color . "lightgray")
      (mouse-color . "lightgray"))
      ;; (background-mode . dark))  ; makes it unhappy
     ;; face definitions 
     (region ((t (:background "blue3"))))
     (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
     (font-lock-comment-face ((t (:foreground "chocolate1"))))
     (font-lock-constant-face ((t (:foreground "Aquamarine"))))
     (font-lock-doc-face ((t (:foreground "LightSalmon"))))
     (font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
     (font-lock-keyword-face ((t (:foreground "Cyan"))))
     (font-lock-string-face ((t (:foreground "LightSalmon"))))
     (font-lock-type-face ((t (:foreground "PaleGreen"))))
     (font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
     (font-lock-warning-face ((t (:foreground "Pink")))))))
(defalias 'dark-color-theme 'clarity-color-theme)
  
(defun greiner-color-theme ()
  "Tweaked version of Greiner theme. White(ish) background, good in the sun."
  (interactive)
  (color-theme-install
   ;; frame parameters
   '(((background-color . "beige")
      (foreground-color . "black")
      (border-color . "black")
      (cursor-color . "black")
      (mouse-color . "black"))
      ;; (background-mode . light))  ; makes it unhappy
     ;; face definitions 
     (region ((t (:background "turquoise1"))))
     (font-lock-builtin-face ((t (:foreground "dark blue"))))
     (font-lock-comment-face ((t (:foreground "firebrick4"))))
     (font-lock-constant-face ((t (:foreground "DodgerBlue4"))))
     (font-lock-doc-face ((t (:foreground "OrangeRed3"))))
     (font-lock-function-name-face ((t (:foreground "dark blue"))))
     (font-lock-keyword-face ((t (:foreground "dark cyan"))))
     (font-lock-string-face ((t (:foreground "OrangeRed3"))))
     (font-lock-type-face ((t (:foreground "dark green"))))
     (font-lock-variable-name-face ((t (:foreground "DarkOrange3"))))
     (font-lock-warning-face ((t (:foreground "Red")))))))
(defalias 'light-color-theme 'greiner-color-theme)

(defadvice compile (before color-compilation-mode-line activate)
  "When compilation starts, make the mode line yellow.  That way, when
the below `compilation-handle-exit' advice reverts the color, we'll
know that the compilation is done. Thanks to Arthur Gleckler!
http://groups/emacs-users/browse_thread/thread/92c07fdebbcff55e
"
  (set-face-background 'mode-line "yellow")) 

(defadvice compilation-handle-exit (after uncolor-compilation-mode-line activate)
  "Once compilation is finished, revert mode line's background
  color. That way, I can see that it has ended."
  (set-face-background 'mode-line "grey75"))

;; this makes the modeline face's colors cooperate with the above changes in
;; terminal windows. specifically, it was the only way i found to override
;; `mode-line-inverse-video' and still have modeline in terminals look decent.
(set-face-inverse-video-p 'mode-line nil)
(set-face-foreground 'mode-line "black")
(set-face-background 'mode-line "grey75")

(defadvice start-my-shells (after color-fringe-arrow activate)
  "Color the marker arrow (for compilation, debugger, etc.) in the margin.
Advice only because it breaks on emacs 23.2 if it runs directly
in .emacs at startup. Looks like set-fringe-bitmap-face isn't
defined while starting with --daemon, but works fine while
starting normally. Grr."
  (make-face 'blue-fringe-arrow)
  (set-face-attribute 'blue-fringe-arrow nil :foreground "blue")
  (if (>= emacs-major-version 23)
      (set-fringe-bitmap-face 'right-triangle 'blue-fringe-arrow)))

;; color added/removed/changed lines in diff-mode.
;; inspired by http://www.doitian.com/2009/07/pretty-emacs-diff-mode/ and
;; http://www.emacswiki.org/emacs/diff-mode-.el .
(custom-set-faces
 '(diff-added ((t (:foreground "Green"))))
 '(diff-changed ((t (:foreground "Yellow"))))
 '(diff-nonexistent ((t (:strike-through nil))))
 '(diff-removed ((t (:foreground "Red")))))

;; TEMPORARY: bug fix to dirtrack to support tramp-based remote shells.
;; submitted upstream: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=9647
(require 'dirtrack)
(defun dirtrack (input)
  "Determine the current directory by scanning the process output for a prompt.
The prompt to look for is the first item in `dirtrack-list'.

You can toggle directory tracking by using the function `dirtrack-mode'.

If directory tracking does not seem to be working, you can use the
function `dirtrack-debug-mode' to turn on debugging output."
  (unless (or (null dirtrack-mode)
              (eq (point) (point-min)))     ; no output?
    (let (prompt-path
	  (current-dir default-directory)
	  (dirtrack-regexp    (nth 0 dirtrack-list))
	  (match-num	      (nth 1 dirtrack-list))
          ;; Currently unimplemented, it seems.  --Stef
	  (multi-line	      (nth 2 dirtrack-list)))
      (save-excursion
        ;; No match
        (if (not (string-match dirtrack-regexp input))
            (dirtrack-debug-message
             (format "Input `%s' failed to match `dirtrack-list'" input))
          (setq prompt-path (match-string match-num input))
          ;; Empty string
          (if (not (> (length prompt-path) 0))
              (dirtrack-debug-message "Match is empty string")
            ;; Transform prompts into canonical forms
            (setq orig-prompt-path (funcall dirtrack-directory-function
                                            prompt-path)
                  prompt-path (shell-prefixed-directory-name orig-prompt-path)
                  current-dir (funcall dirtrack-canonicalize-function
                                       current-dir))
            (dirtrack-debug-message
             (format "Prompt is %s\nCurrent directory is %s"
                     prompt-path current-dir))
            ;; Compare them
            (if (or (string= current-dir prompt-path)
                    (string= current-dir (abbreviate-file-name prompt-path)))
                (dirtrack-debug-message (format "Not changing directory"))
              ;; It's possible that Emacs will think the directory
              ;; won't exist (eg, rlogin buffers)
              (if (file-accessible-directory-p prompt-path)
                  ;; Change directory. shell-process-cd adds the prefix, so we
                  ;; need to give it the original (un-prefixed) path.
                  (and (shell-process-cd orig-prompt-path)
                       (run-hooks 'dirtrack-directory-change-hook)
                       (dirtrack-debug-message
                        (format "Changing directory to %s" prompt-path)))
                (message "Directory %s does not exist" prompt-path)))
            )))))
  input)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; .emacs.google
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((google-home (concat (getenv "HOME") "/w")))
  (if (and (file-accessible-directory-p google-home)
           (file-accessible-directory-p "/google/src"))
      (load-file (concat google-home "/.emacs.google"))))

(custom-set-faces)
