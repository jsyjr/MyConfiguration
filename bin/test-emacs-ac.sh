#!/bin/bash
cat >/tmp/xx.el <<EOF

(add-to-list 'load-path "~/.emacs.d/el-get/yasnippet")
(add-to-list 'load-path "~/.emacs.d/el-get/auto-complete")
(add-to-list 'load-path "~/.emacs.d/el-get/popup")

(setq yas-snippet-dirs "~/emacs/yasnippet")

;;; yasnippet
;;; should be loaded before auto complete so that they can work together
(require 'yasnippet)
(yas-global-mode 1)

;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict")
;----------------

(defun ac-common-setup ()
  ;(add-to-list 'ac-sources 'ac-source-filename)
  )

(defun ac-emacs-lisp-mode-setup ()
  (setq ac-sources (append '(ac-source-features ac-source-functions ac-source-yasnippet ac-source-variables ac-source-symbols) ac-sources)))

(defun ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet ac-source-gtags) ac-sources)))

(defun ac-ruby-mode-setup ())

(defun ac-css-mode-setup ()
  (setq ac-sources (append '(ac-source-css-property) ac-sources)))

(defun ac-config-default ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

;----------------

(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
EOF

emacs -Q --debug-init --load /tmp/xx.el &
