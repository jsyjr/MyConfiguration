;; Time-stamp: "2012-01-02 11:37:03 jyates"

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

;;---------------------------------------------------------------------
;; Augment LOAD-PATH
;;
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

;;---------------------------------------------------------------------
;; The heavy lifting is mostly common
;;
(setq custom-file "~/.emacs.d/my-customizations.el")
(load-library "my-rc-common")

;;---------------------------------------------------------------------
;; Host specific initialization if it exists (my-rc-local-HOST.el[c])
;;
(require (intern (concat "my-rc-local-"
                         (let* ((host (system-name))
                                (idx (string-match "\\." host)))
                           (if idx
                               (substring host 0 idx)
                             host))))
         nil t)

;ao: (load "~/emacs/rc/emacs-rc-desktop.el")

;ao: (load "~/emacs/passwords.el.gpg")

;;---------------------------------------------------------------------

;; for org-mode
;ao: (setq comment-start nil)

;; for emacs-jabber (overrides paredit-newline binding)
;(define-key ctl-x-map "\C-j" jabber-global-keymap)

;;---------------------------------------------------------------------
;; Experimental trash... do not commit if there is anything here!
