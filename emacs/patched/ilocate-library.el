;;; ilocate-library.el --- Interactive replacement for locate-library with completion.
;; ilocate-library.el
;; add minibuffer name completion to locate library
;;
;;
;; Copyright (C) 2001 Michael Slass
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;

;;; Commentary:
;;
;; This package consists of functions to replace
;; `locate-library' and `load-library' with interactive
;; versions that do minibuffer completion.
;;
;; entry points:
;;   ilocate-library :
;;      locate library in load-path
;;   ilocate-library-find-source :
;;      locate library, and read the source (.el),
;;      if it's available, into a new buffer in
;;      the other window
;;   ilocate-load-library :
;;      interactive load-library with completion
;;
;; All functions use a cached table of libraries found in the
;; `load-path'.  The table, `ilocate-library-cache', is
;; generated the first time any of these functions are
;; called, or if the `load-path' has changed since the last
;; invocation of the function.  You may force the refreshing
;; of the cache by calling any of the functions with a
;; prefix arg.
;;
;;

;;; History:
;;
;; created by Mike Slass <miknrene@drizzle.com>
;; version 0.6 9/12/2001
;;   - fix completing-read-library to handle non-existant directories
;;        in the load-path
;;
;; modified by Klaus Berndl <klaus.berndl@sdm.de>
;; version 0.7 30/07/2002
;;   - added a library cache with lazy instantiation. See
;;     `ilocate-library-cache'.
;;
;;
;; version 0.8 July 30, 2002 - Mike Slass
;;    - incorporated Klaus Berndl's changes (see above)
;;    - modified all the ilocate-... interactive functions
;;      to accept only one (optional) argument, a prefix arg.
;;      With prefix arg, all now force a refresh of
;;      `ilocate-library-cache'
;;    - modified completing-read-library to accept an
;;      optional third argument, reload-cache, to force the
;;      refresh
;;    - added "Building libraries list ..." message to
;;      `ilocate-get-all-libs'
;;    - clean up the doc strings
;;
;;
;; version 0.9 31/07/2002
;; modified by Klaus Berndl <klaus.berndl@sdm.de>
;;
;;   - added a mechanism which detects changes of last
;;     library-search-path (so also for `load-path' for
;;     example) and then rebuilds auto. the
;;     `ilocate-library-cache'. See
;;     `ilocate-last-search-path-cache'.
;;
;; modified by Mike Slass
;;    - incorporated changes and suggestions from
;;      Jens Schmidt <Jens.Schmidt@oracle.com>:
;;        * clean up docs
;;        * fix a bug in ilocate-library-find-souce
;;        * renamed symbols to all have ilocate prefix:
;;          completing-read-library ==>
;;            ilocate-completing-read-library
;;          iload-library ==>
;;            ilocate-load-library
;;  - deleted iupdate-library-cache
;;
;;
;; version 0.9.1 09/03/2002 - update maintainer address
;;
;;
;; version 0.9.2 01/14/2012
;; modified by John Yates <john@yates-sheets.org>
;;
;;  - support gzipped files (adjust regexps and doc strings)
;;  - visit files in view mode

;;; Code:
(defun ilocate-get-all-libs (&optional search-path)
  "Return a table of all Lisp files \(.el[c] or .el[c].gz) in `load-path'.

Optional argument SEARCH-PATH is a list of directory names
to search instead of those listed in `load-path'.

The return value is a table suitable for use as the second
argument to `completing-read'.

See also `ilocate-completing-read-library' and `ilocate-library-cache'."
  (message "Building libraries list...")
  (mapcar
   'list
   (let ((lp (or search-path load-path))
         (liblist ()))
     (progn
       (while (car lp)
         ;; iterate over each directory in the search path
         ;; skip any directories that don't exist, or the user
         ;; can't read
         (let* ((dir (car lp))
                (dirfiles
                 (and
                  (file-accessible-directory-p dir)
                  (directory-files dir nil "\\.elc?\\(\\.gz\\)?$" t))))
           (while (car dirfiles)
             ;; iterate over this directory's files
             (let* ((file (car dirfiles))
                    ;; change file name to package name
                    (package (if (string-match "\\.elc?\\(\\.gz\\)?$" file)
                                 (replace-match "" t nil file)
                               file)))
               ;; add the package to the liblist if it's not already there
               (if (not (member package liblist))
                   (setq liblist (cons package liblist))))
             (setq dirfiles (cdr dirfiles))))
         (setq lp (cdr lp)))
       (message "Building libraries list...done")
       liblist))))

(defvar ilocate-library-cache nil
  "Cached table of Lisp libraries.

Set only by `ilocate-get-all-libs' from `ilocate-completing-read-library'.")

(defvar ilocate-last-search-path-cache nil
  "The search path from which `ilocate-library-cache' was built.")

(defun ilocate-completing-read-library (&optional prompt search-path reload-cache)
  "Read Lisp library name from minibuffer with completion.

Allows minibuffer completion to the name of any Lisp file
\(.el[c] or .el[c].gz) in the `load-path'.

Optional first argument PROMPT is a prompt string.  If nil,
use a default prompt: \"Enter library name: \".

Optional second argument SEARCH-PATH is a list of directory
names to search instead of those listed in `load-path'.

The list of libraries is cached in `ilocate-library-cache',
which is created by `ilocate-get-all-libs' the first time
`ilocate-completing-read-library' is called.  The cache is also
rebuilt if the `search-path' has changed since the last
invocation of `ilocate-completing-read-library'

Optional third argument RELOAD-CACHE will force a call to
`ilocate-get-all-libs' to re-scan the `load-path' and
rebuild the cache."
  (completing-read
   (or prompt "Enter library name: " )
   ;; this argument to completing-read is a table of all
   ;; the names of packages found in all the directories in
   ;; the search-path
   ;; - 7/30/2002
   ;;    - the search of the load-path which was originally
   ;;      here was packaged into ilocate-get-all-libs \(to enable
   ;;      caching of the list ) by Klaus Berndl
   ;; rebuild the `ilocate-library-cache' if
   ;;   - that's requested by arg 3
   ;;   - it's nil
   ;;   - the `load-path' has changed
   (if (or reload-cache
           (not ilocate-library-cache)
           (not (equal (or search-path load-path)
                       ilocate-last-search-path-cache)))
       (progn
         (setq ilocate-last-search-path-cache
               (copy-list (or search-path
                              load-path)))
         (setq ilocate-library-cache
               (ilocate-get-all-libs search-path)))
     ilocate-library-cache)
   nil t))

;;;###autoload
(defun ilocate-library (&optional reload-cache)
  "Interactive-only version of `locate-library' with name completion.

This function is intended to be a replacement for the
interactive functionality of `locate-library'.  It makes a
completion list from all .el, .elc, .el.gz and .elc.gz
files in all the directories in your `load-path'.

The list of libraries is cached in `ilocate-library-cache',
which is generated the first time one of the `ilocate-library'
functions is invoked.

With optional prefix arg, RELOAD-CACHE, force refreshing of
the cache before running this function.

Since the value-add of `ilocate-library' makes sense only as
an interactive feature, this function should not be used in
Lisp programs unless you always want user interaction at
this step.  Use `locate-library' otherwise.

See also `ilocate-completing-read-library'."
  (interactive "P")
  (locate-library
   (ilocate-completing-read-library "ilocate library: " nil reload-cache)
   nil nil t))

;;;###autoload
(defun ilocate-load-library (&optional reload-cache)
  "Interactive-only version of `load-library' with name completion.

This function is intended to be a replacement for the
interactive functionality of `load-library'.  It makes a
completion list from all .el, .elc, .el.gz and .elc.gz
files in all the directories in your `load-path'.

The list of libraries is cached in `ilocate-library-cache',
which is generated the first time one of the `ilocate-library'
functions is invoked.

With optional prefix arg, RELOAD-CACHE, force refreshing of
the cache before running this function.

Since the value-add of `ilocate-load-library' makes sense only as an
interactive feature, this function should not be used in
Lisp programs unless you always want user interaction at
this step.  Use `load-library' otherwise.

See also `ilocate-completing-read-library'"
  (interactive "P")
  (load-library
   (ilocate-completing-read-library "iload library: " nil reload-cache)))

;;;###autoload
(defun ilocate-library-find-source (&optional reload-cache)
  "Prompt with completion for name of a Lisp library, and visit the source.

This function uses list of all .el, .elc, .el.gz and .elc.gz
files in the `load-path', to allow minibuffer completion of
the library name.

The list of libraries is cached in `ilocate-library-cache',
which is generated the first time one of the `ilocate-library'
functions is invoked.

With optional prefix arg, RELOAD-CACHE, force refreshing of
the cache before running this function.

Once the library is located, if it is a compiled Lisp file
\(.elc or .elc.gz), this function looks for the corresponding
.el or .el.gz file in the same directory.  If found, it visits
it with `view-file-other-window'.  If no .el nor .el.gz file
is not found, an error is returned.

Since the value-add of `ilocate-library-find-source' makes
sense only as an interactive feature, this function should
not be used in Lisp programs unless you always want user
interaction at this step.

See also `ilocate-completing-read-library'"
  (interactive "P")
  (let* ((library-name (ilocate-completing-read-library
                        "Find source for library: " nil reload-cache))
         ;; use locate-library to change the library name
         ;; into a full path to the library file
         (library-file (locate-library library-name))
         (source-file (if (string-match "elc?\\(\\.gz\\)?$" library-file)
                          (replace-match "el" t t library-file)
                        library-file))
         (compressed-source-file (concat source-file ".gz")))
    (cond
     ((file-exists-p source-file)
      (view-file-other-window source-file))
     ((file-exists-p compressed-source-file)
      (view-file-other-window compressed-source-file))
     (t
      (error "Source for %s is not available" library-name)))))

(provide 'ilocate-library)
;;; ilocate-library.el ends here
