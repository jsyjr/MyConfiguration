(defun finc--hdr-cleanup (hdr)
  "Do clean-ups; return t IFF one or more header dropped."
  (finc--load-file)
  (finc--create-unit-test)
  (finc--strip-copyright)
  (finc--strip-pragma-once)
  (finc--strip-guard)
  (find--strip-namespace-closing-comments)
  (finc--sort-includes)
  )

(defun finc--hdr-validate-change ()
  (compile-unit-test))

(defun finc--hdr-validate-drops ()
  (compile-implementation-if-one-exists)
  (sbsmartbuild))

(defun finc--main-hdr-flow ()
  (finc--locate-all-headers)
  (for h
       (finc--strip-common)
       (finc--expand-rollup-headers)
       (addToList (#includes, PATH)))
  (sbsmartbuild)
  (sortBasedOnNumberOfIncludes)
  (for h
       (when (finc--hdr-cleanup h)
         (compile-implementation)
         (sbsmartbuild)
         )))

(defun finc--locate-all-headers (headers-mk-path)
  "Read headers.mk and construct (0, PATH) list."
  (with-temp-buffer
    (buffer-disable-undo)
    (insert-file-contents-literally headers-mk-path)
    (
