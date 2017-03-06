(defun finc--hdr-cleanup (hdr)
  ""
  (load-file)
  (create-unit-test)
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
  (locate-all-headers)
  (for h
       (finc--strip-common)
       (finc--expand-rollup-headers)
       (addToList (#includes, pathToHdr)))
  (sortBasedOnNumberOfIncludes)
  (for h
       (if (finc--hdr-clean
   
