;;; p4e.el --- Eric's P4 integration mode.

;; Copyright (c) 2005, 2015 The MathWorks Inc.
;;
;; Issues:
;;   (require 'p4e) causes stata on /mathworks/home/.perforce (from
;;   within the p4 utilties. Stat'ing this slows things down.


;;; Commentary:
;;
;; Integrate P4 into Emacs.
;;
;; This is tuned to the way The MathWorks uses Perforce.
;;
;; This is p4e, since p4.el is taken, this is P4, Erics way.
;;
;; Default keymap is:  C-c M  (Capital letter M)
;;
;; This map works on files in buffers, or files on lines in perforce
;; output buffers.

(require 'eieio)
(require 'eieio-base)
(require 'p4e-exec)
(require 'p4e-form)

;;; Code:
(defgroup p4e
  nil
  "Perforce."
  :group 'vc)

;;; Minor mode keymap.
;;
(defcustom p4e-p4-prefix "\C-cM"
  "*Keybinding prefix for all perforce commands in P4E."
  :group 'p4e
  :type 'string)

(defvar p4e-minor-mode-map
  (let ((km (make-sparse-keymap "p4e-minor")))
    (define-key km p4e-p4-prefix (make-sparse-keymap))
    km)
  "Keymap used for editing perforce config files.")

;;; Command Represenatation
;;
(eval-and-compile
  (defvar p4e-commands nil
    "List of installed perforce commands."))

(defclass p4e-command (eieio-instance-tracker)
  ((tracking-symbol :initform 'p4e-commands)
   (name :initarg :name
         :initform ""
	 :type string
	 :documentation "The name of the p4 command to execute")
   (command :initarg :command
	    :type string
	    :documentation "The p4 command to execute.")
   (commandargs :initarg :commandargs
		:initform nil
		:type (or null list)
		:documentation "Argument list for the perforce function.")
   (interactivearg :initarg :interactivearg
		   :initform nil
		   :type (or null list)
		   :documentation "The arguments for the Emacs command.")
   (interactivespec :initarg :interactivespec
		   :initform nil
		   :type (or string list)
		   :documentation "The `interactive' spec for this command.")
   (binding :initarg :binding
	    :type string
	    :documentation "Keybinding for this command.")
   (mode :initarg :mode
	 :initform fundamental-mode
	 :type function
	 :documentation "Major-mode to use on output.")
   (post-command-hook :initarg :post-command-hook
		      :initform nil
		      :type (or function null)
		      :documentation "Function to run after command completes.
Command is run in the originating buffer.")
   )
  "A Perforce command structure.
Use the `p4e-exec' method to run this command from a buffer.")

(defmethod p4e-cmd-name ((cmd p4e-command))
  "Return the user facing name of CMD."
  (oref cmd name))

(defmethod p4e-fcn-symbol ((cmd p4e-command))
  "Return a symbol that is the Emacs Lisp function for the p4e command CMD."
  (intern (concat "p4e-" (p4e-cmd-name cmd) "-cmd")))  

(defmethod p4e-bind-command ((cmd p4e-command) &optional special-map)
  "Bind the command CMD into the keymap km."
  (let ((km (or special-map
		(lookup-key p4e-minor-mode-map p4e-p4-prefix))))
    (when (slot-boundp cmd 'binding)
      (let ((k (oref cmd binding))
	    (fcn (p4e-fcn-symbol cmd))
	    )
	(when (stringp k)
	  (define-key km k fcn)))
      )
    ))

(defmethod p4e-easymenu-vector ((cmd p4e-command))
  "Convert CMD into an easymenu vector."
  (vector (capitalize (p4e-cmd-name cmd))
	  (p4e-fcn-symbol cmd)
	  `(p4e-command-valid ,cmd)
	  )
  )

(defmethod p4e-command-valid ((cmd p4e-command))
  "Return non-nil if it is ok to run CMD right now."
  t)

(defmethod initialize-instance ((cmd p4e-command) &optional fields)
  "Initialize the perforce command CMD.
Create the interactive function for this object."
  (prog1 (call-next-method)
    ;; Create the new method here.
    (let* ((fcn (p4e-fcn-symbol cmd))
	   (cmdargs (oref cmd commandargs))
	   (intargs (oref cmd interactivearg))
	   (allargs (if (not intargs)
			cmdargs
		      (append cmdargs intargs)))
	   )
      ;; Create the interactive user function associated w/ this object.
      (fset fcn `(lambda ,intargs
		   ,(concat "Perforce command " (oref cmd command) ".")
		   (interactive ,(oref cmd interactivespec))
		   (apply 'p4e-exec ,cmd (list ,@allargs))))
      ;; Bind the key for this object.
      (p4e-bind-command cmd)
      )))

(defmethod p4e-exec ((cmd p4e-command) &rest args)
  "Execute the perforce command CMD with optional ARGS."
  (let ((b (apply 'p4e-run-command-and-show (oref cmd command) args)))
    (save-current-buffer
      (set-buffer b)
      (save-excursion
        (let ((o p4e-originator))
          (funcall (oref cmd mode))
          ;; some major modes will kill local variables.  Restore this important one.
          (setq p4e-originator o))
        ))))

(defun p4e-find-command-by-name (name)
  "Find the command with NAME."
  (let ((cmds p4e-commands)
	(ans nil))
    (while (and cmds (not ans))
      (if (string= (p4e-cmd-name (car cmds)) name)
	  (setq ans (car cmds)))
      (setq cmds (cdr cmds)))
    ans))
;;; Commands that work on the current buffer.
;;

(defclass p4e-command-on-file (p4e-command)
  ((buffer-action :initarg :buffer-action
                  :initform none
                  :type symbol   ;; none, revert, kill
                  :documentation "Buffer action to perform after executing \
the command.")
   (ask-first-p :initarg :ask-first-p
		:initform nil
		:type boolean
		:documentation "Ask user before executing this command?")
   )
  "A Perforce command structure that is applied to a file.")

(defmethod initialize-instance ((cmd p4e-command-on-file) &optional fields)
  "Initialize the perforce command CMD for on-file commands.
Binds this command into minor-mode keymaps."
  (prog1 (call-next-method)
    ;; Bind our key into the minor-mode keymaps on all minor modes
    ;; that have file lists in them.
    (p4e-bind-command cmd p4e-form-C-c-map)
    (p4e-bind-command cmd p4e-list-C-c-map)
    ))

(defmethod p4e-guess-filename ((cmd p4e-command-on-file))
  "Guess a filename for executing CMD on.
Can be the file in the current buffer, or a file under the point
in one of the list modes."
  ;; try a file for this buffer.
  (let ((f (buffer-file-name))
	)
    
    ;; If not for this buffer, file on this line in one of our
    ;; fancy major modes?
    (when (and (not f)
	       (or (eq major-mode 'p4e-form-mode)
		   (eq major-mode 'p4e-list-mode)))
      (let* ((fn (p4e-ff-get-file))
	     (s (p4e-split-filename fn))
	     (r (p4e-config-dir))
	     )
	(setq f (expand-file-name (car s) r))))

    ;; Err if no file to work on.
    (when (not f)
      (error "Could not find file for command %s"
	     (oref cmd command)))

    ;; Return a nice filename.
    (file-relative-name f default-directory)))

(defmethod p4e-exec ((cmd p4e-command-on-file) &rest args)
  "Execute the perforce command CMD with optional ARGS."
  (let* ((f (p4e-guess-filename cmd))
	 (na (append args (list f)))
	 (cb (get-file-buffer f)))
    (when (or (not (oref cmd ask-first-p))
	      (y-or-n-p (format "Really %s %s?"
				(oref cmd command)
				f)))
      (apply 'call-next-method cmd na)
      (cond ((and (eq (oref cmd buffer-action) 'revert) cb)
             (save-current-buffer
               (set-buffer cb)
               (save-excursion
                 (revert-buffer nil t))))
            ((and (eq (oref cmd buffer-action) 'kill) cb
                  (not (file-exists-p f))
                  (y-or-n-p "Kill buffer? "))
             (kill-buffer cb)
             ))
      )))

;;; Commands that work on the current buffer's current revision
;;
(defclass p4e-command-on-file-revision (p4e-command-on-file)
  ((filesave :initarg nil
	     :documentation "Text of last calculated filename."))
  "A Perforce command structure that is applied to a file revision.")

(defmethod p4e-guess-filename ((cmd p4e-command-on-file-revision))
  "Guess a filename and revision for executing CMD on.
Can be the file in the current buffer, or a file under the point
in one of the list modes."
  ;; try a file for this buffer.
  (let* ((fname (call-next-method))
	 (res (p4e-run-command-ztag "have" fname))
	 )
    (oset cmd filesave (concat fname "#" (cdr (assoc "haveRev" res))))
    ))

(defmethod p4e-exec ((cmd p4e-command-on-file-revision) &rest args)
  "Execute the command, and set the major mode on the resulting buffer."
  (call-next-method)
  ;; Rename the output buffer.
  (let*
      ((bufName (concat (p4e-cmd-name cmd) ": " (oref cmd filesave)))
       (buf (get-buffer bufName)))
    (if buf (kill-buffer buf))
    (rename-buffer bufName)
    )
  )

;;; Commands that ediff against some revision
;;
(defclass p4e-command-ediff-file-revision (p4e-command)
  ()
  "A Perforce command structure that is applied to a file to perform an ediff.
Note that the `:command' slot is now used to store the name of some p4e
command object!")

(defmethod p4e-exec ((cmd p4e-command-ediff-file-revision) &rest args)
  "Execute the revision command, and ediff against the originator."
  (let* ((slave-cmd (p4e-find-command-by-name (oref cmd command)))
	 (fcnsym (p4e-fcn-symbol slave-cmd))
	 )
    ;; Call the slave fetch command
    (call-interactively fcnsym nil)
    ;; Go into ediff mode w/ the originator buffer
    (ediff-buffers p4e-originator (current-buffer))
    ))

;;; Commands with Forms
;;

(defclass p4e-command-form (p4e-command)
  ((mode :initform 'p4e-form-mode)
   (outcommand :initarg :outcommand )
   )
  "A perforce command structure which allows editing of a form.
Forms use `p4e-form-mode' and the command must accept -o, and -i
flags to enable editor work-arounds.")

(defmethod p4e-exec ((cmd p4e-command-form) &rest args)
  "Execute the perforce command CMD with optional ARGS."
  (let* ((incmd (oref cmd command))
	 (outcmd (if (slot-boundp cmd 'outcommand)
		     (oref cmd outcommand)
		   incmd)))
    (apply 'p4e-run-command-with-form incmd outcmd args)))

;;; Load in support files
;;
(require 'p4e-cmd)

;;; header line
;;
(defcustom p4e-use-header-line (boundp 'default-header-line-format)
  "*Non-nil if we should display version information in the header.
This will only work if you are using Emacs 21."
  :group 'p4e
  :type 'boolean)

(defvar p4e-header-line nil
  "Header line for the current buffer.")

(defvar p4e-fstat-cache nil
  "Cache of the last buffer setup fstat result.")
(make-variable-buffer-local 'p4e-fstat-cache)

(defun p4e-setup-header-line ()
  "Set up the header line for a perforce buffer."
  (interactive) ;; comment out later.
  (let* ((f (file-name-nondirectory (buffer-file-name)))
	 (res (p4e-run-command-ztag "fstat" f))
	 (act (cdr (assoc "action" res)))
	 )
    ;; Cache the fstat
    (setq p4e-fstat-cache res)
    ;; Create the header line
    (make-local-variable 'p4e-header-line)
    (setq p4e-header-line
	  (list "P4E: "
		(cdr (assoc "depotFile" res))
		"#"
		(cdr (assoc "haveRev" res))
		" Head="
		(cdr (assoc "headRev" res))
		(if act
		    (concat " Action="
			    (cdr (assoc "action" res)))
		  "")
		;;"  Next Action: " na
		" -%-\n"
		)
	  header-line-format 'p4e-header-line)
    ))

;;; P4E MENU
;;
(defun p4e-rebuild-menu ()
  "Command for rebuilding the p4e menu.
Uses the contents of `p4e-command-menu' to create it."
  (interactive)
  (let ((menu (p4e-rebuild-menu-recursive p4e-command-menu)))
    ;; Call easymenu on it.
    (easy-menu-define p4e-minor-mode-menu p4e-minor-mode-map
      "P4E Minor mode menu"
      menu)
    ))

(defun p4e-rebuild-menu-recursive (parts)
  "Return an easymenu menu for PARTS."
  (let ((result nil)
	(menuname (car parts))
	)
    (setq parts (cdr parts))
    (while parts
      (cond ((and (stringp (car parts)) (string= (car parts) "-"))
	     (setq result (cons "-" result))
	     )
	    ((stringp (car parts))
	     ;; Create the command
	     (let ((cmd (p4e-find-command-by-name (car parts))))
	       (when (not cmd)
		 (error "Unknown p4e command \"%s\" in menu" (car parts)))
	       (setq result (cons (p4e-easymenu-vector cmd)
				  result))))
	    ((listp (car parts))
	     (setq result (cons (p4e-rebuild-menu-recursive (car parts))
				result)))
	    (t
	     (error "Unknown menu thing %S" (car parts))))
      (setq parts (cdr parts)))
    (cons menuname (nreverse result))))

(p4e-rebuild-menu)

;;; P4E MINOR MODE
;;

(defvar p4e-minor-mode nil
  "Symbol used to define the minor mode for p4e.")
(make-variable-buffer-local 'p4e-minor-mode)

(defun p4e-update-minor-alists ()
  "Update the minor-mode alist information for p4e."
  (let ((mma (assoc 'p4e-minor-mode minor-mode-alist))
	(mmma (assoc 'p4e-minor-mode minor-mode-map-alist))
	(name " P4E"))
    (if mma
	(setcdr mma (list name))
      (add-to-list 'minor-mode-alist '(p4e-minor-mode " P4E")))

    (if mmma
	(setcdr mmma p4e-minor-mode-map)
      (add-to-list 'minor-mode-map-alist
		   (cons 'p4e-minor-mode p4e-minor-mode-map)))
    ))

(p4e-update-minor-alists)

(defun p4e-maybe-turn-on-minor-mode  ()
  "If this buffer is in a perforce directory, enable P4 thingies."
  (when (and (p4e-config-dir) (p4e-client-root-dir))
    (setq p4e-minor-mode t)
    ;; p4e-setup-header-line causes Aquamacs 23 on MAC to loose it tabs
    ;; See Bob Green system.
    ;;    (p4e-setup-header-line)
    ))

(add-hook 'find-file-hooks 'p4e-maybe-turn-on-minor-mode t)

(provide 'p4e)

;;; p4e.el ends here
