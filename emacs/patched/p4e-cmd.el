;;; p4e-cmd.el --- Command definitions for perforce.

;; Copyright (c) 2005, 2015 The MathWorks Inc.
;;
;; < Note: Must provide full copyright info here >
;;

;;; Commentary:
;;
;; Use objects defined in p4e to create lots of perforce commands the easy way.
;;

;;; Code:

;;; Info type commands
;;
(p4e-command :name "info" :command "info" :binding "i")
(p4e-command :name  "opened" :command "opened" :binding "o" :mode #'p4e-list-mode)
(p4e-command :name  "files" :command "files" :commandargs '("*") :binding "f" :mode #'p4e-list-mode)

;; p4 changes never returns
;;(p4e-command-on-file "changes" :command "changes")

;; log probably not needed because vc-p4 gives ^X v l which is better because
;; you can select a version with f
(p4e-command-on-file :name  "log" :command "filelog" :commandargs (list "-l") :binding "l")
(p4e-command :name  "my-pending-changes" :command "changes"
	     :commandargs (list "-s" "pending" "-u" user-login-name))

(p4e-command-form :name "client" :command "client" :binding "C")

;;; diff'ing
(p4e-command-on-file :name  "diff" :command "diff" :binding "=" :mode #'diff-mode)
(p4e-command :name "have" :command "have" :binding "h" :mode #'p4e-list-mode)
(p4e-command-on-file-revision :name "getHaveVersion" :command "print" :commandargs '("-q")
			      :mode #'p4e-mode-of-origin-RO)
(p4e-command-ediff-file-revision :name "ediff" :command "getHaveVersion" :binding "*")

;;; Edit/change type commands
;;
(p4e-command-on-file :name "edit" :command "edit" :binding "e" :buffer-action 'revert)
(p4e-command-on-file :name  "revert" :command "revert" :binding "U" :buffer-action 'revert :ask-first-p t)
(p4e-command-on-file :name  "add" :command "add" :binding "a")
(p4e-command-on-file :name  "delete" :command "delete" :buffer-action 'kill :ask-first-p t)

;; p4 sync not good with sbsync, etc.
;;(p4e-command "sync" :command "sync" :binding "y")

;;; Submit Stuff
;;
(p4e-command-form :name  "change" :command "change" :binding "c")
(p4e-command-form :name  "submit" :command "change" :outcommand "submit" :binding "v")
(p4e-command-form :name  "submit+" :command "change" :outcommand "submit"
		  :interactivearg '(changeset)
		  :interactivespec "sChange Set: ")

;; Help/info
(p4e-command :name "clients" :command "clients" :commandargs '("-u") :binding "\C-c"
	     :interactivearg '(user)
	     :interactivespec "sClients for user: ")
(p4e-command :name  "my-changes" :command "changes"
	     :commandargs (list "-u" user-login-name))
(p4e-command :name "p4 help COMMAND" :command "help" :binding "H"
	     :interactivearg '(command)
	     :interactivespec "sHelp for command: ")


;;; Menu Structure
;;
(defvar p4e-command-menu
  '( "P4E"   ;; First item names to top-level menu.
     "info"
     "client"
     "opened"
     "files"
     "my-pending-changes"
     "-"
     ("Diffs"
      "diff"
      "ediff")
     ("Versions"
      "have"
      ;; "getHaveVersion"    ;; not  sure what this does
      )
     "-"
     "edit"
     "revert"
     "add"
     "delete"
     "-"
     "change"
     "submit"
     "submit+"
     "-"
     "clients"
     "my-changes"
     "p4 help COMMAND"
    )
  "Define the top level structure of the p4e command menu.
Each string entry must be the name of some p4e command object.
If an entry is a list, the first string is the submenu name, and the
other items are placed in a submenu.
A string of '-' is a break between items.

After resetting this variable, you need to run the command:
`p4e-rebuid-menu'")


(provide 'p4e-cmd)

;;; p4e-cmd.el ends here
