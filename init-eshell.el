;; Set up the Correct Path

;;    Need the correct PATH even if we start Emacs from the GUI:


(setenv "PATH"
        (concat
         "/usr/local/bin:/usr/local/sbin:"
         (getenv "PATH")))

;; Pager Setup

;;    If any program wants to pause the output through the =$PAGER=
;;    variable, well, we don't really need that:


(setenv "PAGER" "cat")

;; Don’t Modify the Text

;;    Scrolling through the output and searching for results that can be
;;    copied to the kill ring is a great feature of Eshell. However,
;;    instead of running =end-of-buffer= key-binding, the following
;;    setting means any other key will jump back to the prompt:


(setq eshell-scroll-to-bottom-on-input t)

;; Aliases

;;   Gotta have some [[http://www.emacswiki.org/emacs/EshellAlias][shell aliases]], right?


(defalias 'e 'find-file)
(defalias 'ff 'find-file)
(defalias 'emacs 'find-file)



;; Replacing the window with the new buffer may not be what I want.


(defalias 'ee 'find-file-other-window)



;; Pull up dired, but without parameters, just use the current directory.


(defun eshell/d (&rest args)
  (dired (pop args) "."))



;; Some of my favorite bash aliases, can be even more helpful in
;; Eshell. Like ‘ll’:


(add-hook 'eshell-mode-hook
          (lambda ()
            ;; The 'ls' executable requires the Gnu version on the Mac
            (let ((ls (if (file-exists-p "/usr/local/bin/gls")
                          "/usr/local/bin/gls"
                        "/bin/ls")))
              (eshell/alias "ll" (concat ls " -AlohG --color=always")))))



;; I can never seem to remember that =find= and =chmod= behave
;; differently from Emacs than their Unix counterparts, so at this
;; time, I will prefer the native implementations.


(setq eshell-prefer-lisp-functions nil)

;; Git

;;    My =gst= command is just an alias to =magit-status=, but using the
;;    =alias= doesn't pull in the current working directory, so I make it
;;    a function, instead:


(defun eshell/gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))   ;; The echo command suppresses output



;; What about =gd= to call the Diff command?


(defalias 'gd 'magit-diff-unstaged)
(defalias 'gds 'magit-diff-staged)

;; Find File

;;    We should have an "f" alias for searching the current directory for
;;    a file, and a "ef" for editing that file.


(defun eshell/f (filename &optional dir)
  "Searches in the current directory for files that match the
given pattern. A simple wrapper around the standard 'find'
function."
  (let ((cmd (concat
              "find " (or dir ".")
              "      -not -path '*/.git*'"
              " -and -not -path '*node_modules*'"
              " -and -not -path '*classes*'"
              " -and "
              " -type f -and "
              "-iname '" filename "'")))
    (message cmd)
    (shell-command-to-string cmd)))

(defun eshell/ef (filename &optional dir)
  "Searches for the first matching filename and loads it into a
file to edit."
  (let* ((files (eshell/f filename dir))
         (file (car (s-split "\n" files))))
    (find-file file)))



;; Typing =find= in Eshell runs the =find= function, which doesn’t do
;; what I expect, and creating an alias is ineffective in overriding
;; it, so a function will do:


(defun eshell/find (&rest args)
  "Wrapper around the ‘find’ executable."
  (let ((cmd (concat "find " (string-join args))))
    (shell-command-to-string cmd)))



;; To extend Eshell, we need a two-part function.
;; 1. Parse the Eshell buffer to look for the parameter
;;    (and move the point past the parameter).
;; 2. A predicate function that takes a file as a parameter.

;; For the first step, we have our function /called/ as it helps
;; parse the text at this time.  Based on what it sees, it returns
;; the predicate function used to filter the files:


(defun eshell-org-file-tags ()
  "Helps the eshell parse the text the point is currently on,
looking for parameters surrounded in single quotes. Returns a
function that takes a FILE and returns nil if the file given to
it doesn't contain the org-mode #+TAGS: entry specified."

  (if (looking-at "'\\([^)']+\\)'")
      (let* ((tag (match-string 1))
             (reg (concat "^#\\+TAGS:.* " tag "\\b")))
        (goto-char (match-end 0))

        `(lambda (file)
           (with-temp-buffer
             (insert-file-contents file)
             (re-search-forward ,reg nil t 1))))
    (error "The `T' predicate takes an org-mode tag value in single quotes.")))



;; Add it to the =eshell-predicate-alist= as the =T= tag:


(add-hook 'eshell-pred-load-hook (lambda ()
  (add-to-list 'eshell-predicate-alist '(?T . (eshell-org-file-tags)))))

;; Visual Executables

;;   Eshell would get somewhat confused if I ran the following commands
;;   directly through the normal Elisp library, as these need the better
;;   handling of ansiterm:


(add-hook 'eshell-mode-hook
   (lambda ()
      (add-to-list 'eshell-visual-commands "ssh")
      (add-to-list 'eshell-visual-commands "tail")))

;; Special Prompt

;;   Following [[http://blog.liangzan.net/blog/2012/12/12/customizing-your-emacs-eshell-prompt/][these instructions]], we build a better prompt with the Git
;;   branch in it (Of course, it matches my Bash prompt). First, we need
;;   a function that returns a string with the Git branch in it,
;;   e.g. ":master"


(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (if (> (length git-output) 0)
          (concat " :" (substring git-output 0 -1))
        "(no branch)"))))



;; The function takes the current directory passed in via =pwd= and
;; replaces the =$HOME= part with a tilde. I'm sure this function
;; already exists in the eshell source, but I didn't find it...


(defun pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))



;; Make the directory name be shorter...by replacing all directory
;; names with just its first names. However, we leave the last two to
;; be the full names. Why yes, I did steal this.


(defun pwd-shorten-dirs (pwd)
  "Shorten all directory names in PWD except the last two."
  (let ((p-lst (split-string pwd "/")))
    (if (> (length p-lst) 2)
        (concat
         (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                               (substring elm 0 1)))
                    (butlast p-lst 2)
                    "/")
         "/"
         (mapconcat (lambda (elm) elm)
                    (last p-lst 2)
                    "/"))
      pwd  ;; Otherwise, we just return the PWD
      )))

;; Turn off the default prompt.
(setq eshell-highlight-prompt nil)



;; Break up the directory into a "parent" and a "base":


(defun split-directory-prompt (directory)
  (if (string-match-p ".*/.*" directory)
      (list (file-name-directory directory) (file-name-base directory))
    (list "" directory)))



;; Now tie it all together with a prompt function can color each of the
;; prompts components.


(setq eshell-prompt-function
      (lambda ()
        (let* ((directory (split-directory-prompt (pwd-shorten-dirs (pwd-replace-home (eshell/pwd)))))
               (parent (car directory))
               (name (cadr directory))
               (branch (or (curr-dir-git-branch-string (eshell/pwd)) "")))

          (if (eq 'dark (frame-parameter nil 'background-mode))
              (concat   ;; Prompt for Dark Themes
               (propertize parent 'face `(:foreground "#8888FF"))
               (propertize name   'face `(:foreground "#8888FF" :weight bold))
               (propertize branch 'face `(:foreground "green"))
               (propertize " $"   'face `(:weight ultra-bold))
               (propertize " "    'face `(:weight bold)))

            (concat    ;; Prompt for Light Themes
             (propertize parent 'face `(:foreground "blue"))
             (propertize name   'face `(:foreground "blue" :weight bold))
             (propertize branch 'face `(:foreground "dark green"))
             (propertize " $"   'face `(:weight ultra-bold))
             (propertize " "    'face `(:weight bold)))))))



;; Turn off the default prompt, otherwise, it won't use ours:


(setq eshell-highlight-prompt nil)

;; Stack the Buffer

;;   One of the few things I miss about ZShell is the ability to easily
;;   save off a half-finished command for later invocation. I now have
;;   =M-q= functionality with [[https://github.com/tom-tan/esh-buf-stack][esh-buf-stack]]:


(use-package esh-buf-stack
  :ensure t
  :commands eshell-push-command
  :config
  (setup-eshell-buf-stack)
  (define-key eshell-mode-map (kbd "M-q") 'eshell-push-command))

;; Shell Here

;;   If I make an alias that closes a window easily, I can have a quick
;;   "x" alias that quickly exits and [[file:emacs.org::*Macintosh][closes the window]].


(defun eshell/x ()
  "Closes the EShell session and gets rid of the EShell window."
  (kill-buffer)
  (delete-window))



;; Now making little Shells whenever I need them makes sense:


(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

;; Shell There
;;   :PROPERTIES:
;;   :CUSTOM_ID: remote-shell
;;   :END:

;;   Would be nice to be able to run an eshell session and use Tramp to
;;   connect to the remote host in one fell swoop:


(defun eshell-there (host)
  (interactive "sHost: ")
  (let ((default-directory (format "/%s:" host)))
    (eshell host)))



;; While Eshell /can/ operate on remote systems, it doesn't work too
;; well.  Instead, it is better to use the =shell= command.  Since all
;; the shell functions that Emacs supplies work with Tramp
;; references. This makes it very easy to spawn a shell, or a shell
;; command on a remote system...even as root.

;;  Since the Tramp syntax is a bit verbose, a few /wrapper functions/
;;  would make things easier. Also, having a list of /favorite hosts/
;;  with simpler names would also be a nice feature.

;;  Since Emacs doesn't have a memoize function, define a global
;;  variable, =remote-shell-fav-hosts=, a hash that maps nicknames of
;;  hosts to their IP address.


(defvar remote-shell-fav-hosts (make-hash-table :test 'equal)
  "Table of host aliases for IPs or other actual references.")



;; Can we make a list of what hosts are /pre-known/? What if no hosts
;; have been defined? In this case, we want to call the function,
;; =remote-shell-fav-hosts-get= to populate it:


(defun remote-shell-fav-hosts-map ()
  "Returns the mapping between our simple names of our favorite
hosts and their IP address. If the map is empty, and the function
`remote-shell-fav-hosts-get' has been defined, it calls that
function to populate the map prior to returning it. This may
return an empty map."
  (when (and #'remote-shell-fav-hosts-get
             (hash-table-empty-p remote-shell-fav-hosts))
    (remote-shell-fav-hosts-get))
  remote-shell-fav-hosts)



;; In order to populate the =completing-read=, we need a list of hosts:


(defun remote-shell-fav-hosts-list ()
  "Simply returns a list of known hosts from the cached map, or
populates it first if it is empty and the
`remote-shell-fav-hosts-get' function has been defined."
  (hash-table-keys (remote-shell-fav-hosts-map)))



;; Most remote access is done with Tramp, so this function simplifies
;; the complex Tramp string creation, mostly using an =sudo= pipe for
;; =root= access. If the =remote-shell-fav-hosts= hash is empty, we'll
;; populate it when this is called.


(defun remote-shell-tramp-connection (hostname &optional root directory)
  "Return a TRAMP connection string to HOSTNAME. If ROOT is
non-nil, returns an sudo compatible string."
  (when (null directory)
    (setq directory ""))

  ;; The ip address is either the value from a key in our cache, or whatever we pass in:
  (let ((ipaddr (gethash hostname (remote-shell-fav-hosts-map) hostname)))
    (if root
        (format "/ssh:%s|sudo:%s:%s" ipaddr ipaddr directory)
        (format "/ssh:%s:%s"         ipaddr directory))))



;; If the window containing the results of the shell connection or
;; shell command is the same, we can take advantage of that from
;; multiple functions, so:


(defun remote-shell-buffer-name (hostname &optional command-str default-name)
  "Returns a standard format for our remote shell command buffer
windows based on the HOSTNAME and the COMMAND-STR. Uses
DEFAULT-NAME if specified."
  (cond
   (default-name     default-name)
   (command-str      (let ((command (car (split-string command-str))))
                        (format "*%s:%s*" hostname command)))
   (t                (format "*%s*" hostname))))



;; Our simple wrapper function for accessing a remote shell, should
;; use =ido= if available.


(defun remote-shell (hostname &optional root)
  "Start an shell experience on HOSTNAME, that can be an alias to
a virtual machine in the overcloud. With prefix command, opens
the shell as the root user account."
  (interactive
   (list (if #'ido-completing-read
             (ido-completing-read "Hostname: " (remote-shell-fav-hosts-list))
           (completing-read "Hostname: " (remote-shell-fav-hosts-list)))))

  (when (equal current-prefix-arg '(4))
    (setq root t))
  (let ((default-directory (remote-shell-tramp-connection hostname root)))
    (shell (remote-shell-buffer-name hostname))))



;; Instead of starting a shell, what if we just ran a command using the
;; =shell-command= (so that commands that end in a =&= are automatically
;; ran asynchronously.


(defun remote-shell-command (hostname command
                                      &optional root bufname directory)
  "On HOSTNAME, run COMMAND (if the command ends with &, run
asynchronously). With a `C-u' prefix, run the command as ROOT.
When non-interactive, you can specify BUFNAME for the buffer's
name, and DIRECTORY where the command should run."
  (interactive
   (list (if #'ido-completing-read
             (ido-completing-read "Hostname: " (remote-shell-fav-hosts-list))
           (completing-read "Hostname: " (remote-shell-fav-hosts-list)))
         (read-string "Command: ")))
  (when (equal current-prefix-arg '(4))
    (setq root t))
  (let ((default-directory (remote-shell-tramp-connection hostname root directory)))
    (shell-command command (remote-shell-buffer-name hostname command bufname))))



;; With the above helper functions, we can loop over a list of machines,
;; and kick off remote work on each one:


(defun remote-shell-commands (clients command
                                      &optional root async directory)
  "On each host entry in CLIENTS, run the shell COMMAND,
optionally as ROOT. If ASYNC is non-nil, appends the `&' to the
shell command in order to run it asynchronously. Runs the command
in the default home directory unless DIRECTORY is specified."
  (if async
      (setq command (concat command " &")))
  (dolist (host clients)
    (remote-shell-command host command root nil directory)))



;; The results of each command is stored in a separate buffer, and
;; since we know what the names are, this command will attempt to load
;; them on the side... yeah, this is a bit ugly.


(defun remote-shell-commands-show (clients command)
  "Shows each buffer of a previously executed command. For example:

        (let ((my-favs '(\"os-controller\" \"contrail-controller\"
                         \"compute\" \"nagios\" \"elk\"))
              (command \"chef-client\"))
          (remote-shell-commands my-favs command t t)
          (remote-shell-commands-show my-favs command))"

  (delete-other-windows)
  (let ((first-time t))
    (dolist (host clients)
      (if (not first-time)
          (split-window-vertically)
        (split-window-horizontally)
        (setq first-time nil))

      (other-window 1)
      (switch-to-buffer (remote-shell-buffer-name host command))
      (balance-windows)
      (sit-for 0.5))))

;; Tramp

;;   The ability to edit files on remote systems is a wonderful win,
;;   since it means I don't need to have my Emacs environment running on
;;   remote machines (still a possibility, just not a requirement).

;;   According to [[http://www.gnu.org/software/emacs/manual/html_node/tramp/Filename-Syntax.html][the manual]], I can access a file over SSH, via:

;;   #+BEGIN_EXAMPLE
;;   /ssh:10.52.224.67:blah
;;   #+END_EXAMPLE

;;   However, if I set the default method to SSH, I can do this:

;;   #+BEGIN_EXAMPLE
;;   /10.52.224.67:blah
;;   #+END_EXAMPLE

;;   So, let's do it...


(setq tramp-default-method "ssh")

;; Better Command Line History

;;   On [[http://www.reddit.com/r/emacs/comments/1zkj2d/advanced_usage_of_eshell/][this discussion]] a little gem for using IDO to search back through
;;   the history, instead of =M-R= to display the history in a selectable
;;   buffer.

;;   Also, while =M-p= cycles through the history, =M-P= actually moves
;;   up the history in the buffer (easier than =C-c p= and =C-c n=?):


(add-hook 'eshell-mode-hook
     (lambda ()
       (local-set-key (kbd "M-P") 'eshell-previous-prompt)
       (local-set-key (kbd "M-N") 'eshell-next-prompt)
       (local-set-key (kbd "M-R") 'eshell-list-history)
       (local-set-key (kbd "M-r")
              (lambda ()
                (interactive)
                (insert
                 (ido-completing-read "Eshell history: "
                                      (delete-dups
                                       (ring-elements eshell-history-ring))))))))

;; Smarter Shell

;;   After reading Mickey Petersen's [[http://www.masteringemacs.org/articles/2010/12/13/complete-guide-mastering-eshell/][Mastering EShell]] article, I like the
;;   /smart/ approach where the cursor stays on the command (where it can
;;   be re-edited). Sure, it takes a little while to get used to...


(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

;; Helpers

;;   Sometimes you just need to change something about the current file
;;   you are editing...like the permissions or even execute it. Hitting
;;   =Command-1= will prompt for a shell command string and then append
;;   the current file to it and execute it.


(defun execute-command-on-file-buffer (cmd)
  (interactive "sCommand to execute: ")
  (let* ((file-name (buffer-file-name))
         (full-cmd (concat cmd " " file-name)))
    (shell-command full-cmd)))

(defun execute-command-on-file-directory (cmd)
  (interactive "sCommand to execute: ")
  (let* ((dir-name (file-name-directory (buffer-file-name)))
         (full-cmd (concat "cd " dir-name "; " cmd)))
    (shell-command full-cmd)))

(global-set-key (kbd "A-1") 'execute-command-on-file-buffer)
(global-set-key (kbd "A-!") 'execute-command-on-file-directory)

;; Technical Artifacts

;;   Make sure that we can simply =require= this library.


(provide 'init-eshell)
