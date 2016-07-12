;;; build -- Set up all the dot-files for updating system

(require 'org)         ;; The org-mode goodness
(require 'ob)          ;; org-mode export system
(require 'ob-tangle)   ;; org-mode tangling process

(defun ha/tangle-file (file)
  "Given an 'org-mode' FILE, tangle the source code."
  (interactive "fOrg File: ")
  (find-file file)   ;;  (expand-file-name file \"$DIR\")
  (org-babel-tangle)
  (kill-buffer))

;; (ha/tangle-file "/home/hoarf/tp-src/dot-files/emacs-ruby.org")

(load-file "./.emacs.d/elisp/init-fixes.el")
(load-file "./.emacs.d/elisp/init-f2.el")
(load-file "./.emacs.d/elisp/init-support.el")
(load-file "./.emacs.d/elisp/init-main.el")
(load-file "./.emacs.d/elisp/init-org-mode.el")
(load-file "./.emacs.d/elisp/init-browser.el")
(load-file "./.emacs.d/elisp/init-eshell.el")
(load-file "./.emacs.d/elisp/init-client.el")
(load-file "./.emacs.d/elisp/init-ruby.el")
(load-file "./.emacs.d/elisp/init-python.el")
(load-file "./.emacs.d/elisp/init-linux.el")
(load-file "./.emacs.d/elisp/init-mode-line.el")
