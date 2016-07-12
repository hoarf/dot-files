;; Ruby Mode

;;   While the Ruby mode is supplied with Emacs, it needs to be
;;   associated with a few other file type extensions:


(use-package ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :mode "Berksfile\\'"
  :mode "Vagrantfile\\'"
  :interpreter "ruby"

  :init
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)
  (add-hook 'ruby-mode 'superword-mode)

  :config
  (bind-key "M-<down>" 'ruby-forward-sexp)
  (bind-key "M-<up>" 'ruby-backward-sexp)
  ;; Don't care to have to start up Ruby if I'm just jumping around
  (bind-key "M-." 'find-tag)
  (bind-key "C-c C-e" 'ruby-send-region))  ;; Rebind since Rubocop uses C-c C-r



;; Being able to select code using expand-region, and then sending it
;; to the Ruby REPL is often useful. But what does an /s-expression/
;; mean in Ruby?

;; Other keystrokes to remember:

;; - =C-M-p= / =C-M-n= :: Move to the beginning and end of a block
;; - =C-M-a= / =C-M-e= :: Move to the beginning and end of a function

;; Use [[http://web-mode.org/][web-mode]] for dealing with ERB templates:


(use-package web-mode
  :ensure t
  :mode "\\.erb\\'")

;; Ruby Virtual Manager

;;   Using [[https://github.com/senny/rvm.el][RVM integration]] for Emacs:


(use-package rvm
  :ensure t
  :config
  (rvm-use-default))

;; Ruby REPL

;;   I am not sure I can learn a new language without a REPL connected to
;;   my editor, and for Ruby, this is [[https://github.com/nonsequitur/inf-ruby][inf-ruby]]:


(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

;; Smart Parens

;;   Can I get the same wonder from *paredit* and Lisp in my Ruby using
;;   [[https://github.com/Fuco1/smartparens][smartparens]]? Not really, as it isn’t as pedantic as
;;   =paredit=. Still, it may be good enough for Ruby:


(use-package smartparens
  :ensure t
  :init
    (add-hook 'ruby-mode-hook 'smartparens-strict-mode)
  :diminish smartparens-mode)

;; Rubocop

;;   The lint-like style checker of choice for Ruby is [[https://github.com/bbatsov/rubocop][Rubocop]].
;;   The [[https://github.com/bbatsov/rubocop-emacs][rubocop.el]] mode should just work with [[https://github.com/flycheck/flycheck][Flycheck]].


(use-package rubocop
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'rubocop-mode)
  :diminish rubocop-mode)

;; Food Critic

;;    Let's get [[http://www.foodcritic.io/][Foodcritic]] working with Flycheck, but only if the file
;;    is located in a =recipes= or =cookbooks= directory:


(use-package flycheck
  :no-require t
  :config
  (flycheck-define-checker chef-foodcritic
    "A Chef cookbooks syntax checker using Foodcritic.
See URL `http://acrmp.github.io/foodcritic/'."
    :command ("foodcritic" source)
    :error-patterns
    ((error line-start (message) ": " (file-name) ":" line line-end))
    :modes (enh-ruby-mode ruby-mode)
    :predicate
    (lambda ()
      (let ((parent-dir (file-name-directory (buffer-file-name))))
        (or
         ;; Chef CookBook
         ;; http://docs.opscode.com/chef/knife.html#id38
         (locate-dominating-file parent-dir "recipes")
         ;; Knife Solo
         ;; http://matschaffer.github.io/knife-solo/#label-Init+command
         (locate-dominating-file parent-dir "cookbooks"))))
    :next-checkers ((warnings-only . ruby-rubocop))))

;; Ruby Tools

;;   The little refactoring available with [[https://github.com/rejeep/ruby-tools.el][Ruby Tools]] looks interesting.


(use-package ruby-tools
  :ensure t
  :init
  (add-hook 'ruby-mode-hook 'ruby-tools-mode)
  :diminish ruby-tools-mode)

;; Technical Artifacts

;;   Make sure that we can simply =require= this library.


(provide 'init-ruby)
