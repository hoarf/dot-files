;; Basics

;;   WSGI files are just Python files in disguise, so tell them to use
;;   the Python environment.   Careful with the tabs, my friend.


(use-package python
  :mode ("\\.py\\'" . python-mode)
        ("\\.wsgi$" . python-mode)
  :interpreter ("python" . python-mode)

  :init
  (setq-default indent-tabs-mode nil)

  :config
  (setq python-indent 4)
  (add-hook 'python-mode-hook 'color-identifiers-mode))



;; Once this has been installed, we can enable it:


(use-package elpy
  :ensure t
  :commands elpy-enable
  :init (with-eval-after-load 'python (elpy-enable))

  :config
  (electric-indent-local-mode -1)
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (delete 'elpy-module-flymake elpy-modules)

  (defun ha/elpy-goto-definition ()
    (interactive)
    (condition-case err
      (elpy-goto-definition)
      ('error (find-tag (symbol-name (symbol-at-point))))))

  :bind (:map elpy-mode-map ([remap elpy-goto-definition] . ha/elpy-goto-definition)))

;; Technical Artifacts

;;   Make sure that we can simply =require= this library.


(provide 'init-python)
