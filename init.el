;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)

;; Add melpa to package archives
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Don't use the compiled code if its the older package
(setq load-prefer-newer t)

;; Install 'use-package' if it is not installed.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("eea01f540a0f3bc7c755410ea146943688c4e29bea74a29568635670ab22f9bc" default)))
 '(global-auto-revert-mode t)
 '(ido-vertical-mode t)
 '(package-selected-packages
   (quote
    (multiple-cursors git-gutter git-timemachine hippie-expand ido-completing-read+ use-package aggressive-indent counsel swiper ivy ido-vertical-mode ace-jump-mode company color-theme-monokai monokai-alt-theme cider clojure-mode color-identifiers-mode tagedit smex rainbow-delimiters queue projectile paredit magit exec-path-from-shell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Package configuration with 'use-package'
(require 'use-package)


(eval-and-compile
  (add-to-list 'use-package-keywords :doc t)
  (defun use-package-handler/:doc (name-symbol _keyword _docstring rest state)
    "An identity handler for :doc.
     Currently, the value for this keyword is being ignored.
     This is done just to pass the compilation when :doc is included

     Argument NAME-SYMBOL is the first argument to `use-package' in a declaration.
     Argument KEYWORD here is simply :doc.
     Argument DOCSTRING is the value supplied for :doc keyword.
     Argument REST is the list of rest of the  keywords.
     Argument STATE is maintained by `use-package' as it processes symbols."

    ;; just process the next keywords
    (use-package-process-keywords name-symbol rest state)))


(use-package paredit
  :doc "Better handling of paranthesis when writing Lisp"
  :ensure t
  :diminish paredit-mode
  :init
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode))


(use-package clojure-mode
  :doc "A major mode for editing Clojure code"
  :ensure t
  :init
  ;; This is useful for working with camel-case tokens, like names of
  ;; Java classes (e.g. JavaClassName)
  (add-hook 'clojure-mode-hook #'subword-mode))

(use-package clojure-mode-extra-font-locking
  :doc "Extra syntax highlighting for clojure"
  :ensure t)

(use-package cider
  :doc "Integration with a Clojure REPL cider"
  :ensure t
  :init
  ;; Enable minibuffer documentation
  (add-hook 'cider-mode-hook 'eldoc-mode)
  :config
  ;; Go right to the REPL buffer when it's finished connecting
  (setq cider-repl-pop-to-buffer-on-connect t)
  ;; When there's a cider error, show its buffer and switch to it
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  ;; Where to store the cider history.
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  ;; Wrap when navigating history.
  (setq cider-repl-wrap-history t)
  ;; Attempt to jump at the symbol under the point without having to press RET
  (setq cider-prompt-for-symbol nil))


(use-package ido-completing-read+
  :doc "Allow ido usage in as many contexts as possible"
  :ensure t
  :config
  ;; This enables ido in all contexts where it could be useful, not just
  ;; for selecting buffer and file names
  (ido-mode 1)
  (ido-everywhere 1))

(use-package smex
  :doc "Enhance M-x to allow easier execution of commands"
  :ensure t
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  :bind ("M-x" . smex))

(use-package projectile
  :doc "Project navigation"
  :ensure t
  :config
  ;; Use it everywhere
  (projectile-global-mode t))

(use-package rainbow-delimiters
  :doc "Colorful paranthesis matching"
  :ensure t
  :config
  (rainbow-delimiters-mode t))

(use-package magit
  :doc "Git integration for Emacs"
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package ace-jump-mode
  :doc "Jump around the visible buffer using 'Head Chars'"
  :ensure t
  :bind ("C-M-;" . ace-jump-mode))

(use-package which-key
  :doc "Prompt the next possible key bindings after a short wait"
  :ensure t
  :config
  (which-key-mode t))

(use-package monokai-alt-theme
  :doc "Just another theme"
  :ensure t
  :config
  (load-theme 'monokai-alt t)
  ;; The cursor color in this theme is very confusing.
  ;; Change it to red.
  (set-cursor-color "#ff0000"))

(use-package ido-vertical-mode
  :doc "Show ido vertically"
  :ensure t
  :config
  (ido-vertical-mode t))

(use-package ivy
  :doc "A generic completion mechanism"
  :ensure t
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t))

(use-package swiper
  :doc "A better search"
  :ensure t
  :bind ("C-s" . swiper))

(use-package counsel
  :doc "Ivy enhanced Emacs commands"
  :ensure t)

(use-package aggressive-indent
  :doc "Always keep everything indented"
  :ensure t
  :config
  (global-aggressive-indent-mode t))

(use-package git-timemachine
  :doc "Go through git history in a file"
  :ensure t)

(use-package multiple-cursors
  :doc "A minor mode for editing with multiple cursors"
  :ensure t
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c >" . mc/mark-all-like-this))
  :config
  (setq mc/always-run-for-all t))

(use-package esup
  :doc "Emacs Start Up Profiler (esup) benchmarks Emacs
        startup time without leaving Emacs."
  :ensure t)

(if (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :doc "MacOS does not start a shell at login.
            This makes sure that the env variable
            of shell and GUI Emacs look the same."
      :ensure t
      :config
      (when (memq window-system '(mac ns))
        (exec-path-from-shell-initialize)
        (exec-path-from-shell-copy-envs
         '("PATH")))))
