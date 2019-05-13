;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; Put `customize' code in
(setq custom-file "~/.emacs.d/custom-file.el")

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

;; Turn off unecessary UI elements
(menu-bar-mode -1)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Set fotn size
(set-face-attribute 'default nil :height 140)

;; Use the 'Source Code Pro' font if available
(when (not (eq system-type 'windows-nt))
  (when (member "Source Code Pro" (font-family-list))
    (set-frame-font "Source Code Pro")))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
 x-select-enable-clipboard t

 ;; I'm actually not sure what this does but it's recommended?
 x-select-enable-primary t

 ;; Save clipboard strings into kill ring before replacing them. When
 ;; one selects something in another program to paste it into Emacs, but
 ;; kills something in Emacs before actually pasting it, this selection
 ;; is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t

 ;; Shows all options when running apropos. For more info,
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
 apropos-do-all t

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)

;; Set column width and display column # in mode line
(setq-default fill-column 90
              column-number-mode t)

;; Full path in the title bar
(setq-default frame-title-format "Suvrat's Emacs (%f)")

;; Change all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; For autocompletion
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlight current line
(setq global-hl-line-mode 1)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(global-set-key (kbd "C-c i") 'imenu)

;; Use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; Fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;; Make the command key behave as 'meta'
(setq mac-command-modifier 'meta)

(global-set-key (kbd "M-o") 'other-window)

(defun focus (&optional arg var line)
  "Brings the current window in the middle of the screen. Covering 2/3 of the frame width."
  (interactive "P")
  (delete-other-windows)
  (let* ((window-width (window-body-width))
         (focus-window-width (truncate (* window-width (/ 2.0 3.0))))
         (side-padding-width (/ (- window-width focus-window-width) 2)))
    (split-window-right)
    (split-window-right)
    (shrink-window-horizontally (truncate (* (window-body-width) (/ 1.0 3.0))))
    (windmove-right)
    (enlarge-window-horizontally (window-body-width))
    (windmove-right)
    (switch-to-buffer "*blank*")
    (windmove-left)
    (windmove-left)
    (switch-to-buffer "*blank*")
    (windmove-right)))

;; This mode refreshes buffer contents if the corresponding file is
;; changed on the disk
(setq global-auto-revert-mode t)

;; Allow minibuffer commands while in a minibuffer
(setq enable-recursive-minibuffers t)

;; Delete whitespace just when you save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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


(use-package uniquify
  :doc "Naming convention for files with same names"
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package recentf
  :doc "Recent buffers in a new Emacs session"
  :config
  (setq recentf-auto-cleanup 'never
        recentf-max-saved-items 1000
        recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode t))

(use-package ibuffer
  :doc "Better buffer management"
  :bind ("C-x C-b" . ibuffer))

(use-package company
  :doc "COMplete ANYthing"
  :ensure t
  :bind (:map
         global-map
         ("TAB" . company-complete-common-or-cycle)
         :map company-active-map
         ("C-n" . company-select-next-or-abort)
         ("C-p" . company-select-previous-or-abort))
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))

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
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  :config
  (show-paren-mode t)
  :bind (("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly)))

(use-package highlight-symbol
  :doc "Highlight and jump to symbols"
  :ensure t
  :config
  (highlight-symbol-mode t)
  (setq highlight-symbol-idle-delay 0.5)
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev)))

(use-package clojure-mode
  :doc "A major mode for editing Clojure code"
  :ensure t
  :init
  ;; This is useful for working with camel-case tokens, like names of
  ;; Java classes (e.g. JavaClassName)
  (add-hook 'clojure-mode-hook #'subword-mode)

  ;; Show 'ƒ' instead of 'fn' in clojure mode
  (defun prettify-fns ()
    (font-lock-add-keywords
     nil `(("(\\(fn\\)[\[[:space:]]"
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      "ƒ")
                      nil))))))
  (add-hook 'clojure-mode-hook 'prettify-fns)
  (add-hook 'cider-repl-mode-hook 'prettify-fns)

  ;; Show lambda instead of '#' in '#(...)'
  (defun prettify-anonymous-fns ()
    (font-lock-add-keywords
     nil `(("\\(#\\)("
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      ,(make-char 'greek-iso8859-7 107))
                      nil))))))
  (add-hook 'clojure-mode-hook 'prettify-anonymous-fns)
  (add-hook 'cider-repl-mode-hook 'prettify-anonymous-fns)

  ;; Show '∈' instead of '#' in '#{}' (sets)
  (defun prettify-sets ()
    (font-lock-add-keywords
     nil `(("\\(#\\){"
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      "∈")
                      nil))))))
  (add-hook 'clojure-mode-hook 'prettify-sets)
  (add-hook 'cider-repl-mode-hook 'prettify-sets))

(use-package clojure-mode-extra-font-locking
  :doc "Extra syntax highlighting for clojure"
  :ensure t)

(use-package cider
  :doc "Integration with a Clojure REPL cider"
  ;; Cider has reached 0.20.x but clj-refactor only supports upto 0.18.0. Manually
  ;; download cider-0.18.0 by checking out the tag on github and place it in
  ;; `~/.emacs.d/elpa/` so that the `load-path` tag below, works.
  :load-path "~/.emacs.d/elpa/cider-0.18.0"
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
  (setq cider-prompt-for-symbol nil)
  ;; Always pretty print
  (setq cider-repl-use-pretty-printing t)
  ;; Enable logging client-server messaging in *nrepl-messages* buffer
  (setq nrepl-log-messages t)
  :bind (:map
         cider-mode-map
         ("C-c d" . cider-debug-defun-at-point)
         :map
         cider-repl-mode-map
         ("C-c M-o" . cider-repl-clear-buffer)))

(use-package clj-refactor
  :ensure t
  :config
  (defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

;; WIP: A function which will run `cljr-clean-ns' on all modified Clojure files.
;; ⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯
;; (defun clean-all-modified-ns ()
;;   (interactive)
;;   (let ((extension ".clj")
;;         ;; `shell-command-to-string` always contains a "\n" at its end. `butlast` is
;;         ;; used to get rid of the last empty string returned via `split-string`.
;;         (modified-files (butlast
;;                          (split-string (shell-command-to-string "git diff --name-only")
;;                                        "\n"))))
;;     (if (= (length modified-files) 0)
;;         (message "No files have changed.")
;;       (progn
;;         (dolist (file modified-files)
;;           (when (string-equal (substring file (- (length file) (length extension)))
;;                               extension)
;;             (when (not (string-equal (first (last (split-string file "/")))
;;                                      "project.clj"))
;;               (cljr--clean-ns file))))
;;         (message "Namespaces cleaned! :)")))))

(use-package eldoc
  :doc "Easily accessible documentation for Elisp"
  :config
  (global-eldoc-mode t))

(use-package ido-completing-read+
  :doc "Allow ido usage in as many contexts as possible"
  :ensure t
  :config
  ;; This enables ido in all contexts where it could be useful, not just
  ;; for selecting buffer and file names
  (ido-mode t)
  (ido-everywhere t)
  ;; This allows partial matches, e.g. "uzh" will match "Ustad Zakir Hussain"
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  ;; Includes buffer names of recently open files, even if they're not open now
  (setq ido-use-virtual-buffers t))

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
  (projectile-global-mode t)
  :bind ("C-x f" . projectile-find-file))

(use-package rainbow-delimiters
  :doc "Colorful paranthesis matching"
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

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
  ;; Change it to green
  (set-cursor-color "#9ce22e")
  ;; Show (line,column) in mode-line
  (column-number-mode t)
  ;; Customize theme
  (custom-theme-set-faces
   'user ;; `user' refers to user settings applied via Customize.
   '(font-lock-comment-face ((t (:foreground "tan3"))))
   '(font-lock-doc-face ((t (:foreground "tan3"))))
   '(mode-line ((t (:background "#9ce22e" :foreground "black"
                                :box (:line-width 3 :color "#9ce22e") :weight normal))))
   '(mode-line-buffer-id ((t (:foreground "black" :weight bold))))
   '(mode-line-inactive ((t (:background "#9ce22e" :foreground "grey50"
                                         :box (:line-width 3 :color "#9ce22e") :weight normal))))
   '(org-done ((t (:foreground "chartreuse1" :weight bold))))
   '(org-level-1 ((t (:foreground "RoyalBlue1" :weight bold))))
   '(org-tag ((t (:foreground "#9ce22e" :weight bold))))))

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
  (setq ivy-use-virtual-buffers t)
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)))

(use-package swiper
  :doc "A better search"
  :ensure t
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp)
         ("C-M-s" . isearch-forward)
         ("C-M-r" . isearch-backward)))

(use-package counsel
  :doc "Ivy enhanced Emacs commands"
  :ensure t
  :bind (("M-x" . counsel-M-x)))

(use-package aggressive-indent
  :doc "Always keep everything indented"
  :ensure t
  :preface
  ;; Have a way to save without indentation.
  (defun save-without-aggresive-indentation ()
    (interactive)
    (let ((hooks before-save-hook))
      (setq before-save-hook (remove 'aggressive-indent-indent-defun before-save-hook))
      (save-buffer)
      (setq before-save-hook hooks)))

  :config
  (add-hook 'before-save-hook 'aggressive-indent-indent-defun)

  :bind (("C-c s" . save-without-aggresive-indentation)))

(use-package git-gutter
  :doc "Shows modified lines"
  :ensure t
  :config
  (global-git-gutter-mode t)
  (set-face-background 'git-gutter:modified "yellow")
  (set-face-foreground 'git-gutter:added "green")
  (set-face-foreground 'git-gutter:deleted "red")
  :bind (("C-x C-g" . git-gutter)))

(use-package git-timemachine
  :doc "Go through git history in a file"
  :ensure t)

(use-package region-bindings-mode
  :doc "Define bindings only when a region is selected."
  :ensure t
  :config
  (region-bindings-mode-enable))

(use-package multiple-cursors
  :doc "A minor mode for editing with multiple cursors"
  :ensure t
  :config
  (setq mc/always-run-for-all t)
  :bind
  ;; Use multiple cursor bindings only when a region is active
  (:map region-bindings-mode-map
        ("C->" . mc/mark-next-like-this)
        ("C-<" . mc/mark-previous-like-this)
        ("C-c a" . mc/mark-all-like-this)
        ("C-c h" . mc-hide-unmatched-lines-mode)
        ("C-c l" . mc/edit-lines)))

(use-package esup
  :doc "Emacs Start Up Profiler (esup) benchmarks Emacs
        startup time without leaving Emacs."
  :ensure t)

(use-package org
  :config
  ;; Enable spell check in org
  (add-hook 'org-mode-hook 'turn-on-flyspell)

  (setq org-list-demote-modify-bullet
        '(("+" . "-") ("-" . "+")))

  ;; Hide leading stars
  (setq org-hide-leading-stars t)

  ;; Enable source code highlighting in org-mode.
  (setq org-src-fontify-natively t)

  ;; '!' after the hotkey tells org-mode to add a LOGBOOK entry for every
  ;; status change.
  (setq org-todo-keywords
        '((sequence "TODO(t!)" "WORKING(w!)" "PAUSED(p!)" "BLOCKED(b!)" "NEXT(n!)"
                    "|" "DONE(d@!)" "CANCELLED(c!)")))

  ;; Use logbook
  (setq org-log-into-drawer t)

  ;; Use clocking
  (setq org-clock-into-drawer "CLOCKING")

  ;; Add 'closed' log when marked done
  (setq org-log-done t)

  ;; Org modules
  (setq org-modules '(org-bbdb
                      org-bibtex
                      org-docview
                      org-gnus
                      org-habit
                      org-info
                      org-irc
                      org-mhe
                      org-rmail
                      org-w3m))

  (setq org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("WORKING" :foreground "orange" :weight bold)
          ("PAUSED" :foreground "SlateBlue1" :weight bold)
          ("BLOCKED" :foreground "pink1" :weight bold)
          ("NEXT" :foreground "cyan1" :weight bold)
          ("DONE" :foreground "chartreuse1" :weight bold)
          ("CANCELLED" :foreground "yellow" :weight bold)))

  (setq org-agenda-custom-commands
        '(("i" "My Agenda"
           ((agenda ""
                    ((org-agenda-overriding-header "Agenda")
                     (org-agenda-span 2)))
            (todo "WORKING"
                  ((org-agenda-overriding-header "Currently working")))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next tasks")))
            (alltodo ""
                     ((org-agenda-overriding-header "Unscheduled")
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'scheduled)))))
           nil nil)))


  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda)

  ;; Capture directories
  (setq org-personal-directory "~/workspace/repository-of-things/personal"
        org-work-directory "~/workspace/repository-of-things/work")

  ;; Capture files
  (setq org-reading-list-file (concat org-personal-directory "/reading-list.org")
        org-oncall-file (concat org-work-directory "/oncall.org")
        org-meeting-notes-file (concat org-work-directory "/meeting-notes.org")
        org-hscore-file (concat org-work-directory "/hscore.org")
        org-personal-todo-file (concat org-personal-directory "/todo.org")
        org-habits-file (concat org-personal-directory "/habits.org")
        org-til-file (concat org-personal-directory "/til.org"))

  (setq org-capture-templates
        '(("r" "Reading list item" entry (file org-reading-list-file)
           "* TODO %^{Description}\n  %?\n  :LOGBOOK:\n  - Added: %U\n  :END:")
          ("o" "Oncall ticket" entry (file org-oncall-file)
           "* TODO %^{Type|ONCALL|CSR}-%^{Ticket number} - %^{Description}
  :LOGBOOK:\n  - Added - %U\n  :END:
  - Link: https://helpshift.atlassian.net/browse/%\\1-%\\2" :prepend t)
          ("h" "HSCore task" entry (file org-hscore-file)
           "* TODO %^{Type|HSC}-%^{Ticket number} - %^{Description}
  :LOGBOOK:\n  - Added - %U\n  :END:
  - Link: https://helpshift.atlassian.net/browse/%\\1-%\\2" :prepend t)
          ("m" "Meeting notes" entry (file org-meeting-notes-file)
           "* %^{Agenda}\n  - Attendees: %^{Attendees}, Suvrat
  - Date: %U\n  - Notes:\n    + %?\n  - Action items [/]\n    + [ ] ")
          ("p" "Personal todo item" entry (file org-personal-todo-file)
           "* TODO %^{Description}%?\n  :LOGBOOK:\n  - Added: %U\n  :END:")
          ("t" "Today I learnt" entry (file org-til-file)
           "* %^{Description}\n  - Source: %?\n  -")))

  (setq org-agenda-files (list org-oncall-file
                               org-reading-list-file
                               org-meeting-notes-file
                               org-hscore-file
                               org-personal-todo-file
                               org-habits-file))

  (defun org-move-item-or-tree ()
    (interactive)
    (message "Use f, b, n, p to move items with subtrees. %s %s"
             "F, B, N, P to move items without subtrees."
             "C-{f,b,n,p} for point movement.")
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "f") 'org-shiftmetaright)
      (define-key map (kbd "b") 'org-shiftmetaleft)
      (define-key map (kbd "n") 'org-shiftmetadown)
      (define-key map (kbd "p") 'org-shiftmetaup)
      (define-key map (kbd "F") 'org-metaright)
      (define-key map (kbd "B") 'org-metaleft)
      (define-key map (kbd "N") 'org-metadown)
      (define-key map (kbd "P") 'org-metaup)
      (define-key map (kbd "C-f") 'forward-char)
      (define-key map (kbd "C-b") 'backward-char)
      (define-key map (kbd "C-n") 'next-line)
      (define-key map (kbd "C-p") 'previous-line)
      (set-transient-map map t)))

  :bind (:map global-map
        ("C-c g" . org-clock-goto)
        :map org-mode-map
        ("C-M-g" . org-move-item-or-tree)))


(use-package pdf-tools
  :doc "Better pdf viewing"
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("j" . image-next-line)
              ("k" . image-previous-line)))


;; Thanks to - Narendra Joshi (https://gitlab.com/narendraj9/dot-emacs)
(defun upload-region (beg end)
  "Upload the contents of the selected region in current buffer.
   It uses transfer.sh Link to the uploaded file is copied to
   clipboard.  Creates a temp file if the buffer isn't associted
   witha file.  Argument BEG beginning point for region.
   Argument END ending point for region."
  (interactive "r")
  (let* ((buf-file-path (buffer-file-name))
         (file-path (or buf-file-path
                        (create-file-for-buffer)))
         (file-name (file-name-nondirectory file-path))
         (upload-url (format "https://transfer.sh/%s"
                             file-name))
         (url-request-method "PUT")
         (url-request-data (buffer-substring-no-properties beg end))
         (url-callback (lambda (_)
                         (search-forward "\n\n")
                         (let ((url-link (buffer-substring (point)
                                                           (point-max))))
                           (kill-new url-link)
                           (message "Link copied to clipboard: %s"
                                    (s-trim url-link))
                           (kill-buffer (current-buffer))))))
    (url-retrieve upload-url url-callback)))


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
         '("PATH" "ANDROID_HOME" "LEIN_USERNAME" "LEIN_PASSPHRASE"
           "LEIN_JVM_OPTS" "NPM_TOKEN" "LANGUAGE" "LANG" "LC_ALL"
           "MOBY_ENV" "JAVA_8_HOME" "JAVA_7_HOME" "JAVA_HOME" "PS1"
           "NVM_DIR" "GPG_TTY")))))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)


(defun insert-comment-with-description (comment-syntax comment)
  "------------- Inserts a comment like this (till the end of `fill-column' ------------"
  (let* ((comment-length (length comment))
         (comment-syntax ";;")
         (current-column-pos (current-column))
         (space-on-each-side (/ (- fill-column
                                   current-column-pos
                                   comment-length
                                   (length comment-syntax)
                                    ;; Single space on each side of comment
                                   (if (> comment-length 0) 2 0)
                                   ;; Single space after comment syntax sting
                                   1)
                                2)))
    (if (< space-on-each-side 2)
        (message "Comment string is too big to fit in one line")
      (progn
        (insert comment-syntax)
        (insert " ")
        (dotimes (_ space-on-each-side) (insert "-"))
        (when (> comment-length 0) (insert " "))
        (insert comment)
        (when (> comment-length 0) (insert " "))
        (dotimes (_ (if (= (% comment-length 2) 0)  space-on-each-side (- space-on-each-side 1)))
          (insert "-"))))))

(defun clj-insert-comment-with-description ()
  "----------------------- Inserts a Clojure comment like this ----------------------"
  (interactive)
  (insert-comment-with-description ";;" (read-from-minibuffer "Comment: ")))
