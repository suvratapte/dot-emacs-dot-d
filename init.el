;;; init.el --- My Emacs configuration.
;;; Commentary:
;;; Author: Suvrat Apte
;;; Created on: 02 November 2015
;;; Copyright (c) 2021 Suvrat Apte <suvratapte@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want to
;; Public License, Version 2, which is included with this distribution.
;; See the file LICENSE.txt

;;; Code:


;; ────────────────────────────── Set up 'package' ─────────────────────────────
(require 'package)

;; Add melpa to package archives.
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Load and activate emacs packages. Do this first so that the packages are loaded before
;; you start trying to modify them.  This also sets the load path.
;; Called automatically since Emacs 27; keep for older versions.
(when (< emacs-major-version 27)
  (package-initialize))

;; Install 'use-package' if it is not installed (built-in since Emacs 29).
(when (and (< emacs-major-version 29)
           (not (package-installed-p 'use-package)))
  (package-refresh-contents)
  (package-install 'use-package))


;; ────────────────────── Performance (LSP / long-running) ─────────────────────
(setq gc-cons-threshold (* 100 1024 1024)     ; 100 MB -- reduce GC pauses for LSP
      read-process-output-max (* 1024 1024))   ; 1 MB -- faster LSP communication

;; ──────────────────────────── Use better defaults ────────────────────────────
(setq
 ;; Don't use the compiled code if its the older package.
 load-prefer-newer t

 ;; Do not show the startup message.
 inhibit-startup-message t

 ;; Do not put 'customize' config in init.el; give it another file.
 custom-file "~/.emacs.d/custom-file.el"

 ;; Use your name in the frame title. :)
 frame-title-format (format "%s's Emacs" (if (or (equal user-login-name "suvratapte")
                                                 (equal user-login-name "suvrat.apte"))
                                             "Suvrat"
                                           (capitalize user-login-name)))

 ;; Do not create lockfiles.
 create-lockfiles nil

 ;; Emacs can automatically create backup files. This tells Emacs to put all backups in
 ;; ~/.emacs.d/backups. More info:
 ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
 backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))

 ;; Do not autosave.
 auto-save-default nil

 ;; Allow commands to be run on minibuffers.
 enable-recursive-minibuffers t

 ;; Do not ring bell
 ring-bell-function 'ignore)

;; Buffer-local defaults
(setq-default
 ;; 72 is too less for the fontsize that I use.
 fill-column 90

 ;; Don't use hard tabs
 indent-tabs-mode nil)

;; Show (line,column) in mode-line
(column-number-mode t)


;; Delete regions
(cua-selection-mode t)

;; Load `custom-file` manually as we have modified the default path.
(load-file custom-file)

;; Change all yes/no questions to y/n type
(setopt use-short-answers t)

;; Make the command key behave as 'meta'
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-right-command-modifier 'hyper)
  (setq mac-option-modifier 'super))

;; `C-x o' is a 2 step key binding. `M-o' is much easier.
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "H-o") 'other-frame)

;; Set font and a binding to adjust the size globally
(set-face-attribute 'default nil
                    :family "Source Code Pro for Powerline"
                    ;; Font size
                    :height 200
                    :weight 'light
                    :width 'normal)
(global-set-key (kbd "C-M-=") 'global-text-scale-adjust)


;; Unbind `save-buffers-kill-terminal` to avoid accidentally quiting Emacs.
(global-unset-key (kbd "C-x C-c"))

;; Delete whitespace just when a file is saved.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable narrowing commands.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(global-set-key (kbd "H-r") 'narrow-to-region)
(global-set-key (kbd "H-d") 'narrow-to-defun)
(global-set-key (kbd "H-w") 'widen)
(global-set-key (kbd "H-c") 'calendar)

;; Automatically update buffers if file content on the disk has changed.
(global-auto-revert-mode t)


;; ────────────────────── Disable unnecessary UI elements ──────────────────────
(progn

  ;; Do not show tool bar.
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

  ;; Do not show scroll bar.
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  ;; Highlight line on point.
  (global-hl-line-mode t))


;; ──────────────────── Better interaction with X clipboard ────────────────────
(setq
 ;; Makes killing/yanking interact with the clipboard.
 select-enable-clipboard t

 ;; Save clipboard strings into kill ring before replacing them. When
 ;; one selects something in another program to paste it into Emacs, but
 ;; kills something in Emacs before actually pasting it, this selection
 ;; is gone unless this variable is non-nil.
 save-interprogram-paste-before-kill t

 ;; Shows all options when running apropos. For more info,
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html.
 apropos-do-all t

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)


;; ─────────────────── Added functionality (Generic usecases) ──────────────────
(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(defun comment-pretty ()
  "Insert a comment with '─' (C-x 8 RET BOX DRAWINGS LIGHT HORIZONTAL) on each side."
  (interactive)
  (let* ((comment-char "─")
         (comment (read-from-minibuffer "Comment: "))
         (comment-length (length comment))
         (current-column-pos (current-column))
         (space-on-each-side (/ (- fill-column
                                   current-column-pos
                                   comment-length
                                   (length comment-start)
                                   ;; Single space on each side of comment
                                   (if (> comment-length 0) 2 0)
                                   ;; Single space after comment syntax sting
                                   1)
                                2)))
    (if (< space-on-each-side 2)
        (message "Comment string is too big to fit in one line")
      (progn
        (insert comment-start)
        (when (equal comment-start ";")
          (insert comment-start))
        (insert " ")
        (dotimes (_ space-on-each-side) (insert comment-char))
        (when (> comment-length 0) (insert " "))
        (insert comment)
        (when (> comment-length 0) (insert " "))
        (dotimes (_ (if (= (% comment-length 2) 0)
                      (- space-on-each-side 1)
                      space-on-each-side))
          (insert comment-char))))))

(global-set-key (kbd "C-c ;") 'comment-pretty)

;; Thanks to Narendra Joshi.
(defun upload-region (beg end)
  "Upload the contents of the selected region in current buffer.

   It uses transfer.sh Link to the uploaded file is copied to
   clipboard.  Creates a temp file if the buffer isn't associted
   witha file.  Argument BEG beginning point for region.
   Argument END ending point for region."
  (interactive "r")
  (let* ((file-path (or (buffer-file-name) "upload.txt"))
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


;; Start Emacsserver so that emacsclient can be used.
;; I'm not using the server these days so commenting this out.
;; (server-start)

(use-package edit-server
  :ensure t
  :config
  (add-to-list 'load-path "~/.emacs.d/lisp")
  (edit-server-start))



;; ──────────────── Additional packages and their configurations ───────────────
(require 'use-package)

;; Add `:doc' support for use-package so that we can use it like what a doc-strings is for
;; functions.
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


;; ────────────────────────────── Generic packages ─────────────────────────────
(use-package delight
  :ensure t)

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
  (recentf-mode t)
  :delight)

(use-package ibuffer
  :doc "Better buffer management"
  :bind ("C-x C-b" . ibuffer))

(use-package projectile
  :doc "Project navigation"
  :ensure t
  :config
  ;; Use it everywhere
  (projectile-mode t)
  :bind ("C-x f" . projectile-find-file)
  :delight)

(use-package magit
  :doc "Git integration for Emacs"
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package ace-jump-mode
  :doc "Jump around the visible buffer using 'Head Chars'"
  :ensure t
  :bind ("C-." . ace-jump-mode))

(use-package dumb-jump
  :doc "Dumb ag version of M-."
  :ensure t
  :bind ("C-M-." . dumb-jump-go))

(use-package which-key
  :doc "Prompt the next possible key bindings after a short wait"
  :ensure t
  :config
  (which-key-mode t)
  :delight)

(use-package smex
  :doc "Enhance M-x to allow easier execution of commands"
  :ensure t
  ;; Using counsel-M-x for now. But smex is still useful for history of M-x.
  :disabled t
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  :delight)

(use-package ivy
  :doc "A generic completion mechanism"
  :ensure t
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t

        ;; Display index and count both.
        ivy-count-format "(%d/%d) "

        ;; By default, all ivy prompts start with `^'. Disable that.
        ivy-initial-inputs-alist nil)

  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("C-c C-r" . ivy-resume))
  :delight)

(use-package ivy-rich
  :doc "Have additional information in empty space of ivy buffers."
  :disabled t
  :ensure t
  :custom
  (ivy-rich-path-style 'abbreviate)
  :config
  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  (ivy-rich-mode 1)
  :delight)

(use-package ivy-posframe
  :doc "Custom positions for ivy buffers."
  :disabled t
  :ensure t
  :config

  (when (member "Hasklig" (font-family-list))
    (setq ivy-posframe-parameters
          '((font . "Hasklig"))))

  (setq ivy-posframe-border-width 10)

  (setq ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (swiper . ivy-display-function-fallback)
          (swiper-isearch . ivy-display-function-fallback)
          (counsel-rg . ivy-display-function-fallback)
          (t . ivy-posframe-display-at-frame-center)))

  (ivy-posframe-mode t)

  ;; Due to a bug in macOS, changing ivy-posframe-border background color does not
  ;; work. Instead, go to the elisp file and change the background color to black.

  :delight)

(use-package swiper
  :doc "A better search"
  :ensure t
  :bind (("C-s" . swiper-isearch)
         ("H-s" . isearch-forward-regexp)))

(use-package counsel
  :doc "Ivy enhanced Emacs commands"
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-'" . counsel-imenu)
         ("C-c s" . counsel-rg)
         ;; Not using this these days.
         ;; ("M-y" . counsel-yank-pop)
         :map counsel-find-file-map
         ("RET" . ivy-alt-done))
  :config
  (defun my/counsel-rg-hidden ()
    (interactive)
    (let ((counsel-rg-base-command
           "rg -M 120 --with-filename --no-heading --line-number --color never --hidden %s ."))
      (counsel-rg))))

(use-package aggressive-indent
  :doc "Intended Indentation"
  :ensure t
  :config
  ;; (add-hook 'before-save-hook 'aggressive-indent-indent-defun)
  ;; Have a way to save without indentation.
  ;; (defun save-without-aggresive-indentation ()
  ;;   (interactive)
  ;;   (remove-hook 'before-save-hook 'aggressive-indent-indent-defun)
  ;;   (save-buffer)
  ;;   (add-hook 'before-save-hook 'aggressive-indent-indent-defun))
  ;; :bind (("C-x s" . save-without-aggresive-indentation))
  :delight)

(use-package git-gutter
  :disabled t
  :doc "Shows modified lines"
  :ensure t
  :config
  (setq git-gutter:modified-sign "|")
  (setq git-gutter:added-sign "|")
  (setq git-gutter:deleted-sign "|")
  (global-git-gutter-mode t)
  :delight)

(use-package git-timemachine
  :doc "Go through git history in a file"
  :ensure t)

(use-package region-bindings-mode
  :doc "Define bindings only when a region is selected."
  :ensure t
  :config
  (region-bindings-mode-enable)
  :delight)

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
        ("C-c l" . mc/edit-lines))
  :delight)

(use-package esup
  :doc "Emacs Start Up Profiler (esup) benchmarks Emacs
        startup time without leaving Emacs."
  :ensure t)

(use-package pdf-tools
  :doc "Better pdf viewing"
  :disabled t
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("j" . image-next-line)
              ("k" . image-previous-line))
  :delight)

(use-package define-word
  :doc "Dictionary in Emacs."
  :ensure t
  :bind ("C-c w" . define-word-at-point))

(use-package exec-path-from-shell
  :doc "MacOS does not start a shell at login. This makes sure
        that the env variable of shell and GUI Emacs look the
        same."
  :ensure t
  :if (eq system-type 'darwin)
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs
     '("PATH" "ANDROID_HOME" "LEIN_USERNAME" "LEIN_PASSPHRASE"
       "LEIN_JVM_OPTS" "NPM_TOKEN" "LANGUAGE" "LANG" "LC_ALL"
       "MOBY_ENV" "JAVA_8_HOME" "JAVA_7_HOME" "JAVA_HOME" "PS1"
       "NVM_DIR" "GPG_TTY"))))

(use-package darkroom
  :doc "Focused editing."
  :ensure t
  :config
  (setq darkroom-text-scale-increase 1.5)
  :bind ("C-c d" . darkroom-mode)
  :delight)

(use-package flyspell
  :config
  ;; Flyspell should be able to learn a word without the
  ;; `flyspell-correct-word-before-point` pop up.
  ;; Refer:
  ;; https://stackoverflow.com/questions/22107182/in-emacs-flyspell-mode-how-to-add-new-word-to-dictionary
  (defun flyspell-learn-word-at-point ()
    "Add word at point to list of correct words."
    (interactive)
    (let ((current-location (point))
          (word (flyspell-get-word)))
      (when (consp word)
        (flyspell-do-correct 'save nil
                             (car word) current-location
                             (cadr word) (caddr word)
                             current-location))))

  ;; This color is specific to `nord` theme.
  (set-face-attribute 'flyspell-incorrect nil :underline '(:style line :color "#bf616a"))
  (set-face-attribute 'flyspell-duplicate nil :underline '(:style line :color "#bf616a"))

  :bind ("H-l" . flyspell-learn-word-at-point)
  :delight)

(use-package company-emoji
  :ensure t
  :config
  ;; This interfered with Rust associated function auto completions
  ;; after typing `::`.
  ;; (add-to-list 'company-backends 'company-emoji)
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji")
                    nil 'prepend))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package unfill
  :ensure t)


;; ──────────────────────────────── Code editing ───────────────────────────────

(global-display-line-numbers-mode)

(use-package company
  :doc "COMplete ANYthing"
  :ensure t
  :bind (:map
         global-map
         ("TAB" . company-complete-common-or-cycle)
         ;; Use hippie expand as secondary auto complete. It is useful as it is
         ;; 'buffer-content' aware (it uses all buffers for that).
         ("M-/" . hippie-expand)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t)

  (global-company-mode t)

  ;; LSP + company integration via capf
  (add-to-list 'company-backends 'company-capf)

  ;; Configure hippie expand as well.
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))

  :delight)

(use-package paredit
  :doc "Better handling of paranthesis when writing Lisp"
  :ensure t
  :hook ((clojure-mode
          cider-repl-mode
          emacs-lisp-mode
          eval-expression-minibuffer-setup
          ielm-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode
          haskell-mode
          haskell-interactive-mode) . enable-paredit-mode)
  :config
  (show-paren-mode t)

  (with-eval-after-load 'paredit
    (define-key minibuffer-local-map (kbd "RET") #'exit-minibuffer)
    (define-key minibuffer-local-map (kbd "<return>") #'exit-minibuffer))

  :bind (("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly))
  :delight)

(use-package rainbow-delimiters
  :doc "Colorful paranthesis matching"
  :disabled t ;; This is not really needed when you use paredit
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :delight)

(use-package highlight-symbol
  :doc "Highlight and jump to symbols"
  :ensure t
  :config
  (set-face-background 'highlight-symbol-face (face-background 'highlight))
  (setq highlight-symbol-idle-delay 0.5)
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  :bind (("M-n" . highlight-symbol-next)
         ("M-p" . highlight-symbol-prev))
  :delight)

(use-package yasnippet
  :ensure t
  :hook
  ((prog-mode . yas-minor-mode)
   (text-mode . yas-minor-mode))
  :config
  (add-to-list 'hippie-expand-try-functions-list
               'yas-hippie-try-expand)
  (yas-reload-all)
  :delight)


(use-package expand-region
  :doc "Better navigation between nested expressions."
  :ensure t
  :bind ("C-c =" . er/expand-region))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((clojure-mode . lsp)
         (clojurescript-mode . lsp)
         (clojurec-mode . lsp))
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)

  ;; Performance
  (lsp-restart 'auto-restart)

  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)

  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  ;; (lsp-rust-analyzer-display-parameter-hints nil)
  ;; (lsp-rust-analyzer-display-reborrow-hints nil)

  ;; For integration with yasnippet
  (lsp-enable-snippet t)
  (lsp-completion-enable-additional-text-edit t)

  ;; Clojure: let CIDER handle indentation and completion
  (lsp-enable-indentation nil)
  (lsp-enable-completion-at-point nil)
  ;; Avoid duplicate eldoc from LSP when CIDER is also providing it
  (lsp-eldoc-enable-hover nil)

  :config
  (add-to-list 'lsp-disabled-clients 'lsp-javascript)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t))


;; ─────────────────────────── Programming languages ───────────────────────────

(use-package clojure-mode
  :doc "A major mode for editing Clojure code"
  :ensure t
  :config
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

;; clojure-mode-extra-font-locking removed: built into modern clojure-mode.

(use-package cider
  :doc "Integration with a Clojure REPL cider"
  :ensure t

  :init
  ;; Enable minibuffer documentation
  (add-hook 'cider-mode-hook 'eldoc-mode)

  :config

  (setq
   ;; Go right to the REPL buffer when it's finished connecting
   cider-repl-pop-to-buffer-on-connect t

   ;; When there's a cider error, show its buffer and switch to it
   cider-show-error-buffer t
   cider-auto-select-error-buffer t

   ;; Where to store the cider history.
   cider-repl-history-file "~/.emacs.d/cider-history"

   ;; Wrap when navigating history.
   cider-repl-wrap-history t

   ;; Attempt to jump at the symbol under the point without having to press RET
   cider-prompt-for-symbol nil

   ;; Always pretty print
   cider-repl-use-pretty-printing t

   ;; Log client-server messaging in *nrepl-messages* buffer
   nrepl-log-messages nil

   ;; Use xref for M-. navigation (works with clojure-lsp too)
   cider-use-xref t

   ;; Restore thread stop ability on Java 21+
   cider-enable-nrepl-jvmti-agent t)

  (add-to-list 'cider-jack-in-nrepl-middlewares "cider.nrepl/cider-middleware")

  ;; REPL should expect input on the next line + unnecessary palm trees!
  (defun cider-repl-prompt-custom (namespace)
    "Return a prompt string that mentions NAMESPACE."
    (format "🌴 %s 🌴 \n" namespace))
  (setq cider-repl-prompt-function 'cider-repl-prompt-custom)

  :bind (:map
         cider-mode-map
         ("H-t" . cider-test-run-test)
         ("H-n" . cider-test-run-ns-tests)
         :map
         cider-repl-mode-map
         ("C-c M-o" . cider-repl-clear-buffer))
  :delight)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)

  ;; Do not display errors on left fringe.
  (setq flycheck-indication-mode nil)
  :delight)

;; flycheck-joker removed: clj-kondo (bundled in clojure-lsp) supersedes it.

(use-package flycheck-clj-kondo
  :doc "Standalone clj-kondo flycheck integration.
        When clojure-lsp is running, LSP provides diagnostics via its
        bundled clj-kondo. This package serves as fallback for when
        LSP is not connected (e.g. editing a single file)."
  :ensure t
  :after clojure-mode)

;; clj-refactor: disabled -- clojure-lsp handles most refactorings
;; (rename, extract function, clean ns, add missing require, thread/unthread, etc.)
;; via lsp-execute-code-action. Enable clj-refactor only if you need its
;; REPL-dependent features (e.g. hotload dependency).

(use-package eldoc
  :doc "Easily accessible documentation for Elisp"
  :config
  (global-eldoc-mode t)
  :delight)

(use-package python
  :ensure t
  :custom
  (python-indent-offset 4))

(use-package anaconda-mode
  :ensure t
  :diminish anaconda-mode
  :hook python-mode
  :custom (python-indent-offset 4)
  :delight)

(use-package company-anaconda
  :ensure t
  :after (company anaconda-mode)
  :config (add-hook 'python-mode-hook
                    (lambda () (add-to-list 'company-backends 'company-anaconda))))

(use-package haskell-mode
  :ensure t
  :config
  :bind (("C-c M-o" . haskell-interactive-mode-clear)
         ("C-c C-z" . haskell-interactive-switch)))

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)

  (defun my/rustic-mode-setup ()
    ;; so that run C-c C-c C-r works without having to confirm, but don't try to
    ;; save rust buffers that are not file visiting. Once
    ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
    ;; no longer be necessary.
    (when buffer-file-name
      (setq-local buffer-save-without-query t)))

  (add-hook 'rustic-mode-hook 'my/rustic-mode-setup))




;; ─────────────────────────────── Look and feel ───────────────────────────────

(use-package monokai-alt-theme
  :doc "Just another theme"
  :disabled t
  :ensure t
  :config
  (load-theme 'monokai-alt t)
  ;; The cursor color in this theme is very confusing.
  ;; Change it to green
  (set-cursor-color "#9ce22e")
  ;; Customize theme
  (custom-theme-set-faces
   'user ;; `user' refers to user settings applied via Customize.
   '(font-lock-comment-face ((t (:foreground "tan3"))))
   '(font-lock-doc-face ((t (:foreground "tan3"))))
   '(mode-line ((t (:background "#9ce22e"
                                :foreground "black"
                                :box (:line-width 3 :color "#9ce22e")
                                :weight normal))))
   '(mode-line-buffer-id ((t (:foreground "black" :weight bold))))
   '(mode-line-inactive ((t (:background "#9ce22e"
                                         :foreground "grey50"
                                         :box (:line-width 3 :color "#9ce22e")
                                         :weight normal))))
   '(org-done ((t (:foreground "chartreuse1" :weight bold))))
   '(org-level-1 ((t (:foreground "RoyalBlue1" :weight bold))))
   '(org-tag ((t (:foreground "#9ce22e" :weight bold)))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(font-lock-comment-face ((((class color) (min-colors 89))
                              (:foreground "#b2b2b2" :slant italic))))
   '(font-lock-doc-face ((((class color) (min-colors 89))
                          (:foreground "#cc0000"))))
   '(mode-line ((((class color) (min-colors 89))
                 (:box nil :background "#5fafd7" :foreground "#ffffff"))))
   '(mode-line-buffer-id ((((class color) (min-colors 89))
                           (:box nil :foreground "#3a3a3a" :background nil :bold t))))
   '(mode-line-inactive ((((class color) (min-colors 89))
                          (:box nil :background "#dadada" :foreground "#9e9e9e"))))
   '(org-done ((((class color) (min-colors 89))
                (:bold t :weight bold :foreground "#008700" :background "#d7ff87"
                       :box (:line-width 1 :style none)))))
   '(org-level-1 ((((class color) (min-colors 89)) (:bold t :foreground "#5fafd7"))))
   '(org-tag ((((class color) (min-colors 89))
               (:background "#9e9e9e" :foreground "#ffffff" :bold t :weight bold)))))
  :delight)

(use-package ewal-spacemacs-themes
  :disabled t
  :ensure t
  :config
  (setq-default spacemacs-theme-comment-bg nil
                spacemacs-theme-comment-italic t)
  (load-theme 'spacemacs-dark t)
  :delight)

(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord t)

  (set-face-attribute 'flycheck-error nil :underline '(:style line :color "#bf616a"))
  (set-face-attribute 'flycheck-warning nil :underline '(:style line :color "#ebcb8b"))
  (set-face-attribute 'flycheck-info nil :underline '(:style line :color "#b48ead")))

(use-package powerline
  :doc "Better mode line"
  :ensure t
  :config
  (powerline-center-theme))

(use-package fira-code-mode
  :doc "Fira code + ligatures"
  :ensure t
  :disabled t
  :config
  (setq fira-code-mode-disabled-ligatures '("x" "[]"))
  (global-fira-code-mode)
  :delight)

(use-package hasklig-mode
  :hook (find-file after-change-major-mode)
  ;; This ^ is a hack to enable hasklig-mode for all buffers. There is no global
  ;; hasklig mode. :(
  :init
  :disabled t
  (when (member "Hasklig" (font-family-list))
    (set-face-attribute 'default nil :font "Hasklig-15")))

(use-package emojify
  :doc "Display Emoji in Emacs."
  :ensure t
  :disabled t
  :init
  (add-hook 'after-init-hook #'global-emojify-mode)
  :delight)


;; ─────────────────────────────────── *ORG* ───────────────────────────────────
(load-file "~/.emacs.d/org-config.el")

;; Open agenda view when Emacs is started.
;; Do it only if it's Suvrat's computer.
(when (equal user-login-name "suvratapte")
  (jump-to-org-agenda)
  (delete-other-windows))

(provide 'init)

;;; init.el ends here
