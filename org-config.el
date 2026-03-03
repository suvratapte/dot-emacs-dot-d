;;; org-config.el --- My Org mode configuration.
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

;; My org mode setup is not generic yet. So I have added checks to make it
;; execute only if it's my computer.


;; ─────────────────────────────────────── *ORG* ──────────────────────────────────────
(use-package org
  :if (or (equal user-login-name "suvratapte")
          (equal user-login-name "suvrat"))
  :config

  ;; Enable spell check in org
  (add-hook 'org-mode-hook 'turn-on-flyspell)

  ;; Custom directory and file path variables
  (defvar org-personal-directory nil "Personal org directory.")
  (defvar org-work-directory nil "Work org directory.")
  (defvar org-personal-reading-list-file nil "Personal reading list file.")
  (defvar org-work-reading-list-file nil "Work reading list file.")
  (defvar org-oncall-file nil "Oncall tickets file.")
  (defvar org-work-meeting-notes-file nil "Work meeting notes file.")
  (defvar org-personal-meeting-notes-file nil "Personal meeting notes file.")
  (defvar org-work-todo-file nil "Work todo file.")
  (defvar org-personal-todo-file nil "Personal todo file.")
  (defvar org-til-file nil "Today I learnt file.")
  (defvar org-todo-keyword-faces-spacemacs-theme nil "TODO faces for spacemacs theme.")
  (defvar org-todo-keyword-faces-nord-theme nil "TODO faces for nord theme.")

  (setq
   org-list-demote-modify-bullet '(("+" . "-") ("-" . "+"))

   ;; Hide leading stars
   org-hide-leading-stars t

   ;; Enable source code highlighting in org-mode.
   org-src-fontify-natively t

   ;; '!' after the hotkey tells org-mode to add a LOGBOOK entry for every
   ;; status change.
   org-todo-keywords '((sequence
                        "TODO(t)" "WORKING(w)" "WAITING(W)"
                        "|" "DONE(d!)"))

   ;; Use logbook
   org-log-into-drawer t

   ;; Use clocking
   org-clock-into-drawer "CLOCKING"

   ;; Add 'closed' log when marked done
   org-log-done t

   ;; New lines (etc) should start with proper indentation
   org-adapt-indentation t

   org-modules '(org-bbdb
                 org-bibtex
                 org-docview
                 org-gnus
                 org-habit
                 org-info
                 org-irc
                 org-mhe
                 org-rmail
                 org-w3m
                 ;; After version 9.2, "<{char} TAB" expansion has been moved to
                 ;; `org-tempo`.
                 org-tempo)

   ;; C-{a,e} should behave differently on headings
   org-special-ctrl-a/e t

   org-todo-keyword-faces-spacemacs-theme
   '(("TODO" :foreground "red" :weight bold)
     ("WORKING" :foreground "#a45bad" :weight bold)
     ("WAITING" :foreground "cyan1" :weight bold)
     ("DONE" :foreground "#2d9574" :weight bold))

   org-todo-keyword-faces-nord-theme
   '(("TODO" :foreground "#bf616a" :weight bold)
     ("WORKING" :foreground "#b48ead" :weight bold)
     ("WAITING" :foreground "#ebcb8b" :weight bold)
     ("DONE" :foreground "#a3be8c" :weight bold))

   org-todo-keyword-faces org-todo-keyword-faces-nord-theme

   org-hide-emphasis-markers t

   org-ellipsis " ▾"

   org-pretty-entities t

   org-fontify-quote-and-verse-blocks t

   org-enforce-todo-dependencies t

   org-enforce-todo-checkbox-dependencies t

   ;; When dependencies are enforced (by both vars above), agenda views highlight tasks
   ;; highlights blocked tasks i.e. tasks with incomplete sub tasks.
   org-agenda-dim-blocked-tasks nil

   org-habit-show-habits-only-for-today t

   org-agenda-skip-scheduled-if-done t

   org-agenda-skip-scheduled-if-deadline-is-shown t

   org-agenda-skip-deadline-if-done t

   org-agenda-custom-commands
   '(("i" "My Agenda"
      ((agenda ""
               ((org-agenda-overriding-header "Agenda\n")
                (org-agenda-span 3)))

       ;; (tags-todo "STYLE=\"habit\""
       ;;            ((org-agenda-files (list org-habits-file))
       ;;             (org-agenda-overriding-header "Habits\n")))
       )
      nil nil))

   org-agenda-block-separator
   (propertize
    "────────────────────────────────────────────────────────────────────────\n"
    'face '(:foreground "#81a1c1"))

   ;; Capture directories
   org-directory "~/work/org"
   org-personal-directory (concat org-directory "/personal")
   org-work-directory (concat org-directory "/artis")

   ;; Capture files
   org-personal-reading-list-file (concat org-personal-directory "/reading-list.org")
   org-work-reading-list-file (concat org-work-directory "/reading-list.org")
   org-oncall-file (concat org-work-directory "/oncall.org")
   org-work-meeting-notes-file (concat org-work-directory "/meeting-notes.org")
   org-personal-meeting-notes-file (concat org-personal-directory "/meeting-notes.org")
   org-work-todo-file (concat org-work-directory "/todo.org")
   org-personal-todo-file (concat org-personal-directory "/todo.org")
   org-til-file (concat org-personal-directory "/til.org")

   org-capture-templates
   '(("t" "Work todo item" entry (file org-work-todo-file)
      "* TODO %^{Description}%?\n  :LOGBOOK:\n  - Added: %U\n  :END:")
     ("T" "Work todo item" entry (file org-personal-todo-file)
      "* TODO %^{Description}%?\n  :LOGBOOK:\n  - Added: %U\n  :END:")
     ("m" "Work meeting notes" entry (file org-work-meeting-notes-file)
      "* TODO %^{Agenda}\n  - Attendees: %^{Attendees}, Suvrat
  - Date: %U\n  - Notes:\n    + %?\n  - Action items [/]\n    + [ ] ")
     ("M" "Personal meeting notes" entry (file org-personal-meeting-notes-file)
      "* TODO %^{Agenda}\n  - Attendees: %^{Attendees}, Suvrat
  - Date: %U\n  - Notes:\n    + %?\n  - Action items [/]\n    + [ ] ")
     ("r" "Work reading list item" entry (file org-work-reading-list-file)
      "* TODO %^{Description}\n  :LOGBOOK:\n  - Added: %U\n  :END:\n  %x")
     ("R" "Personal reading list item" entry (file org-personal-reading-list-file)
      "* TODO %^{Description}\n  :LOGBOOK:\n  - Added: %U\n  :END:\n  %?")
     ("o" "Oncall ticket" entry (file org-oncall-file)
      "* TODO %^{Description}
  :PROPERTIES:
  :END:
  :LOGBOOK:\n  - Added - %U\n  :END:" :prepend t)

     ("l" "Today I learnt" entry (file org-til-file)
      "* %^{Description}\n  - Source: %?\n  -"))

   org-agenda-files (list org-oncall-file
                          ;; Excluding the reading list file since it has many TODOs.
                          ;; org-reading-list-file
                          org-work-todo-file
                          org-work-meeting-notes-file
                          org-work-reading-list-file
                          org-personal-meeting-notes-file
                          org-personal-todo-file
                          )

   ;; Do not show clock in the modeline. It hides other important things.
   org-clock-clocked-in-display 'frame-title)

  ;;;; ---------- Headline scaling ----------
  (set-face-attribute 'org-level-1 nil :weight 'bold :height 1.25)
  (set-face-attribute 'org-level-2 nil :weight 'bold :height 1.15)
  (set-face-attribute 'org-level-3 nil :weight 'semi-bold :height 1.08)
  (set-face-attribute 'org-level-4 nil :weight 'semi-bold)
  (set-face-attribute 'org-level-5 nil :weight 'normal)

  ;;;; ---------- Code blocks (nord0) ----------
  (set-face-attribute 'org-block nil :background "#2E3440" :extend t)
  (set-face-attribute 'org-block-begin-line nil :background "#2E3440" :foreground "#81A1C1" :extend t)
  (set-face-attribute 'org-block-end-line nil :background "#2E3440" :foreground "#81A1C1" :extend t)
  (set-face-attribute 'org-code nil :foreground "#88C0D0")
  (set-face-attribute 'org-verbatim nil :foreground "#88C0D0")

  ;;;; ---------- TODO / DONE ----------
  (set-face-attribute 'org-todo nil :weight 'bold :foreground "#BF616A")
  (set-face-attribute 'org-done nil :weight 'bold :foreground "#A3BE8C")

  ;;;; ---------- Quotes & tables ----------
  (set-face-attribute 'org-quote nil :inherit 'org-block :slant 'italic)
  (set-face-attribute 'org-table nil :foreground "#81A1C1")

  ;;;; ---------- Optional niceties ----------
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'org-indent-mode)

  (defun custom-agenda-view ()
    "Show my custom agenda view."
    (interactive)
    (org-agenda nil "i")
    (org-fit-window-to-buffer)
    (org-agenda-redo))

  (defun jump-to-org-agenda ()
    "Jump to the agenda buffer.
     Credits: John Wigley."
    (interactive)
    (when (not (minibufferp))
      (let ((buf (get-buffer "*Org Agenda*"))
            wind)
        (if buf
            (if (setq wind (get-buffer-window buf))
                (select-window wind)
              (if (called-interactively-p 'interactive)
                  (progn
                    (select-window (display-buffer buf t t))
                    (org-fit-window-to-buffer)
                    (org-agenda-redo))
                (with-selected-window (display-buffer buf)
                  (org-fit-window-to-buffer)
                  (org-agenda-redo))))
          (custom-agenda-view)))))

  ;; Temporarily disabled. It is not proving out to be useful enough.
  ;; (run-with-idle-timer 300 t 'jump-to-org-agenda)

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

  ;; Copied from:
  ;; https://emacs.stackexchange.com/questions/13360/org-habit-graph-on-todo-list-agenda-view
  (defvar my/org-habit-show-graphs-everywhere t
    "If non-nil, show habit graphs in all types of agenda buffers.

Normally, habits display consistency graphs only in
\"agenda\"-type agenda buffers, not in other types of agenda
buffers.  Set this variable to any non-nil variable to show
consistency graphs in all Org mode agendas.")

  (defun my/org-agenda-mark-habits ()
    "Mark all habits in current agenda for graph display.

This function enforces `my/org-habit-show-graphs-everywhere' by
marking all habits in the current agenda as such.  When run just
before `org-agenda-finalize' (such as by advice; unfortunately,
`org-agenda-finalize-hook' is run too late), this has the effect
of displaying consistency graphs for these habits.

When `my/org-habit-show-graphs-everywhere' is nil, this function
has no effect."
    (when (and my/org-habit-show-graphs-everywhere
               (not (get-text-property (point) 'org-series)))
      (let ((cursor (point))
            item data)
        (while (setq cursor (next-single-property-change cursor 'org-marker))
          (setq item (get-text-property cursor 'org-marker))
          (when (and item (org-is-habit-p item))
            (with-current-buffer (marker-buffer item)
              (setq data (org-habit-parse-todo item)))
            (put-text-property cursor
                               (next-single-property-change cursor 'org-marker)
                               'org-habit-p data))))))

  (advice-add #'org-agenda-finalize :before #'my/org-agenda-mark-habits)

  ;; org clock in and out should trigger state changes automatically.
  ;; This is extremely useful! :)
  ;; TODO: Figure out how this works.

  (defun my/org-clock-in-set-working (&rest _)
    "Set this task's status to 'WORKING'."
    (org-todo "WORKING"))
  (advice-add #'org-clock-in :after #'my/org-clock-in-set-working)

  (defun my/org-clock-out-set-waiting (&rest _)
    "Set this task's status to 'WAITING'."
    (org-todo "WAITING"))
  (advice-add #'org-clock-out :after #'my/org-clock-out-set-waiting)

  :bind (:map
         global-map
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-stored-links)
         ("C-c g" . org-clock-goto)
         ("C-c o" . jump-to-org-agenda)
         ("C-c b" . org-switchb)
         :map
         org-mode-map
         ("C-c M-." . org-timestamp-inactive)
         ("C-M-g" . org-move-item-or-tree)
         ("H-i" . org-clock-in)
         ("H-o" . org-clock-out)
         ("H-p" . org-set-property))
  :delight)


(use-package org-bullets
  :if (or (equal user-login-name "suvratapte")
          (equal user-login-name "suvrat"))
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (setq org-bullets-bullet-list '("♕" "♖" "♗" "♘" "♙"))
  :delight)


(use-package org-super-agenda
  :if (or (equal user-login-name "suvratapte")
          (equal user-login-name "suvrat"))
  :ensure t
  :config
  ;; Configure this.
  (setq org-super-agenda-groups
        '((:name "Today"
                 :scheduled today
                 :deadline today)
          (:name "Overdue"
                 :deadline past)
          (:name "Personal"
                 :and (:tag "personal" :not (:tag "long_running")))
          (:name "Personal - Long Running"
                 :and (:tag "personal" :tag "long_running"))
          (:name "Work"
                 :and (:tag "work" :not (:tag "long_running")))
          (:name "Work - Long Running"
                 :and (:tag "work" :tag "long_running"))
          (:name "Habits - Today"
                 :tag "habit")))
  (org-super-agenda-mode t))


(use-package org-roam
  :ensure t
  :disabled t
  :config
  (setq org-roam-directory (concat org-personal-directory "/org-roam"))
  (setq org-roam-completion-everywhere t)

  (org-roam-db-autosync-enable)

  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)))


(use-package org-roam-ui
  :ensure t
  :disabled t)


(provide 'org-config)

;;; org-config.el ends here
