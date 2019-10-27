
;; ―――――――――――――――――――――――――――――――――――――――― *ORG* ――――――――――――――――――――――――――――――――――――――
(use-package org
  :config
  ;; Enable spell check in org
  (add-hook 'org-mode-hook 'turn-on-flyspell)

  (setq-default
   org-list-demote-modify-bullet '(("+" . "-") ("-" . "+"))
   ;; Hide leading stars
   org-hide-leading-stars t

   ;; Enable source code highlighting in org-mode.
   org-src-fontify-natively t

   ;; '!' after the hotkey tells org-mode to add a LOGBOOK entry for every
   ;; status change.
   org-todo-keywords '((sequence
                        "TODO(t!)" "WORKING(w!)" "WAITING(W!)" "NEXT(n!)"
                        "|" "DONE(d@!)"))

   ;; Use logbook
   org-log-into-drawer t

   ;; Use clocking
   org-clock-into-drawer "CLOCKING"

   ;; Add 'closed' log when marked done
   org-log-done t

   org-modules '(org-bbdb
                 org-bibtex
                 org-docview
                 org-gnus
                 org-habit
                 org-info
                 org-irc
                 org-mhe
                 org-rmail
                 org-w3m)

   ;; C-{a,e} should behave differently on headings
   org-special-ctrl-a/e t

   org-todo-keyword-faces
   '(("TODO" :foreground "red" :weight bold)
     ("WORKING" :foreground "#a45bad" :weight bold)
     ("WAITING" :foreground "#9f8766" :weight bold)
     ("NEXT" :foreground "cyan1" :weight bold)
     ("DONE" :foreground "#2d9574" :weight bold))

   org-agenda-custom-commands
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
      nil nil))

   ;; Capture directories
   org-personal-directory "~/workspace/repository-of-things/personal"
   org-work-directory "~/workspace/repository-of-things/work"

   ;; Capture files
   org-reading-list-file (concat org-personal-directory "/reading-list.org")
   org-oncall-file (concat org-work-directory "/oncall.org")
   org-meeting-notes-file (concat org-work-directory "/meeting-notes.org")
   org-hscore-file (concat org-work-directory "/hscore.org")
   org-personal-todo-file (concat org-personal-directory "/todo.org")
   org-habits-file (concat org-personal-directory "/habits.org")
   org-til-file (concat org-personal-directory "/til.org")

   org-capture-templates
   '(("r" "Reading list item" entry (file org-reading-list-file)
      "* TODO %^{Description}\n  %?\n  :LOGBOOK:\n  - Added: %U\n  :END:")
     ("o" "Oncall ticket" entry (file org-oncall-file)
      "* TODO %^{Type|ONCALL|CSR}-%^{Ticket number} - %^{Description}
  :PROPERTIES:
  :LINK:     https://helpshift.atlassian.net/browse/%\\1-%\\2
  :END:
  :LOGBOOK:\n  - Added - %U\n  :END:" :prepend t)
     ("h" "HSCore task" entry (file org-hscore-file)
      "* TODO %^{Type|HSC}-%^{Ticket number} - %^{Description}
  :PROPERTIES:
  :LINK:     https://helpshift.atlassian.net/browse/%\\1-%\\2
  :END:
  :LOGBOOK:\n  - Added - %U\n  :END:" :prepend t)
     ("m" "Meeting notes" entry (file org-meeting-notes-file)
      "* %^{Agenda}\n  - Attendees: %^{Attendees}, Suvrat
  - Date: %U\n  - Notes:\n    + %?\n  - Action items [/]\n    + [ ] ")
     ("p" "Personal todo item" entry (file org-personal-todo-file)
      "* TODO %^{Description}%?\n  :LOGBOOK:\n  - Added: %U\n  :END:")
     ("t" "Today I learnt" entry (file org-til-file)
      "* %^{Description}\n  - Source: %?\n  -"))

   org-agenda-files (list org-oncall-file
                          ;; Excluding the reading list file since it has many TODOs.
                          ;; org-reading-list-file
                          org-meeting-notes-file
                          org-hscore-file
                          org-personal-todo-file
                          org-habits-file)

   ;; Do not show clock in the modeline. It hides other important things.
   org-clock-clocked-in-display 'frame-title)

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

  :bind (:map
         global-map
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c g" . org-clock-goto)
         :map
         org-mode-map
         ("C-M-g" . org-move-item-or-tree)))


(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (setq org-bullets-bullet-list '("♕" "♖" "♗" "♘" "♙")))
