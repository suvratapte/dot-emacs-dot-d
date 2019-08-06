
;; ―――――――――――――――――――――――――――――――――――――――― *ORG* ――――――――――――――――――――――――――――――――――――――
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
          ("WORKING" :foreground "#a45bad" :weight bold)
          ("PAUSED" :foreground "SlateBlue1" :weight bold)
          ("BLOCKED" :foreground "pink1" :weight bold)
          ("NEXT" :foreground "cyan1" :weight bold)
          ("DONE" :foreground "#2d9574" :weight bold)
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


(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode)
  (setq org-bullets-bullet-list '("♕" "♖" "♗" "♘" "♙")))
