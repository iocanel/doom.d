;;; -*- lexical-binding: t; -*-

(defvar my/theme-light 'doom-one-light "The default light theme")
(defvar my/theme-dark 'doom-one "The default dark theme")
(defvar my/theme-selected 'doom-one "The selected theme")

(defun my/toggle-theme ()
  "Toggle between light and dark theme."
  (interactive)
  (if (eq my/theme-selected my/theme-dark)
      (setq my/theme-selected my/theme-light)
    (setq my/theme-selected my/theme-dark))
  (load-theme my/theme-selected))

(defun my/laptop-mode()
  "Modify theme for latpop use."
  (interactive)
  (set-face-attribute 'default nil :height 75)
  (when (boundp 'treemacs-root-face)
    (set-face-attribute 'treemacs-root-face nil :height 90)))

(defun my/desktop-mode()
  "Modify theme for latpop use."
  (interactive)
  (set-face-attribute 'default nil :height 130)
  (when (boundp 'treemacs-root-face)
    (set-face-attribute 'treemacs-root-face nil :height 130)))

(defun my/comf-mode()
  "Modify theme for comfortable use."
  (interactive)
  (set-face-attribute 'default nil :height 150)
  (when (boundp 'treemacs-root-face)
    (set-face-attribute 'treemacs-root-face nil :height 160)))

(defun my/presentation-mode()
  "Modify theme for presentations use."
  (interactive)
  (set-face-attribute 'default nil :height 250)
  (when (boundp 'treemacs-root-face)
    (set-face-attribute 'treemacs-root-face nil :height 260)))

(defun my/asciinema-mode()
  "Modify theme for asciinema use."
  (interactive)
  (setq mode-line-format nil)
  (setq-default mode-line-format nil)
  (setq idee-tree-enabled nil)
  (setq inhibit-message t)
  (setq-default inhibit-message t)
  (set-face-attribute 'default nil :height 150)
  (when (boundp 'treemacs-root-face)
    (set-face-attribute 'treemacs-root-face nil :height 160)))

(defvar my/selected-screen-mode 'my/comf-mode "The current screen mode to use.")
(funcall my/selected-screen-mode)

(display-battery-mode 1)

(map! "C-x C-k" #'kill-current-buffer)
(map! "C-x C-b" #'switch-to-buffer)

(map! "C-s" #'consult-line)
(map! "C-S" #'consult-line-multi)

(use-package! consult-ag
  :config
  (defun my/consult-ag (&optional target initial)
    "Consult ag for query in TARGET file(s) with INITIAL input."
    (interactive)
    (let* ((prompt-dir (consult--directory-prompt "Search: " target))
           (default-directory (cdr prompt-dir)))
      (consult--read (consult--async-command #'consult-ag--builder
                       (consult--async-map #'consult-ag--format))
                     :prompt (car prompt-dir)
                     :lookup #'consult--lookup-member
                     :state (consult-ag--grep-state)
                     :initial (consult--async-split-initial initial)
                     :require-match t
                     :category 'file
                     :sort nil)))

  :bind ("C-c i g" . my/consult-ag))

(setq display-line-numbers-type 'relative)

(map!
 "C-q" #'er/expand-region
 "C-c m m" #'mc/mark-next-like-this
 "C-c m u" #'mc/umark-next-like-this
 "C-c m s" #'mc/skip-next-like-this
 "C-c m e" #'mc/edit-lines)

(map! "M-o" #'evil-window-next)

;;;###autoload
(defun my/next-code-block ()
  "Jump to the next code block."
  (interactive)
  (re-search-forward "^[[:space:]]*\\(#\\+begin_src\\)" nil t)
  (next-line)
  (beginning-of-line))

;;;###autoload
(defun my/previous-code-block ()
  "Jump to the next code block."
  (interactive)
  (re-search-backward "^[[:space:]]*\\(#\\+end_src\\)" nil t)
  (re-search-backward "^[[:space:]]*\\(#\\+begin_src\\)" nil t)
  (next-line)
  (beginning-of-line))

;;;###autoload
(defun my/code-block-p ()
  "Return non-nil if in code block."
  (let* ((previous-end-pos (save-excursion
                             (progn (re-search-backward "^[[:space:]]*\\(#\\+end_src\\)" nil t)
                                    (point))))
         (previous-begin-pos (save-excursion (progn (re-search-backward "^[[:space:]]*\\(#\\+begin_src\\)" nil t)
                                                      (point)))))
     (or (eq previous-end-pos (point)) (> previous-begin-pos previous-end-pos))))

;;;###autoload
(defun my/ensure-in-code-block ()
    "Jump to the next code block if not current not in code block."
    (interactive)
    (when (not (my/code-block-p))
      (my/next-code-block)))

(after! 'org
(org-babel-do-load-languages 'org-babel-load-languages '((shell .t)
                                                           (ruby . t)
                                                           (java . t)
                                                           (typescript . t)
                                                           (plantuml . t))))

(require 'ob-shell)
(use-package org-babel-eval-in-repl
  :custom (eir-shell-type 'vterm)
  :bind (:map org-mode-map
              ("M-e" . ober-eval-block-in-repl)))

(defun my/not-empty (s)
  "Returns non-nil if S is not empty."
  (and s (stringp s) (not (= (length s) 0))))

;; Let's intercept eir-insert to make sure the text entered is trimmed.
(defun my/eir-insert-trimmed (orig string)
  "Eir insert but with trimmed arguments."
  (let ((trimmed (replace-regexp-in-string "^[ \t\n]+" "" (replace-regexp-in-string "[ \n]+$" "" string))))
    (when (my/not-empty trimmed)
      (apply orig (list trimmed)))))

(defun my/eir-send-not-empty-to-repl (orig fun-change-to-repl fun-execute region-string)
  "Eir send to repl but ignore empty commands."
  (when (my/not-empty region-string)
    (apply orig (list fun-change-to-repl fun-execute region-string))))

(advice-add 'eir-insert :around #'my/eir-insert-trimmed)
(advice-add 'eir-send-to-repl :around #'my/eir-send-not-empty-to-repl)

(advice-add 'ober-eval-block-in-repl :before #'my/ensure-in-code-block)
(advice-add 'ober-eval-block-in-repl :after #'my/next-code-block)

(map! :map org-mode-map
      "M-p" #'my/previous-code-block
      "M-n" #'my/next-code-block
      "M-r" #'+eval:open-repl-other-window)

(defun my/org-inline-image--get-current-image ()
  "Return the overlay associated with the image under point."
  (car (-select (lambda (i) (eq (overlay-get i 'org-image-overlay) t)) (overlays-at (point)))))

(defun my/org-inline-image--get (prop)
  "Return the value of property PROP for image under point."
  (let ((image (my/org-inline-image--get-current-image)))
    (when image
      (overlay-get image prop))))

(defun my/org-inline-image-animate ()
  "Animate the image if it's possible."
  (interactive)
  (let ((image-props (my/org-inline-image--get 'display)))
    (when (image-multi-frame-p image-props)
      (image-animate image-props))))

(defun my/org-inline-image-animate-auto ()
  "Automatically animate image inlined in org-mode."
  (interactive)
  (when (eq 'org-mode major-mode)
    (while-no-input
      (run-with-idle-timer 0.3 nil 'my/org-inline-image-animate))))

(setq org-inline-image--get-current-image (byte-compile 'my/org-inline-image--get-current-image))
(setq org-inline-image-animate  (byte-compile 'my/org-inline-image-animate ))

(setq org-capture-templates
      '(
        ("c" "Calendar")
        ("cw" "Work Event" entry (file  "~/Documents/org/calendars/work.org") "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
        ("cp" "Personal Event" entry (file  "~/Documents/org/calendars/personal.org") "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")

        ("i" "Inbox")
        ("iw" "Work Inbox" entry (file+olp "~/Documents/org/gtg/inbox.org" "Inbox" "Work") "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n" :prepend t)
        ("ip" "Personal Inbox" entry (file+olp "~/Documents/org/gtg/inbox.org" "Inbox" "Personal") "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n" :prepend t)

        ("e" "Email Workflow")
        ("ef" "Follow Up" entry (file+olp "~/Documents/org/gtg/inbox.org" "Inbox" "Email" "Follow Up") "* TODO Follow up with %:fromname on %a :email:\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i" :immediate-finish t)
        ("er" "Read Later" entry (file+olp "~/Documents/org/gtg/inbox.org" "Inbox" "Email" "Read Later") "* TODO Read %:subject :email: \nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n\n%i" :immediate-finish t)

        ("p" "Project" entry (file+headline "~/Documents/org/para/projects.org" "Projects")(file "~/Documents/org/templates/project.orgtmpl"))
        ("b" "BJJ")
        ("bm" "Moves" entry (file+olp "~/Documents/org/bjj/BJJ.org" "Moves")(file "~/Documents/org/templates/bjj-move.orgtmpl"))
        ("bs" "Submission" entry (file+olp "~/Documents/org/bjj/BJJ.org" "Techniques" "Submissions")(file "~/Documents/org/templates/bjj-submission.orgtmpl"))
        ("bc" "Choke" entry (file+olp "~/Documents/org/bjj/BJJ.org" "Techniques" "Chokes")(file "~/Documents/org/templates/bjj-choke.orgtmpl"))
        ("bw" "Sweeps" entry (file+olp "~/Documents/org/bjj/BJJ.org" "Techniques" "Sweeps")(file "~/Documents/org/templates/bjj-sweep.orgtmpl"))
        ("be" "Escapes" entry (file+olp "~/Documents/org/bjj/BJJ.org" "Techniques" "Escapes")(file "~/Documents/org/templates/bjj-escape.orgtmpl"))
        ("bt" "Takedowns" entry (file+olp "~/Documents/org/bjj/BJJ.org" "Techniques" "Takedowns")(file "~/Documents/org/templates/bjj-takedown.orgtmpl"))
        ("bp" "Passes" entry (file+olp "~/Documents/org/bjj/BJJ.org" "Techniques" "Passes")(file "~/Documents/org/templates/bjj-pass.orgtmpl"))
        ("bf" "FAQ" entry (file+olp "~/Documents/org/bjj/BJJ.org" "FAQ")(file "~/Documents/org/templates/bjj-faq.orgtmpl"))

        ("h" "Habit" entry (file+olp "~/Documents/org/habits.org" "Habits") (file "~/Documents/org/templates/habit.orgtmpl"))

        ("f" "Flashcards")
        ("fq" "Quotes" entry (file+headline "~/Documents/org/flashcards/quotes.org" "Quotes") "* %?\n%u" :prepend t)
        ("fS" "Stories"  entry (file+headline "~/Documents/org/flashcards/stories.org" "Stories") "* Story :drill:\n %t\n %^{The story}\n")
        ("fe" "Emacs")
        ("fef" "Emacs facts"  entry (file+headline "~/Documents/org/flashcards/emacs.org" "Emacs") "* Fact :drill:\n %t\n %^{The fact}\n")
        ("feq" "Emacs questions"  entry (file+headline "~/Documents/org/flashcards/emacs.org" "Emacs") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}")
        ("fh" "History")
        ("fhf" "History facts"  entry (file+headline "~/Documents/org/flashcards/history.org" "History") "* Fact :drill:\n %t\n %^{The fact}\n")
        ("fhq" "History questions"  entry (file+headline "~/Documents/org/flashcards/history.org" "History") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}")
        ("fm" "Maths")
        ("fmf" "Math facts"  entry (file+headline "~/Documents/org/flashcards/maths.org" "Maths") "* Fact :drill:\n %t\n %^{The fact}\n")
        ("fmq" "Math questions"  entry (file+headline "~/Documents/org/flashcards/maths.org" "Maths") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}")
        ("fc" "Computer Science")
        ("fcf" "Computer Science facts"  entry (file+headline "~/Documents/org/flashcards/computer-science.org" "Computer Science") "* Fact :drill:\n %t\n %^{The fact}\n")
        ("fcq" "Computer Science questions"  entry (file+headline "~/Documents/org/flashcards/computer-science.org" "Computer Science") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}")
        ("fs" "Sports")
        ("fsf" "Sports facts"  entry (file+headline "~/Documents/org/flashcards/sports.org" "Sports") "* Fact :drill:\n %t\n %^{The fact}\n")
        ("fsq" "Sports questions"  entry (file+headline "~/Documents/org/flashcards/sports.org" "Sports") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}")
        ("fn" "Nutrition")
        ("ft" "Trading")
        ("ftf" "Trading facts"  entry (file+headline "~/Documents/org/flashcards/trading.org" "Trading") "* Fact :drill:\n %t\n %^{The fact}\n")
        ("ftq" "Trading questions"  entry (file+headline "~/Documents/org/flashcards/trading.org" "Trading") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}")
        ("fl" "Languages")
        ("fls" "Spanish"  entry (file+headline "~/Documents/org/flashcards/languages/spanish.org" "Spanish") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}")))

(defadvice org-switch-to-buffer-other-window
    (after supress-window-splitting activate)
  "Delete the extra window if we're in a capture frame"
  (if (equal "org-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (when (and (equal "org-capture" (frame-parameter nil 'name))
             (not (eq this-command 'org-capture-refile)))
    (delete-frame)))

(defadvice org-capture-refile
    (after delete-capture-frame activate)
  "Advise org-refile to close the frame"
  (delete-frame))

(setq lsp-keymap-prefix "C-c l")

(map! :map lsp-mode-map
      (:leader
       (:prefix "l"
                :desc "Lsp" "l" #'lsp-mode
                :desc "Lsp Rename" "r" #'lsp-rename
                :desc "Lsp Find Symbol" "s" #'lsp-ui-find-workspace-symbol
                :desc "Lsp Find References" "r" #'lsp-find-references
                :desc "Lsp Find Implementations" "i" #'lsp-find-implementation
                :desc "Lsp Find Declaration" "d" #'lsp-find-declaration)))

(map! :map lsp-mode-map
      (:leader
       (:prefix "d"
                :desc "Dap Hydra" "h" #'dap-hydra
                :desc "Dap Debug" "d" #'dap-debug
                :desc "Dap Debug Last" "l" #'dap-debug-last
                :desc "Dap Debug Restart" "r" #'dap-debug-restart
                :desc "Continue" "c" #'dap-continue
                :desc "Step in" "i" #'dap-step-in
                :desc "Step in" "o" #'dap-step-out
                :desc "Toggle breakpoint" "b" #'dap-breakpoint-toggle
                :desc "Breakpoint condition" "c" #'dap-breakpoint-condition
                :desc "Delete all breakpoints" "D" #'dap-breakpoint-delete-all)))

(setq c-basic-offset 2
      tab-width 2)

(use-package! quickmarks
  :defer t
  :commands (qm-init qm-avatar-by-name qm-logo-by-name qm-url-by-name)
  :config
  (qm-init))

(use-package! imgflip
  :defer t
  :commands (qm-init qm-avatar-by-name qm-logo-by-name qm-url-by-name)
  :config
  (qm-init))

ls -al
