;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

;;
;; Org Mode
;;

;;;###autoload
(defun iocanel/org-drill ()
  "Require, configure and call org-drill."
  (interactive)
  (require 'org-drill)
  (setq org-drill-scope 'directory)
  (find-file "~/Documents/org/roam/index.org")
  (org-drill)
  (org-save-all-org-buffers))

;; (use-package! org-drill
;;   :init (setq org-drill-scope 'directory))

(after! org
  (add-to-list 'org-modules 'org-habit t)
  (setq-default org-display-custom-times nil)
  (setq org-use-tag-inheritance nil)
  (setq org-agenda-tag-filter-preset '("-drill"))
  (setq org-agenda-files (directory-files-recursively "~/Documents/org" "\.org$"))
  (setq org-stuck-projects '("+project" ("TODO" "NEXT" "NEXTACTION") nil ""))
  (setq org-tag-alist '((:startgroup . nil)
                        ("project" . ?p) ("area" . ?a) ("resource" . ?r)
                        (:endgroup . nil)
                        (:startgroup . nil)
                        ("@work" . ?w) ("@home" . ?h)
                        (:endgroup . nil)
                        ("@laptop" . ?l)))

  (setq org-capture-templates
        '(
          ("c" "Calendar")
          ("cw" "Work Event" entry (file  "~/Documents/org/calendars/work.org") "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
          ("cp" "Personal Event" entry (file  "~/Documents/org/calendars/personal.org") "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")

          ("i" "Inbox")
          ("iw" "Work Inbox" entry (file+olp "~/Documents/org/inbox.org" "Inbox" "Work") "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n" :prepend t)
          ("ip" "Personal Inbox" entry (file+olp "~/Documents/org/inbox.org" "Inbox" "Personal") "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n" :prepend t)

          ("p" "Project" entry (file+headline "~/Documents/org/roam/projects.org" "Projects")(file "~/Documents/org/templates/project.orgtmpl"))
          ("b" "BJJ")
          ("bm" "Moves" entry (file+olp "~/Documents/org/roam/BJJ.org" "Moves")(file "~/Documents/org/templates/bjj-move.orgtmpl"))
          ("bs" "Submission" entry (file+olp "~/Documents/org/roam/BJJ.org" "Techniques" "Submissions")(file "~/Documents/org/templates/bjj-submission.orgtmpl"))
          ("bc" "Choke" entry (file+olp "~/Documents/org/roam/BJJ.org" "Techniques" "Chokes")(file "~/Documents/org/templates/bjj-choke.orgtmpl"))
          ("bw" "Sweeps" entry (file+olp "~/Documents/org/roam/BJJ.org" "Techniques" "Sweeps")(file "~/Documents/org/templates/bjj-sweep.orgtmpl"))
          ("be" "Escapes" entry (file+olp "~/Documents/org/roam/BJJ.org" "Techniques" "Escapes")(file "~/Documents/org/templates/bjj-escape.orgtmpl"))
          ("bt" "Takedowns" entry (file+olp "~/Documents/org/roam/BJJ.org" "Techniques" "Takedowns")(file "~/Documents/org/templates/bjj-takedown.orgtmpl"))
          ("bf" "FAQ" entry (file+olp "~/Documents/org/roam/BJJ.org" "FAQ")(file "~/Documents/org/templates/bjj-faq.orgtmpl"))

          ("h" "Habit" entry (file+olp "~/Documents/org/roam/habits.org" "Habits") (file "~/Documents/org/templates/habit.orgtmpl"))

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
          ("fnf" "Nutrition facts"  entry (file+headline "~/Documents/org/flashcards/nutrition.org" "Nutrition") "* Fact :drill:\n %t\n %^{The fact}\n")
          ("fnq" "Nutrition questions"  entry (file+headline "~/Documents/org/flashcards/nutrition.org" "Nutrition") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}")
          ("fl" "Languages")
          ("fls" "Spanish"  entry (file+headline "~/Documents/org/flashcards/languages/spanish.org" "Spanish") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}"))))

;; Org roam
(setq org-roam-directory "~/Documents/org/roam")

(after! org-roam
  (run-with-idle-timer 2 nil
                       (lambda () (progn
                                    (require 'org-roam)
                                    (add-hook! 'org-mode-hook #'org-roam-mode)
                                    (map! "C-c n l" #'org-roam)
                                    (map! "C-c n t" #'org-roam-today)
                                    (map! "C-c n f" #'org-roam-find-file)
                                    (map! "C-c n i" #'org-roam-insert)
                                    (map! "C-c n g" #'org-roam-show-graph)))))


;; Google Calendar
;; (defun iocanel/org-gcal-init ()
;;   (setq org-gcal-client-id (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show services/google/vdirsyncer/ikanello@redhat.com/client-id"))
;;         org-gcal-client-secret (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show services/google/vdirsyncer/ikanello@redhat.com/secret"))
;;         org-gcal-file-alist '(("iocanel@gmail.com" .  "~/Documents/org/calendars/personal.org")
;;                               ("ikanello@redhat.com" . "~/Documents/org/calendars/work.org"))))

;; (run-with-idle-timer 3 nil (lambda () (iocanel/org-gcal-init)))

;; Super Agenda
;; (use-package! org-super-agenda
;;   :after org-agenda
;;   :init
;;   (setq org-super-agenda-groups '((:name none
;;                                          :time-grid t
;;                                          :scheduled today)
;;                                   (:name "Due today"
;;                                          :deadline today)
;;                                   (:name "Overdue"
;;                                          :deadline past)
;;                                   (:name "Due soon"
;;                                          :deadline future)))
;;   :config (org-super-agenda-mode))

;; Deft
(use-package! deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Documents/org/notes")
  (deft-use-filename-as-title t))

;;
;; Org Functions
;;
(defun iocanel/org-heading (heading tags)
  "Format the HEADING and the TAGS in the desired way."
  (format "%-80s %s" heading tags))

(defun iocanel/org-trim-tags (h)
  "Removes all tags that are present in H."
  (if h (string-trim  (replace-regexp-in-string ":[a-zA-Z0-9_\\.-:]+:$" "" h)) nil))

(defun iocanel/org-get-entries (tag &optional f)
  (interactive)
  "Collects all headings that contain TAG from the current buffer or from file F."
  (if f (mapcar #'iocanel/org-trim-tags (org-map-entries #'org-get-heading tag 'file))
    (mapcar #'iocanel/org-trim-tags (org-map-entries #'org-get-heading tag 'agenda))))
