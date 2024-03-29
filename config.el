;;; -*- lexical-binding: t; -*-

(general-auto-unbind-keys)

(map! [escape] #'keyboard-quit)

(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024))

(setq large-file-warning-threshold nil)

(setq
 doom-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 18)
 doom-big-font (font-spec :family "Ubuntu" :size 15)
 doom-big-font (font-spec :family "SauceCodePro Nerd Font Mono" :size 24))

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
  (set-face-attribute 'default nil :width 'ultra-expanded)
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

(defun my/consult-line-with-selection (&optional start end)
    "Swiper variation using selected from START to END text as initial input."
    (interactive (if (use-region-p) (list (region-beginning) (region-end))))
    (let (
          (start (or start (if (use-region-p) (region-beginning) nil)))
          (end (or end (if (use-region-p) (region-end) nil))))
      (if (and (use-region-p) start end)
          (consult-line (buffer-substring start end))
        (consult-line))))

(map! "C-s" #'my/consult-line-with-selection)

(map! "C-c i g" #'+vertico/project-search)

(map! :leader
      "s b" #'my/consult-line-with-selection
      "s m" #'consult-mark
      "s M" #'consult-bookmark
      "s r" #'vertico-repeat)

(setq display-line-numbers-type 'relative)

(map!
 "C-q" #'er/expand-region
 "C-c m m" #'mc/mark-next-like-this
 "C-c m u" #'mc/umark-next-like-this
 "C-c m s" #'mc/skip-next-like-this
 "C-c m e" #'mc/edit-lines)

(use-package! dired-subtree
    :commands (dired-subtree-toggle dired-subtree-cycle)
    :config
    :bind (:map dired-mode-map
                ("<tab>" . dired-subtree-toggle)
                ("<backtab>" . dired-subtree-cycle)))

(defun my/dired-expand-all ()
  (interactive)
  "Expand all subtrees in the dired buffer."
  (let ((has-more t))
    (while has-more
      (condition-case ex
          (progn
            (dired-next-dirline 1)
            (dired-subtree-toggle))
        ('error (setq has-more nil))))))

(map! :map dired-mode-map "S-<tab>" #'my/dired-expand-all)

(map! (:leader "p a" #'projectile-add-known-project))
(map! (:leader "p p" #'projectile-switch-project))
(map! (:leader "SPC" #'projectile-find-file-dwim))

(after! projectile
  (setq! projectile-project-root-functions '(projectile-root-local projectile-root-bottom-up))
  (setq! projectile-project-root-files-bottom-up
      (append '(".projectile" ".git"))))

(setq! auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"))

(setq! +doom-dashboard-menu-sections
  '(("Reload last session"
     :icon (all-the-icons-octicon "history" :face 'doom-dashboard-menu-title)
     :when (cond ((modulep! :ui workspaces)
                  (file-exists-p (expand-file-name persp-auto-save-fname persp-save-dir)))
                 ((require 'desktop nil t)
                  (file-exists-p (desktop-full-file-name))))
     :face (:inherit (doom-dashboard-menu-title bold))
     :action doom/quickload-session)
    ("Open agenda"
     :icon (all-the-icons-octicon "calendar" :face 'doom-dashboard-menu-title)
     :when (fboundp 'org-agenda)
     :action org-agenda)
    ("Open roam"
     :icon (all-the-icons-octicon "book" :face 'doom-dashboard-menu-title)
     :when (fboundp 'org-roam-node-find)
     :action org-roam-node-find)
    ("Recently opened files"
     :icon (all-the-icons-octicon "file-text" :face 'doom-dashboard-menu-title)
     :action recentf-open-files)
    ("Open project"
     :icon (all-the-icons-octicon "briefcase" :face 'doom-dashboard-menu-title)
     :action projectile-switch-project)
    ("Jump to bookmark"
     :icon (all-the-icons-octicon "bookmark" :face 'doom-dashboard-menu-title)
     :action bookmark-jump)
    ("Open private configuration"
     :icon (all-the-icons-octicon "tools" :face 'doom-dashboard-menu-title)
     :when (file-directory-p doom-user-dir)
     :action doom/open-private-config)
    ("Open documentation"
     :icon (all-the-icons-octicon "book" :face 'doom-dashboard-menu-title)
     :action doom/help)))

(defadvice! horizontal-split-and-follow (&rest args)
  "Switch focus to the newly created window when splitting horizontally."
  :after '(split-window-horizontally split-window-right)
  (balance-windows)
  (other-window 1))

(defadvice! vertical-split-and-follow (&rest args)
  "Switch focus to the newly created window when splitting vertically."
  :after '(split-window-vertically split-window-below)
  (balance-windows)
  (other-window 1))

(map! "M-o" #'evil-window-next)

(setq org-agenda-files (append
                        '("~/Documents/org/quickmarks.org"
                          "~/Documents/org/github.org"
                          "~/Documents/org/habits.org"
                          "~/Documents/org/nutrition.org"
                          "~/Documents/org/roam/Inbox.org")
                          (directory-files-recursively "~/Documents/org/jira" "\.org$")))

(defun my/org-agenda-browse-at-point ()
  "Browse  the url of the specified item."
  (interactive)
  (let ((agenda-window-configuration (current-window-configuration)))
    (org-agenda-switch-to)
    (let ((url (car
                (mapcar (lambda (p) (replace-regexp-in-string (regexp-quote "\"") "" (org-entry-get (point) p)))
                        (seq-filter (lambda (n) (string-suffix-p "url" n t))
                                    (mapcar (lambda (e) (car e)) (org-entry-properties)))))))
      (when url (browse-url  url)))
    (set-window-configuration agenda-window-configuration)))

(defun my/org-agenda-archive-at-point ()
  "Browse  the url of the specified item."
  (interactive)
  (let ((agenda-window-configuration (current-window-configuration)))
    (org-agenda-switch-to)
    (my/org-archive)
    (set-window-configuration agenda-window-configuration)))

(defun my/org-agenda-export ()
  "Export the content of org-agenda"
  (interactive)
  (org-eval-in-environment (org-make-parameter-alist
                            `(org-agenda-span 'day
                                              org-agenda-use-time-grid t
                                              org-agenda-remove-tags t
                                              org-agenda-window-setup 'nope))
    (let* ((wins (current-window-configuration))
           org-agenda-sticky)
      (save-excursion
        (with-current-buffer
            (get-buffer-create org-agenda-buffer-name)
          (pop-to-buffer (current-buffer))
          (org-agenda nil "t")
          (let ((result (buffer-string)))
            (with-temp-file "~/.agenda" (insert result)))))
      (set-window-configuration wins))))

(use-package! org-super-agenda
  :commands (my/org-agenda-browse-at-point my/org-agenda-archive-at-point my/org-agenda-export my/org-archive my/org-refile)
  :config
  (setq org-super-agenda-groups '((:name "Events" :time-grid t :todo "TODAY")
                                  (:name "Habbits" :tag "habit" :todo "TODAY")
                                  (:name "Due" :deadline past)
                                  (:name "Jira" :tag "jira")
                                  (:name "Email" :tag "email")
                                  (:name "Github pulls" :tag "pull")
                                  (:name "Github issues" :tag "issue"))
        ;; agenda
        org-agenda-scheduled-leaders '("" "")
        org-agenda-tag-filter-preset '("-drill")
        org-agenda-start-day "+0"
        org-agenda-start-on-weekday nil
        org-agenda-span 2
        org-agenda-files (append
                          (directory-files-recursively "~/Documents/org/jira" "\.org$")
                          '("~/Documents/org/roam/Inbox.org" "~/Documents/org/habits.org" "~/Documents/org/github.org" "~/Documents/org/nutrition.org"))
        ;; Refile
        org-refile-targets '(
                             ;; P.A.R.A
                             ("~/Documents/org/roam/Projects.org" :maxlevel . 10)
                             ("~/Documents/org/roam/Areas.org" :maxlevel . 10)
                             ("~/Documents/org/roam/Resources.org" :maxlevel . 10)
                             ("~/Documents/org/roam/Archives.org" :maxlevel . 10)))
  :hook (org-agenda-mode . org-super-agenda-mode)
  :bind (:map org-agenda-mode-map
              ("C-a" . my/org-agenda-archive-at-point)
              ("C-b" . my/org-agenda-browse-at-point)))

(map!
 :map evil-motion-state-map
 "C-b" nil
 :map org-agenda-keymap
 "j" #'org-agenda-next-line
 "k" #'org-agenda-previous-line
 :map org-agenda-mode-map
 "j" #'org-agenda-next-line
 "k" #'org-agenda-previous-line
 :map org-super-agenda-header-map
      "j" nil
      "k" nil)

(setq! org-roam-directory "~/Documents/org/roam")

(setq! org-roam-capture-templates '(("d" "default" plain "%?" :target (file+head "${title}.org" "#+title: ${title}\n") :unnarrowed t)))
(setq! org-roam-dailies-capture-templates `(("d" "default" entry "* %?" :target (file+head "%<%Y-%m-%d>.org"
                                                                                 ,(concat "#+title: %<%Y-%m-%d>\n"
                                                                                         "* Daily Checklist\n"
                                                                                         "** TODO Log weight\n"
                                                                                         "** TODO Check emails\n"
                                                                                         "** TODO Check github issues / pull requests"
                                                                                         )))))

(defun my/org-roam-extract-subtree-and-insert ()
  "Convert current subtree at point to a node, extract it into a new file and insert a ref to it."
  (interactive)
  (save-excursion
    (org-back-to-heading-or-point-min t)
    ;; Get the stars of the heading
    (let ((stars (car (split-string (buffer-substring (bol) (eol))))))
      (when (bobp) (user-error "Already a top-level node"))
      (org-id-get-create)
      (save-buffer)
      (org-roam-db-update-file)
      (let* ((template-info nil)
             (node (org-roam-node-at-point))
             (template (org-roam-format-template
                        (string-trim (org-capture-fill-template org-roam-extract-new-file-path))
                        (lambda (key default-val)
                          (let ((fn (intern key))
                                (node-fn (intern (concat "org-roam-node-" key)))
                                (ksym (intern (concat ":" key))))
                            (cond
                             ((fboundp fn)
                              (funcall fn node))
                             ((fboundp node-fn)
                              (funcall node-fn node))
                             (t (let ((r (read-from-minibuffer (format "%s: " key) default-val)))
                                  (plist-put template-info ksym r)
                                  r)))))))
             (file-path
              (expand-file-name
               (read-file-name "Extract node to: " (file-name-as-directory org-roam-directory) template nil template)
               org-roam-directory)))
        (when (file-exists-p file-path)
          (user-error "%s exists. Aborting" file-path))
        (org-cut-subtree)
        (save-buffer)
        (with-current-buffer (find-file-noselect file-path)
          (org-paste-subtree)
          (while (> (org-current-level) 1) (org-promote-subtree))
          (save-buffer)
          (org-roam-promote-entire-buffer)
          (save-buffer))
        ;; Insert a link to the extracted node
        (insert (format "%s [[id:%s][%s]]\n" stars (org-roam-node-id node) (org-roam-node-title node)))))))

(after! f
  (defvar my/logseq-folder "~/Documents/logseq/BJJORG")

  ;; You probably don't need to change these values
  (defvar my/logseq-pages (f-expand (f-join my/logseq-folder "pages")))
  (defvar my/logseq-journals (f-expand (f-join my/logseq-folder "journals")))
  ;;(defvar my/rich-text-types [bold italic subscript link strike-through superscript underline inline-src-block footnote-reference inline-babel-call entity])
  (defvar my/rich-text-types '(bold italic subscript link strike-through superscript underline inline-src-block))

  (defun my/textify (headline)
    (save-excursion
      (apply 'concat (flatten-list
                      (my/textify-all (org-element-property :title headline))))))

  (defun my/textify-all (nodes) (mapcar 'my/subtextify nodes))

  (defun my/with-length (str) (cons (length str) str))

  (defun my/subtextify (node)
    (cond ((not node) "")
          ((stringp node) (substring-no-properties node))
          ((member (org-element-type node) my/rich-text-types)
           (list (my/textify-all (cddr node))
                 (if (> (org-element-property :post-blank node))
                     (make-string (org-element-property :post-blank node) ?\s)
                   "")))
          (t "")))

  (defun my/logseq-journal-p (file) (string-match-p (concat "^" my/logseq-journals) file))

  (defun my/ensure-file-id (file)
    "Visit an existing file, ensure it has an id, return whether the a new buffer was created"
    (setq file (f-expand file))
    (if (my/logseq-journal-p file)
        `(nil . nil)
      (let* ((buf (get-file-buffer file))
             (was-modified (buffer-modified-p buf))
             (new-buf nil)
             has-data
             org
             changed
             sec-end)
        (when (not buf)
          (setq buf (find-file-noselect file))
          (setq new-buf t))
        (set-buffer buf)
        (setq org (org-element-parse-buffer))
        (setq has-data (cddr org))
        (goto-char 1)
        (when (not (and (eq 'section (org-element-type (nth 2 org))) (org-roam-id-at-point)))
          ;; this file has no file id
          (setq changed t)
          (when (eq 'headline (org-element-type (nth 2 org)))
            ;; if there's no section before the first headline, add one
            (insert "\n")
            (goto-char 1))
          (org-id-get-create)
          (setq org (org-element-parse-buffer)))
        (when (nth 3 org)
          (when (not (org-collect-keywords ["title"]))
            ;; no title -- ensure there's a blank line at the section end
            (setq changed t)
            (setq sec-end (org-element-property :end (nth 2 org)))
            (goto-char (1- sec-end))
            (when (and (not (equal "\n\n" (buffer-substring-no-properties (- sec-end 2) sec-end))))
              (insert "\n")
              (goto-char (1- (point)))
              (setq org (org-element-parse-buffer)))
            ;; copy the first headline to the title
            (insert (format "#+title: %s" (string-trim (my/textify (nth 3 org)))))))
        ;; ensure org-roam knows about the new id and/or title
        (when changed (save-buffer))
        (cons new-buf buf))))

  (defun my/logseq-to-roam-buffer (buffer)
    "Convert BUFFER links from using logseq format to org-roam.
Logseq is using file references, which org-roam is using ids.
This function covnerts fuzzy anf file: links to id links."
    (save-excursion
      (let* (changed
             link)
        (set-buffer buffer)
        (goto-char 1)
        (while (search-forward "[[" nil t)
          (setq link (org-element-context))
          (setq newlink (my/logseq-to-roam-link link))
          (when newlink
            (setq changed t)
            (goto-char (org-element-property :begin link))
            (delete-region (org-element-property :begin link) (org-element-property :end link))
            ;; note, this format string is reall =[[%s][%s]]= but =%= is a markup char so one's hidden
            (insert newlink)))
        ;; ensure org-roam knows about the changed links
        (when changed (save-buffer)))))

  (defun my/logseq-to-roam ()
    "Convert the current buffer from logseq to roam."
    (interactive)
    (my/logseq-to-roam-buffer (current-buffer)))

  (defun my/roam-to-logseq-buffer (buffer)
    "Convert BUFFER links from using logseq format to org-roam.
Logseq is using file references, which org-roam is using ids.
This function covnerts fuzzy anf file: links to id links."
    (save-excursion
      (let* (changed)
        (with-current-buffer buffer
          (goto-char 1)
          (while (search-forward "[[id:" nil t)
            (let* ((id (car (split-string (buffer-substring-no-properties (point) (eol)) "]")))
                   (node (org-roam-node-from-id id))
                   (title (org-roam-node-title node)))
              (when title
                (setq file (car (org-id-find id)))
                (setq link (org-element-context))
                (setq newlink (format "[[%s]]" title))
                (when newlink
                  (setq changed t)
                  (goto-char (org-element-property :begin link))
                  (delete-region (org-element-property :begin link) (org-element-property :end link))
                  ;; note, this format string is reall =[[%s][%s]]= but =%= is a markup char so one's hidden
                  (insert newlink)))
              ;; ensure org-roam knows about the changed links
              (when changed (save-buffer))))))))

  (defun my/roam-to-logseq ()
    "Convert the current buffer from roam to logseq."
    (interactive)
    (my/roam-to-logseq-buffer (current-buffer)))

  (defun my/logseq-to-roam-link (link)
    "Convert the LINK from logseq format to roam.
Logseq is using file references, which org-roam is using ids.
This function covnerts fuzzy anf file: links to id links."
    (let (filename
          id
          linktext
          newlink)
      (when (eq 'link (org-element-type link))
        (when (equal "fuzzy" (org-element-property :type link))
          (setq filename (f-expand (f-join my/logseq-pages
                                           (concat (org-element-property :path link) ".org"))))
          (setq linktext (org-element-property :raw-link link)))
        (when (equal "file" (org-element-property :type link))
          (setq filename (f-expand (org-element-property :path link)))
          (if (org-element-property :contents-begin link)
              (setq linktext (buffer-substring-no-properties
                              (org-element-property :contents-begin link)
                              (org-element-property :contents-end link)))
            (setq linktext (buffer-substring-no-properties
                            (+ (org-element-property :begin link) 2)
                            (- (org-element-property :end link) 2)))))
        (when (and filename (f-exists-p filename))
          (setq id (caar (org-roam-db-query [:select id :from nodes :where (like file $s1)]
                                            filename)))
          (when id
            (setq newlink (format "[[id:%s][%s]]%s"
                                  id
                                  linktext
                                  (if (> (org-element-property :post-blank link))
                                      (make-string (org-element-property :post-blank link) ?\)
                                                   ""))))
            (when (not (equal newlink
                              (buffer-substring-no-properties
                               (org-element-property :begin link)
                               (org-element-property :end link))))
              newlink))))))

  (defun my/roam-file-modified-p (file-path)
    (let ((content-hash (org-roam-db--file-hash file-path))
          (db-hash (caar (org-roam-db-query [:select hash :from files
                                             :where (= file $s1)] file-path))))
      (not (string= content-hash db-hash))))

  (defun my/modified-logseq-files ()
    (emacsql-with-transaction (org-roam-db)
      (seq-filter 'my/roam-file-modified-p
                  (org-roam--list-files my/logseq-folder))))

  (defun my/check-logseq ()
    (interactive)
    (let (created
          files
          bufs
          unmodified
          cur
          bad
          buf)
      (setq files (org-roam--list-files my/logseq-folder))
      ;; make sure all the files have file ids
      (dolist (file-path files)
        (setq file-path (f-expand file-path))
        (setq cur (my/ensure-file-id file-path))
        (setq buf (cdr cur))
        (push buf bufs)
        (when (and (not (my/logseq-journal-p file-path)) (not buf))
          (push file-path bad))
        (when (not (buffer-modified-p buf))
          (push buf unmodified))
        (when (car cur)
          (push buf created)))
      ;; patch fuzzy links
      (mapc 'my/logseq-to-roam-buffer (seq-filter 'identity bufs))
      (dolist (buf unmodified)
        (when (buffer-modified-p buf)
          (save-buffer unmodified)))
      (mapc 'kill-buffer created)
      (when bad
        (message "Bad items: %s" bad))
      nil)))

(use-package! org-transclusion
              :after org
              :init
              (map!
               :leader
               :prefix "n"
               :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

(defun my/org-current-level ()
  "Get the level of the current heading."
  (save-excursion
    (outline-previous-heading)
    (let* ((line (buffer-substring-no-properties (bol) (eol)))
           (stars (car (split-string line " ")))
           (level (length stars)))
           level)))

(defun my/org-roam-transclusion-insert ()
  "Insert / Embed an node using org-transclusion."
  (interactive)
  (let ((level (my/org-current-level)))
    (insert "#+transclude: ")
    (org-roam-node-insert)
    (when level (insert (format " :level %s" (+ 1 level))))
    (org-transclusion-add)))

(map! :map org-mode-map
      (:leader
       (:prefix "n"
                (:prefix "r"
                 :desc "Transclude an org-roam node" "t" #'my/org-roam-transclusion-insert))))

(setq my/inbox-file "~/Documents/org/roam/Inbox.org")
(setq my/archive-file "~/Documents/org/roam/Archives.org")

(defun my/org-find-archive-target (tag)
  "Find the archive target for the specified TAG.
The idea is that the archive file has multiple headings one for each category.
When a tagged item is archived it should go to an archive with at least one matching tag
or to the 'Unsorted' when none is matched. Archives are expected to be tagged with the archive tag."
  (or (car
       (car
        (org-ql-query
          :select '(list (substring-no-properties (org-get-heading t t)))
          :from my/archive-file
          :where `(tags "archive" ,tag))))
      "Unsorted"))

  (defun my/org-refile (file headline &optional new-state)
    "Refile item to the target FILE under the HEADLINE and set the NEW-STATE."
    (let ((pos (save-excursion
                 (find-file file)
                 (org-find-exact-headline-in-buffer headline))))
      (save-excursion
        (org-refile nil nil (list headline file nil pos))
        (org-refile-goto-last-stored)
        (when new-state (org-todo new-state)))))

  (defun my/org-archive ()
    "Mark item as complete and refile to archieve."
    (interactive)
      (save-window-excursion
        (when (equal "*Org Agenda*" (buffer-name)) (org-agenda-goto))
        (let* ((tags (org-get-tags))
               (headline (if tags (car (mapcar (lambda (tag) (my/org-find-archive-target tag)) tags)) nil)))
          (my/org-refile my/archive-file headline "DONE")))
        ;; Redo the agenda
        (when (equal "*Org Agenda*" (buffer-name)) (org-agenda-redo)))

(defun my/org-auto-archive ()
  "Archieve all completed items in my inbox."
  (interactive)
    (save-window-excursion
      (find-file my/inbox-file)
      (goto-char 0)
      (let ((pos))
        (while (not (eq (point) pos))
          (setq pos (point))
          (outline-next-heading)
          (let* ((line (buffer-substring-no-properties (bol) (eol)))
                 (line-without-stars (replace-regexp-in-string "^[\\*]+ " "" line)))
          (when (string-prefix-p "DONE" line-without-stars)
            (my/org-archive)
            (goto-char 0) ;; We need to go back from the beggining to avoid loosing entries
      (save-buffer)))))))

(after! 'org
(org-babel-do-load-languages 'org-babel-load-languages '((shell .t)
                                                           (ruby . t)
                                                           (java . t)
                                                           (javascript . t)
                                                           (typescript . t)
                                                           (plantuml . t))))
;;
;; To allow yas snippet integration with org babel and avoid org-mode shadowing the block mode (when it comes to snippets)
;;

(defun my/yas-org-babel-integration-hook ()
  (setq-local yas-buffer-local-condition
              '(not (org-in-src-block-p t))))

(add-hook 'org-mode-hook #'my/yas-org-babel-integration-hook)

(defvar my/last-tangle-source-buffer nil "Holds the last buffer edited by org-tangle.")
(defvar my/last-tangle-source-buffer-point 0 "Holds the original cursor position before tangle was called.")

(defun my/org-tangle-prepare ()
  "Replace the comment references to standard noweb ones.
Comment format is '(//|#|;;)add: <reference id>'."
  (setq my/last-tangle-source-buffer (current-buffer))
  (setq my/last-tangle-source-buffer-point (point))
        (get-buffer-create "**tangle**")
  (copy-to-buffer "**tangle**" (point-min) (point-max))
  (goto-char (point-min))
  (while (re-search-forward "//add:\\([a-zA-Z0-9_-]+\\)" nil t)
    (let* ((text (buffer-substring (match-beginning 1) (match-end 1)))
           (new-text (format "<<%s>>" text)))
      (replace-match new-text))))


(defun my/org-tangle-restore (&rest args)
  "Restore the original buffer as it was before 'my/org-tangle-prepare'."
    (with-current-buffer "**tangle**"
      (copy-to-buffer my/last-tangle-source-buffer (point-min) (point-max)))
    (switch-to-buffer my/last-tangle-source-buffer)
    (goto-char my/last-tangle-source-buffer-point))

(add-hook 'org-babel-pre-tangle-hook #'my/org-tangle-prepare)
(advice-add 'org-babel-tangle :after #'my/org-tangle-restore)

(require 'ob-shell)
(use-package! org-babel-eval-in-repl
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

(require 'ob-java)
(defun org-babel-expand-body:java (body params)
  "Expand BODY with PARAMS.
  BODY could be a few statements, or could include a full class
  definition specifying package, imports, and class.  Because we
  allow this flexibility in what the source block can contain, it
  is simplest to expand the code block from the inside out."
  (let* ((fullclassname (or (cdr (assq :classname params)) ; class and package
                            (org-babel-java-find-classname body)))
         (classname (car (last (split-string fullclassname "\\.")))) ; just class name
         (packagename (if (string-match-p "\\." fullclassname)       ; just package name
                          (file-name-base fullclassname)))
         (var-lines (org-babel-variable-assignments:java params))
         (imports-val (assq :imports params))
         (imports (if imports-val
                      (split-string (org-babel-read (cdr imports-val) nil) " ")
                    nil)))
    (with-temp-buffer
      (insert body)

      ;; insert variables from source block headers
      (when var-lines
        (goto-char (point-min))
        (org-babel-java--move-past org-babel-java--class-re)   ; move inside class
        (insert (mapconcat 'identity var-lines "\n"))
        (insert "\n"))

      ;; add imports from source block headers
      (when imports
        (goto-char (point-min))
        (org-babel-java--move-past org-babel-java--package-re) ; if package is defined, move past it
        (insert (mapconcat (lambda (package) (concat "import " package ";")) imports "\n") "\n"))

      ;; add package at the top
      (goto-char (point-min))
      (when (and packagename (not (re-search-forward org-babel-java--package-re nil t)))
        (insert (concat "package " packagename ";\n")))

      ;; return expanded body
      (buffer-string))))

(setq! plantuml-default-exec-mode 'jar)
(when (not (file-exists-p plantuml-jar-path)) (plantuml-download-jar))

(after! org
  (setq! org-capture-templates
         '(
           ("c" "Calendar")
           ("cw" "Work Event" entry (file  "~/Documents/org/calendars/work.org") "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
           ("cp" "Personal Event" entry (file  "~/Documents/org/calendars/personal.org") "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")

           ("i" "Inbox")
           ("iw" "Work Inbox" entry (file+olp "~/Documents/org/roam/Inbox.org" "Work") "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n" :prepend t)
           ("ip" "Personal Inbox" entry (file+olp "~/Documents/org/roam/Inbox.org" "Personal") "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n" :prepend t)

           ("e" "Email Workflow")
           ("ef" "Follow Up" entry (file+olp "~/Documents/org/raom/Inbox.org" "Email" "Follow Up") "* TODO Follow up with %:fromname on %a :email:\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i" :immediate-finish t)
           ("er" "Read Later" entry (file+olp "~/Documents/org/roam/Inbox.org" "Email" "Read Later") "* TODO Read %:subject :email: \nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n\n%i" :immediate-finish t)

           ("p" "Project" entry (file+headline "~/Documents/org/roam/Projects.org" "Projects")(file "~/Documents/org/templates/project.orgtmpl"))
           ("d" "System design" entry (file+headline "~/Documents/org/system-design/system-design.org" "System Design") (file "~/Documents/org/templates/system-design.orgtmpl"))

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
           ("fls" "Spanish"  entry (file+headline "~/Documents/org/flashcards/languages/spanish.org" "Spanish") "* Question :drill:\n %t\n %^{The question} \n** Answer: \n%^{The answer}"))))

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

;;;###autoload
(defun my/org-drill ()
  "Require, configure and call org-drill."
  (interactive)
  (require 'org-drill)
  (let ((org-drill-scope 'directory))
    (find-file "~/Documents/org/roam/index.org")
    (org-drill)
    (org-save-all-org-buffers)))

;;;###autoload
(defun my/org-drill-buffer ()
  "Require, configure and call org-drill."
  (interactive)
  (require 'org-drill)
  (let  ((org-drill-scope 'file))
    (org-drill)
    (org-save-all-org-buffers)))
:init (setq org-drill-scope 'directory)

;;;###autoload
(defun my/org-drill-match ()
  "Require, configure and call org-drill."
  (interactive)
  (require 'org-drill)
  (let ((org-drill-scope 'directory)
        (org-drill-match (read-string "Please specify a filter (e.g. tag, property etc) for the drill: ")))
    (find-file "~/Documents/org/roam/index.org")
    (org-drill)
    (org-save-all-org-buffers)))

(use-package! org-drill :after org)

(use-package! org-habit
  :after org
  :config
  (setq org-habit-following-days 7
        org-habit-preceding-days 35
        org-habit-show-habits t)
  (defvar my/org-habit-capture-alist '() "An association list that maps capture keys to habit headings")

  (defun my/org-habit-check-captured ()
    "Check if there is a habit matching that latest captured item and mark it as done."
    (message "Checking for habits linked to the captured template ...")
    (let* ((key  (plist-get org-capture-plist :key))
           (habit (cdr (assoc key my/org-habit-capture-alist))))
      (if habit
          (progn
            (message "Found linked habit:%s" habit)
            (when (not org-note-abort) (my/org-habit-mark habit)))
        (message "No habit found for capture template with key:%s." key))))

  (defun my/org-habit-mark (heading)
    (save-excursion
      (let* ((habits-file "/home/iocanel/Documents/org/habits.org")
             (original (current-buffer))
             (buf (find-file habits-file)))
        (with-current-buffer buf
          (goto-char (point-min))
          (re-search-forward (concat "TODO " heading ".*:habit:"))
          (org-habit-parse-todo)
          (org-todo 'done)
          (save-buffer t))
      (switch-to-buffer original t t))))

  (advice-add 'org-drill :after (lambda() (my/org-habit-mark "Org Drill")))
  (add-hook 'org-capture-after-finalize-hook 'my/org-habit-check-captured))

(defun my/dired-file-as-plantuml-link-to-clipboard ()
  "Create an Org link to the currently selected file in Dired and copy it to the clipboard."
  (interactive)
  (let* ((file (dired-get-filename))
         (name (file-name-base file))
         (cleaned-name (replace-regexp-in-string "^[0-9]+\\(\\.\\)[[:blank:]]+" "" name))
         (extension (file-name-extension file))
         (link (format "[[\"file:%s\" %s]]"  file cleaned-name)))
    (kill-new link)
    (message "Plantuml link to file copied to clipboard: %s" file)))

(defun my/dired-file-as-org-link-to-clipboard ()
  "Create an Org link to the currently selected file in Dired and copy it to the clipboard."
  (interactive)
  (let* ((file (dired-get-filename))
         (name (file-name-base file))
         (cleaned-name (replace-regexp-in-string "^[0-9]+\\(\\.\\)[[:blank:]]+" "" name))
         (extension (file-name-extension file))
         (protocol (if (string-match-p "\\(\\.\\(mp4\\|mkv\\|avi\\)\\)$" file) "mpv" "file"))
         (link (format "[[%s:%s][%s]]" protocol file cleaned-name)))
    (kill-new link)
    (message "Org link to file copied to clipboard: %s" file)))

(after! dired
(define-key dired-mode-map (kbd "C-c o l") 'my/dired-file-as-org-link-to-clipboard)
(define-key dired-mode-map (kbd "C-c u l") 'my/dired-file-as-plantuml-link-to-clipboard))

(use-package! org-github-issues
  :init
  (defvar my/github-repositories nil "The list of watch repositories by org-github-issues")
  :commands (org-github-issues-sync-all my/org-github-issues-eww-at-point my/org-github-issues--show-open-workspace-issues)
  :config
  (setq
   gh-user "iocanel"
   org-github-issues-user "iocanel"
   org-github-issues-org-file "~/Documents/org/github.org"
   org-github-issues-tags '("github")
   org-github-issues-issue-tags '("issue")
   org-github-issues-pull-tags '("pull")
   org-github-issues-tag-transformations '((".*" "")) ;; force all labels to empty string so that they can be ommitted.
   org-github-issues-auto-schedule "+0d"
   org-github-issues-filter-by-assignee t
   org-github-issues-headline-prefix t))

(defun my/org-github-issues-eww-at-point ()
  "Browse the issue that corresponds to the org entry at point."
  (interactive)
  (let ((url (my/org-github-issues--url-at-point)))
    (when url
      (other-window 1)
      ;(idee/jump-to-non-ide-window)
      (split-window-horizontally)
      (eww url))))

(defun my/org-github-issues--show-open-project-issues (root)
  "Show all the project issues currently assigned to me."
  (let* ((project (projectile-ensure-project root))
         (project-name (projectile-project-name project)))
    (org-ql-search org-github-issues-org-file
                   `(and (property "GH_URL")
                         (string-match (regexp-quote ,project-name) (org-entry-get (point) "GH_URL")))
                   :title (format "Github issues for %s" project-name))
    (goto-char (point-min))
    (org-agenda-next-line)))

(defun my/org-github-issues--show-open-workspace-issues (workspace)
  "Show all the workspace issues currently assigned to me."
  (let* ((name (treemacs-project->name workspace))
         (projects (treemacs-workspace->projects workspace))
         (project-names (mapcar (lambda (p) (treemacs-project->name p)) projects))
         (main-project (car project-names)))
    (when main-project
      (org-ql-search org-github-issues-org-file
                     `(and (property "GH_URL")
                           (or (string-match (regexp-quote ,main-project) (org-entry-get (point) "GH_URL"))
                               (seq-filter (lambda (p) (string-match (regexp-quote p) (org-entry-get (point) "GH_URL"))) project-names)))
                     :title (format "Github issues for %s" name))
      (goto-char (point-min))
      (org-agenda-next-line))))

(defun my/org-github-issues--url-at-point ()
  "Utility that fetches the url of the issue at point."
  (save-excursion
    (let ((origin (current-buffer)))
      (when (eq major-mode 'org-agenda-mode) (org-agenda-switch-to))
      (let* ((p (point))
             (url (string-trim (org-entry-get nil "GH_URL"))))
        (when (not (equal origin (current-buffer))) (switch-to-buffer origin))
        url))))

(use-package! org-jira
  :commands (my/org-jira-get-issues my/org-jira-hydra my/org-jira-get-issues my/org-jira-select-board my/org-jira-select-spring)
  :custom (org-jira-property-overrides '("CUSTOM_ID" "self"))
  :bind (:map evil-normal-state-map ("SPC j" . org-jira-hydra))
  :config
  (setq jiralib-url "https://issues.redhat.com/"
        jiralib-user-login-name "ikanello1@redhat.com"
        jira-password nil
        jira-token (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show websites/redhat.com/ikanello1@redhat.com/token"))
        org-jira-working-dir "~/Documents/org/jira/"
        org-jira-projects-list '("ENTSBT" "SB" "QUARKUS"))
  (setq jiralib-token `("Authorization" . ,(concat "Bearer " jira-token))))

(defun my/org-jira-get-issues ()
    "Sync using org-jira and postprocess."
    (interactive)
    (org-jira-get-issues (org-jira-get-issue-list org-jira-get-issue-list-callback))
    (my/org-jira-postprocess))

  (defun my/org-jira-issue-id-at-point ()
    "Returns the ID of the current issue."
    (save-excursion
      (org-previous-visible-heading 1)
      (org-element-property :ID (org-element-at-point))))


(defun my/org-jira-update-issue-description()
  "Move the selected issue to an active sprint."
  (interactive)
  (let* ((issue-id (org-jira-parse-issue-id))
         (filename (buffer-file-name))
         (org-issue-description (org-trim (org-jira-get-issue-val-from-org 'description)))
         (update-fields (list (cons 'description org-issue-description))))
    (jiralib-update-issue issue-id update-fields
                          (org-jira-with-callback
                           (message (format "Issue '%s' updated!" issue-id))
                           (jiralib-get-issue
                            issue-id
                            (org-jira-with-callback
                             (org-jira-log "Update get issue for refresh callback hit.")
                             (-> cb-data list org-jira-get-issues)))))))


(defun my/org-jira-postprocess ()
  "Postprocess the org-jira project files. It shcedules all jira issues so that they appear on agenda"
  (interactive)
  (mapcar (lambda (p)
            (let ((scheduled (format "%s  SCHEDULED: <%s>\n" (make-string 2 32) (org-read-date nil nil "+0d") ))
                  (github-project-file (concat (file-name-as-directory org-jira-working-dir) (format "%s.org" p))))
              (with-temp-buffer
                (insert-file jira-project-file)
                (goto-char (point-min))
                (while (re-search-forward "^\*\* TODO" nil t)
                  (let* ((tags (org-get-tags)))
                    (add-to-list 'tags "jira")
                    (org-set-tags tags)
                    (org-set-property "SCHEDULED" scheduled)
                    (write-file jira-project-file)))))) '("QUARKUS" "SB" "ENTSBT")))

;;
;;  Variables
;;
(defvar my/org-jira-selected-board nil)
(defvar my/org-jira-selected-sprint nil)
(defvar my/org-jira-selected-epic nil)

(defvar my/org-jira-boards-cache ())
(defvar my/org-jira-sprint-by-board-cache ())
(defvar my/org-jira-epic-by-board-cache ())

;;
;; Custom functions
;;

;;
;; Boards
;;
(defun my/org-jira-get-boards-list()
  "List all boards."
  (unless my/org-jira-boards-cache
    (setq my/org-jira-boards-cache (jiralib--agile-call-sync "/rest/agile/1.0/board" 'values)))
  my/org-jira-boards-cache)

(defun my/org-jira-get-board-id()
  "Select a board if one not already selected."
  (unless my/org-jira-selected-board
    (setq my/org-jira-selected-board (my/org-jira-board-completing-read)))
  (cdr (assoc 'id my/org-jira-selected-board)))

(defun my/org-jira-get-board()
  "Select a board if one not already selected."
  (unless my/org-jira-selected-board
    (setq my/org-jira-selected-board (my/org-jira-board-completing-read)))
  my/org-jira-selected-board)

(defun my/org-jira-board-completing-read()
  "Select a board by name."
  (when (not (file-exists-p (my/org-jira--get-boards-file)))
    (my/org-jira-get-boards-list))

  (let* ((boards (with-current-buffer (org-jira--get-boards-buffer)
                        (org-map-entries (lambda()
                                           `((id . ,(org-entry-get nil "id"))
                                             (self . ,(org-entry-get nil "url"))
                                             (name . ,(org-entry-get nil "name")))) t  'file)))
          (board-names (mapcar #'(lambda (a) (cdr (assoc 'name a))) boards))
         (board-name (completing-read "Choose board:" board-names)))
    (car (seq-filter #'(lambda (a) (equal (cdr (assoc 'name a)) board-name)) boards))))

(defun my/org-jira-select-board()
  "Select a board."
  (interactive)
  (setq my/org-jira-selected-board (cdr (assoc 'name (my/org-jira-board-completing-read)))))

;;
;; Sprint
;;
(defun my/org-jira-get-project-boards(project-id)
  "Find the board of the project.")

(defun my/org-jira-get-sprints-by-board(board-id &optional filter)
  "List all sprints by BOARD-ID."
  (let ((board-sprints-cache (cdr (assoc board-id my/org-jira-sprint-by-board-cache))))
    (unless board-sprints-cache
      (setq board-sprints-cache (jiralib--agile-call-sync (format "/rest/agile/1.0/board/%s/sprint" board-id)'values)))

    (add-to-list 'my/org-jira-sprint-by-board-cache `(,board-id . ,board-sprints-cache))
    (if filter
        (seq-filter filter board-sprints-cache)
      board-sprints-cache)))

(defun my/org-jira--active-sprint-p(sprint)
  "Predicate that checks if SPRINT is active."
  (not (assoc 'completeDate sprint)))

(defun my/org-jira-sprint-completing-read(board-id)
  "Select an active sprint by name."
  (let* ((sprints (my/org-jira-get-sprints-by-board board-id 'my/org-jira--active-sprint-p))
         (sprint-names (mapcar #'(lambda (a) (cdr (assoc 'name a))) sprints))
         (sprint-name (completing-read "Choose sprint:" sprint-names)))
    (car (seq-filter #'(lambda (a) (equal (cdr (assoc 'name a)) sprint-name)) sprints))))

(defun my/org-jira-move-issue-to-sprint(issue-id sprint-id)
  "Move issue with ISSUE-ID to sprint with SPRINT-ID."
  (jiralib--rest-call-it (format "/rest/agile/1.0/sprint/%s/issue" sprint-id) :type "POST" :data (format "{\"issues\": [\"%s\"]}" issue-id)))

(defun my/org-jira-assign-current-issue-to-sprint()
  "Move the selected issue to an active sprint."
  (interactive)
  (let* ((issue-id (my/org-jira-parse-issue-id))
         (board-id (cdr (assoc 'id (my/org-jira-get-board))))
         (sprint-id (cdr (assoc 'id (my/org-jira-sprint-completing-read board-id)))))

    (my/org-jira-move-issue-to-sprint issue-id sprint-id)))

(defun my/org-jira-get-sprint-id()
  "Select a sprint id if one not already selected."
  (unless my/org-jira-selected-sprint
    (setq my/org-jira-selected-sprint (my/org-jira-sprint-completing-read)))
  (cdr (assoc 'id my/org-jira-selected-sprint)))

(defun my/org-jira-get-sprint()
  "Select a sprint if one not already selected."
  (unless my/org-jira-selected-sprint
    (setq my/org-jira-selected-sprint (my/org-jira-select-sprint)))
  my/org-jira-selected-sprint)

(defun my/org-jira-select-sprint()
  "Select a sprint."
  (interactive)
  (setq my/org-jira-selected-sprint (my/org-jira-sprint-completing-read (my/org-jira-get-board-id))))

;;
;; Epics
;;
(defun my/org-jira-get-epics-by-board(board-id &optional filter)
  "List all epics by BOARD-ID."
  (interactive)
  (let ((board-epics-cache (cdr (assoc board-id my/org-jira-epic-by-board-cache))))
    (unless board-epics-cache
      (setq board-epics-cache (jiralib--agile-call-sync (format "/rest/agile/1.0/board/%s/epic" board-id)'values)))

    (add-to-list 'my/org-jira-epic-by-board-cache `(,board-id . ,board-epics-cache))
    (if filter
        (seq-filter filter board-epics-cache)
      board-epics-cache)))

(defun my/org-jira--active-epic-p(epic)
  "Predicate that checks if EPIC is active."
  (not (equal (assoc 'done epic) 'false)))


(defun my/org-jira-epic-completing-read(board-id)
  "Select an active epic by name."
  (let* ((epics (my/org-jira-get-epics-by-board board-id 'my/org-jira--active-epic-p))
         (epic-names (mapcar #'(lambda (a) (cdr (assoc 'name a))) epics))
         (epic-name (completing-read "Choose epic:" epic-names)))
    (car (seq-filter #'(lambda (a) (equal (cdr (assoc 'name a)) epic-name)) epics))))

(defun my/org-jira-move-issue-to-epic(issue-id epic-id)
  "Move issue with ISSUE-ID to epic with SPRINT-ID."
  (jiralib--rest-call-it (format "/rest/agile/1.0/epic/%s/issue" epic-id) :type "POST" :data (format "{\"issues\": [\"%s\"]}" issue-id)))

(defun my/org-jira-assign-current-issue-to-epic()
  "Move the selected issue to an active epic."
  (interactive)
  (let* ((issue-id (my/org-jira-parse-issue-id))
         (board-id (cdr (assoc 'id (my/org-jira-get-board))))
         (epic-id (cdr (assoc 'id (my/org-jira-epic-completing-read board-id)))))

    (my/org-jira-move-issue-to-epic issue-id epic-id)))

(defun my/org-jira-get-epic-id()
  "Select a epic id if one not already selected."
  (unless my/org-jira-selected-epic
    (setq my/org-jira-selected-epic (my/org-jira-epic-completing-read)))
  (cdr (assoc 'id my/org-jira-selected-epic)))

(defun my/org-jira-get-epic()
  "Select a epic if one not already selected."
  (unless my/org-jira-selected-epic
    (setq my/org-jira-selected-epic (my/org-jira-select-epic)))
  my/org-jira-selected-epic)

(defun my/org-jira-select-epic()
  "Select a epic."
  (interactive)
  (setq my/org-jira-selected-epic (my/org-jira-epic-completing-read (my/org-jira-get-board-id))))

(defun my/org-jira-create-issue-with-defaults()
  "Create an issue and assign to default sprint and epic."
  (org-jira-create-issue)
  (my/org-jira-move-issue-to-epic)
  (my/org-jira-move-issue-to-sprint))

(defun my/org-jira-hydra ()
    "Define (if not already defined org-jira hydra and invoke it."
    (interactive)
    (unless (boundp 'org-jira-hydra/body)
      (defhydra org-jira-hydra (:hint none :exit t)
        ;; The '_' character is not displayed. This affects columns alignment.
        ;; Remove s many spaces as needed to make up for the '_' deficit.
        "
         ^Actions^           ^Issue^              ^Buffer^                         ^Defaults^
                           ?I?
         ^^^^^^-----------------------------------------------------------------------------------------------
          _L_ist issues      _u_pdate issue       _R_efresh issues in buffer       Select _B_oard ?B?
          _C_reate issue     update _c_omment                                    Select _E_pic ?E?
                           assign _s_print                                     Select _S_print ?S?
                           assign _e_print                                     Create issue with _D_efaults
                           _b_rowse issue
                           _r_efresh issue
                           _p_rogress issue
  [_q_]: quit
"
        ("I" nil (or (my/org-jira-issue-id-at-point) ""))
        ("L" my/org-jira-get-issues)
        ("C" org-jira-create-issue)

        ("u" org-jira-update-issue)
        ("c" org-jira-update-comment)
        ("b" org-jira-browse-issue)
        ("s" my/org-jira-assign-current-issue-to-sprint)
        ("e" my/org-jira-assign-current-issue-to-epic)
        ("r" org-jira-refresh-issue)
        ("p" org-jira-progress-issue)

        ("R" org-jira-refresh-issues-in-buffer)

        ("B" my/org-jira-select-board (format "[%s]" (or my/org-jira-selected-board "")) :exit nil)
        ("E" my/org-jira-select-epic (format "[%s]" (or my/org-jira-selected-epic "")) :exit nil)
        ("S" my/org-jira-select-sprint (format "[%s]" (or my/org-jira-selected-sprint "")) :exit nil)
        ("D" my/org-jira-create-with-defaults)

        ("q" nil "quit")))
    (org-jira-hydra/body))

;;;###autoload
(defun +org-present-hide-blocks-h ()
  "Hide org #+ constructs."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*\\(#\\+\\)\\(\\(?:BEGIN\\|END\\|ATTR\\)[^[:space:]]+\\).*" nil t)
      (org-flag-region (match-beginning 1)
                       (match-end 0)
                       org-tree-slide-mode
                       'block))))

;;;###autoload
(defun +org-present-hide-leading-stars-h ()
  "Hide leading stars in headings."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(\\*+\\)" nil t)
      (org-flag-region (match-beginning 1)
                       (match-end 1)
                       org-tree-slide-mode
                       'headline))))

(use-package! adoc-mode)
;;(use-package! org-asciidoc)

(setq org-hugo-base-dir "~/workspace/src/github.com/iocanel/iocanel.github.io")

(defun my/org-hugo-set-export-file-name ()
  "Set the export file name to index.md."
  (interactive)
  (let ((name (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name)))))
    (save-excursion
      (goto-char 0)
      (if (re-search-forward "^#\\+EXPORT_FILE_NAME" nil t)
          (progn
            (move-beginning-of-line 1)
            (kill-line))
        (progn
          (while (string-prefix-p "#+" (buffer-substring (bol) (eol)))
            (next-line 1))
          (previous-line 1)
          (move-end-of-line 1)
          (insert "\n")))
        (insert "#+EXPORT_FILE_NAME: index.md"))))

(defun my/org-hugo-set-bundle ()
  "Set the hugo bundle property to match the directory."
  (interactive)
  (let ((name (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name)))))
    (save-excursion
      (goto-char 0)
      (if (re-search-forward "^#\\+HUGO_BUNDLE" nil t)
          (progn
            (move-beginning-of-line 1)
            (kill-line))
        (progn
          (while (string-prefix-p "#+" (buffer-substring (bol) (eol)))
            (next-line 1))
          (previous-line 1)
          (move-end-of-line 1)
          (insert "\n")))
        (insert (format! "#+HUGO_BUNDLE: %s" name)))))

(defun my/org-hugo-prepare()
  "Prepare document for export via ox-hugo."
  (interactive)
  (my/org-hugo-set-bundle)
  (my/org-hugo-set-export-file-name))

(use-package! mpv
  :commands (my/mpv-timestamp-inc my/mpv-timestamp-dec my/mpb-insert-playback-position)
  :init
  (add-to-list 'auto-mode-alist '("\\.avi\\'" . my/mpv-play-buffer))
  (add-to-list 'auto-mode-alist '("\\.mp4\\'" . my/mpv-play-buffer))
  (add-to-list 'auto-mode-alist '("\\.mkv\\'" . my/mpv-play-buffer))
  :config
  (add-hook 'org-metareturn-hook #'my/metareturn-mpv-insert-playback-position)
  (add-hook 'org-open-at-point-functions #'mpv-seek-to-position-at-point)
  (org-link-set-parameters "mpv" :follow #'mpv-play :complete #'org-mpv-complete-link))

(defun org-mpv-complete-link (&optional arg)
  "Link completion for mpv."
  (replace-regexp-in-string "file:" "mpv:" (org-link-complete-file arg) t t))

;;;###autoload
(defun my/mpv-play-buffer ()
  "Open video FILENAME with mpv."
  (interactive)
  (mpv-play (buffer-file-name))
  (kill-buffer))

;;;###autoload
(defun my/mpv-insert-playback-position-as-org-timer ()
  "Isert the playback position as org timer"
  (interactive)
  (mpv-insert-playback-position t))

(defun my/metareturn-mpv-insert-playback-position ()
  "Optioanlly, isert the playback position as org timer."
  (when-let ((item-beg (org-in-item-p)))
    (when (and (not (bound-and-true-p org-timer-start-time))
               (mpv-live-p)
               (save-excursion
                 (goto-char item-beg)
                 (and (not (org-invisible-p)) (org-at-item-timer-p))))
      (mpv-insert-playback-position t))))

(defun my/mpv-timestamp-apply-offset (offset)
  "Apply the specified OFFSET to the current timestamp."
  (let* ((line (buffer-substring-no-properties (bol) (eol)))
         (hhmmss (replace-regexp-in-string "[ -]+" "" (car (split-string line "::"))))
         (seconds (org-timer-hms-to-secs hhmmss))
         (new-seconds (+ offset seconds))
         (new-hhmmss (org-timer-secs-to-hms new-seconds)))
    (replace-string-in-region hhmmss new-hhmmss (bol) (eol))
    (save-excursion
      (goto-char (bol))
      (search-forward-regexp "[0-9]+")
      (org-open-at-point))))

;;;###autoload
(defun my/mpv-timestamp-inc ()
  "Increase the timestamp in the current line by 1."
  (interactive)
  (my/mpv-timestamp-apply-offset 1))

;;;###autoload
(defun my/mpv-timestamp-dec ()
  "Decrease the timestamp in the current line by 1."
  (interactive)
  (my/mpv-timestamp-apply-offset -1))

(map! :leader
      "+" #'my/mpv-timestamp-inc
      "=" #'my/mpv-timestamp-inc
      "-" #'my/mpv-timestamp-dec)

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

(use-package! gnuplot)

(after! org
     (setq org-capture-templates (append org-capture-templates '(
                                                                 ("h" "Health")
                                                                 ("hw" "Weight" table-line (file+olp "~/Documents/org/weight.org" "Weight" "Weight Table")(file "~/Documents/org/templates/weight.orgtmpl"))))))

(setq lsp-idle-delay 0.500)
(setq lsp-log-io nil) ; if set to true can cause a performance hit

(after! lsp-treemacs
  (lsp-treemacs-sync-mode 1))

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

(setq
 lsp-java-vmargs '("-XX:+UseAdaptiveSizePolicy" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Xmx8G" "-Xms2G" "-Xverify:none" "-jar")
 lsp-java-java-path "/home/iocanel/.sdkman/candidates/java/current/bin/java"
 lsp-java-save-action-organize-imports nil
 lsp-java-autobuild-enabled nil
 lsp-java-maven-download-sources t
 lsp-java-import-maven-enabled nil
 lsp-java-import-gradle-enabled nil
 lsp-java-max-concurrent-builds 1
 lsp-inhibit-message nil
 lsp-java-format-on-type-enabled nil
 lsp-java-completion-guess-arguments t
 lsp-java-completion-overwrite nil
 c-basic-offset 2
 tab-width 2)

;;(add-to-list 'yas-snippet-dirs '(idee/emacs-snippets-dir idee/emacs-templates-dir))

(map!
    :leader
    "m" #'idee/maven-hydra/body)

(use-package! quickmarks
  :commands (qm-init qm-install qm-avatar-by-name qm-logo-by-name qm-url-by-name)
  :config
  (setq! qm-org-capture-file "~/Documents/org/quickmarks.org")
  (setq! qm-src-dir (concat (file-name-as-directory (concat (file-name-as-directory (concat (file-name-as-directory straight-base-dir)  "straight")) "repos")) "quickmarks.el"))
  (setq! qm-snippet-dir +snippets-dir)
  (qm-init))

(use-package! imgflip
  :commands (qm-init qm-avatar-by-name qm-logo-by-name qm-url-by-name)
  :config
  (qm-init))

(use-package! mu4e
  :config
  (setq
        user-mail-address "iocanel@gmail.com"
        user-full-name "Ioannis Canellos"
        mu4e-maildir "~/.mail"
        mu4e-get-mail-command "mbsync -a -c ~/.mbsyncrc"

        ;; Having Error: 102: failed to move message
        ;; The following block of config is suggested by https://github.com/djcb/mu/issues/2053
        mu4e-index-lazy-check nil
        mu4e-change-filenames-when-moving t

        mu4e-compose-context-policy 'pick-first
        mu4e-context-policy 'pick-first

        mu4e-update-interval nil
        mu4e-headers-results-limit 1000000
        ;; Why would I want to leave my message open after I've sent it
        message-kill-buffer-on-exit t
        ;; Don't ask for a 'context' upon opening mu4
        mu4e-context-policy 'pick-first
        ;; Don't ask to quit... why is this the default
        mu4e-confirm-quit nil
        mu4e-headers-visible-lines 25

        message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "msmtp"
        message-sendmail-extra-arguments '("-C" "/home/iocanel/.config/msmtp/config" "--read-envelope-from")
        message-sendmail-f-is-evil 't
        message-kill-buffer-on-exit t
        doom-modeline-mu4e t

        mu4e-bookmarks
        '(
          ("date:2d..now AND flag:unread AND NOT flag:trashed AND not flag:list AND date:30d..now AND (to:iocanel or ikanello)" "Must read" ?i)

          ("NOT flag:trashed AND NOT maildir:\"/Archived\"" "Messages (all)" ?U)
          ("flag:unread AND NOT flag:trashed AND NOT maildir:\"/Archived\"" "Messages (unread)" ?u)

          ("not flag:list AND date:30d..now AND (to:iocanel or ikanello)" "Personal (all)" ?P)
          ("flag:unread AND not flag:list AND date:30d..now AND (to:iocanel or ikanello)" "Personal (unread)" ?p)

          ;; Github
          ("from:github AND AND NOT flag:trashed AND NOT maildir:\"/Archived\"" "Github (all)" ?G)
          ("flag:unread AND from:github AND AND NOT flag:trashed AND NOT maildir:\"/Archived\"" "Github (unread)" ?g)
          ("flag:unread AND from:notifications@github.com AND AND NOT flag:trashed AND cc:review_requested AND NOT maildir:\"/Archived\"" "Github (review)" ?r)
          ("flag:unread AND from:notifications@github.com AND AND NOT flag:trashed AND cc:mention AND NOT maildir:\"/Archived\"" "Github (mentions)" ?m)

          ;; Events
          ("mime:text/calendar" "Events (all)" ?E)
          ("flat:unread AND mime:text/calendar" "Events (unread)" ?e)

          ;; Period
          ("date:today" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)))

  (set-email-account! "iocanel@gmail.com"
                      '((smtpmail-smtp-user               . "iocanel@gmail.com")
                        (mail-reply-to                    . "iocanel@gmail.com")
                        (user-mail-address                . "iocanel@gmail.com")
                        (user-full-name                   . "Ioannis Canellos")
                        (mu4e-user-mail-address-list      . "~/.mail/iocanel@gmail.com")
                        (mu4e-drafts-folder               . "/iocanel@gmail.com/[Email] Actionable")
                        (mu4e-refile-folder               . "/iocanel@gmail.com/[Email] Archived")
                        (mu4e-drafts-folder               . "/iocanel@gmail.com/[Email] Deferred")
                        (mu4e-trash-folder                . "/iocanel@gmail.com/Trash")
                        (mu4e-sent-folder                 . "/iocanel@gmail.com/Sent")
                        (mu4e-compose-complete-addresses  . t)

                        (message-send-mail-function       . message-send-mail-with-sendmail)
                        (sendmail-program                 . "/usr/bin/msmtp")
                        (message-sendmail-extra-arguments . ("-C" "/home/iocanel/.config/msmtp/config" "--read-envelope-from"))
                        (message-sendmail-f-is-evil       . t)
                        (mu4e-sent-messages-behavior      . delete)
                        (mu4e-compose-signature           . t))
                      t)

  (set-email-account! "ikanello@redhat.com"
                      '((smtpmail-smtp-user               . "ikanello@redhat.com")
                        (mail-reply-to                    . "ikanello@redhat.com")
                        (user-mail-address                . "ikanello@redhat.com")
                        (user-full-name                   . "Ioannis Canellos")
                        (mu4e-user-mail-address-list      . "~/.mail/ikanello@redhat.com")
                        (mu4e-drafts-folder               . "/ikanello@gmail.com/Drafts")
                        (mu4e-refile-folder               . "/ikanello@gmail.com/[Email] Actionable")
                        (mu4e-refile-folder               . "/ikanello@gmail.com/[Email] Archived")
                        (mu4e-refile-folder               . "/ikanello@gmail.com/[Email] Deferred")
                        (mu4e-trash-folder                . "/ikanello@gmail.com/Trash")
                        (mu4e-sent-folder                 . "/ikanello@gmail.com/Sent")
                        (mu4e-compose-complete-addresses  . t)
                        (message-send-mail-function       . message-send-mail-with-sendmail)
                        (sendmail-program                 . "/usr/bin/msmtp")
                        (message-sendmail-extra-arguments . ("-C" "/home/iocanel/.config/msmtp/config" "--read-envelope-from"))
                        (message-sendmail-f-is-evil       . t)
                        (mu4e-sent-messages-behavior      . delete)
                        (mu4e-compose-signature           .  t))
                      nil)

(setq +mu4e-gmail-accounts '(("iocanel@gmail.com" . "/iocanel@gmail.com")
                             ("ikanello@redhat.com" . "/ikanello@redhat.com")))
)

;; Capturing, source: https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Mail-05.org#adding-custom-actions-for-quick-capturing
  (defun my/mu4e-capture-follow-up (&optional msg)
    "Create a follow up todo item."
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "ef"))

  (defun my/mu4e-capture-read-later (&optional msg)
    "Create a read later todo item."
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "er"))

(map!
 :map mu4e-headers-mode-map
      "C-r" #'my/mu4e-capture-read-later
      "C-f" #'my/mu4e-capture-follow-up
 :map mu4e-loading-mode-map
      "C-r" #'my/mu4e-capture-read-later
      "C-f" #'my/mu4e-capture-follow-up
 :map mu4e-view-mode-map
      "C-r" #'my/mu4e-capture-read-later
      "C-f" #'my/mu4e-capture-follow-up)

(defun my/mu4e-mark-thread-as-read()
    "Skip all messages that are part of the thread or share subject."
    (interactive)
    ;; 1st pass mark similar
    (save-excursion
      (select-window (get-buffer-window "*mu4e-headers*"))
      (let ((subject (my/mu4e-strip-subject (mu4e-message-field (mu4e-message-at-point) :subject))))
        (recenter)
        (mu4e-headers-mark-for-each-if
         '(read)
         (lambda (msg _param)
           (let* ((value (my/mu4e-strip-subject (mu4e-msg-field msg :subject))))
             (message "Comparing: [%s] with [%s]" subject value)
             (string-match-p (regexp-quote subject) (or value "")))))))
    (mu4e-mark-execute-all t)
    ;; 2nd pass mark thread
    (save-excursion
      (select-window (get-buffer-window "*mu4e-headers*"))
        (mu4e-headers-mark-thread t '(read))))

  (defun my/mu4e-strip-subject(subject)
    "Strip prefixes like ``RE:` `FWD:`` etc."
        (replace-regexp-in-string "^Re: " "" (replace-regexp-in-string "^Fwd: " "" subject)))
(map!
 :map mu4e-headers-mode-map
      "M-m" #'my/mu4e-mark-thread-as-read
 :map mu4e-loading-mode-map
      "M-m" #'my/mu4e-mark-thread-as-read
 :map mu4e-view-mode-map
      "M-m" #'my/mu4e-mark-thread-as-read)

;;
  ;; Functions
  ;;

  (defun my/mu4e-force-next-unread()
    "View next unread closing the current message if stuck in loading."
    (interactive)
    (if (eq 'mu4e-loading-mode major-mode)
        (progn
          (select-window (get-buffer-window "*mu4e-headers*"))
          (delete-other-windows)
          (mu4e-view-headers-next-unread)
          (mu4e-headers-view-message))
      (mu4e-view-headers-next-unread)))

  (defun my/mu4e-view-unread()
    "Open my unread messages."
    (interactive)
    (require 'mu4e)
    (mu4e-headers-search
     (mu4e-bookmark-query (car (remove-if-not (lambda (s) (equal (mu4e-bookmark-name s) "Unread messages")) (mu4e-bookmarks))))))

  ;;
  ;; Advices
  ;;

  (defadvice mu4e-view-headers-next (around scroll-down-mu4e-header activate)
    "Scroll down the mu4e-header window when moving onto next email"
    (when (not hl-line-sticky-flag) (setq hl-line-sticky-flag t))
    (save-excursion
      (select-window (get-buffer-window "*mu4e-headers*"))
      (recenter))
    ad-do-it)

  (defadvice mu4e-view-headers-prev (around scroll-up-mu4e-header activate)
    "Scroll up the mu4e-header window when moving onto prev email"
    (when (not hl-line-sticky-flag) (setq hl-line-sticky-flag t))
    (save-excursion
      (select-window (get-buffer-window "*mu4e-headers*"))
      (recenter))
    ad-do-it)

  (defadvice mu4e-view-headers-next-unread (around scroll-down-mu4e-header activate)
    "Scroll down the mu4e-header window when moving onto next email"
    (when (not hl-line-sticky-flag) (setq hl-line-sticky-flag t))
    (save-excursion
      (select-window (get-buffer-window "*mu4e-headers*"))
      (recenter))
    ad-do-it)

  (defadvice mu4e-view-headers-prev-unread (around scroll-down-mu4e-header activate)
    "Scroll down the mu4e-header window when moving onto next email"
    (when (not hl-line-sticky-flag) (setq hl-line-sticky-flag t))
    (save-excursion
      (other-window 1)
      (recenter))
    ad-do-it)

  (ad-activate 'mu4e-view-headers-next)
  (ad-activate 'mu4e-view-headers-prev)
  (ad-activate 'mu4e-view-headers-next-unread)
  (ad-activate 'mu4e-view-headers-prev-unread)

(map!
 :map mu4e-headers-mode-map
      "M-u" #'mu4e-headers-next-unread
 :map mu4e-loading-mode-map
      "M-u" #'my/mu4e-force-next-unread
 :map mu4e-view-mode-map
      "M-u" #'my/mu4e-force-next-unread)

(defun my/mu4e-get-incoming-count ()
  "Count the number of unread messages."
  (let* ((query "flag:unread AND NOT flag:trashed AND NOT maildir:\"/Archived\"")
         (command (format "mu find '%s' 2>/dev/null | wc -l" query)))
    (string-trim (shell-command-to-string command))))

(after! org-gcal
  (setq org-gcal-client-id (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show services/google/vdirsyncer/ikanello@redhat.com/client-id"))
        org-gcal-client-secret (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show services/google/vdirsyncer/ikanello@redhat.com/secret"))
        org-gcal-file-alist '(("iocanel@gmail.com" .  "~/Documents/org/calendars/personal.org")
                              ("ikanello@redhat.com" . "~/Documents/org/calendars/work.org"))))

(use-package! eaf
  :commands (eaf-open-browser eaf-open find-file)
  :config
  (use-package! ctable)
  (use-package! deferred)
  (use-package! epc)
  ;;
  (require 'eaf-browser))

(use-package! codegpt
  :bind (("C-c g c" . codegpt)
         ("C-c g d" . codegpt-doc)
         ("C-c g e" . codegpt-explain)
         ("C-c g f" . codegpt-fix)
         ("C-c g i" . codegpt-improve))
  :config
    (setq! openai-user (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show services/openai/iocanel/user-id"))
           openai-key (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string "pass show services/openai/iocanel/api-key"))
           codegpt-tunnel 'chat
           codegpt-model "gpt-3.5-turbo"))
