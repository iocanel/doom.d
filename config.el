;;; .doom.d/config.el -*- lexical-binding: t; -*-


;; Place your private configuration here

(load! "+functions")
(load! "+bindings")
(load! "+org")
(load! "+email")


;;
;; Dashboard
;;

;; Dashboard customization. Credits: https://github.com/ragone/.doom.d/blob/master/config.org
(defun iocanel/agenda ()
  "My org agenda."
  (interactive)
  (org-agenda nil "n"))

(setq +doom-dashboard-banner-padding '(0 . 0)
      +doom-dashboard-menu-sections
      '(("Agenda"
         :icon (all-the-icons-octicon "calendar" :face 'font-lock-keyword-face)
         :when (fboundp 'org-agenda)
         :face (:inherit (font-lock-keyword-face bold))
         :action iocanel/agenda)
        ("Email"
         :icon (all-the-icons-octicon "mail" :face 'font-lock-keyword-face)
         :action =mu4e))
      +doom-dashboard-banner-file "emacs.png"
      +doom-dashboard-banner-dir "~/.doom.d/"
      +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        doom-dashboard-widget-shortmenu
        doom-dashboard-widget-loaded
        doom-dashboard-widget-footer))


;;
;; Editor
;;

(setq display-line-numbers-type 'relative)
;; Flyespell
(add-hook! 'text-mode-hook #'flyspell-mode)
(add-hook! 'prog-mode-hook #'flyspell-prog-mode)


;;
;; Projectile
;;
(after! projectile
        (setq projectile-project-root-files-bottom-up '(".git" ".projectile")))

;;
;; LSP Java
;;
(setq lsp-java-vmargs '("-noverify" "-Xmx2G" "-XX:+UseG1GC" "-XX:+UseStringDeduplication")
      lsp-java-save-action-organize-imports nil
      lsp-java-maven-download-sources t
      lsp-java-autobuild-enabled nil
      lsp-java-import-gradle-enabled nil
      lsp-inhibit-message t
      lsp-java-format-on-type-enabled nil
      lsp-java-completion-guess-arguments t
      lsp-java-completion-overwrite nil
      lsp-enable-file-watchers nil
      lsp-prefer-capf t
      lsp-idle-delay 0.500
      company-minimum-prefix-length 1
      company-idle-delay 0.0
      lsp-keymap-prefix "C-l"
      c-basic-offset 2
      tab-width 2
      indent-tabs-mode nil
      read-process-output-max (* 3(* 1024 1024)))

(setq dap-java-test-runner (locate-user-emacs-file ".local/etc/eclipse.jdt.ls/server/test-runner/junit-platform-console-standalone.jar") )

(defun iocanel/java-config()
  "Configure fringe and dap-mode."
  (setq left-fringe-width 30
        right-fringe-width 0)
  (define-key dap-mode-map [left-fringe mouse-1] 'dap-breakpoint-toggle)
  (define-key dap-mode-map [left-fringe mouse-3] 'dap-breakpoint-condition)
  (dap-mode t)
  (dap-ui-mode t))

(add-hook! 'java-mode-hook 'iocanel/java-config)

(defun iocanel/lsp-workspace-clear ()
  "Wipe lsp workspace."
  (interactive)
  (setq lsp--session (make-lsp-session))
  (lsp-restart-workspace))

;;
;; IDEE
;;
(run-with-idle-timer 1 nil #'idee-init)
(run-with-idle-timer 1 nil #'idee-lsp-init)
(run-with-idle-timer 1 nil #'idee-java-init)

;;  Disable Doom Java templates
(setq +file-templates-alist (delq (assoc  "/main\\.java$" +file-templates-alist) +file-templates-alist))
(setq +file-templates-alist (delq (assoc  "/src/.+\\.java$" +file-templates-alist) +file-templates-alist))

;;
;; Treemacs
;;
(after! treemacs
  (setq treemacs-follow-after-init          t
        treemacs-recenter-after-project-jump 'always
        treemacs-recenter-after-file-follow 'always)

  (treemacs-follow-mode t)
  (treemacs-fringe-indicator-mode t)
  (map! "M-0" #'treemacs-select-window)
  (bind-keys ("M-0" . treemacs-select-window))
  (map! :desc "Capture Inbox" :o "M0" 'treemacs-select-window))

;;
;; Forge
;;

;; Let's overide the way that handle the token
(after! forge
  (defun ghub--token (host username package &optional nocreate forge)
    (replace-regexp-in-string "\n\\'" ""  (shell-command-to-string (format "pass show %s/%s" host username)))))


;;
;; Python
;;
(setq python-shell-interpreter "python3.7")


;;
;; Asciidoc
;;
(after! adoc-mode
  (add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))
  (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode)))


;;
;; Flutter
;;

;;;###autoload
(defun iocanel/android-start-emulator ()
  "Require android-mode and start emulator"
  (require 'android-mode)
  (android-start-emulator))

(use-package! dart-mode
  :init
  (setq lsp-dart-analysis-sdk-dir "~/tools/flutter/bin/cache/dart-sdk/"
        lsp-dart-sdk-dir "~/tools/flutter/bin/cache/dart-sdk")
  :hook ((dart-mode . smartparens-mode)
         (dart-mode . lsp)))

(use-package! flutter
  :after dart-mode
  :init
  (setq flutter-sdk-path "~/tools/flutter/")
  :bind
  (:map dart-mode-map
        ("C-M-x" . #'flutter-run-or-hot-reload)))
