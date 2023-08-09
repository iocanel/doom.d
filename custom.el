(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval progn
      (setq lsp-session-file
       (concat
        (project-root
         (project-current))
        ".lsp/session")
       lsp-java-workspace-dir
       (concat
        (project-root
         (project-current))
        ".lsp/workspace/data")
       lsp-java-workspace-cache-dir
       (concat
        (project-root
         (project-current))
        ".lsp/workspace/cache"))
      (add-to-list 'eglot-server-programs
       (append
        '(java-mode)
        (my/jdtls-start-command nil))))
     (eval progn
      (setq lsp-session-file
            (concat
             (project-root
              (project-current))
             ".lsp/session")
            lsp-java-workspace-dir
            (concat
             (project-root
              (project-current))
             ".lsp/workspace/data")
            lsp-java-workspace-cache-dir
            (concat
             (project-root
              (project-current))
             ".lsp/workspace/cache"))
      (add-to-list 'eglot-server-programs
                   `(java-mode "jdtls" "-configuration" "/opt/eclipse.jdt.ls/config_linux" "-data" ,lsp-java-workspace-dir)))
     (eval setq-local org-roam-directory
      (locate-dominating-file default-directory ".dir-locals.el"))
     (eval progn
      (setq lsp-session-file
            (concat
             (project-root
              (project-current))
             ".lsp/session")
            lsp-java-workspace-dir
            (concat
             (project-root
              (project-current))
             ".lsp/workspace/data")
            lsp-java-workspace-cache-dir
            (concat
             (project-root
              (project-current))
             ".lsp/workspace/cache")))
     (org-roam-dailies-capture-templates
      ("d" "default" entry "* %?" :target
       (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\12* Training Log\12 #+SESSION: B%(format \"%s\" (length (directory-files \"~/Documents/org/roam-jj/daily\" t \"\\.org$\"))) \12** Techniques\12 %?\12** Rolling\12")))
     (org-roam-directory . "/home/iocanel/Documents/org/roam-jj")
     (eval progn
      (setq lsp-session-file
            (concat
             (projectile-project-root)
             ".lsp/session")
            lsp-java-workspace-dir
            (concat
             (projectile-project-root)
             ".lsp/workspace/data")
            lsp-java-workspace-cache-dir
            (concat
             (projectile-project-root)
             ".lsp/workspace/cache"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ts-fold-replacement-face ((t (:foreground unspecified :box nil :inherit font-lock-comment-face :weight light)))))
(put 'upcase-region 'disabled nil)
