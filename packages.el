;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

;;
;; Core
;;

;; Asciidoc
(package! adoc-mode)
(package! org-asciidoc :recipe (:host github :repo "yashi/org-asciidoc" :files ("ox-asciidoc.el")))

;; Helm
(package! helm :pin "1003539c2ec66e7697903f4b4cb354adea70040e")

;; Dired
(package! dired-subtree)

;; Org
(package! org-ql)
(package! org-drill)
(package! org-super-agenda)
(package! org-babel-eval-in-repl)
(package! org-sync :recipe (:host github :repo "arbox/org-sync" :files ("org-sync.el" "org-sync-github.el")))
(package! org-github-issues :recipe (:host github :repo "iensu/org-github-issues"))
(package! org-jira :recipe (:host github :repo "ahungry/org-jira"))
(package! org-transclusion)

;; Email
;;(package! mu4e :recipe (:host github :repo "djcb/mu" :branch "v1.8.13" :files (:defaults "mu4e/*.el")))

;; IDE
(package! idee :recipe (:host github :repo "iocanel/idee" :files (:defaults "*.el")))

;; Copilot
(package! copilot :recipe (:host github :repo "zerolfx/copilot.el" :files ("dist" "copilot.el")))

;;
;; Utils
;;
(package! quickmarks :recipe (:host github :repo "iocanel/quickmarks.el" :files ("quickmarks.el")))
(package! imgflip :recipe (:host github :repo "iocanel/imgflip.el" :files ("imgflip.el")))
(package! openwith)
(package! mpv)

;;
;; OpenAI
;;
(package! openai :recipe (:host github :repo "emacs-openai/openai"))
(package! chatgpt :recipe (:host github :repo "emacs-openai/chatgpt"))
(package! codegpt :recipe (:host github :repo "emacs-openai/codegpt"))


;;
;; EAF (Emacs Application Framework)
;;

(when (package! eaf :recipe (:host github
                             :repo "emacs-eaf/emacs-application-framework"
                             :files ("*.el" "*.py" "core" "app" "*.json")
                             :build (:not compile)
                             :pre-build (("python" "install-eaf.py" "--install" "pdf-viewer" "browser" "--ignore-sys-deps"))))

  (package! eaf-org-previewer :recipe (:host github :repo "emacs-eaf/eaf-org-previewer"))
  (package! eaf-markdown-previewer :recipe (:host github :repo "emacs-eaf/eaf-markdown-previewer"))

  (package! ctable :recipe (:host github :repo "kiwanami/emacs-ctable"))
  (package! deferred :recipe (:host github :repo "kiwanami/emacs-deferred"))
  (package! epc :recipe (:host github :repo "kiwanami/emacs-epc")))

;;
;; Troubleshooting
;;
(package! afternoon-theme :recipe (:host github :repo "osener/emacs-afternoon-theme"))
