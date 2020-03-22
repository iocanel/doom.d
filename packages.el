;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)


;;
;; Core Packages
;;
(package! swiper)
(package! avy)
(package! mark-multiple)

;;
;; Modes
;;

(package! adoc-mode)

;(package! projectile :pin "748b2a584e13b03e8adc51ab901d036d75038408")
;(package! treemacs :pin "71a01f409a319d57eb3832e93e8a412fbc9d7a65")
;(package! treemacs-persp :pin "71a01f409a319d57eb3832e93e8a412fbc9d7a65")
;(package! treemacs-evil :pin "71a01f409a319d57eb3832e93e8a412fbc9d7a65")
;(package! treemacs-projectile :pin "71a01f409a319d57eb3832e93e8a412fbc9d7a65")
;(package! treemacs-magit :pin "71a01f409a319d57eb3832e93e8a412fbc9d7a65")

;;
;; Org Mode
;;
;(package! org-super-agenda)
(package! org-drill)
(package! org-links)
(package! org-roam :recipe (:host github :repo "jethrokuan/org-roam"))
;(package! org-gcal :recipe (:host github :repo "iocanel/org-gcal.el"))

;;
;; Mu4e
;;
(package! mu4e-alert)

;;
;; Development
;;

;; IDEE
;; Requirement:
(package! async-await)
(package! dap-mode)
(package! demo-it)
(package! editorconfig)
(package! queue)
(package! helm-ag)
(package! helm-projectile)
(package! helm-lsp)

;; Packages:
(package! idee :recipe (:host github :repo "iocanel/idee"))
(package! idee-lsp :recipe (:host github :repo "iocanel/idee"))
(package! idee-java :recipe (:host github :repo "iocanel/idee"))

;;
;; Flutter
;;
(package! dart-mode)
(package! flutter)
