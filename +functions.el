;;; ~/.doom.d/+functions.el -*- lexical-binding: t; -*-

;;
;; Functions
;;
(defun iocanel/flyspell-save-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(defun iocanel/helm-ag-toggle-hidden ()
  (interactive)
  "Toggle -u flag in ag command options."
  (when (not helm-ag-command-option) (setq helm-ag-command-option ""))
  (if (string-match-p (regexp-quote "-u") helm-ag-command-option)
      (setq helm-ag-command-option (replace-regexp-in-string (regexp-quote "-u") "" helm-ag-command-option))
    (setq helm-ag-command-option (string-trim (concat helm-ag-command-option " -u")))))


(defun iocanel/swiper-with-selection (&optional start end)
  "Swiper variation that uses selected text as initial input."
  (interactive (if (use-region-p) (list (region-beginning) (region-end))))
  (if (use-region-p)
      (swiper (buffer-substring start end))
    (swiper))
  (keyboard-escape-quit))

(defun iocanel/split-and-follow-horizontally ()
  "Split window horizontally and then jump to other window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(defun iocanel/split-and-follow-vertically ()
  "Split window verticaly and then jump to other window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))


;; Utils

(defun iocanel/file-as-string (f)
  "Return F content."
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-string)))

(defun iocanel/check-cves ()
  "Update the list of CVE issues"
  (interactive)
  (shell-command "/home/iocanel/scripts/work/collect-cve.sh ~/Documents/work/cve.org")
  (find-file "~/Documents/work/cve.org"))
