;;; ~/.doom.d/bindings.el -*- lexical-binding: t; -*-

; Window creattion
(global-set-key (kbd "C-x 2") 'iocanel/split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'iocanel/split-and-follow-vertically)

(map! "M-0" #'treemacs-select-window)
(map! "M-o" #'other-window)
(map! "M-s" #'avy-goto-char)

(map! "C-s" #'iocanel/swiper-with-selection)
(map! "C-q" #'er/expand-region)
(map! "C-c m" #'mark-next-like-this)

(map! "C-x C-x" #'kill-current-buffer)


;;
;; Org-mode
;;
(define-key evil-normal-state-map (kbd "SPC o t") #'org-insert-structure-template)

; Flyspell
(after! flyspell
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous))

; Multiedit
(after! evil-multiedit
  ;; Evil multiedit
  ;; Highlights all matches of the selection in the buffer.
  (define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

  ;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
  ;; incrementally add the next unmatched match.
  (define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
  ;; Match selected region.
  (define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
  ;; Insert marker at point
  (define-key evil-insert-state-map (kbd "M-d") 'evil-multiedit-toggle-marker-here)

  ;; Same as M-d but in reverse.
  (define-key evil-normal-state-map (kbd "M-e") 'evil-multiedit-match-and-prev)
  (define-key evil-visual-state-map (kbd "M-e") 'evil-multiedit-match-and-prev)

  ;; OPTIONAL: If you prefer to grab symbols rather than words, use
  ;; `evil-multiedit-match-symbol-and-next` (or prev).

  ;; Restore the last group of multiedit regions.
  (define-key evil-visual-state-map (kbd "C-M-e") 'evil-multiedit-restore)

  ;; RET will toggle the region under the cursor
  (define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

  ;; ...and in visual mode, RET will disable all fields outside the selected region
  (define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

  ;; For moving between edit regions
  (define-key evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
  (define-key evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
  (define-key evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
  (define-key evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev)

  ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match))

;; Dap
(define-key evil-normal-state-map (kbd "SPC d") #'dap-hydra/body)
(define-key evil-normal-state-map (kbd "SPC D") #'dap-debug)
