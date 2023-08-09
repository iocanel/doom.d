(defun my/plantuml-get-entity-names ()
  "Get all the entity names of the current buffer"
  (let ((entity-regexp "entity[[:space:]\n]+\"?\\(.*?\\)\\(\\s-*as[[:space:]]+\\([^[:space:]\n]+\\)\\)?\"?[[:space:]\n]*\\({\\|$\\)"))
    (save-excursion
      (goto-char (point-min))
      (let (names)
        (while (re-search-forward entity-regexp nil t)
          (push (or (match-string 3) (match-string 1)) names))
        names))))

(defun my/read-lines ()
  "Read the current buffer as lines."
  (let ((lines '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (push (buffer-substring-no-properties (line-beginning-position) (line-end-position)) lines)
        (forward-line)))
    (reverse lines)))

(defun my/first-word-matches-p (str arg)
  "Return non-nil if STR starts has ARG as the first word"
  (let ((first-word (car (split-string str))))
    (cond
     ((stringp arg)
      (string= first-word arg))
     ((listp arg)
      (member first-word arg))
     (t nil))))

(defun my/get-last-alphanumeric (str)
  "Return the last alphanumeric of STR"
 (when (string-match "\\([[:alnum:]]+\\)\\([^[:alnum:]]*\\)\\'" str)
    (match-string 1 str)))

(defun my/plantuml-get-component-names (&optional component-type)
  "Get all the component names of the current buffer"
  (let ((component-type (or component-type '("rectangle" "square" "circle" "actor" "node" "collection" "package" "folder" "database" "queue" "entity"))))
    (mapcar #'my/get-last-alphanumeric (seq-filter (lambda (l) (my/first-word-matches-p l component-type)) (my/read-lines)))))
