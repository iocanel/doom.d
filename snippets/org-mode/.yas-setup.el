(defun my/get-url-from-clipboard ()
  "Check if the clipboard content is a URL and return it."
  (interactive)
  (let ((clipboard-content (x-get-clipboard)))
    (if (string-match-p "\\`\\(http\\|https\\|ftp\\|file\\|news\\|mailto\\):" clipboard-content)
        clipboard-content
      "http://example.com")))
