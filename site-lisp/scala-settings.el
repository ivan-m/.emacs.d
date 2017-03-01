(eval-after-load 'ensime

  (defun scala-mode-newline-comments ()
    "Custom newline appropriate for `scala-mode'."
    ;; shouldn't this be in a post-insert hook?
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))

  (define-key scala-mode-map (kbd "RET") 'scala-mode-newline-comments)

  (add-hook 'scala-mode-hook
            (lambda ()
              (company-mode)
              (ensime-mode)
              (scala-mode:goto-start-of-code))))
