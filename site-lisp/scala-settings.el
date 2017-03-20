(eval-when-compile (require 'req-package))

(req-package scala-mode
  :require
  ensime
  company
  :init
  (setq scala-indent:indent-value-expression t)
  :commands
  scala-mode
  :bind
  (:map scala-mode-map
   ("RET" . scala-mode-newline-comments))
  :config
  (defun scala-mode-newline-comments ()
    "Custom newline appropriate for `scala-mode'."
    ;; shouldn't this be in a post-insert hook?
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))

  (add-hook 'scala-mode-hook
            (lambda ()
              (company-mode)
              (ensime-mode)
              (scala-mode:goto-start-of-code))))

(req-package sbt-mode)

(req-package ensime
  ;; Needs to match tool version, so this is easier
  :pin melpa-stable
  :init
  (setq ensime-graphical-tooltips t)
  :commands
  ensime
  ensime-mode)

(provide 'scala-settings)
