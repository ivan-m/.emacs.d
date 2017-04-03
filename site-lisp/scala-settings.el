(eval-when-compile (require 'req-package))

(req-package scala-mode
  :require
  ensime
  company
  :init
  (setq scala-indent:indent-value-expression t)

  (add-hook 'scala-mode-hook
            (lambda ()
              (company-mode)
              (ensime-mode)
              (scala-mode:goto-start-of-code)))
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
    (scala-indent:insert-asterisk-on-multiline-comment)))

(req-package sbt-mode)

(req-package ensime
  ;; Needs to match tool version, so this is easier
  :pin melpa-stable
  :init
  (setq ensime-graphical-tooltips t)
  (setq ensime-auto-generate-config t)
  :commands
  ensime
  ensime-mode)

(provide 'scala-settings)
