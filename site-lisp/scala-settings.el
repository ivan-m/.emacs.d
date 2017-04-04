(eval-when-compile (require 'req-package))

(req-package scala-mode
  :require
  ensime
  company
  :init
  (setq scala-indent:align-parameters t)
  (setq scala-indent:align-forms t)

  (add-hook 'scala-mode-hook
            (lambda ()
              (company-mode 1)
              (flycheck-mode 1)
              (ensime-mode)
              (scala-mode:goto-start-of-code)))
  :commands
  scala-mode
  :interpreter
  ("scala" . scala-mode)
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

(req-package sbt-mode
  :commands
  sbt-start
  sbt-command
  sbt-hydra
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

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
