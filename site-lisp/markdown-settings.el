(eval-when-compile (require 'req-package))

(req-package markdown-mode
  :init
  (setq markdown-command "pandoc -Ss")
  (setq markdown-indent-on-enter nil)
  (setq markdown-italic-underscore t)
  (setq markdown-use-pandoc-style-yaml-metadata t)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (electric-indent-local-mode -1)))
  :commands
  markdown-mode
  :config
  (set-face-attribute 'markdown-code-face nil :background nil)
  :mode
  (("\\.txt" . markdown-mode)
   ("\\.md" . markdown-mode)
   ("\\.text" . markdown-mode)
   ("[cC]hange\\.?[lL]og?\\'" . markdown-mode)))

(req-package markdown-mode
  :require flyspell
  :init (add-hook 'markdown-mode-hook 'flyspell-mode))

(req-package pandoc-mode
  :require
  markdown-mode
  :commands
  pandoc-mode
  :init
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  (add-hook 'markdown-mode-hook 'pandoc-mode))

(req-package mmm-mode
  :init
  (setq mmm-global-mode 'maybe)
  (setq mmm-parse-when-idle t)

  (add-hook 'mmm-haskell-mode-submode-hook 'turn-on-haskell-indent)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (mmm-mode 1)))
  :commands
  mmm-mode
  add-mode-ext-class)

(req-package mmm-pandoc
  :require mmm-mode
  :loader :path
  :init
  (setq mmm-pandoc-prefer-backticks t)
  (setq mmm-parse-when-idle t))

(provide 'markdown-settings)
