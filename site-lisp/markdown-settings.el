(eval-when-compile (require 'req-package))

(req-package markdown-mode
  :init
  (setq markdown-command "pandoc -Ss")
  (setq markdown-indent-on-enter nil)
  (setq markdown-italic-underscore t)
  :commands
  markdown-mode
  :mode
  (("\\.txt" . markdown-mode)
   ("\\.md" . markdown-mode)
   ("\\.text" . markdown-mode)
   ("[cC]hange\\.?[lL]og?\\'" . markdown-mode)))

(req-package markdown-mode
  :requires flyspell
  :config (add-hook 'markdown-mode-hook 'flyspell-mode))

(req-package pandoc
  :requires
  markdown-mode
  :config
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
  (add-hook 'markdown-mode-hook 'pandoc-mode))

(req-package mmm-mode
  :init
  (setq mmm-global-mode 'maybe)
  (setq mmm-parse-when-idle t)
  :commands
  mmm-mode
  :config
  (add-hook 'mmm-haskell-mode-submode-hook 'turn-on-haskell-indent))

(req-package mmm-pandoc
  :require mmm-mode
  :loader :path
  :init
  (setq mmm-pandoc-prefer-backticks t)
  (setq mmm-parse-when-idle t))

(provide 'markdown-settings)
