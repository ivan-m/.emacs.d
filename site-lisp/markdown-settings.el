;; (autoload 'markdown-mode "markdown-mode.el"
;;   "Major mode for editing Markdown files" t)
;;(markdown-mode-autoloads)
;;(markdown-mode+-autoloads)
(setq auto-mode-alist
      (cons '("\\.txt" . markdown-mode)
      (cons '("\\.md" . markdown-mode)
      (cons '("\\.text" . markdown-mode)
      (cons '("[cC]hange\\.?[lL]og?\\'" . markdown-mode) auto-mode-alist)))))

(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'turn-on-orgtbl)

(require 'mmm-mode)
(require 'mmm-pandoc)
(add-hook 'mmm-haskell-mode-submode-hook 'turn-on-haskell-simple-indent)
