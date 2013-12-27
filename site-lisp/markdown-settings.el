;; (autoload 'markdown-mode "markdown-mode.el"
;;   "Major mode for editing Markdown files" t)
;(markdown-mode-autoloads)
;(markdown-mode+-autoloads)
(setq auto-mode-alist
      (cons '("\\.txt" . markdown-mode)
      (cons '("\\.md" . markdown-mode)
      (cons '("\\.text" . markdown-mode) auto-mode-alist))))

;(load "pandoc-mode")
;(pandoc-mode-autoloads)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

(add-hook 'markdown-mode-hook 'turn-on-pandoc)
(add-hook 'markdown-mode-hook 'flyspell-mode)
