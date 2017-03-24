(eval-when-compile (require 'req-package))

(req-package auctex
  :init
  (setq LaTeX-verbatim-environments '("verbatim" "verbatim*" "comment"))
  (setq TeX-PDF-mode t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-auto-untabify t)

  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  :mode
  ("\\.tex\\'" . latex-mode)
  :bind (:map LaTeX-mode-map
         ("\C-c\C-t\C-x" . TeX-toggle-escape))
  :commands
  latex-mode
  LaTeX-mode
  plain-tex-mode
  :config
  (defun TeX-toggle-escape nil (interactive)
         (if (string= LaTeX-command "latex") "latex -shell-escape" "latex")))

(req-package auctex
  :require reftex
  :init (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

(req-package latex
  :require auctex flyspell
  :init (add-hook 'LaTeX-mode-hook 'flyspell-mode))

(req-package auctex
  :require org
  :init (add-hook 'LaTeX-mode-hook 'orgtbl-mode))

(req-package reftex
  :init
  (setq reftex-plug-into-AUCTeX t)

  (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
  (add-hook 'reftex-mode-hook 'imenu-add-menubar-index)
  :commands
  turn-on-reftex)

(req-package preview
  :require auctex latex
  :init
  (setq preview-auto-cache-preamble t)
  (setq preview-default-option-list
        '("displaymath" "floats" "graphics" "textmath" "sections" "footnotes" "showlabels"))
  :commands
  LaTeX-preview-setup)

(req-package latex
  :require auctex preview
  :init (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup))

(provide 'auctex-settings)
