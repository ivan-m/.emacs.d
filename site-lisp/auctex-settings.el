(eval-when-compile (require 'req-package))

(req-package auctex
  :init
  (setq LaTeX-verbatim-environments '("verbatim" "verbatim*" "comment"))
  (setq TeX-PDF-mode t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-auto-untabify t)
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
         (if (string= LaTeX-command "latex") "latex -shell-escape" "latex"))
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode))

(req-package auctex
  :requires reftex
  :config (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

(req-package auctex
  :requires flyspell
  :config (add-hook 'LaTeX-mode-hook 'flyspell-mode))

(req-package auctex
  :requires org
  :config (add-hook 'LaTeX-mode-hook 'orgtbl-mode))

(req-package auctex
  :requires preview
  :config (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup))

(req-package reftex
  :init
  (setq reftex-plug-into-AUCTeX t)
  :commands
  turn-on-reftex
  :config
  (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
  (add-hook 'reftex-mode-hook 'imenu-add-menubar-index))

(req-package preview
  :init
  (setq preview-auto-cache-preamble t)
  (setq preview-default-option-list
        '("displaymath" "floats" "graphics" "textmath" "sections" "footnotes" "showlabels"))
  :commands
  LaTeX-preview-setup)

(provide 'auctex-settings)
