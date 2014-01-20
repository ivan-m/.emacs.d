;; (load "auctex.el" nil t t)
(load "preview.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-auto-untabify t)
;;(list "pdfLaTeX" "pdflatex '\\nonstopmode\\input{%t}'" 'TeX-run-LaTeX nil t)

(require 'reftex)

(add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'reftex-mode-hook 'imenu-add-menubar-index)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'orgtbl-mode)

(setq reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(defun TeX-toggle-escape nil (interactive)
  (if (string= LaTeX-command "latex") "latex -shell-escape" "latex"))

(add-hook 'LaTeX-mode-hook
          (lambda nil
            (define-key LaTeX-mode-map "\C-c\C-t\C-x" 'TeX-toggle-escape)))


(setq LaTeX-section-hook
      '(LaTeX-section-heading
        LaTeX-section-title
        ;; LaTeX-section-toc
        LaTeX-section-section
        LaTeX-section-label))


;; (setq TeX-region "tempTex")

;; Check TeX-style-private for beamer customisations

;; xdvik
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/tex-utils")
;; (require 'xdvi-search)
