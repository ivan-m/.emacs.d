(TeX-add-style-hook "beamer" 'my-beamer-mode)

(defun my-beamer-mode ()
  "My adds on for when in beamer."

  ;; Tell reftex to treat \lecture and \frametitle as section commands
  ;; so that C-c = gives you a list of frametitles and you can easily
  ;; navigate around the list of frames.
  ;; If you change reftex-section-level, reftex needs to be reset so that
  ;; reftex-section-regexp is correctly remade.
;;;   (require 'reftex)
   (set (make-local-variable 'reftex-section-levels)
        '(("lecture" . 1) ("frametitle" . 2)))
   (reftex-reset-mode)

  ;; add some extra functions.
  (define-key LaTeX-mode-map "\C-cf" 'beamer-template-frame)
  (define-key LaTeX-mode-map "\C-cg" 'beamer-titled-template-frame)
  (define-key LaTeX-mode-map "\C-\M-x" 'tex-frame)
)

(defun tex-frame ()
  "Run pdflatex on current frame.
Frame must be declared as an environment."
  (interactive)
  (let (beg)
    (save-excursion
      (search-backward "\\begin{frame}")
      (setq beg (point))
      (forward-char 1)
      (LaTeX-find-matching-end)
      (TeX-pin-region beg (point))
      (letf (( (symbol-function 'TeX-command-query) (lambda (x) "LaTeX")))
        (TeX-command-region))
        )
      ))


(defun beamer-template-frame ()
  "Create a simple template and move point to after \\frametitle."
  (interactive)
  (LaTeX-environment-menu "frame")
  (insert "\\frametitle{}")
  (backward-char 1))


(defun beamer-titled-template-frame ();title)
  "Prompt for a subsection title and use that for the frame title."
  (interactive) ; "sTitle of the subsection: ")
  (let ((LaTeX-section-hook '(LaTeX-section-title LaTeX-section-section)))
    (LaTeX-section 3))
  (LaTeX-environment-menu "frame")
  (insert "\\frametitle{\\insertsubsection}
")
  (indent-relative))
