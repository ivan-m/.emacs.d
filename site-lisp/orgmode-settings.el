(eval-when-compile (require 'req-package))

;; Having problems? Try this: http://emacs.stackexchange.com/a/16616

(req-package org
  :require
  org-plus-contrib
  :init
  (setq org-completion-use-ido t)
  (setq org-log-done t)
  (setq org-CUA-compatible t)
  ;; So C-k doesn't kill whole sub-trees.
  (setq org-ctrl-k-protect-subtree t)
  :comands
  turn-on-orgtbl
  org-mode
  orgtbl-mode
  :mode
  ("\\.org$" . org-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb))
  :config

  (require 'org-element) ;; to get org-element--set-regexps
  ;; From http://stackoverflow.com/questions/24169333/how-can-i-emphasize-or-verbatim-quote-a-comma-in-org-mode/24173780
  (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  ;;(custom-set-variables `(org-emphasis-alist ',org-emphasis-alist)) ;; doesn't seem to be needed
  (org-element--set-regexps)

  ;; Make windmove work in org-mode:
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)


  ;; Font screws up in --daemon mode
  (defun org-column-view-uses-fixed-width-face ()
    ;; copy from org-faces.el
    (when (fboundp 'set-face-attribute)
      ;; Make sure that a fixed-width face is used when we have a column
      ;; table.
      (set-face-attribute 'org-column nil
                          :height (face-attribute 'default :height)
                          :family (face-attribute 'default :family))))

  (when (and (fboundp 'daemonp) (daemonp))
    (add-hook 'org-mode-hook 'org-column-view-uses-fixed-width-face))

  (add-hook 'org-mode-hook
            (lambda ()
              (when (outline-invisible-p)
                (save-excursion
                  (outline-previous-visible-heading 1)
                  (org-show-subtree))))))

(req-package org-pandoc)

(req-package outline-magic)

(provide 'orgmode-settings)
