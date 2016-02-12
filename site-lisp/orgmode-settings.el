;; Having problems? Try this: http://emacs.stackexchange.com/a/16616

(eval-after-load 'org
  '(progn
     (require 'org-element) ;; to get org-element--set-regexps
     ;; From http://stackoverflow.com/questions/24169333/how-can-i-emphasize-or-verbatim-quote-a-comma-in-org-mode/24173780
     (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,")
     (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
     ;;(custom-set-variables `(org-emphasis-alist ',org-emphasis-alist)) ;; doesn't seem to be needed
     (org-element--set-regexps)))


;; (require 'org-install)

;;(require 'org-export)
;;(require 'org-e-latex)

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

;; So C-k doesn't kill whole sub-trees.
(setq org-ctrl-k-protect-subtree t)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-cc" 'org-capture) ; clashes with haskell-emacs settings
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; (global-set-key "\C-cv" 'cfw:open-org-calendar)

(setq org-completion-use-ido t)

(setq org-log-done t)

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(setq org-CUA-compatible t)

(add-hook 'org-mode-hook
          (lambda ()
            (when (outline-invisible-p)
              (save-excursion
                (outline-previous-visible-heading 1)
                (org-show-subtree)))))

;; Google calendar integration: down sync only!
;;(require 'calfw-ical)
;;(cfw:open-ical-calendar "https://www.google.com/calendar/ical/ivan.miljenovic%40gmail.com/private-fabd13d01b77518b0d051b81ff77b537/basic.ics")


;; (defun my-open-calendar ()
;;   (interactive)
;;   (cfw:open-calendar-buffer
;;    :contents-sources
;;    (list
;;     (cfw:org-create-source "Green")  ; orgmode source
;;     (cfw:howm-create-source "Blue")  ; howm source
;;     (cfw:cal-create-source "Orange") ; diary source
;;     (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
;;     (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
;;    )))
