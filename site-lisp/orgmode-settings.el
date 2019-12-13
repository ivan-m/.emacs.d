(eval-when-compile (require 'req-package))

;; Having problems? Try this: http://emacs.stackexchange.com/a/16616

(req-package org
  :ensure
  org-plus-contrib
  :init
  (setq org-completion-use-ido t)
  (setq org-log-done t)
  (setq org-CUA-compatible t)
  (setq org-return-follows-link t)
  (setq org-src-tab-acts-natively t)
  ;; So C-k doesn't kill whole sub-trees.
  (setq org-ctrl-k-protect-subtree t)

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
                  (org-show-subtree)))))

  ;; Make windmove work in org-mode:
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  :commands
  org-mode
  :mode
  ("\\.org$" . org-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb))
  :config

  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

  (defadvice org-babel-execute-src-block (around load-language nil activate)
    "Load language if needed"
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      ad-do-it))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (dot . t)
     (shell . t)))

  ;; (require 'org-element) ;; to get org-element--set-regexps
  ;; ;; From http://stackoverflow.com/questions/24169333/how-can-i-emphasize-or-verbatim-quote-a-comma-in-org-mode/24173780
  ;; (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n")
  ;; (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  ;; ;;(custom-set-variables `(org-emphasis-alist ',org-emphasis-alist)) ;; doesn't seem to be needed
  ;; (org-element--set-regexps)

  ;; https://github.com/howardabrams/dot-files/blob/master/emacs-org.org
  (define-key org-mode-map [remap org-return] (lambda () (interactive)
                                                (if (org-in-src-block-p)
                                                    (org-return)
                                                  (org-return-indent))))

  ;; Based upon https://github.com/howardabrams/dot-files/blob/master/emacs-org.org
  (defun paste-html-to-org ()
    "Assumes the contents of the system clip/paste-board to be
HTML, this calls out to `pandoc' to convert it for the org-mode
format."
    (interactive)
    (let* ((clip (if (system-type-is-darwin)
                     "pbpaste -Prefer rts"
                   "xclip -out -selection 'clipboard' -t text/html"))
           (format (if (eq mode-name "Org") "org" "markdown"))
           (pandoc (concat "pandoc -f rts -t " format))
           (cmd    (concat clip " | " pandoc))
           (text   (shell-command-to-string cmd)))
      (kill-new text)
      (yank)))

  ;; https://github.com/howardabrams/dot-files/blob/master/emacs-org.org#better-org-return

  (require 'org-inlinetask)

  (defun ha/org-return (&optional ignore)
    "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
    (interactive "P")
    (if ignore
        (org-return)
      (cond
       ;; Open links like usual
       ((eq 'link (car (org-element-context)))
        (org-return))
       ;; lists end with two blank lines, so we need to make sure we are also not
       ;; at the beginning of a line to avoid a loop where a new entry gets
       ;; created with only one blank line.
       ((and (org-in-item-p) (not (bolp)))
        (if (org-element-property :contents-begin (org-element-context))
            (org-insert-heading)
          (beginning-of-line)
          (setf (buffer-substring
                 (line-beginning-position) (line-end-position)) "")
          (org-return)))
       ((org-at-heading-p)
        (if (not (string= "" (org-element-property :title (org-element-context))))
            (progn (org-end-of-meta-data)
                   (org-insert-heading))
          (beginning-of-line)
          (setf (buffer-substring
                 (line-beginning-position) (line-end-position)) "")))
       ((org-at-table-p)
        (if (-any?
             (lambda (x) (not (string= "" x)))
             (nth
              (- (org-table-current-dline) 1)
              (org-table-to-lisp)))
            (org-return)
          ;; empty row
          (beginning-of-line)
          (setf (buffer-substring
                 (line-beginning-position) (line-end-position)) "")
          (org-return)))
       (t
        (org-return)))))

  (defun scimax/org-return (&optional ignore)
    "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
    (interactive "P")
    (if ignore
        (org-return)
      (cond

       ((eq 'line-break (car (org-element-context)))
        (org-return-indent))

       ;; Open links like usual, unless point is at the end of a line.
       ;; and if at beginning of line, just press enter.
       ((or (and (eq 'link (car (org-element-context))) (not (eolp)))
            (bolp))
        (org-return))

       ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
       ;; Johansson!
       ((org-inlinetask-in-task-p)
        (org-return))

       ;; checkboxes too
       ((org-at-item-checkbox-p)
        (org-insert-todo-heading nil))

       ;; lists end with two blank lines, so we need to make sure we are also not
       ;; at the beginning of a line to avoid a loop where a new entry gets
       ;; created with only one blank line.
       ((org-in-item-p)
        (if (save-excursion (beginning-of-line) (org-element-property :contents-begin (org-element-context)))
            (org-insert-item)
          (beginning-of-line)
          (delete-region (line-beginning-position) (line-end-position))
          (org-return)))

       ;; org-heading
       ((org-at-heading-p)
        (if (not (string= "" (org-element-property :title (org-element-context))))
            (progn (org-end-of-meta-data)
                   (org-insert-heading-respect-content)
                   (outline-show-entry))
          (beginning-of-line)
          (setf (buffer-substring
                 (line-beginning-position) (line-end-position)) "")))

       ;; tables
       ((org-at-table-p)
        (if (-any?
             (lambda (x) (not (string= "" x)))
             (nth
              (- (org-table-current-dline) 1)
              (org-table-to-lisp)))
            (org-return)
          ;; empty row
          (beginning-of-line)
          (setf (buffer-substring
                 (line-beginning-position) (line-end-position)) "")
          (org-return)))

       ;; fall-through case
       (t
        (org-return)))))

  (define-key org-mode-map (kbd "RET")  'scimax/org-return)
  )

(req-package org-table
  :ensure
  org-plus-contrib
  :require
  org
  :commands
  turn-on-orgtbl
  orgtbl-mode
  :config
  (defun org-table-transform-in-place ()
    "Just like `ORG-TABLE-EXPORT', but instead of exporting to a
  file, replace table with data formatted according to user's
  choice, where the format choices are the same as
  org-table-export."
    (interactive)
    (unless (org-at-table-p) (user-error "No table at point"))
    (org-table-align)
    (let* ((format
            (completing-read "Transform table function: "
                             '("orgtbl-to-tsv" "orgtbl-to-csv" "orgtbl-to-latex"
                               "orgtbl-to-html" "orgtbl-to-generic"
                               "orgtbl-to-texinfo" "orgtbl-to-orgtbl"
                               "orgtbl-to-unicode")))
           (curr-point (point)))
      (if (string-match "\\([^ \t\r\n]+\\)\\( +.*\\)?" format)
          (let ((transform (intern (match-string 1 format)))
                (params (and (match-end 2)
                             (read (concat "(" (match-string 2 format) ")"))))
                (table (org-table-to-lisp
                        (buffer-substring-no-properties
                         (org-table-begin) (org-table-end)))))
            (unless (fboundp transform)
              (user-error "No such transformation function %s" transform))
            (save-restriction
              (with-output-to-string
                (delete-region (org-table-begin) (org-table-end))
                (insert (funcall transform table params) "\n")))
            (goto-char curr-point)
            (beginning-of-line)
            (message "Tranformation done."))
        (user-error "Table export format invalid"))))

  (define-key org-mode-map (kbd "\C-x |") 'org-table-transform-in-place))

(req-package csv-mode
  :ensure
  nil
  :require
  org
  org-table
  :init
  (add-hook 'csv-mode-hook 'turn-on-orgtbl))

;; (req-package org-pandoc)

(req-package outline-magic
  :commands
  outline-cycle
  outline-next-line
  outline-move-subtree-up
  outline-move-subtree-down
  outline-promote
  outline-demote)

;; (req-package ob
;;   :ensure nil :require org)
(req-package ob-emacs-lisp
  :ensure
  nil
  :require
  org
  :commands
  org-babel-execute:emacs-lisp
  org-babel-expand-body:emacs-lisp
  org-babel-execute:elisp
  org-babel-expand-body:elisp)
(req-package ob-shell
  :ensure
  nil
  :require
  org
  :commands
  org-babel-execute:shell
  org-babel-expand-body:shell)
(req-package ob-sql
  :ensure
  nil
  :require
  org
  :commands
  org-babel-execute:sql
  org-babel-expand-body:sql)
(req-package ob-dot
  :ensure
  nil
  :require
  org
  :commands
  org-babel-execute:dot
  org-babel-expand-body:dot)
(req-package ob-sql-mode
  :commands
  org-babel-execute:sql-mode
  org-babel-expand-body:sql-mode)
(req-package org-beautify-theme
  :init
  (setq org-beautify-theme-use-box-hack nil))
(req-package org-bullets
  :init
  (add-hook 'after-init-hook
            (lambda ()
              (if (display-graphic-p)
                  (my/init-org-bullets)
                (add-hook 'after-make-frame-functions 'my/init-org-bullets))))
  :config
  (defun my/init-org-bullets (&optional frame)
    (when (display-graphic-p frame)
      (when frame (select-frame frame))
      (add-hook 'org-mode-hook 'org-bullets-mode)
      (remove-hook 'after-make-frame-functions 'my/init-org-bullets))))

(req-package org-rich-yank
  :commands
  org-rich-yank
  :bind
  (:map org-mode-map
   ("C-M-y" . org-rich-yank)))

(provide 'orgmode-settings)
