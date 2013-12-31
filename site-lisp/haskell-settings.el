(require 'haskell-mode)
(require 'haskell-simple-indent)
(require 'haskell-interactive-mode)
(require 'haskell-checkers)

(require 'shm)

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Make flycheck aware of sandboxes.

(require 'flycheck)

; From https://github.com/gridaphobe/dotfiles/blob/master/emacs.d/config/my-haskell.el
(defun my/find-cabal-sandbox-pkg-db ()
  (let* ((cabal-file (haskell-cabal-find-file))
         (cabal-root (when cabal-file (file-name-directory cabal-file)))
         (pkg-db (file-expand-wildcards
                  (concat cabal-root ".cabal-sandbox/*.conf.d"))))
    (if pkg-db (car pkg-db))))

(defvar flycheck-haskell-options
  '("-Wall" "-isrc" "-fno-warn-missing-signatures"))

(flycheck-define-checker haskell-hdevtools
  "A Haskell syntax and type checker using hdevtools.

See URL `https://github.com/bitc/hdevtools'."
  :command
  ("hdevtools" "check"
   (eval (apply #'append (mapcar (lambda (o) (list "-g" o)) flycheck-haskell-options)))
   (eval (let ((pkg-db (my/find-cabal-sandbox-pkg-db)))
           (if pkg-db
               (list "-g" "-package-db" "-g" pkg-db))))
   source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ":"
            (or " " "\n ") "Warning:" (optional "\n")
            (one-or-more " ")
            (message (one-or-more not-newline)
                     (zero-or-more "\n"
                                   (one-or-more " ")
                                   (one-or-more not-newline)))
            line-end)
   (error line-start (file-name) ":" line ":" column ":"
          (or (message (one-or-more not-newline))
              (and "\n" (one-or-more " ")
                   (message (one-or-more not-newline)
                            (zero-or-more "\n"
                                          (one-or-more " ")
                                          (one-or-more not-newline)))))
          line-end))
  :modes haskell-mode
  :next-checkers ((warnings-only . haskell-hlint)))

(flycheck-define-checker my-haskell-ghc
  "A Haskell syntax and type checker using ghc.

See URL `http://www.haskell.org/ghc/'."
   :command ("ghc" (eval flycheck-haskell-options)
             (eval (let ((pkg-db (my/find-cabal-sandbox-pkg-db)))
                     (if pkg-db (list "-package-db" pkg-db))))
             source-inplace)
   :error-patterns
   ((warning line-start (file-name) ":" line ":" column ":"
             (or " " "\n ") "Warning:" (optional "\n")
             (one-or-more " ")
             (message (one-or-more not-newline)
                      (zero-or-more "\n"
                                    (one-or-more " ")
                                    (one-or-more not-newline)))
             line-end)
    (error line-start (file-name) ":" line ":" column ":"
           (or (message (one-or-more not-newline))
               (and "\n" (one-or-more " ")
                    (message (one-or-more not-newline)
                             (zero-or-more "\n"
                                           (one-or-more " ")
                                           (one-or-more not-newline)))))
           line-end))
   :modes haskell-mode
   :next-checkers ((warnings-only . haskell-hlint)))

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; Need to find out how to do this without a require.
;; (require 'auto-complete)
;; (ac-define-source ghc-mod
;;   '((depends ghc)
;;     (candidates . (ghc-select-completion-symbol))
;;     (symbol . "s")
;;     (cache)))

(add-hook 'haskell-mode-hook 'haskell-hook)
;(add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)
;(add-hook 'haskell-interactive-mode-hook 'haskell-interactive-hook)

(defun haskell-hook ()
  (structured-haskell-mode 1)
  (turn-on-haskell-simple-indent)

  (capitalized-words-mode)

  (turn-on-haskell-decl-scan)

  ;(ghc-init)

  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face prepend)))

  (flyspell-prog-mode)

  (auto-insert-mode 1)

  ;(setq ac-sources '(ac-source-words-in-same-mode-buffers
  ;                   ac-source-dictionary
  ;                   ac-source-ghc-mod))
  )

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; Based upon http://www.serpentine.com/blog/2007/10/09/using-emacs-to-insert-scc-annotations-in-haskell-code/

(defun toggle-scc-at-point (&optional arg)
  "Insert or kill (with universal-argument) an SCC annotation at
point."
  (interactive "P")
  (if (equal arg nil)
      (insert-scc-at-point)
    (kill-scc-at-point)))

(defun insert-scc-at-point ()
  "Insert an SCC annotation at point."
  (interactive)
  (if (or (looking-at "\\b\\|[ \t]\\|$") (and (not (bolp))
                                              (save-excursion
                                                (forward-char -1)
                                                (looking-at "\\b\\|[ \t]"))))
      (let ((space-at-point (looking-at "[ \t]")))
        (unless (and (not (bolp)) (save-excursion
                                    (forward-char -1)
                                    (looking-at "[ \t]")))
          (insert " "))
        (insert "{-# SCC \"\" #-}")
        (unless space-at-point
          (insert " "))
        (forward-char (if space-at-point -5 -6)))
    (error "Not over an area of whitespace")))

(defun kill-scc-at-point ()
  "Kill the SCC annotation at point."
  (interactive)
  (save-excursion
    (let ((old-point (point))
          (scc "\\({-#[ \t]*SCC \"[^\"]*\"[ \t]*#-}\\)[ \t]*"))
      (while (not (or (looking-at scc) (bolp)))
        (forward-char -1))
      (if (and (looking-at scc)
               (<= (match-beginning 1) old-point)
               (> (match-end 1) old-point))
          (kill-region (match-beginning 0) (match-end 0))
        (error "No SCC at point")))))

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; https://github.com/chrisdone/chrisdone-emacs/blob/master/config/haskell.el

(defun haskell-insert-doc ()
  "Insert the documentation syntax."
  (interactive)
  (insert "-- | "))

(defun haskell-insert-undefined ()
  "Insert undefined."
  (interactive)
  (if (and (boundp 'structured-haskell-mode)
           structured-haskell-mode)
      (shm-insert-string "undefined")
    (insert "undefined")))

(defun haskell-move-right ()
  (interactive)
  (haskell-move-nested 1))

(defun haskell-move-left ()
  (interactive)
  (haskell-move-nested -1))

(defun haskell-who-calls (&optional prompt)
  "Grep the codebase to see who uses the symbol at point."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (let ((existing (get-buffer "*who-calls*")))
      (when existing
        (kill-buffer existing)))
    (let ((buffer
           (grep-find (format "cd %s && find . -name '*.hs' -exec grep -inH -e %s {} +"
                              (haskell-session-current-dir (haskell-session))
                              sym))))
      (with-current-buffer buffer
        (rename-buffer "*who-calls*")
        (switch-to-buffer-other-window buffer)))))

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(require 'skeleton)
(require 'autoinsert)

; Skeletons
(define-skeleton haskell-module-skeleton
  "Haskell hs file header"
  "Brief description: "
  "{- \|\n"
  '(setq module-name (haskell-guess-module-name))
  "   Module      : " module-name "\n"
  "   Description : " str | (concat "The \"" module-name "\" module") "\n"
  "   Copyright   : (c) Ivan Lazar Miljenovic\n"
  "   License     : 3-Clause BSD-style\n"
  "   Maintainer  : Ivan.Miljenovic@gmail.com\n"
  "\n"
  "   " _ "\n"
  "\n"
  " -}\n"
  "module " module-name " where\n\n")

(add-to-list 'auto-insert-alist '("\\.hs\\'" . haskell-module-skeleton))

; code block support in latex haskell

; (require 'markdown-code-mode)
; (add-to-list 'auto-mode-alist '("\\.lhs" . markdown-code-mode))

;; (add-hook 'literate-haskell-mode-hook
;;           (lambda ()
;;             (turn-on-pandoc)
;;             (turn-on-haskell-indent)
;;             (fset 'haskell-latex-code-block
;;                   [?\\ ?b ?e ?g ?i ?n ?{ ?c ?o ?d ?e ?} return return return return ?\\ ?e ?n ?d ?{ ?c ?o ?d ?e ?} up up])
;;             (global-set-key [(control ?c) ?e] 'haskell-latex-code-block)
;;             ))



;; (autoload 'haskell-indentation-mode "haskell-indentation"
;;   "Major mode for editing Haskell scripts - kuribas' indentation mode." t)

                                        ; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
                                        ; (add-hook 'haskell-mode-hook (lambda nil 'haskell-indentation-mode)) ;;'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; (setq inferior-haskell-find-project-root nil)
;; (add-hook 'haskell-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c C-r") 'inferior-haskell-reload-file)
;;             (local-set-key (kbd "C-c h") 'haskell-hoogle)
;;             (setq comment-padding " ")
;;             (setq comment-start "--")
;;             (setq comment-end-skip " *\\(-}\\)?")
;;             (auto-fill-mode 1)
;;             (set (make-local-variable
;;                   'fill-nobreak-predicate)
;;                  (lambda ()
;;                    (not (eq (get-text-property (point) 'face)
;;                             'font-lock-comment-face))))
;;             ))

                                        ; from http://www.emacswiki.org/emacs/AutoFillMode
;; (add-hook 'haskell-mode-hook
;;           (lambda ()
;;             (auto-fill-mode 1)
;;             (set (make-local-variable 'fill-nobreak-predicate)
;;                  (lambda ()
;;                    (not (or (eq (get-text-property (point) 'face)
;;                                 'font-lock-comment-face)
;;                             (eq (get-text-property (point) 'face)
;;                                 'font-lock-doc-face)))))))

                                        ;(require 'cabal-mode)

;; (add-hook 'haskell-mode-hook
;;           #'(lambda ()
;;               (setq comment-padding " ")
;;               (setq comment-start "--")))

;; (add-hook 'haskell-mode-hook
;;           (lambda ()
;;             (auto-fill-mode 1)
;;             (set (make-local-variable
;;                   'fill-nobreak-predicate)
;;                  (lambda ()
;;                    (not (eq (get-text-property (point) 'face)
;;                             'font-lock-comment-face))))))


(add-hook 'literate-haskell-mode-hook
          (lambda ()
            (turn-on-pandoc)
            (turn-on-haskell-indent)
            (fset 'haskell-latex-code-block
                  [?\\ ?b ?e ?g ?i ?n ?{ ?c ?o ?d ?e ?} return return return return ?\\ ?e ?n ?d ?{ ?c ?o ?d ?e ?} up up])
            (global-set-key [(control ?c) ?e] 'haskell-latex-code-block)
            ))

;; (require 'mmm-auto)
;; (require 'mmm-haskell)
;; (setq mmm-global-mode 'maybe)
;; (add-to-list 'mmm-mode-ext-classes-alist
;;              '(markdown-mode "\\.lhs$" haskell))

;; (add-to-list 'auto-mode-alist '("\\.lhs" . markdown-mode))

;; (require 'mumamo-markdown)

(autoload 'markdown-code-mode "markdown-code-mode" "" t)
(add-to-list 'auto-mode-alist '("\\.lhs" . markdown-code-mode))

;; (add-to-list 'auto-mode-alist '("\\.lhs" . literate-haskell-mode))

;; (add-to-list 'auto-mode-alist '("\\.lhs" . markdown-mumamo-mode))

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; Based upon https://github.com/paul7/dev-conf/blob/master/.emacs-haskell
(defvar cabal-use-sandbox t)
; (setq-default haskell-program-name "ghci")
(defun cabal-toggle-sandboxing-local ()
  (interactive)
  (set (make-local-variable 'cabal-use-sandbox) (not cabal-use-sandbox))
  (message (format "This buffer haskell-process-type is ``%s''"
                   (set (make-local-variable 'haskell-process-type)
                        (if cabal-use-sandbox
                            'cabal-repl
                          'ghci)))))

(defun cabal-toggle-sandboxing ()
  (interactive)
  (setq cabal-use-sandbox (not cabal-use-sandbox))
  (message (format "haskell-process-type is ``%s''"
                   (setq haskell-process-type
                        (if cabal-use-sandbox
                            'cabal-repl
                          'ghci)))))
; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Keybindings


(define-key haskell-mode-map (kbd "<return>") 'haskell-simple-indent-newline-same-col)
(define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)
(define-key haskell-mode-map [?\C-c ?\C-l] 'haskell-process-load-file)
(define-key haskell-mode-map [?\C-c ?\C-r] 'haskell-process-reload-file)
(define-key haskell-mode-map [f5] 'haskell-process-load-or-reload)

;; Switch to the REPL.
(define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
;; "Bring" the REPL, hiding all other windows apart from the source
;; and the REPL.
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

;; Build the Cabal project.
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;; Interactively choose the Cabal command to run.
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

;; Get the type and info of the symbol at point, print it in the
;; message buffer.
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)

;; Contextually do clever things on the space key, in particular:
;;   1. Complete imports, letting you choose the module name.
;;   2. Show the type of the symbol after the space.
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

;; Jump to the imports. Keep tapping to jump between import
;; groups. C-u f8 to jump back again.
(define-key haskell-mode-map [f8] 'haskell-navigate-imports)

;; Jump to the definition of the current symbol.
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

(define-key haskell-mode-map (kbd "M-,") 'haskell-who-calls)

;; Move the code below the current nesting left one.
(define-key haskell-mode-map (kbd "M-<left>") 'haskell-move-left)

;; Move the code below the current nesting right one.
(define-key haskell-mode-map (kbd "M-<right>") 'haskell-move-right)

(define-key haskell-mode-map (kbd "C-c C-s") 'toggle-scc-at-point)
(define-key haskell-mode-map (kbd "C-c l") 'hs-lint)
(define-key haskell-mode-map (kbd "C-c C-y") 'cabal-toggle-sandboxing-local)
(define-key haskell-mode-map (kbd "C-c C-u") 'haskell-insert-undefined)


; Don't use C-c c or C-c C-c so that computations in ghci can still be killed.
(define-key haskell-interactive-mode-map (kbd "C-z C-c") 'haskell-process-cabal-build)
(define-key haskell-interactive-mode-map (kbd "C-z c") 'haskell-process-cabal)
;(define-key haskell-interactive-mode-map (kbd "C-c C-l") 'switch-to-haskell)


(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;(define-key haskell-cabal-mode-map (kbd "C-c C-l") 'switch-to-haskell)
