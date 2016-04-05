(require 'haskell-mode)
(require 'haskell-interactive-mode)
;; (require 'haskell-checkers)
(require 'hs-lint)
(require 'shm-case-split)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Make flycheck aware of sandboxes.

(eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; Need to find out how to do this without a require.
;; (require 'auto-complete)
;; (ac-define-source ghc-mod
;;   '((depends ghc)
;;     (candidates . (ghc-select-completion-symbol))
;;     (symbol . "s")
;;     (cache)))

(add-hook 'haskell-mode-hook 'haskell-hook)

(defun haskell-hook ()
  (interactive-haskell-mode 1)

  (structured-haskell-mode 1)

  (electric-indent-local-mode -1)

  (capitalized-words-mode)

  (turn-on-haskell-decl-scan)

  #'hindent-mode

  ;; (ghc-init)

  (auto-insert-mode 1)

  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face prepend)))

  (when (buffer-file-name)
    (haskell-file-hook)

    (when (equal (file-name-extension (buffer-file-name)) "lhs")
      (haskell-literate-hook)
      )

    )

  ;; (setq ac-sources '(ac-source-words-in-same-mode-buffers
  ;;                    ac-source-dictionary
  ;;                    ac-source-ghc-mod))
  )

(defun haskell-file-hook ()

  (flyspell-prog-mode)

  (flycheck-mode 1)
  )

(defun haskell-literate-hook ()
  (structured-haskell-mode 0)
  (haskell-indent-mode 1)
  (flycheck-mode 0)
  )

(add-hook 'haskell-cabal-mode-hook 'cabal-hook)

(defun cabal-hook ()
  (electric-indent-local-mode -1)
  )

(add-hook 'haskell-interactive-mode-hook 'structured-haskell-repl-mode)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; https://github.com/chrisdone/chrisdone-emacs/blob/master/config/haskell.el

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

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(require 'skeleton)
(require 'autoinsert)

;; Skeletons
(define-skeleton haskell-module-skeleton
  "Haskell hs file header"
  "Brief description (leave blank for default): "
  "{- \|\n"
  "   Module      : " (setq v1 (or (haskell-guess-module-name) "Main")) "\n"
  "   Description : " str | (concat "The \\\"" v1 "\\\" module") "\n"
  "   Copyright   : " (haskell-cabal-guess-setting "copyright") | (concat "(c) " user-full-name) "\n"
  "   License     : " (haskell-cabal-guess-setting "license") | "BSD-style (see the file LICENSE)" "\n"
  "   Maintainer  : " (haskell-cabal-guess-setting "maintainer") | user-mail-address "\n"
  "\n"
  "   " _ "\n"
  "\n"
  " -}\n"
  "module " v1 " where\n\n")

(add-to-list 'auto-insert-alist '("\\.hs\\'" . haskell-module-skeleton))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Keybindings

(defun shm-contextual-space ()
  "Do contextual space first, and run shm/space if no change in
the cursor position happened."
  (interactive)
  (if (looking-back "import")
      (call-interactively 'haskell-mode-contextual-space)
    (progn
      (let ((ident (haskell-ident-at-point)))
        (when ident
          (and interactive-haskell-mode
               (haskell-process-do-try-type ident))))
      (call-interactively 'shm/space))))

(defun shm-repl-tab ()
  "TAB completion or jumping."
  (interactive)
  (unless (shm/jump-to-slot)
    (call-interactively 'haskell-interactive-mode-tab)))

(define-key haskell-mode-map [?\C-c ?\C-l] 'haskell-process-load-file)
(define-key haskell-mode-map [?\C-c ?\C-r] 'haskell-process-reload)
(define-key haskell-mode-map [f5] 'haskell-process-load-file)

;; Switch to the REPL.
(define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
;; "Bring" the REPL, hiding all other windows apart from the source
;; and the REPL.
(define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

;; Build the Cabal project.
;; Interactively choose the Cabal command to run.
(define-key haskell-mode-map (kbd "C-c c") 'ebal-execute)

;; Get the type and info of the symbol at point, print it in the
;; message buffer.
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)

;; Jump to the imports. Keep tapping to jump between import
;; groups. C-u f8 to jump back again.
(define-key haskell-mode-map [f8] 'haskell-navigate-imports)

;; Jump to the definition of the current symbol.
(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

(define-key haskell-mode-map (kbd "M-,") 'haskell-who-calls)

;; Move the code below the current nesting left one.
(define-key haskell-mode-map (kbd "C-,") 'haskell-move-left)

;; Move the code below the current nesting right one.
(define-key haskell-mode-map (kbd "C-.") 'haskell-move-right)

(define-key haskell-mode-map (kbd "C-c C-s") 'haskell-mode-toggle-scc-at-point)
(define-key haskell-mode-map (kbd "C-c l") 'hs-lint)

(if (system-type-is-darwin)
    (define-key shm-map (kbd "<s-backspace>") 'shm/delete))

(define-key shm-map (kbd "SPC") 'shm-contextual-space)

;; Don't use C-c c or C-c C-c so that computations in ghci can still be killed.
(define-key haskell-interactive-mode-map (kbd "C-z c") 'ebal-execute)
(define-key shm-repl-map (kbd "TAB") 'shm-repl-tab)

;;(define-key haskell-interactive-mode-map (kbd "C-c C-l") 'switch-to-haskell)


(define-key haskell-cabal-mode-map (kbd "C-c c") 'ebal-execute)
(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;(define-key haskell-cabal-mode-map (kbd "C-c C-l") 'switch-to-haskell)

(push '(configure "--enable-tests" "--enable-benchmarks")
      ebal-global-option-alist)

(push '(install "--only-dependencies" "--enable-tests" "--enable-benchmarks")
      ebal-global-option-alist)
