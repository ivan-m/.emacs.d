(eval-when-compile (require 'req-package))

(define-minor-mode haskell-stack-mode
  "Extra functionality for stack.yaml files"
  :init-value nil
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c c") 'ebal-execute)
            map))

(add-hook 'find-file-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (string= (file-name-nondirectory (buffer-file-name)) "stack.yaml"))
              (haskell-stack-mode 1))))

(req-package shm
  :require hindent
  :commands
  structured-haskell-mode
  structured-haskell-repl-mode
  :init
  (setq shm-auto-insert-skeletons t)
  (setq shm-indent-point-after-adding-where-clause t)
  (setq shm-use-hdevtools t)
  :functions system-type-is-darwin
  :config
  (if (system-type-is-darwin)
      (define-key shm-map (kbd "<s-backspace>") 'shm/delete)))

(req-package shm-case-split
  :require shm haskell-process
  :commands shm/case-split)

(req-package shm
  :require
  haskell-interactive-mode
  :bind
  (:map shm-repl-map
        ("TAB" . shm-repl-tab))
  :config
  (defun shm-repl-tab ()
    "TAB completion or jumping."
    (interactive)
    (unless (shm/jump-to-slot)
      (call-interactively 'haskell-interactive-mode-tab))))

(req-package haskell-process
  :require haskell-mode)

(req-package ebal
  :require
  haskell-mode
  haskell-cabal
  haskell-interactive-mode
  :init
  (setq ebal-completing-read-function 'ebal-ido-completing-read)
  :commands
  ebal-execute
  :bind
  (:map haskell-mode-map
   ("C-c c" . ebal-execute)

   :map haskell-interactive-mode-map
   ;; Don't use C-c c or C-c C-c so that computations in ghci can still be killed.
   ("C-z c" . ebal-execute)

   :map haskell-cabal-mode-map
   ("C-c c" . ebal-execute))
  :config
  (push '(configure "--enable-tests" "--enable-benchmarks")
        ebal-global-option-alist)

  (push '(install "--only-dependencies" "--enable-tests" "--enable-benchmarks")
        ebal-global-option-alist))

(req-package hindent
  :diminish hindent-mode
  :commands hindent-mode)

(req-package company-ghci
  :require company
  :config (push 'company-ghci company-backends))

(req-package company-cabal
  :require company
  :config (push 'company-cabal company-backends))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(req-package haskell-doc
  :require
  haskell-mode
  :commands
  haskell-doc-mode
  :diminish
  haskell-doc-mode)

(req-package haskell
  :require
  haskell-mode
  :init
  (setq haskell-ask-also-kill-buffers nil)
  (setq haskell-literate-default 'bird)
  (setq haskell-notify-p t)
  (setq haskell-process-check-cabal-config-on-load t)
  (setq haskell-process-load-or-reload-prompt t)
  (setq haskell-process-log t)
  (setq haskell-process-prompt-restart-on-cabal-change t)
  (setq haskell-process-suggest-haskell-docs-imports t)
  (setq haskell-process-suggest-language-pragmas t)
  (setq haskell-process-suggest-no-warn-orphans t)
  (setq haskell-process-suggest-overloaded-strings t)
  (setq haskell-stylish-on-save t)
  (setq haskell-interactive-mode-eval-mode 'haskell-mode)
  (setq haskell-interactive-mode-hide-multi-line-errors t)

  (add-hook 'haskell-mode-hook
            (lambda ()
              (haskell-doc-mode 1)
              (interactive-haskell-mode 1)
              (haskell-decl-scan-mode 1)
              (electric-indent-local-mode -1)
              (auto-insert-mode 1)
              (subword-mode)
              (font-lock-add-keywords
               nil
               '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face prepend)))
              (setq process-connection-type nil)

              (when (buffer-file-name)
                (flyspell-prog-mode)
                (if (equal (file-name-extension (buffer-file-name)) "lhs")
                    (haskell-literate-hook)
                  (haskell-file-hook)))))

  :commands
  haskell-mode
  literate-haskell-mode

  :diminish
  (interactive-haskell-mode . "Int")

  :interpreter
  (("runhaskell" . haskell-mode)
   ("runghc" . haskell-mode))

  :mode
  (("\\.hsc\\'" . haskell-mode)
   ("\\.l[gh]s\\'" . literate-haskell-mode)
   ("\\.[gh]s\\'" . haskell-mode))

  :bind
  (:map haskell-mode-map
        ("C-c C-l" . haskell-process-load-file)
        ("C-c C-r" . haskell-process-reload)
        ([f5] . haskell-process-load-file)

        ;; Switch to the REPL.
        ("C-c C-z" . haskell-interactive-switch)
        ;; "Bring" the REPL, hiding all other windows apart from the source
        ;; and the REPL.
        ("C-`" . haskell-interactive-bring)

        ;; Build the Cabal project.
        ;; Interactively choose the Cabal command to run.
        ("C-c C-c" . nil)

        ;; Get the type and info of the symbol at point, print it in the
        ;; message buffer.
        ("C-c C-t" . haskell-process-do-type)
        ("C-c C-i" . haskell-process-do-info)

        ;; Jump to the imports. Keep tapping to jump between import
        ;; groups. C-u f8 to jump back again.
        ([f8] . haskell-navigate-imports)

        ;; Jump to the definition of the current symbol.
        ("M-." . haskell-mode-tag-find)

        ("M-," . haskell-who-calls)

        ;; Move the code below the current nesting left one.
        ("C-," . haskell-move-left)

        ;; Move the code below the current nesting right one.
        ("C-." . haskell-move-right)

        ("C-c C-s" . haskell-mode-toggle-scc-at-point))

  :config
  (defun haskell-file-hook ()
    (structured-haskell-mode 1)
    (flycheck-mode 1)
    (company-mode 1)
    (hindent-mode))

  (defun haskell-literate-hook ()
    (structured-haskell-mode 0)
    (haskell-indent-mode 1)
    (flycheck-mode 0))

  (defun haskell-insert-doc ()
    "Insert the documentation syntax."
    (interactive)
    (insert "-- | "))

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

  (defun my-haskell-guess-module-name ()
    (interactive)
    (let ((guessed (haskell-guess-module-name-from-file-name (buffer-file-name))))
      (if (eq "" guessed)
          nil
        guessed)))

  ;; Skeletons
  (define-skeleton haskell-module-skeleton
    "Haskell hs file header"
    "Brief description (leave blank for default): "
    "{- \|\n"
    "   Module      : " (setq v1 (or (my-haskell-guess-module-name) "Main")) "\n"
    "   Description : " str | (concat "The \\\"" v1 "\\\" module") "\n"
    "   Copyright   : " (haskell-cabal-get-field "copyright") | (concat "(c) " user-full-name) "\n"
    "   License     : " (haskell-cabal-get-field "license") | "BSD-style (see the file LICENSE)" "\n"
    "   Maintainer  : " (haskell-cabal-get-field "maintainer") | user-mail-address "\n"
    "\n"
    "   " _ "\n"
    "\n"
    " -}\n"
    "module " v1 " where\n\n"))

(req-package autoinsert
  :require
  skeleton
  haskell

  :config
  (add-to-list 'auto-insert-alist '("\\.hs\\'" . haskell-module-skeleton)))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(req-package haskell-interactive-mode
  :require shm
  :init
  (add-hook 'haskell-interactive-mode-hook 'structured-haskell-repl-mode))

(req-package haskell-interactive-mode
  :require company
  :init
  (add-hook 'haskell-interactive-mode-hook
            (lambda ()
              (company-mode 1))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(req-package haskell-cabal
  :require haskell-mode
  :commands
  haskell-cabal-mode

  :mode
  ("\\.cabal\\'" . haskell-cabal-mode)

  :bind
  (:map haskell-cabal-mode-map
        ("C-`" . haskell-interactive-bring)
        ("C-c C-z" . haskell-interactive-switch))

  :init
  (add-hook 'haskell-cabal-mode-hook
            (lambda ()
              (electric-indent-local-mode -1))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(req-package ghci-script-mode
  :require
  haskell-mode
  :commands
  ghci-script-mode
  :mode
  ("\\.ghci\\'" . ghci-script-mode))

(req-package haskell-c2hs
  :require
  haskell-mode
  :commands
  haskell-c2hs-mode
  :mode
  ("\\.chs\\'" . haskell-c2hs-mode))

(req-package ghc-core
  :require
  haskell-mode
  :commands
  ghc-core-mode
  :mode
  (("\\.dump-simpl\\'" . ghc-core-mode)
   ("\\.hcr\\'" . ghc-core-mode)))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(req-package flycheck-haskell
  :require flycheck
  :init
  (setq flycheck-haskell-runghc-command '("runghc"))
  ;; Make flycheck aware of sandboxes.
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(req-package hs-lint
  :require haskell-mode
  :loader :path
  :commands hs-lint
  :bind
  (:map haskell-mode-map
        ("C-c l" . hs-lint)))


(provide 'haskell-settings)
