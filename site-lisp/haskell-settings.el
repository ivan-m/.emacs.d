(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-checkers)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Make flycheck aware of sandboxes.

(eval-after-load 'flycheck
  '(progn
     (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
     (require 'flycheck-hdevtools)))

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
  (structured-haskell-mode 1)

  (electric-indent-local-mode -1)

  (capitalized-words-mode)

  (turn-on-haskell-decl-scan)

  #'hindent-mode

  ;; (ghc-init)

  (auto-insert-mode 1)

  (font-lock-add-keywords nil
                          '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face prepend)))

  (lambda ()
    (when (buffer-file-name)
      (haskell-file-hook)))

  ;; (setq ac-sources '(ac-source-words-in-same-mode-buffers
  ;;                    ac-source-dictionary
  ;;                    ac-source-ghc-mod))
  )

(defun haskell-file-hook ()

  (flyspell-prog-mode)

  (set-tab-stop-width 2)

  (haskell-outline-hook)
  )

;; From http://www.emacswiki.org/emacs/TabStopList
(defun set-tab-stop-width (width)
  "Set all tab stops to WIDTH in current buffer.

    This updates `tab-stop-list', but not `tab-width'.

    By default, `indent-for-tab-command' uses tabs to indent, see
    `indent-tabs-mode'."
  (interactive "nTab width: ")
  (let* ((max-col (car (last tab-stop-list)))
         ;; If width is not a factor of max-col,
         ;; then max-col could be reduced with each call.
         (n-tab-stops (/ max-col width)))
    (set (make-local-variable 'tab-stop-list)
         (mapcar (lambda (x) (* width x))
                 (number-sequence 1 n-tab-stops)))
    ;; So preserve max-col, by adding to end.
    (unless (zerop (% max-col width))
      (setcdr (last tab-stop-list)
              (list max-col)))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun toggle-scc-at-point (&optional arg)
  "Insert or kill (with universal-argument) an SCC annotation at
point."
  (interactive "P")
  (if (equal arg nil)
      (haskell-mode-insert-scc-at-point)
    (haskell-mode-kill-scc-at-point)))

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

(defun haskell-guess-license ()
  "Guess the license of this project.
If there is no valid .cabal file to get the license field from,
return nil."
  (interactive)
  (when buffer-file-name
    (let ((cabal-file (haskell-cabal-find-file (file-name-directory buffer-file-name))))
      (when (and cabal-file (file-readable-p cabal-file))
        (with-temp-buffer
          (insert-file-contents cabal-file)
          (haskell-cabal-get-setting "license"))))))

;; Skeletons
(define-skeleton haskell-module-skeleton
  "Haskell hs file header"
  "Brief description: "
  "{- \|\n"
  '(setq module-name (haskell-guess-module-name))
  '(setq project-license (haskell-guess-license))
  "   Module      : " module-name "\n"
  "   Description : " str | (concat "The \"" module-name "\" module") "\n"
  "   Copyright   : (c) Ivan Lazar Miljenovic\n"
  "   License     : " project-license | "BSD-style (see the file LICENSE)" "\n"
  "   Maintainer  : Ivan.Miljenovic@gmail.com\n"
  "\n"
  "   " _ "\n"
  "\n"
  " -}\n"
  "module " module-name " where\n\n")

(add-to-list 'auto-insert-alist '("\\.hs\\'" . haskell-module-skeleton))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; Based upon https://github.com/paul7/dev-conf/blob/master/.emacs-haskell
(defvar cabal-use-sandbox t)
;; (setq-default haskell-program-name "ghci")
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

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Keybindings

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

(define-key haskell-mode-map (kbd "C-c C-s") 'toggle-scc-at-point)
(define-key haskell-mode-map (kbd "C-c l") 'hs-lint)
(define-key haskell-mode-map (kbd "C-c C-y") 'cabal-toggle-sandboxing-local)

;; Don't use C-c c or C-c C-c so that computations in ghci can still be killed.
(define-key haskell-interactive-mode-map (kbd "C-z C-c") 'haskell-process-cabal-build)
(define-key haskell-interactive-mode-map (kbd "C-z c") 'haskell-process-cabal)
(define-key haskell-interactive-mode-map (kbd "C-c C-y") 'cabal-toggle-sandboxing-local)
;;(define-key haskell-interactive-mode-map (kbd "C-c C-l") 'switch-to-haskell)


(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;(define-key haskell-cabal-mode-map (kbd "C-c C-l") 'switch-to-haskell)
