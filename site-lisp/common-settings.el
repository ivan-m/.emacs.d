(eval-when-compile
  (progn
    (require 'req-package)
    (require 'cl)))

(setq user-full-name "Ivan Lazar Miljenovic"
      user-mail-address "Ivan.Miljenovic@gmail.com"

      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-default-init t

      frame-title-format "%b %+%+ %f"
      icon-title-format frame-title-format
      column-number-mode t
      size-indication-mode t
      use-dialog-box nil

      default-directory "~/"

      after-save-hook 'executable-make-buffer-file-executable-if-script-p
      require-final-newline t
      select-enable-primary nil
      select-enable-clipboard t
      select-active-regions t

      history-length 1000
      history-delete-duplicates t

      bury-successful-compilation t
      compilation-message-face 'default

      text-mode-hook '(turn-on-auto-fill text-mode-hook-identify))

(setq-default indent-tabs-mode nil
              tab-width 4
              truncate-lines t
              indicate-buffer-boundaries 'left
              indicate-empty-lines +1)

(set-scroll-bar-mode nil)
(tool-bar-mode -1)

(add-to-list 'completion-ignored-extensions ".hi")

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(defun refill-paragraphs-to-be-one-line ()
  "fill individual paragraphs with large fill column"
  (interactive)
  (let ((fill-column 100000))
    (fill-individual-paragraphs (point-min) (point-max))))

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

;; Stop C-z from minimizing emacs.
;(when window-system
;;    (global-unset-key "\C-z")) ; iconify-or-deiconify-frame (C-x C-z)
;; Starting as a daemon seems to mean that the window-system check fails
(global-unset-key "\C-z")
(global-unset-key (kbd "C-x C-z"))

(global-set-key (kbd "C-x a r") 'align-regexp)

(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

;; make mouse middle-click only paste from primary X11 selection, not clipboard and kill ring.
(global-set-key [mouse-2] 'mouse-yank-primary)

;; Avoid some issues that appear to be due to ssh'ing in to a VM
(global-set-key (kbd "<Scroll_Lock>") '(lambda () (interactive) nil))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(fset 'yes-or-no-p 'y-or-n-p)

(set-fontset-font t 'unicode "Symbola" nil 'prepend)

;; Need to use next-line and previous-line here as they remember which
;; column you were in (and forward-line doesn't).

(defun sfp-page-down ()
  (interactive)
  (setq this-command 'next-line)
  (next-line
   (- (window-text-height)
      next-screen-context-lines)))

(defun sfp-page-up ()
  (interactive)
  (setq this-command 'previous-line)
  (previous-line
   (- (window-text-height)
      next-screen-context-lines)))

(global-set-key [next] 'sfp-page-down)
(global-set-key [prior] 'sfp-page-up)

(turn-on-auto-fill)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; defadvice

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

(defadvice kill-line (before check-position activate)
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1))))

;; https://www.emacswiki.org/emacs/DebugMessages
;;
;; We don't want this enabled by default
;;
;; (defadvice message (before who-said-that activate)
;;   "Find out who said that thing. and say so."
;;   (let ((trace nil) (n 1) (frame nil))
;;     (while (setq frame (backtrace-frame n))
;;       (setq n     (1+ n)
;;             trace (cons (cadr frame) trace)) )
;;     (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 0)))
;;     (ad-set-args 1 (cons trace (ad-get-args 1))) ))
;;
;; (ad-disable-advice 'message 'before 'who-said-that)
;; (ad-update 'message)

(defadvice message (before when-was-that activate)
  "Add timestamps to `message' output."
  (ad-set-arg 0 (concat (format-time-string "[%Y-%m-%d %T %Z] ")
                        (ad-get-arg 0)) ))

;; (ad-disable-advice 'message 'before 'when-was-that)
;; (ad-update 'message)

(defadvice Info-follow-nearest-node (around gitman activate)
  "When encountering a cross reference to the `gitman' info
manual, then instead of following that cross reference show
the actual manpage using the function `man'."
  (let ((node (Info-get-token
               (point) "\\*note[ \n\t]+"
               "\\*note[ \n\t]+\\([^:]*\\):\\(:\\|[ \n\t]*(\\)?")))
    (if (and node (string-match "^(gitman)\\(.+\\)" node))
        (progn (require 'man)
               (man (match-string 1 node)))
      ad-do-it)))

;; https://github.com/alezost/alect-themes#emacs-bug-in-themed-variables
(defadvice custom-theme-set-variables
    (around fix-inhibit-bug activate)
  "Allow setting of undefined variables in themes."
  (let (custom--inhibit-theme-enable)
    ad-do-it))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; OS-specific functionality

(if (system-type-is-darwin)
    (progn
      (setenv "DICTIONARY" "en_AU")
      (setenv "LANG" "en_AU")
      (global-set-key [s-left] 'beginning-of-line)
      (global-set-key [s-right] 'end-of-line)
      (global-unset-key [?\s-q])
      (setq mac-option-modifier 'super)
      (setq mac-command-modifier 'meta)
      (global-set-key (kbd "<home>") 'beginning-of-line)
      (global-set-key (kbd "<end>") 'end-of-line)
      (setq default-frame-alist (quote ((font . "Menlo-11"))))
      (require 'server)
      (unless (server-running-p)
        (server-start))))

(if (system-type-is-win)
    (progn
      (global-set-key [apps] 'execute-extended-command)

      ;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
      ;;(setq w32-pass-lwindow-to-system nil)
      (setq w32-lwindow-modifier 'super) ; Left Windows key

      (setq w32-pass-rwindow-to-system nil)
      (setq w32-rwindow-modifier 'super) ; Right Windows key

      (setq w32-pass-apps-to-system nil)
      ;;(setq w32-apps-modifier 'hyper) ; Menu/App key
      ))

(if (system-type-is-gnu)
    (progn
      (setq default-frame-alist (quote ((font . "DejaVu Sans Mono-10"))))
      (setq interprogram-cut-function 'x-select-text)
      (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)))


;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun extract-proxy-from-env (env)
  "Get the proxy component from ENV.

i.e. no protocol/scheme, no trailing slash, just foobar:port."
  (save-match-data
    (let ((env-value (getenv env)))
      (string-match "://\\([^/]+\\)/?$" env-value)
      (match-string 1 env-value))))

(req-package diminish
  :config
  ;; Doesn't seem to have a dedicated file for it
  (diminish 'auto-fill-function))

(req-package eldoc
  :diminish eldoc-mode)

(req-package bind-key)

(req-package fringe-current-line
  :init
  (setq fcl-fringe-bitmap 'filled-rectangle)
  :config
  (global-fringe-current-line-mode 1))

(req-package windmove
  :config
  (defun ignore-error-wrapper (fn)
    "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
    (lexical-let ((fn fn))
      (lambda ()
        (interactive)
        (ignore-errors
          (funcall fn)))))

  ;; As we're using a function call, we can't do this with :bind
  (global-set-key [S-left] (ignore-error-wrapper 'windmove-left))
  (global-set-key [S-right] (ignore-error-wrapper 'windmove-right))
  (global-set-key [S-up] (ignore-error-wrapper 'windmove-up))
  (global-set-key [S-down] (ignore-error-wrapper 'windmove-down)))

(req-package tramp
  :defer 30
  :init
  (setq tramp-default-method "ssh")
  :config
  (setq backup-enable-predicate
         (lambda (name)
           (and (normal-backup-enable-predicate name)
                (not
                 (let ((method (file-remote-p name 'method)))
                   (when (stringp method)
                     (member method '("su" "sudo")))))))))

(req-package recentf
  :init
  (setq recentf-max-saved-items 1000)
  ;; Needs to be done before it's started: https://www.emacswiki.org/emacs/RecentFiles#toc12
  (setq recentf-auto-cleanup 'never)
  :config
  (add-to-list 'recentf-exclude "^/ssh:.*")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude ".*-autoloads\\.el\\'")
  (add-to-list 'recentf-exclude "ido\\.last")
  (add-to-list 'recentf-exclude "recentf")
  (add-to-list 'recentf-exclude "[/\\]\\.elpa/")

  ;; Need to use this function apparently
  (setq recentf-save-file (recentf-expand-file-name (expand-file-name "recentf" user-emacs-directory)))

  (recentf-mode 1)
  ;; Save recent list when idle for five minutes.
  (run-with-idle-timer (* 5 60) t 'recentf-save-list))

(req-package notifications)

(req-package lorem-ipsum
  :commands
  lorem-ipsum-insert-paragraphs
  lorem-ipsum-insert-sentences
  lorem-ipsum-insert-list)

(req-package generic-x
  :ensure nil
  :mode ("smb\\.conf$" . samba-generic-mode))

(req-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(req-package align-cols
  :ensure nil
  :commands align-cols)

(req-package whitespace
  :init
  (setq whitespace-style '(face tabs trailing empty))
  :diminish global-whitespace-mode
  :config
  (global-whitespace-mode 1))

(req-package flycheck
  :init
  ;; These help with responsiveness
  (setq flycheck-check-syntax-automatically '(save))
  (setq flycheck-display-errors-delay 0.3)
  :commands
  flycheck-mode
  :config
  (add-to-list 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
  (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc))

(req-package flycheck-color-mode-line)

(req-package flyspell
  :if (system-type-is-gnu)
  :functions system-type-is-gnu system-type-is-win
  :require
  ispell
  :init
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  (add-hook 'prog-mode-hook
            (lambda ()
              (flyspell-prog-mode)))
  (add-hook 'text-mode-hook
            (lambda ()
              (flyspell-mode 1)))
  :diminish flyspell-mode
  :commands
  flyspell-mode
  flyspell-prog-mode
  :bind
  (:map flyspell-mode-map
        ("C-." . nil)
        ;; ("C-M-i" . nil)
        ("C-," . nil)))

(req-package auto-highlight-symbol
  :init
  (setq ahs-case-fold-search nil)
  ;; Allow trailing '
  (setq ahs-include "^[0-9A-Za-z/_.,:;*+=&%|$#@!^?-]+'?$")
  :diminish auto-highlight-symbol-mode
  :config
  (add-to-list 'ahs-modes 'haskell-mode)
  (add-to-list 'ahs-modes 'scala-mode)
  (global-auto-highlight-symbol-mode 1))

(req-package subword
  :commands subword-mode
  :diminish subword-mode)

(req-package mmm-mode
  :diminish mmm-mode
  :commands
  mmm-mode)

(req-package subword
  :diminish subword-mode)

(req-package company
  :init
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-code-ignore-case t)
  (setq company-etags-ignore-case t)
  :diminish company-mode
  :commands company-mode
  :config
  (push 'company-capf company-backends)
  (push 'company-dabbrev-code company-backends))

;; (req-package company-quickhelp
;;   :require
;;   company
;;   :init
;;   (add-hook company-mode-hook (lambda () (company-quickhelp-mode 1)))
;;   :commands
;;   company-quickhelp-mode
;;   company-quickhelp-manual-begin
;;   :bind (:map company-active-map
;;               ("C-c h" . #'company-quickhelp-manual-begin)))

(req-package goto-chg
  :commands
  goto-last-change
  goto-last-change-reverse
  :bind (("C->" . goto-last-change)
         ("C-<" . gogo-last-change-reverse)))

(req-package paredit
  :commands
  paredit-mode)

(req-package paredit-menu
  :require paredit
  :commands
  paredit-menu)

(req-package hydra
  :config
  (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                       :color pink
                                       :post (deactivate-mark))
    "
  ^_k_^     _d_elete    _s_tring
_h_   _l_   _o_k        _y_ank
  ^_j_^     _n_ew-copy  _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _p_aste
"
    ("h" backward-char nil)
    ("l" forward-char nil)
    ("k" previous-line nil)
    ("j" next-line nil)
    ("e" hydra-ex-point-mark nil)
    ("n" copy-rectangle-as-kill nil)
    ("d" delete-rectangle nil)
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1)) nil)
    ("y" yank-rectangle nil)
    ("u" undo nil)
    ("s" string-rectangle nil)
    ("p" kill-rectangle nil)
    ("o" nil nil))
  (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

(req-package csv-mode
  :commands
  csv-mode
  :mode
  "\\.csv\'")

(req-package ispell
  :if (or (system-type-is-gnu) (system-type-is-win))
  :functions system-type-is-gnu system-type-is-win
  :init
  (setq ispell-highlight-p t)
  (setenv "DICPATH" (expand-file-name "dictionaries" user-emacs-directory))
  (setq ispell-program-name "hunspell"))

(req-package font-utils
  :init
  (setq font-utils-less-feedback t))

(req-package unicode-fonts
  :require font-utils
  :functions system-type-is-gnu
  :init
  (if (system-type-is-gnu)
      (setq unicode-fonts-block-font-mapping
            '(("Alchemical Symbols"
               ("Symbola"))
              ("Alphabetic Presentation Forms"
               ("DejaVu Sans:width=condensed" "FreeMono"))
              ("Arrows"
               ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
              ("Block Elements"
               ("DejaVu Sans Mono" "FreeMono" "DejaVu Sans:width=condensed" "Symbola"))
              ("Box Drawing"
               ("DejaVu Sans Mono" "FreeMono" "DejaVu Sans" "Symbola"))
              ("Combining Diacritical Marks Supplement"
               ("FreeSerif" "DejaVu Sans:width=condensed"))
              ("Combining Diacritical Marks for Symbols"
               ("Cambria Math" "Symbola"))
              ("Combining Diacritical Marks"
               ("DejaVu Sans:width=condensed" "DejaVu Sans Mono" "FreeMono"))
              ("Combining Half Marks"
               ("Symbola"))
              ("Control Pictures"
               ("Symbola" "FreeMono"))
              ("Currency Symbols"
               ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
              ("Cyrillic Supplement"
               ("DejaVu Sans:width=condensed" "Symbola"))
              ("Cyrillic"
               ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
              ("Dingbats"
               ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola"))
              ("Emoticons"
               ("Symbola"))
              ("General Punctuation"
               ("DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
              ("Geometric Shapes"
               ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
              ("Gothic"
               ("FreeSerif"))
              ("Greek Extended"
               ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "FreeMono"))
              ("Greek and Coptic"
               ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola"))
              ("IPA Extensions"
               ("DejaVu Sans Mono" "Symbola" "FreeMono"))
              ("Latin Extended-C"
               ("DejaVu Sans:width=condensed"))
              ("Latin Extended-D"
               ("FreeMono" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"))
              ("Letterlike Symbols"
               ("DejaVu Sans:width=condensed" "Symbola"))
              ("Mathematical Alphanumeric Symbols"
               ("Symbola"))
              ("Mathematical Operators"
               ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
              ("Miscellaneous Mathematical Symbols-A"
               ("Symbola"))
              ("Miscellaneous Mathematical Symbols-B"
               ("Symbola"))
              ("Miscellaneous Symbols and Pictographs"
               ("Symbola"))
              ("Miscellaneous Symbols and Arrows"
               ("Symbola"))
              ("Miscellaneous Symbols"
               ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola"))
              ("Miscellaneous Technical"
               ("Symbola"))
              ("Musical Symbols"
               ("Symbola"))
              ("Number Forms"
               ("DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
              ("Optical Character Recognition"
               ("Symbola" "FreeMono"))
              ("Playing Cards"
               ("DejaVu Sans:width=condensed" "Symbola"))
              ("Specials"
               ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
              ("Superscripts and Subscripts"
               ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
              ("Supplemental Arrows-A"
               ("DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
              ("Supplemental Arrows-B"
               ("Symbola"))
              ("Supplemental Mathematical Operators"
               ("Symbola"))
              ("Supplemental Punctuation"
               ("DejaVu Sans Mono" "Symbola"))
              ("Transport and Map Symbols"
               ("Symbola")))))
  (setq unicode-fonts-existence-checks 'first)
  (setq unicode-fonts-skip-font-groups
        '(chinese-simplified chinese-traditional low-quality-glyphs microsoft-only multicolor non-free))

  (add-hook 'after-init-hook
            (lambda ()
              (if (display-graphic-p)
                  (my/init-fonts)
                (add-hook 'after-make-frame-functions 'my/init-fonts))))
  :config
  ;; Taken from https://github.com/rolandwalker/unicode-fonts/issues/3
  (defun my/init-fonts (&optional frame)
    (when (display-graphic-p frame)
      (when frame (select-frame frame))
      (unicode-fonts-setup)
      (remove-hook 'after-make-frame-functions 'my/init-fonts))))

(req-package ascii-art-to-unicode
  :commands
  aa2u
  aa2u-rectangle
  aa2u-mark-as-text
  aa2u-mark-rectangle-as-text)

(req-package paren
  :init
  (setq show-paren-style 'mixed)
  :config
  (show-paren-mode 1))

(req-package saveplace
  :init
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
  :config
  (save-place-mode 1))

(req-package sql
  :init
  (setq sql-product 'postgres)
  :commands
  sql-mode
  :mode
  (("\\.sql$" . sql-mode)
   ("\\.cql$" . sql-mode)))

(req-package uniquify
  :ensure nil
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(req-package image-file
  :config
  (auto-image-file-mode 1))

(req-package files
  :ensure nil
  :init
  (setq backup-by-copying t)
  (setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))
  (setq auto-save-file-name-transforms `((".*" ,(expand-file-name "backups" user-emacs-directory) t)))
  (setq delete-old-versions t))

(req-package delsel
  :init
  (setq delete-active-region t)
  :config
  (delete-selection-mode 1))

(req-package dired
  :ensure nil
  :init
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-alh")
  (setq wdired-allow-to-change-permissions t))

(req-package dired-x
  :ensure
  nil
  :config
  (setq dired-omit-verbose nil)
  ;;(setq-default dired-omit-files-p t)
  ;; toggle `dired-omit-mode' with C-x M-o
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|_flymake\\.hs$\\|\\.hie$"))

(req-package unkillable-scratch
  :config
  (unkillable-scratch 1))

(req-package man
  :init
  (setq Man-notify-method 'pushy)
  :commands
  man)

(req-package woman
  :init
  (setq woman-fill-frame t)
  (setq woman-imenu t)
  (setq woman-use-own-frame nil)
  :commands
  woman
  :config
  (set-face-attribute 'woman-italic nil :inherit italic :foreground "#3cb370")
  (set-face-attribute 'woman-bold nil :inherit bold :foreground "#00aff5"))

(req-package compile
  :init
  (setq compilation-message-face 'default))

(req-package bury-successful-compilation
  :config
  (bury-successful-compilation 1))

(req-package ediff
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(req-package darcsum
  :commands darcsum-whatsnew
  :bind ("C-c d" . darcsum-whatsnew))

(req-package helm
  :commands helm-mini
  :bind ([f9] . helm-mini))

(req-package graphviz-dot-mode
  :commands
  graphviz-dot-mode
  :mode
  "\\.gv\\'")

(req-package nix-mode
  :commands
  nix-mode
  :mode
  "\\.nix\\'")

(req-package nixos-options
  :if (system-type-is-gnu)
  :functions system-type-is-gnu)

(req-package yaml-mode
  :init
  (add-to-list 'interpreter-mode-alist '("ansible-playbook" . yaml-mode))
  :commands
  yaml-mode
  :mode
  "\\.y[a]ml\\'")

;; (req-package zoom-frm
;;   :ensure nil
;;   :demand t
;;   :init
;;   (setq zoom-frame/buffer 'buffer)
;;   :commands
;;   zoom-in/out
;;   zoom-in
;;   zoom-out
;;   :bind (:map ctl-x-map
;;          ([(control ?+)] . zoom-in/out)
;;          ([(control ?-)] . zoom-in/out)
;;          ([(control ?=)] . zoom-in/out)
;;          ([(control ?0)] . zoom-in/out))
;;   :config
;;   (global-set-key (if (boundp 'mouse-wheel-down-event) ; Emacs 22+
;;                       (vector (list 'control
;;                                     mouse-wheel-down-event))
;;                     [C-mouse-wheel])    ; Emacs 20, 21
;;                   'zoom-in)
;;   (when (boundp 'mouse-wheel-up-event) ; Emacs 22+
;;     (global-set-key (vector (list 'control mouse-wheel-up-event))
;;                     'zoom-out)))

(req-package apropos
  :ensure nil
  :init
  (setq apropos-do-all t)
  :commands apropos)

;; (req-package dockerfile-mode
;;   :mode ("Dockerfile" . dockerfile-mode))

;; (req-package ansible
;;   :require
;;   yaml-mode
;;   :init
;;   (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
;;   :commands
;;   ansible)

;; (req-package ansible-doc
;;   :require
;;   yaml-mode
;;   :init
;;   (add-hook 'yaml-mode-hook #'ansible-doc-mode)
;;   :commands
;;   ansible-doc-mode)

;; (req-package company-ansible
;;   :require
;;   company
;;   yaml-mode
;;   :init
;;   (add-to-list 'company-backends 'company-ansible)
;;   (add-hook 'yaml-mode-hook '(lambda () (company-mode 1))))

;; (req-package jinja2-mode
;;   :init
;;   ;; Variant typically used with Ansible
;;   (add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))
;;   :commands
;;   jinja2-mode)

;; (req-package editorconfig
;;   :diminish editorconfig-mode
;;   :commands
;;   editorconfig-mode
;;   :config
;;   (editorconfig-mode 1))

;; (req-package terraform-mode
;;   :init
;;   (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
;;   :commands
;;   terraform-format-on-save-mode
;;   terraform-mode
;;   :mode
;;   ("\\.tf\\(vars\\)?\\'" . terraform-mode))

;; (req-package company-terraform
;;   :require
;;   terraform-mode
;;   company
;;   :init
;;   (add-hook 'terraform-mode-hook (lambda () (company-mode 1)))
;;   :config
;;   (add-to-list 'company-backends 'company-terraform))

;; (req-package plantuml-mode
;;   :init
;;   (setq plantuml-jar-path (expand-file-name "~/.nix-profile/lib/plantuml.jar"))
;;   :functions system-type-is-darwin
;;   :config
;;   (if (system-type-is-darwin)
;;       (progn
;;         (setenv "GRAPHVIZ_DOT" (expand-file-name "~/.nix-profile/bin/dot"))
;;         (add-to-list 'plantuml-java-args "-Dhttps.proxyHost=127.0.0.1")
;;         (add-to-list 'plantuml-java-args "-Dhttps.proxyPort=3129")))
;;   :commands
;;   plantuml-mode
;;   :mode
;;   "\\.p\\(lant\\)?uml$")

;; (req-package flycheck-plantuml
;;   :require
;;   flycheck
;;   plantuml-mode
;;   :hook
;;   (plantuml-mode . flycheck-mode)
;;   (flycheck-mode . flycheck-plantuml-setup))

(req-package suggest
  :bind ((:prefix-map lisp-evaluation-map
          :prefix "C-c e"
          ("s" . suggest))))

(req-package lua-mode
  :commands
  lua-mode
  :mode
  ("\\.lua\\'" . lua-mode))

(req-package projectile
  :demand t
  :init
  (setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)

  (setq projectile-mode-line-prefix " P") ; '(:eval (format " P[%s]" (projectile-project-name))))
  (setq projectile-mode-line-function
        (lambda ()
          (let ((project-name (projectile-project-name)))
            (format "%s[%s]"
                    projectile-mode-line-prefix
                    (or project-name "-")))))

  (defvar my-project-name nil)
  (put 'my-project-name 'safe-local-variable #'stringp)

  (defun projectile-project-name--prefer-mine (orig-fun &rest args)
    (or my-project-name (apply orig-fun args)))

  ;;(w32-register-hot-key [s-f])
  :config
  (projectile-mode +1)

  (advice-add 'projectile-project-name :around #'projectile-project-name--prefer-mine))
  ;; :bind
  ;; (:map projectile-mode-map
  ;;  ("s-d" . projectile-find-dir)
  ;;  ("s-p" . projectile-switch-project)
  ;;  ("s-f" . projectile-find-file)
  ;;  ("s-g" . projectile-grep)))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; From https://github.com/jordonbiondo/.emacs.d/blob/master/init.el
(req-package restclient
  :require
  json-mode
  :init
  (add-hook 'restclient-mode-hook
            (defun jordon-setup-restclient-mode ()
              (require 'js)
              (setq-local indent-line-function 'js-indent-line)
              (setq-local js-indent-level 2)
              (setq restclient-inhibit-cookies t)))
  :mode ("\\.\\(http\\|rest\\)$" . restclient-mode)
  :config
  ;; (add-hook 'restclient-response-loaded-hook
  ;;           (defun maybe-prettify-restclient-errors ()
  ;;             (if (search-forward "\0" nil t 1)
  ;;                 (let ((size (buffer-size)))
  ;;                   (delete-region (point-min) (point-max))
  ;;                   (insert (format "Binary file: %s" size)))
  ;;               (let ((things '("&lt;" "<br>" "&nbsp;" "\\n")))
  ;;                 (when (-any?
  ;;                        (lambda (str)
  ;;                          (save-excursion (search-forward str nil t 1)))
  ;;                        things)
  ;;                   (let ((reps '(("<br>" . "\n") ("&nbsp;" . " ") ("\\n" . "\n"))))
  ;;                     (dolist (rep reps)
  ;;                       (replace-string (car rep) (cdr rep) nil 1 (point-max)))))
  ;;                 (when (save-excursion
  ;;                         (goto-char (point-min))
  ;;                         (search-forward "<!DOCTYPE html>" (line-end-position) t 1))
  ;;                   (message "Rendering html...")
  ;;                   (let ((file (make-temp-file "restreponse" nil ".html"))
  ;;                         (str (buffer-string)))
  ;;                     (with-temp-file file (insert str))
  ;;                     (let ((response-text
  ;;                            (save-window-excursion
  ;;                              (eww-open-file file)
  ;;                              (let ((str (buffer-string)))
  ;;                                (kill-buffer (current-buffer))
  ;;                                str))))
  ;;                       (setf (buffer-string) response-text)
  ;;                       (goto-char (point-min))
  ;;                       (when (save-excursion (search-forward-regexp "\\_<def " nil t 1))
  ;;                         (ruby-mode)))))))))
  (add-hook 'restclient-response-loaded-hook
            (defun jordon-restclient-delete-headers-when-ok ()
              (when (equal major-mode 'js-mode)
                (save-excursion
                  (goto-char (point-max))
                  (when (and (search-backward "200 OK" nil t)
                             (search-backward "}" nil t))
                    (forward-char 1)
                    (delete-region (point) (point-max))
                    (newline)
                    (json-mode))))))
  ;(add-hook 'restclient-response-loaded-hook 'jordon-nice-wrap-mode)
  (add-hook 'restclient-response-loaded-hook
            (defun pulse-entire-buffer ()
              (save-excursion
                (goto-char (point-min))
                (pulse-momentary-highlight-region (point-min) (point-max))))))

(req-package restclient
  :require
  rainbow-delimiters
  :init
  (add-hook 'restclient-mode-hook 'rainbow-delimiters-mode))

(req-package json-mode
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (setq-local js-indent-level 2))))

(req-package hideshow
  :ensure
  nil
  :commands
  hs-minor-mode)

(req-package json-mode
  :require
  hideshow
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (hs-minor-mode 1))))

(req-package company-restclient
  :require
  restclient
  company
  :config
  (add-hook 'restclient-mode-hook (lambda () (company-mode 1)))
  (add-to-list 'company-backends 'company-restclient))

(provide 'common-settings)
