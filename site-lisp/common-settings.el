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
      scoll-bar-mode 'left
      size-indication-mode t
      tool-bar-mode nil
      use-dialog-box nil

      default-directory "~/"

      tab-width 4
      truncate-lines t
      indent-tabs-mode nil
      after-save-hook 'executable-make-buffer-file-executable-if-script-p
      require-final-newline t

      apropos-do-all t
      history-length 1000
      history-delete-duplicates t

      bury-successful-compilation t
      compilation-message-face 'default

      text-mode-hook '(turn-on-auto-fill table-recognize text-mode-hook-identify))

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

(global-set-key (if (boundp 'mouse-wheel-down-event) ; Emacs 22+
                    (vector (list 'control
                                  mouse-wheel-down-event))
                  [C-mouse-wheel])    ; Emacs 20, 21
                'zoom-in)
(when (boundp 'mouse-wheel-up-event) ; Emacs 22+
  (global-set-key (vector (list 'control mouse-wheel-up-event))
                  'zoom-out))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-save-hook 'backup-each-save)

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

(defadvice message (before who-said-that activate)
  "Find out who said that thing. and say so."
  (let ((trace nil) (n 1) (frame nil))
    (while (setq frame (backtrace-frame n))
      (setq n     (1+ n)
            trace (cons (cadr frame) trace)) )
    (ad-set-arg 0 (concat "<<%S>>:\n" (ad-get-arg 0)))
    (ad-set-args 1 (cons trace (ad-get-args 1))) ))

;; (ad-disable-advice 'message 'before 'who-said-that)
(ad-update 'message)

(defadvice message (before when-was-that activate)
  "Add timestamps to `message' output."
  (ad-set-arg 0 (concat (format-time-string "[%Y-%m-%d %T %Z] ")
                        (ad-get-arg 0)) ))


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

(defadvice custom-theme-set-variables
    (around fix-inhibit-bug activate)
  "Allow setting of undefined variables in themes."
  (let (custom--inhibit-theme-enable)
    ad-do-it))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

;; OS-specific functionality

;; Check if system is Darwin/Mac OS X
(defun system-type-is-darwin ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin")
  )

;; Check if system is GNU/Linux
(defun system-type-is-gnu ()
  (interactive)
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux")
  )

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

(if (system-type-is-gnu)
    (setq default-frame-alist (quote ((font . "DejaVu Sans Mono-10")))))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(req-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (if (system-type-is-darwin)
      (exec-path-from-shell-copy-envs '("http_proxy" "https_proxy" "HTTP_PROXY" "HTTPS_PROXY" "no_proxy"))))

(req-package diminish
  :config
  ;; Doesn't seem to have a dedicated file for it
  (diminish 'auto-fill-function))

(req-package bind-key)

(req-package alect-themes
  :config
  ;; When using emacs --daemon, it seems that the cursor color isn't
  ;; set.  As such, use this to set it manually (needs to be set after
  ;; customize).
  ;;
  ;; Unfortunately, there doesn't seem to be any way of making this work
  ;; for all themes, and the theme-specific cursor color needs to be
  ;; used.
  (add-to-list 'default-frame-alist
               `(cursor-color . ,(alect-get-color 'dark 'cursor)))
  (load-theme 'alect-dark t)
  (set-face-attribute 'font-lock-type-face nil :foreground "#be59d8"))

(req-package windmove
  :init (defun ignore-error-wrapper (fn)
          "Funtion return new function that ignore errors.
   The function wraps a function with `ignore-errors' macro."
          (lexical-let ((fn fn))
            (lambda ()
              (interactive)
              (ignore-errors
                (funcall fn)))))
  :bind (([S-left]  . (ignore-error-wrapper 'windmoveleft))
         ([S-right] . (ignore-error-wrapper 'windmove-right))
         ([S-up]    . (ignore-error-wrapper 'windmove-up))
         ([S-down]  . (ignore-error-wrapper 'windmove-down))))

(req-package tramp
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
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  ;; Needs to be done before it's started: https://www.emacswiki.org/emacs/RecentFiles#toc12
  (add-to-list 'recentf-exclude "^/ssh:.*")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (add-to-list 'recentf-exclude ".*-autoloads\\.el\\'")
  (add-to-list 'recentf-exclude "[/\\]\\.elpa/")
  (setq recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1)
  ;; Save recent list when idle for five minutes.
  (run-with-idle-timer (* 5 60) t 'recentf-save-list))

(req-package notifications)

(req-package lorem-ipsum)

(req-package generic-x
  :mode ("smb\\.conf$" . samba-generic-mode))

(req-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(req-package align-cols
  :command align-cols)

(req-package whitespace
  :init
  (setq whitespace-style '(face tabs trailing empty))
  :diminish global-whitespace-mode
  :config
  (global-whitespace-mode 1))

(req-package flycheck
  :init
  (setq flycheck-check-syntax-automatically 'save)
  :config
  (add-to-list 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(req-package flyspell
  :init
  (setq flyspell-issue-message-flag nil)
  (setq flyspell-issue-welcome-flag nil)
  :diminish flyspell-mode
  :config
  (define-key flyspell-mode-map (kbd "C-.") nil)
  ;; '(define-key flyspell-mode-map (kbd "C-M-i") nil)
  (define-key flyspell-mode-map (kbd "C-,") nil))

(req-package auto-highlight-symbol
  :init
  (setq ahs-case-fold-search nil)
  :diminish auto-highlight-symbol-mode
  :config
  ;;(add-to-list 'ahs-modes 'haskell-mode)
  (global-auto-highlight-symbol-mode 1))

(req-package cap-words
  :diminish capitalized-words-mode)

(req-package mmm-mode
  :diminish mmm-mode)

(req-package subword
  :diminish subword-mode)

(req-package company
  :diminish (company-mode . "Co.")
  :config
  (push 'company-capf company-backends)
  (push 'company-dabbrev-code company-backends))

(req-package goto-chg
  :commands
  goto-last-change
  goto-last-change-reverse
  :bind (("C->" . goto-last-change)
         ("C-<" . gogo-last-change-reverse)))

(req-package paredit)

(req-package paredit-menu
  :require paredit)

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

(req-package org
  :commands
  turn-on-orgtbl
  org-mode
  :mode
  ("\\.org$" . org-mode)
  :config
  (add-hook 'fundamental-mode-hook 'turn-on-orgtbl)
  :defer t)

(req-package csv-mode
  :config org
  (add-hook 'csv-mode-hook 'turn-on-orgtbl))

(req-package ispell
  :init
  (setq ispell-highlight-p t)
  (setq ispell-program-name (if (system-type-is-darwin)
                                "/usr/local/bin/hunspell"
                              "hunspell")))

(req-package rw-hunspell
  :init
  (set-language-environment "UTF-8")
  (setq rw-hunspell-default-dictionary "en_AU_dictionaries")
  (setq rw-hunspell-dicpath-list '((expand-file-name "dictionaries" user-emacs-directory)))
  (setq rw-hunspell-make-dictionary-menu t)
  (setq rw-hunspell-use-rw-ispell t)
  :require
  ispell
  rw-language-and-country-codes
  rw-ispell
  :config
  (add-hook 'after-init-hook 'rw-hunspell-setup))

(req-package font-utils
  :init
  (font-utils-less-feedback t))

(req-package unicode-fonts
  :requires font-utils
  :init
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
           ("Symbola"))))
  (setq unicode-fonts-existence-checks 'first)
  (setq unicode-fonts-skip-font-groups
        '(chinese-simplified chinese-traditional low-quality-glyphs microsoft-only multicolor non-free))
  :config
  ;; Taken from https://github.com/rolandwalker/unicode-fonts/issues/3
  (defun my/init-fonts (&optional frame)
    (when (display-graphic-p frame)
      (when frame (select-frame frame))
      (unicode-fonts-setup)
      (remove-hook 'after-make-frame-functions 'my/init-fonts)))

  (add-hook 'after-init-hook
            (lambda ()
              (if (display-graphic-p)
                  (my/init-fonts)
                (add-hook 'after-make-frame-functions 'my/init-fonts)))))

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
  :mode ("\\.sql$" . sql-mode))

(req-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(req-package image-file
  :config
  (auto-image-file-mode 1))

(req-package files
  :init
  (setq backup-by-copying t)
  (setq backup-directory-alist '((expand-file-name "backups" user-emacs-directory)))
  (setq delete-old-versions t))

(req-package delsel
  :config
  (delete-selection-mode 1))

(req-package dired
  :init
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-alh")
  (setq wdired-allow-to-change-permissions t))

(req-package unkillable-scratch
  :config
  (unkillable-scratch 1))

(req-package man
  :init
  (setq Man-notify-method 'pushy))

(req-package woman
  :init
  (setq woman-fill-frame t)
  (setq woman-imenu t)
  (setq woman-use-own-frame nil)
  (set-face-attribute 'woman-italic nil :inherit italic :foreground "#3cb370")
  (set-face-attribute 'woman-bold nil :inherit bold :foreground "#00aff5")
  :commands
  woman)

(req-package compile
  :init
  (setq compilation-message-face 'default))

(req-package bury-successful-compilation
  :config
  (bury-succesful-compilation 1))

(req-package ediff
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))


;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(provide 'common-settings)