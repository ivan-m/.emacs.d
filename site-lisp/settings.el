; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Startup and display settings

(eval-when-compile (require 'cl))

(exec-path-from-shell-initialize)

; From https://github.com/alezost/alect-themes#emacs-2431-and-earlier
(when (and (version<= "24" emacs-version)
           (version< emacs-version "24.4"))
  (defun face-spec-recalc (face frame)
    "Reset the face attributes of FACE on FRAME according to its specs.
This applies the defface/custom spec first, then the custom theme
specs, then the override spec."
    (while (get face 'face-alias)
      (setq face (get face 'face-alias)))
    (face-spec-reset-face face frame)
    ;; If FACE is customized or themed, set the custom spec from
    ;; `theme-face' records, which completely replace the defface spec
    ;; rather than inheriting from it.
    (let ((theme-faces (get face 'theme-face)))
      (if theme-faces
          (dolist (spec (reverse theme-faces))
            (face-spec-set-2 face frame (cadr spec)))
        (face-spec-set-2 face frame (face-default-spec face))))
    (face-spec-set-2 face frame (get face 'face-override-spec))))


(setq frame-title-format "Emacs : %b %+%+ %f")
(setq default-directory "~/")

;(add-hook 'server-switch-hook 'raise-frame)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

; Stop C-z from minimizing emacs.
;(when window-system
;    (global-unset-key "\C-z")) ; iconify-or-deiconify-frame (C-x C-z)
; Starting as a daemon seems to mean that the window-system check fails
(global-unset-key "\C-z")

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Loading packages

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(alect-themes
    auctex
    auto-complete
    auto-highlight-symbol
    bbdb
    csv-mode
    darcsum
    desktop
    dynamic-fonts
    exec-path-from-shell
    flycheck-color-mode-line
    flycheck-hdevtools
    flycheck-ledger
    ghc
    ghci-completion
    git-commit-mode
    git-rebase-mode
    graphviz-dot-mode
    haskell-mode
    helm
    ido-at-point
    ido-hacks
    ido-ubiquitous
    imenu-anywhere ;; uses ido, works better than idomenu
    ledger-mode
    lorem-ipsum
    magit
    markdown-mode
    markdown-mode+
    org
    org-pandoc
    pandoc-mode
    rainbow-delimiters
    rw-hunspell
    rw-ispell
    rw-language-and-country-codes
    smex
    unicode-fonts
    )
  )

; If a package isn't already installed (but is available), install it.
(dolist (p my-packages)
  (when (and (not (package-installed-p p))
             (assoc p package-archive-contents))
    (condition-case err
        (package-install p)
      (error
       (message "%s" (error-message-string err))))))

(load "ido-settings")
(load "helm-settings")
(load "darcsum-settings")
(load "markdown-settings")
(load "auctex-settings")
(load "haskell-settings")
(load "clipboard-settings")
(load "gtd")

(require 'tramp)
(defun tramp-set-auto-save ()
  (auto-save-mode -1))

; Doesn't seem to like being customized.
(require 'recentf)
(recentf-mode 1)
; Save recent list when idle for five minutes.
(run-with-idle-timer (* 5 60) t 'recentf-save-list)

(require 'notifications)

(require 'undohist)
(undohist-initialize)

(autoload 'Lorem-ipsum-insert-paragraphs "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-sentences "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-list "lorem-ipsum" "" t)

; smb-mode
(autoload 'smb-mode "smb-mode" nil t)
(setq auto-mode-alist (append '(("smb\\.conf$" . smb-mode))
                              auto-mode-alist))

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Global settings

(auto-fill-mode)

(global-set-key (kbd "C-x a r") 'align-regexp)

; Mainly programming settings
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(line-number-mode t)
(transient-mark-mode 1)

(setq require-final-newline 't)

(require 'whitespace)
(setq-default whitespace-style '(face empty tabs lines-tail trailing))
(setq-default whitespace-line-column 80)
(global-whitespace-mode 1)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(fset 'yes-or-no-p 'y-or-n-p) ; will allow you to type just "y" instead of "yes" when you exit.

(setq desktop-dirname user-emacs-directory)

(defadvice kill-line (before check-position activate)
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1))))

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

(autoload 'align-cols "align-cols" "Align text in the region." t)

(auto-image-file-mode t)

;'Woman' offers completion better than 'man'.
(defalias 'man 'woman)

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Small config

;(global-fixmee-mode 1)
;(add-hook 'after-change-major-mode-hook 'fci-mode)

;; (define-globalized-minor-mode global-fci-mode fci-mode
;;   (lambda ()
;;     ; fci-rule-column is buffer local
;;     (setq fci-rule-column 80)
;;     (fci-mode 1)))

;; (global-fci-mode 1)

; Doesn't play nicely
;(add-hook 'Custom-mode-hook 'turn-off-fci-mode)
;(add-hook 'package-menu-mode-hook 'turn-off-fci-mode)

(add-hook 'fundamental-mode-hook 'turn-on-orgtbl)
(add-hook 'csv-mode-hook 'turn-on-orgtbl)

(setq exheres-skeleton-realname "Ivan Lazar Miljenovic <Ivan.Miljenovic@gmail.com>")

; From http://exherbo.org/docs/emacs.html
(eval-after-load "diff"
  '(progn
     (require 'skeleton)
     (define-skeleton insert-exherbo-patch-header-skeleton
       "Inserts the standard Exherbo header for patches"
       nil
       "Source: " exheres-skeleton-realname "\n"
       "Upstream: \n"
       "Reason: \n")
     (require 'diff-mode)
     (define-key diff-mode-map "\C-c\C-p" 'insert-exherbo-patch-header-skeleton)))

;; (setq lpr-command "gtklp")
;; (setq-default ps-print-header nil)
;; (set-variable 'lpr-switches nil)

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; scratch buffer

;; bury *scratch* buffer instead of kill it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

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


; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Fonts and spelling

; Localisation

(set-language-environment "UTF-8")

(require 'rw-language-and-country-codes)
(require 'rw-ispell)
(require 'rw-hunspell)
(setq ispell-program-name "hunspell")

; Using the Daemon seems to prevent the specified default face from
; being used, so force it again.
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-10"))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mode-map (kbd "C-.") nil)
    ; '(define-key flyspell-mode-map (kbd "C-M-i") nil)
     (define-key flyspell-mode-map (kbd "C-,") nil)))
