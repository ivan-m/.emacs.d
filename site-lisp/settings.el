;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Startup and display settings

(eval-when-compile (require 'cl))

(exec-path-from-shell-initialize)

;; From https://github.com/alezost/alect-themes#emacs-2431-and-earlier
(when (version< emacs-version "24.3.50")
  (defun face-spec-recalc-new (face frame)
    "Improved version of `face-spec-recalc'."
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
    (face-spec-set-2 face frame (get face 'face-override-spec)))

  (defadvice face-spec-recalc (around new-recalc (face frame) activate)
    "Use `face-spec-recalc-new' instead."
    (face-spec-recalc-new face frame)))

(defadvice custom-theme-set-variables
    (around fix-inhibit-bug activate)
  "Allow setting of undefined variables in themes."
  (let (custom--inhibit-theme-enable)
    ad-do-it))

(setq frame-title-format "%b %+%+ %f"
      icon-title-format frame-title-format)
(setq default-directory "~/")

;; (add-hook 'server-switch-hook 'raise-frame)

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

;; Stop C-z from minimizing emacs.
;(when window-system
;;    (global-unset-key "\C-z")) ; iconify-or-deiconify-frame (C-x C-z)
;; Starting as a daemon seems to mean that the window-system check fails
(global-unset-key "\C-z")
(global-unset-key (kbd "C-x C-z"))

; Get current system's name
(defun insert-system-name()
  (interactive)
  "Get current system's name"
  (insert (format "%s" system-name))
  )

;; Get current system type
(defun insert-system-type()
  (interactive)
  "Get current system type"
  (insert (format "%s" system-type))
  )

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
      (global-set-key [s-left] 'beginning-of-line)
      (global-set-key [s-right] 'end-of-line)
      (server-start)))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Loading packages

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

;; Doesn't seem to like being customized.
(require 'recentf)
(recentf-mode 1)
;; Save recent list when idle for five minutes.
(run-with-idle-timer (* 5 60) t 'recentf-save-list)

(require 'notifications)

(autoload 'Lorem-ipsum-insert-paragraphs "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-sentences "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-list "lorem-ipsum" "" t)

;; smb-mode
(autoload 'smb-mode "smb-mode" nil t)
(setq auto-mode-alist (append '(("smb\\.conf$" . smb-mode))
                              auto-mode-alist))

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

;; http://endlessparentheses.com/automatically-configure-magit-to-access-github-prs.html
(defun endless/add-PR-fetch ()
  "If refs/pull is not defined on a GH repo, define it."
  (let ((fetch-address
         "+refs/pull/*/head:refs/pull/origin/*")
        (magit-remotes
         (magit-get-all "remote" "origin" "fetch")))
    (unless (or (not magit-remotes)
                (member fetch-address magit-remotes))
      (when (string-match
             "github" (magit-get "remote" "origin" "url"))
        (magit-git-string
         "config" "--add" "remote.origin.fetch"
         fetch-address)))))

(add-hook 'magit-mode-hook #'endless/add-PR-fetch)

;; http://endlessparentheses.com/easily-create-github-prs-from-magit.html?source=rss
(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-remote)
                       "url"))
           (cdr (or (magit-get-remote-branch)
                    (user-error "No remote branch"))))))

(eval-after-load 'magit
  '(define-key magit-mode-map "V"
     #'endless/visit-pull-request-url))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Global settings

(turn-on-auto-fill)

(global-set-key (kbd "C-x a r") 'align-regexp)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(fset 'yes-or-no-p 'y-or-n-p) ; will allow you to type just "y" instead of "yes" when you exit.

(eval-after-load 'desktop
  '(setq desktop-dirname user-emacs-directory))

(add-hook 'after-save-hook 'backup-each-save)

(defadvice kill-line (before check-position activate)
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1))))

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

(autoload 'align-cols "align-cols" "Align text in the region." t)


(global-set-key (if (boundp 'mouse-wheel-down-event) ; Emacs 22+
                    (vector (list 'control
                                  mouse-wheel-down-event))
                  [C-mouse-wheel])    ; Emacs 20, 21
                'zoom-in)
(when (boundp 'mouse-wheel-up-event) ; Emacs 22+
  (global-set-key (vector (list 'control mouse-wheel-up-event))
                  'zoom-out))

;; 'Woman' offers completion better than 'man'.
; (defalias 'man 'woman)

;; Don't have the indicators from these minor modes appear in the
;; modeline.
(eval-after-load "whitespace"
  '(diminish 'global-whitespace-mode))
(eval-after-load "flyspell"
  '(diminish 'flyspell-mode))
(eval-after-load "auto-highlight-symbol"
  '(diminish 'auto-highlight-symbol-mode))
(eval-after-load "cap-words"
  '(diminish 'capitalized-words-mode))
(eval-after-load "mmm-mode"
  '(diminish 'mmm-mode))
;; Doesn't seem to have a dedicated file for it
(diminish 'auto-fill-function)

(global-set-key (kbd "C->") 'goto-last-change)
(global-set-key (kbd "C-<") 'goto-last-change-reverse)

(eval-after-load 'outline
  '(progn
    (require 'outline-magic)
    (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle)))

(eval-after-load "paredit.el"
  '(require 'paredit-menu))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Small config

;; (global-fixmee-mode 1)
;; (add-hook 'after-change-major-mode-hook 'fci-mode)

;; (define-globalized-minor-mode global-fci-mode fci-mode
;;   (lambda ()
;;     ; fci-rule-column is buffer local
;;     (setq fci-rule-column 80)
;;     (fci-mode 1)))

;; (global-fci-mode 1)

;; Doesn't play nicely
;; (add-hook 'Custom-mode-hook 'turn-off-fci-mode)
;; (add-hook 'package-menu-mode-hook 'turn-off-fci-mode)

(add-hook 'fundamental-mode-hook 'turn-on-orgtbl)
(add-hook 'csv-mode-hook 'turn-on-orgtbl)

(bury-successful-compilation 1)

;; IIRC, this will need to be here otherwise exheres-skeleton-realname isn't evaluated until after exheres-mode is loaded.

;; (setq exheres-skeleton-realname "Ivan Lazar Miljenovic <Ivan.Miljenovic@gmail.com>")

;; From http://exherbo.org/docs/emacs.html
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

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; scratch buffer

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


;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Fonts and spelling

;; Localisation

(set-language-environment "UTF-8")

(if (system-type-is-darwin)
    (setq ispell-program-name "/usr/local/bin/hunspell"))

(if (system-type-is-gnu)
    (setq ispell-program-name "hunspell"))

(require 'rw-language-and-country-codes)
(require 'rw-ispell)
(require 'rw-hunspell)


(add-hook 'after-init-hook 'rw-hunspell-setup)

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mode-map (kbd "C-.") nil)
     ;; '(define-key flyspell-mode-map (kbd "C-M-i") nil)
     (define-key flyspell-mode-map (kbd "C-,") nil)))

(set-fontset-font t 'unicode "Symbola" nil 'prepend)

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
              (add-hook 'after-make-frame-functions 'my/init-fonts))))
