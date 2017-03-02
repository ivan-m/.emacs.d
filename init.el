;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Specifying paths.

(defconst site-lisp-dir
  (expand-file-name "site-lisp" user-emacs-directory))

(setq custom-theme-directory
      (expand-file-name "themes" site-lisp-dir))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" site-lisp-dir))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)

(defun init-compile-dir (dir)
  ;; Byte-compile a directory when starting emacs.
  (with-no-warnings (byte-recompile-directory dir 0)))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Loading existing packages. Need to do this here as byte-compilation
;; depends on some packages being available/loaded.

(require 'package)
(setq package-enable-at-startup nil)

;; I don't want package-selected-packages saved anywhere, so re-define
;; this function to prevent it.
(defun package--save-selected-packages (&rest opt) nil)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defun try-install (package)
  "Try and install the specified package manually; die if it fails"
  (when (and (not (package-installed-p package))
             (assoc package package-archive-contents))
    (condition-case err
        (package-install package)
      (error
       (message "%s" (error-message-string err))))))

(try-install 'req-package)
(require 'req-package)

;; Load all packages from site-lisp-dir
(req-package load-dir
  :force true
  :init
  (setq force-load-messages nil)
  (setq load-dir-debug nil)
  (setq load-dir-recursive t)
  :config
  (load-dir-one site-lisp-dir)
  (req-package-finish))

;; (when (boundp 'package-pinned-packages)
;;   (setq package-pinned-packages
;;         '((ensime . "melpa-stable"))))

;; (defvar my-packages
;;   '(alect-themes
;;     auctex
;;     auto-complete
;;     auto-highlight-symbol
;;     auto-package-update
;;     bbdb
;;     bug-reference-github
;;     bury-successful-compilation
;;     cider
;;     clojure-cheatsheet
;;     company
;;     company-cabal
;;     company-ghci
;;     crux
;;     csv-mode
;;     darcsum
;;     desktop
;;     diff-hl
;;     diminish
;;     diredful
;;     dynamic-fonts
;;     ebal
;;     exec-path-from-shell
;;     flycheck-color-mode-line
;;     flycheck-haskell
;;     flycheck-ledger
;;     ghc
;;     github-notifier
;;     goto-chg
;;     graphviz-dot-mode
;;     haskell-mode
;;     helm
;;     hindent
;;     htmlize
;;     ido-at-point
;;     ido-ubiquitous
;;     imenu-anywhere ;; uses ido, works better than idomenu
;;     ledger-mode
;;     lorem-ipsum
;;     magit
;;     magit-filenotify
;;     markdown-mode
;;     markdown-mode+
;;     mmm-mode
;;     nix-mode
;;     nixos-options
;;     org
;;     org-pandoc
;;     outline-magic
;;     pandoc-mode
;;     paradox
;;     paredit
;;     paredit-menu
;;     rainbow-delimiters
;;     rw-hunspell
;;     rw-ispell
;;     rw-language-and-country-codes
;;     shm
;;     smex
;;     sql-indent
;;     sqlup-mode
;;     typed-clojure-mode
;;     unicode-fonts
;;     unkillable-scratch
;;     yaml-mode
;;     zoom-frm
;;     )
;;   )

;; ;; If a package isn't already installed (but is available), install it.
;; (dolist (p my-packages)
;;   (when (and (not (package-installed-p p))
;;              (assoc p package-archive-contents))
;;     (condition-case err
;;         (package-install p)
;;       (error
;;        (message "%s" (error-message-string err))))))

;; (auto-package-update-maybe)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Byte-compile settings.

(init-compile-dir site-lisp-dir)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Actually get things going.

;; Load this first as it needs to load items for settings, etc.
(load "settings")

(load custom-file :noerror)

;; When using emacs --daemon, it seems that the cursor color isn't
;; set.  As such, use this to set it manually (needs to be set after
;; customize).
;;
;; Unfortunately, there doesn't seem to be any way of making this work
;; for all themes, and the theme-specific cursor color needs to be
;; used.

(add-to-list 'default-frame-alist
             `(cursor-color . ,(alect-get-color 'dark 'cursor)))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
