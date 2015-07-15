;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Specifying paths.

(defconst site-lisp-dir
  (expand-file-name "site-lisp" user-emacs-directory))

(setq custom-theme-directory
      (expand-file-name "themes" site-lisp-dir))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)

(defun init-compile-dir (dir)
  ;; Byte-compile a directory when starting emacs.
  (with-no-warnings (byte-recompile-directory dir 0)))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Loading existing packages. Need to do this here as byte-compilation
;; depends on some packages being available/loaded.

(require 'package)

;; In case we want to only load a sub-set.
;; (setq package-load-list
;;       '((fill-column-indicator t)))

(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)


(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(alect-themes
    auctex
    auto-complete
    auto-highlight-symbol
    auto-package-update
    bbdb
    bury-successful-compilation
    csv-mode
    darcsum
    desktop
    diminish
    dynamic-fonts
    exec-path-from-shell
    flycheck-color-mode-line
    flycheck-haskell
    flycheck-ledger
    ghc
    git-commit-mode
    git-rebase-mode
    goto-chg
    graphviz-dot-mode
    haskell-mode
    helm
    hindent
    htmlize
    ido-at-point
    ido-hacks
    ido-ubiquitous
    imenu-anywhere ;; uses ido, works better than idomenu
    ledger-mode
    lorem-ipsum
    magit
    magit-filenotify
    magit-gh-pulls
    markdown-mode
    markdown-mode+
    org
    org-pandoc
    outline-magic
    pandoc-mode
    paradox
    paredit
    paredit-menu
    rainbow-delimiters
    rw-hunspell
    rw-ispell
    rw-language-and-country-codes
    shm
    smex
    unicode-fonts
    unkillable-scratch
    zoom-frm
    )
  )

;; If a package isn't already installed (but is available), install it.
(dolist (p my-packages)
  (when (and (not (package-installed-p p))
             (assoc p package-archive-contents))
    (condition-case err
        (package-install p)
      (error
       (message "%s" (error-message-string err))))))

;; (auto-package-update-maybe)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Byte-compile settings.

(init-compile-dir site-lisp-dir)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Actually get things going.

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" site-lisp-dir))

;; Load this first as it needs to load items for settings, etc.
(load "settings")

(load custom-file)

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
