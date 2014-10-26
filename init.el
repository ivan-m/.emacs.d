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
  (let ((byte-compile-warnings '(not
                                 free-vars
                                 unresolved
                                 callargs
                                 redefine
                                 obsolete
                                 noruntime
                                 cl-functions
                                 interactive-only
                                 make-local
                                 mapcar
                                 constants
                                 suspicious)))
    (byte-recompile-directory dir 0)))

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
    bbdb
    csv-mode
    darcsum
    desktop
    diminish
    dynamic-fonts
    exec-path-from-shell
    flycheck-color-mode-line
    flycheck-haskell
    flycheck-hdevtools
    flycheck-ledger
    ghc
    ghci-completion
    git-commit-mode
    git-rebase-mode
    goto-chg
    graphviz-dot-mode
    haskell-mode
    helm
    htmlize
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
    paradox
    rainbow-delimiters
    rw-hunspell
    rw-ispell
    rw-language-and-country-codes
    shm
    smex
    unicode-fonts
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

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
