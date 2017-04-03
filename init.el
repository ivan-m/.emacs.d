;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Specifying paths.

(defconst site-lisp-dir
  (expand-file-name "site-lisp" user-emacs-directory))

(setq custom-theme-directory
      (expand-file-name "themes" user-emacs-directory))

(defconst lib-dir
  (expand-file-name "lib" user-emacs-directory))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" site-lisp-dir))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)

(add-to-list 'load-path lib-dir)

(defun init-compile-dir (dir)
  ;; Byte-compile a directory when starting emacs.
  (with-no-warnings (byte-recompile-directory dir 0)))

;; Want to make sure these are always available, no matter which order
;; the -settings files are loaded in.

;; Check if system is Darwin/Mac OS X
(defun system-type-is-darwin ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin"))

;; Check if system is GNU/Linux
(defun system-type-is-gnu ()
  (interactive)
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux"))

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

;; Just calling :pin in req-package blocks doesn't work:
;; https://github.com/jwiegley/use-package/issues/343#issuecomment-220463365
(setq package-pinned-packages '((ensime . "melpa-stable")))

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

;; Byte-compile settings; make sure we do this after req-package is installed.
(init-compile-dir site-lisp-dir)
(init-compile-dir lib-dir)
(init-compile-dir custom-theme-directory)

;; Just in case something went wrong with the byte-compilation
(setq load-prefer-newer t)

(req-package load-dir
  :force true
  :init
  (setq force-load-messages nil)
  (setq load-dir-debug nil)
  (setq load-dir-recursive t)
  :config
  (load-dir-one site-lisp-dir))

(req-package-finish)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; req-package / use-package don't seem to play nicely with themes:
;; https://github.com/jwiegley/use-package/issues/351

(try-install 'alect-themes)
(load-theme 'alect-dark t)

;; When using emacs --daemon, it seems that the cursor color isn't
;; set.  As such, use this to set it manually (needs to be set after
;; customize).
;;
;; Unfortunately, there doesn't seem to be any way of making this work
;; for all themes, and the theme-specific cursor color needs to be
;; used.
(add-to-list 'default-frame-alist
             `(cursor-color . ,(alect-get-color 'dark 'cursor)))
(set-face-attribute 'font-lock-type-face nil :foreground "#be59d8")

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Now actually load the custom settings; this shouldn't be much.

(load custom-file :noerror)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
