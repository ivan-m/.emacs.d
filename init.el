;;; -*- no-byte-compile: t -*-
;; Don't byte-compile, as it will cause issues (loading older
;; byte-compiled file, etc.)

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Specifying paths.

(setq default-gc-cons-threshold gc-cons-threshold)

(setq gc-cons-threshold 100000000)

(setq toggle-debug-on-error t)

(defconst site-lisp-dir
  (expand-file-name "site-lisp" user-emacs-directory))

(setq custom-theme-directory
      (expand-file-name "themes" user-emacs-directory))

(defconst lib-dir
  (expand-file-name "lib" user-emacs-directory))

;; Work config found here; don't store in git!
(defconst work-dir
  (expand-file-name "work" user-emacs-directory))

;; Directory containing work-specific configurations.
;; This shouldn't be stored in Git.
(defconst work-site-lisp-dir
  (expand-file-name "site-lisp" work-dir))

;; Directory containing work-specific libraries.
;; This shouldn't be stored in Git.
(defconst work-lib-dir
  (expand-file-name "lib" work-dir))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Additional settings for work purposes
(setq work-custom-file (expand-file-name "work-custom.el" work-dir))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)

(add-to-list 'load-path lib-dir)

(if (file-directory-p work-lib-dir)
    ;; https://www.emacswiki.org/emacs/LoadPath
    (let ((default-directory  work-lib-dir))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path)))

(if (file-directory-p work-site-lisp-dir)
  (add-to-list 'load-path work-site-lisp-dir))

(defun init-compile-dir (dir)
  ;; Byte-compile a directory when starting emacs.
  (with-no-warnings (byte-recompile-directory dir 0))
  )

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

(defun system-type-is-win ()
  (interactive)
  "Return true if system is Windows-based"
  (memq system-type '(windows-nt ms-dos cygwin)))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Loading existing packages. Need to do this here as byte-compilation
;; depends on some packages being available/loaded.

(require 'package)
(setq package-enable-at-startup nil)

;; I don't want package-selected-packages saved anywhere, so re-define
;; this function to prevent it.
(defun package--save-selected-packages (&rest opt) nil)

;; Don't suggest removing packages
(defun package--removable-packages () nil)

(let* ((no-ssl (and (system-type-is-win)
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (setq package-archives `(,(cons "gnu" (concat proto "://elpa.gnu.org/packages/"))
                           ,(cons "melpa" (concat proto "://melpa.org/packages/"))
                           ,(cons "melpa-stable" (concat proto "://stable.melpa.org/packages/"))
                           ;; ,(cons "org" "http://orgmode.org/elpa/") ; use org-mode shipped with Emacs
                           )))

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

(setq use-package-compute-statistics t)
(setq use-package-always-ensure t)
(setq use-package-enable-imenu-support t)

(try-install 'req-package)

(require 'req-package)

;; Byte-compile settings; make sure we do this after req-package is installed.
(init-compile-dir site-lisp-dir)
(init-compile-dir lib-dir)
(init-compile-dir custom-theme-directory)
(if (file-directory-p work-lib-dir) (init-compile-dir work-lib-dir))
(if (file-directory-p work-site-lisp-dir) (init-compile-dir work-site-lisp-dir))

;; Just in case something went wrong with the byte-compilation
(setq load-prefer-newer t)

;; To enable packages below to override this value
(setq chosen-theme 'alect-dark)

(let ((file-name-handler-alist nil))

  (req-package url-methods
    :ensure nil
    :commands
    url-scheme-register-proxy)

  ;; Explicitly set this up to try and make sure proxy is set
  ;; correctly for the rest.
  (req-package exec-path-from-shell
    :require
    url-methods
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    :config
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "http_proxy" "https_proxy" "HTTP_PROXY" "HTTPS_PROXY" "no_proxy" "NO_PROXY" "GIT_SSH" "NIX_PROFILES" "NIX_PATH" "NIX_REMOTE"))
    (exec-path-from-shell-initialize)

    ;; This should automatically get populated from the environment variables when needed.

    ;; ;; 10.0.2.2 is the host system when running in VirtualBox.
    ;; (setq url-proxy-services '(("no_proxy"  ."^\\(localhost\\|10.0.2.2\\|127.0.0.1\\)")))
    ;; (url-scheme-register-proxy "http")
    ;; (url-scheme-register-proxy "https")
    )

  (req-package load-dir
    :force true
    :init
    (setq force-load-messages nil)
    (setq load-dir-debug nil)
    (setq load-dir-recursive t)
    :config
    (add-to-list 'load-dir-ignored "custom\.elc?")
    (load-dir-one site-lisp-dir)
    (if (file-directory-p work-site-lisp-dir) (load-dir-one work-site-lisp-dir)))

  (req-package-finish))

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; req-package / use-package don't seem to play nicely with themes:
;; https://github.com/jwiegley/use-package/issues/351

(try-install 'alect-themes)

(setq alect-overriding-faces
      '((font-lock-type-face ((nil :foreground  "#be59d8")))))

(load-theme chosen-theme t)

;; When using emacs --daemon, it seems that the cursor color isn't
;; set.  As such, use this to set it manually (needs to be set after
;; customize).
;;
;; Unfortunately, there doesn't seem to be any way of making this work
;; for all themes, and the theme-specific cursor color needs to be
;; used.
(add-to-list 'default-frame-alist
             `(cursor-color . ,(alect-get-color 'dark 'cursor)))

;; Doesn't seem to work above
(set-face-attribute 'font-lock-type-face nil :foreground "#be59d8")

;; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;; Now actually load the custom settings; this shouldn't be much.

(load custom-file :noerror)

(if (file-exists-p work-custom-file)
    (load work-custom-file :noerror))

(setq gc-cons-threshold default-gc-cons-threshold)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
