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

;; Byte-compile settings; make sure we do this after req-package is installed.
(with-no-warnings (byte-recompile-directory site-lisp-dir 0))

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
;; Now actually load the custom settings; this shouldn't be much.

(load custom-file :noerror)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
