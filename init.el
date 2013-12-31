; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Specifying paths.

; covered by site-start?
;(load "/usr/share/emacs/site-lisp/site-exherbo")

(defconst site-lisp-dir
  (expand-file-name "site-lisp" user-emacs-directory))

(setq custom-theme-directory
      (expand-file-name "themes" site-lisp-dir))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Loading existing packages. Need to do this here as byte-compilation
; depends on some packages being available/loaded.

(require 'package)

;; In case we want to only load a sub-set.
;; (setq package-load-list
;;       '((fill-column-indicator t)))

(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Until structured-haskell-mode gets available via package.el,
; load manually.

(defconst shm-loc
  (expand-file-name "structured-haskell-mode/elisp"
                    user-emacs-directory))

(add-to-list 'load-path shm-loc)

(let ((byte-compile-warnings nil))
  (byte-recompile-directory shm-loc 0))

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Byte-compile settings.

(let ((byte-compile-warnings nil))
  (byte-recompile-directory site-lisp-dir 0))

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Actually get things going.

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" site-lisp-dir))

; Load this first as it needs to load items for settings, etc.
(load "settings")

(load custom-file)
