;; (require 'helm)
;; (require 'helm-config)
;; (require 'helm-match-plugin) ; Optional
;; (require 'helm-man)
;; (require 'helm-elisp)

;; (defun my-helm ()
;;   (interactive)
;;   (helm-other-buffer
;;    '(helm-c-source-buffers-list
;;      helm-c-source-recentf
;;      helm-c-source-buffer-not-found
;;      helm-c-source-file-name-history
;;      helm-c-source-info-pages
;;      helm-c-source-info-elisp
;;      helm-c-source-man-pages
;;      helm-c-source-locate
;;      ; helm-c-source-emacs-commands
;;      )
;;    helm-buffer ; " *my-helm*"
;;    ))

(global-set-key [f9] 'helm-mini)
