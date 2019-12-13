;;; -*- no-byte-compile: t -*-
;; Don't byte-compile, as it will cause issues (loading older
;; byte-compiled file, etc.)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((flycheck-disabled-checkers emacs-lisp-checkdoc)
     (eval defun cell-to-list
           (s)
           (concat "["
                   (mapconcat
                    (quote cell-quote)
                    (split-string s)
                    ", ")
                   "]"))
     (eval defun cell-to-maybe
           (s)
           (if
               (string= "" s)
               "Nothing"
             (cell-parens
              (concat "Just "
                      (cell-quote s)))))
     (eval defun cell-parens
           (s)
           (concat "(" s ")"))
     (eval defun cell-quote
           (s)
           (concat "\"" s "\""))
     (eval turn-on-orgtbl)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
