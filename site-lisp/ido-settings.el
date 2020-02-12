(eval-when-compile (require 'req-package))

(req-package ido
  :demand t
  :init
  (setq ido-all-frames t)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-case-fold t)
  (setq ido-confirm-unique-completion t)
  (setq ido-default-buffer-method 'selected-window)
  (setq ido-default-file-method 'selected-window)
  (setq ido-enable-flex-matching t)
  (setq ido-enable-last-directory-history t)
  (setq ido-enable-tramp-completion nil)
  (setq ido-ignore-directories '("\\`CVS/" "\\`\\.\\./" "\\`\\./" "_darcs"))
  (setq ido-ignore-files
        '(".*_flymake.hs" ".*\\.hie" "\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "\\`_darcs/" "\\`\\.DS_Store" "\\`cabal\\.sandbox\\.config" "\\`\\.cabal-sandbox/"))
  (setq ido-max-work-file-list 100)
  (setq ido-read-file-name-non-ido '(TeX-master-file-ask))
  (setq ido-rotate-file-list-default t)
  (setq ido-save-directory-list-file (expand-file-name "ido.last" user-emacs-directory))
  (setq ido-use-filename-at-point 'guess)
  (setq ido-use-virtual-buffers t)

  :require
  recentf

  :commands
  ido-completing-read
  ido-completion-map
  ido-complete

  :bind (("C-x C-r" . recentf-ido-find-file)
         :map ido-file-dir-completion-map
         ("SPC" . self-insert-command))
  :config
  (ido-mode 1)
  (ido-everywhere 1)

  (defun recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
      (when file
        (find-file file)))))

(req-package ido-at-point
  :require ido)

(req-package ido-hacks
  :defer t
  :require ido)

(req-package ido-completing-read+
  :require ido
  :config
  (ido-ubiquitous-mode t))

(req-package crux
  :config
  (crux-reopen-as-root-mode))

(req-package smex
  :require ido-completing-read+
  :init
  (setq smex-auto-update nil)
  (setq smex-save-file (expand-file-name "smex-items" user-emacs-directory))

  (add-hook 'after-load-functions 'smex-update-after-load)

  ;; speed smex up
  (defun smex-update-after-load (unused)
    (when (boundp 'smex-cache)
      (smex-update)))
  :commands
  smex
  smex-major-mode-commands
  :bind (("M-x"     . smex)
         ("M-X"     . smex-major-mode-commands)
         ("<menu>"  . smex)
         ("C-c M-x" . execute-extended-command)
         ("<apps>"  . smex))
  :config
  (smex-initialize))

(req-package smex
  :bind ("s-SPC" . smex)
  :functions system-type-is-darwin
  :if (system-type-is-darwin))

(req-package icomplete
  :config
  (icomplete-mode 1))

(req-package imenu
  :demand t
  :init
  (setq imenu-auto-rescan t)
  :commands
  imenu
  :bind ("C-c i" . imenu)
  :config
  ;; from http://www.emacswiki.org/cgi-bin/wiki/ImenuMode
  ;; (defun try-to-add-imenu ()
  ;;   (condition-case nil (imenu-add-menubar-index) (error nil)))
  ;; (add-hook 'font-lock-mode-hook 'try-to-add-imenu)
  )

(req-package artist
  :require ido
  :bind
  (:map artist-mode-map
   ("C-C C-a C-o" . artist-ido-select-operation)
   ("C-c C-a C-c" . artist-ido-select-settings))
  :config

  ;; integrate ido with artist-mode: https://www.emacswiki.org/emacs/ArtistMode
  (defun artist-ido-select-operation (type)
    "Use ido to select a drawing operation in artist-mode"
    (interactive (list (ido-completing-read "Drawing operation: "
                                            (list "Pen" "Pen Line" "line" "straight line" "rectangle"
                                                  "square" "poly-line" "straight poly-line" "ellipse"
                                                  "circle" "text see-thru" "text-overwrite" "spray-can"
                                                  "erase char" "erase rectangle" "vaporize line" "vaporize lines"
                                                  "cut rectangle" "cut square" "copy rectangle" "copy square"
                                                  "paste" "flood-fill"))))
    (artist-select-operation type))

  (defun artist-ido-select-settings (type)
    "Use ido to select a setting to change in artist-mode"
    (interactive (list (ido-completing-read "Setting: "
                                            (list "Set Fill" "Set Line" "Set Erase" "Spray-size" "Spray-chars"
                                                  "Rubber-banding" "Trimming" "Borders"))))
    (if (equal type "Spray-size")
        (artist-select-operation "spray set size")
      (call-interactively (artist-fc-get-fn-from-symbol
                           (cdr (assoc type '(("Set Fill" . set-fill)
                                              ("Set Line" . set-line)
                                              ("Set Erase" . set-erase)
                                              ("Rubber-banding" . rubber-band)
                                              ("Trimming" . trimming)
                                              ("Borders" . borders)
                                              ("Spray-chars" . spray-chars)))))))))

(provide 'ido-settings)
