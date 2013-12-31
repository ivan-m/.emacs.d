(require 'ido)
(eval-after-load "ido" '(require 'ido-hacks))

;; (defadvice ido-find-file (after find-file-sudo disable) ;activate
;;   "Find file as root if necessary."
;;   (unless (and buffer-file-name
;;                (file-writable-p buffer-file-name))
;;     (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun find-alternate-file-with-sudo ()
  "Re-open with sudo."
  (interactive)
  (find-alternate-file (concat "/sudo::" (buffer-file-name))))

; ido-like functionality for minibuffer commands
; set above
; (mcomplete-mode)

;; (setq smex-save-file "~/.emacs.d/smex.save") ;; keep my ~/ clean
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "<menu>") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; (global-set-key (kbd "C-c M-x") 'smex-update-and-run)

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "<menu>") 'smex)

; speed smex up
(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache)
    (smex-update)))
(add-hook 'after-load-functions 'smex-update-after-load)

;; This is your old M-x.
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

;; (setq ido-execute-command-cache nil)

;; (defun ido-execute-command ()
;;   (interactive)
;;   (call-interactively
;;    (intern
;;     (ido-completing-read
;;      "M-x "
;;      (all-completions "" obarray 'commandp)))))

  ;; (interactive)
  ;; (call-interactively
  ;;  (intern
  ;;   (ido-completing-read
  ;;    "M-x "
  ;;    (progn
  ;;      (unless ido-execute-command-cache
  ;;        (mapatoms (lambda (s)
  ;;                    (when (commandp s)
  ;;                      (setq ido-execute-command-cache
  ;;                            (cons (format "%S" s) ido-execute-command-cache))))))
  ;;      ido-execute-command-cache)))))


;; (global-set-key "\M-x" 'ido-execute-command)
;; (global-set-key (kbd "<menu>") 'ido-execute-command)

; Still needed?
(eval-after-load "icomplete" '(progn (require 'icomplete+)))

(defun my-icompleting-read(prompt choices)
  (let ((ido-make-buffer-list-hook
         (lambda ()
           (setq ido-temp-list choices))))
    (ido-read-buffer prompt)))


(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map [tab] 'ido-complete)))

; sort ido filelist by mtime instead of alphabetically
;(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
;(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
                      (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
                  (if (= (nth 0 ta) (nth 0 tb))
                      (> (nth 1 ta) (nth 1 tb))
                    (> (nth 0 ta) (nth 0 tb)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (if (string-equal (substring x 0 1) ".") x))
              ido-temp-list))))

;; from http://www.emacswiki.org/cgi-bin/wiki/ImenuMode
(defun try-to-add-imenu ()
  (condition-case nil (imenu-add-menubar-index) (error nil)))
(add-hook 'font-lock-mode-hook 'try-to-add-imenu)

(global-set-key (kbd "C-c i") 'ido-imenu-anywhere)


(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key "\C-x\C-r" 'recentf-ido-find-file)

    ;;; integrate ido with artist-mode
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
                                            ("Spray-chars" . spray-chars))))))))

(add-hook 'artist-mode-init-hook
          (lambda ()
            (define-key artist-mode-map (kbd "C-c C-a C-o") 'artist-ido-select-operation)
            (define-key artist-mode-map (kbd "C-c C-a C-c") 'artist-ido-select-settings)))
