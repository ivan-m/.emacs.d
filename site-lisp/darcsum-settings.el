(global-set-key (kbd "C-c d") 'darcsum-whatsnew)

;; (add-hook 'find-file-hooks 'warn-if-darcs-file)
;; (defun warn-if-darcs-file()
;;   (let ((f (buffer-file-name (current-buffer))))
;;     (and f (string-match "_darcs" f)
;;          (if (y-or-n-p "This is a _darcs file, open the real file? ")
;;              (jump-to-real-file-from-darcs)
;;            (push '(:propertize "_DARCS-FILE:" face font-lock-warning-face)
;;                  mode-line-buffer-identification)))))
;; (defun jump-to-real-file-from-darcs()
;;   (interactive)
;;   (let* ((f (buffer-file-name (current-buffer)))
;;          (match (string-match "_darcs/current" f)))
;;     (and f match
;;          (find-alternate-file
;;           (concat (substring f 0 (match-beginning 0))
;;                   (substring f (match-end 0)))))))

;; ; prevent accidental editing of files in darcs repository
;; (add-hook 'find-file-hooks 'label-darcs-file-with-warning)
;; ;; warn against accidental writing to a _darcs file
;; (add-hook 'write-file-hooks 'warn-writing-darcs-file)
;; ; affix a warning label to any _darcs file buffer
;; (defun label-darcs-file-with-warning()
;;   (let ((f (buffer-file-name (current-buffer))))
;;     (and f (string-match "_darcs" f)
;;          (rename-buffer (concat "_DARCS-FILE:" (buffer-name)) t))))
;; ; prevent accidental writing of files in darcs repository
;; (defun warn-writing-darcs-file()
;;   (let ((f (buffer-file-name (current-buffer))))
;;     (and f (string-match "_darcs" f)
;;          (if (not (y-or-n-p "WARNING: YOU ARE ABOUT TO WRITE TO an _darcs file, are you sure you want to do this? "))
;;            (keyboard-quit)))))
