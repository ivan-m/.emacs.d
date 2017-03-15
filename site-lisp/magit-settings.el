(eval-when-compile (require 'req-package))

(req-package magit
  :init
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (setq magit-delete-by-moving-to-trash nil)
  (setq magit-diff-use-overlays nil)
  (setq magit-merge-arguments "--no-ff")
  (setq magit-push-always-verify nil)
  (setq magit-use-overlays nil)
  :commands
  magit-status
  :config
  ;; http://endlessparentheses.com/automatically-configure-magit-to-access-github-prs.html
  (defun endless/add-PR-fetch ()
    "If refs/pull is not defined on a GH repo, define it."
    (let ((fetch-address
           "+refs/pull/*/head:refs/pull/origin/*")
          (magit-remotes
           (magit-get-all "remote" "origin" "fetch")))
      (unless (or (not magit-remotes)
                  (member fetch-address magit-remotes))
        (when (string-match
               "github" (magit-get "remote" "origin" "url"))
        (magit-git-string
         "config" "--add" "remote.origin.fetch"
         fetch-address)))))

  (add-hook 'magit-mode-hook #'endless/add-PR-fetch)

  ;; http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html
  (defun endless/visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
                         (magit-get-push-remote)
                         "url"))
           (magit-get-current-branch))))

  :bind (:map magit-mode-map
              ("O" . endless/visit-pull-request-url)))

(req-package magit-filenotify
  :require magit
  :commands magit-filenotify-mode
  :config (add-hook 'magit-status-mode-hook 'magit-filenotify-mode)
  :functions system-type-is-gnu
  :if (system-type-is-gnu))

(req-package bug-reference-github)

(req-package github-notifier)

(provide 'magit-settings)
