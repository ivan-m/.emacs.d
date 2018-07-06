(eval-when-compile (require 'req-package))

(req-package magit
  :init
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (setq magit-delete-by-moving-to-trash nil)
  (setq magit-diff-use-overlays nil)
  (setq magit-merge-arguments '("--no-ff"))
  (setq magit-push-arguments '("--follow-tags"))
  (setq magit-push-always-verify nil)
  (setq magit-use-overlays nil)

  (when (system-type-is-win)
    (setq vc-handled-backends (delq 'Git vc-handled-backends))
    (setq magit-need-cygwin-noglob t)
    (setq magit-commit-dhow-diff nil)
    (setq magit-git-executable "/usr/libexec/git-core/git"))

  (add-hook 'magit-mode-hook #'endless/add-PR-fetch)
  :commands
  magit-status
  :config
  (magit-define-popup-switch 'magit-push-popup
    ?t "Follow tags" "--follow-tags")

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

  ;; http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html
  (defun endless/visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com\\(?:/\\|:\\)\\(.+\\)\\.git\\'" "\\1"
              (magit-get "remote"
                         (magit-get-push-remote)
                         "url"))
             (magit-get-current-branch))))

  ;; https://github.com/magit/magit/issues/3230
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace)

  ;; Based upon https://github.com/magit/magit/wiki/Tips-and-Tricks#show-staged-and-unstaged-changes-but-nothing-else
  (define-derived-mode magit-staging-mode magit-status-mode "Magit staging"
    "Mode for showing staged and unstaged changes."
    :group 'magit-status)
  (defun magit-staging-refresh-buffer ()
    (magit-insert-section (status)
      ;; (magit-insert-untracked-files)
      (magit-insert-unstaged-changes)
      (magit-insert-staged-changes)))
  (defun magit-staging ()
    (interactive)
    (magit-mode-setup #'magit-staging-mode))

  :bind (:map magit-mode-map
              ("O" . endless/visit-pull-request-url)))

(req-package magit-filenotify
  :require magit
  :commands magit-filenotify-mode
  :init (add-hook 'magit-status-mode-hook 'magit-filenotify-mode)
  :functions system-type-is-gnu
  :if (system-type-is-gnu))

(req-package bug-reference-github)

(req-package github-notifier)

(req-package git-gutter-fringe
  :init
  (setq git-gutter-fr:side 'right-fringe)
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode 1))

(provide 'magit-settings)
