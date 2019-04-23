(eval-when-compile (require 'req-package))

(req-package magit
  :init
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (setq magit-delete-by-moving-to-trash nil)
  (setq magit-diff-use-overlays nil)
  (setq magit-merge-arguments '("--no-ff"))
  (setq magit-cherry-pick-arguments '("-x"))
  (setq magit-push-arguments '("--follow-tags"))
  (setq magit-push-always-verify nil)
  (setq magit-use-overlays nil)

  (when (system-type-is-win)
    (setq vc-handled-backends (delq 'Git vc-handled-backends))
    (setq magit-need-cygwin-noglob t)
    (setq magit-commit-show-diff nil)
    (setq magit-git-executable "/usr/libexec/git-core/git"))

  (add-hook 'magit-mode-hook #'endless/add-PR-fetch)
  :commands
  magit-status
  magit-staging
  :config
  (transient-append-suffix 'magit-log "-A"
    '("-m" "Omit merge commits" "--no-merges"))
  (push "--first-parent" magit-log-arguments)
  (transient-append-suffix 'magit-log "-m"
    '("-1" "First parent in merge" "--first-parent"))

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
      (magit-insert-untracked-files)
      (magit-insert-unstaged-changes)
      (magit-insert-staged-changes)))
  (defun magit-staging ()
    (interactive)
    (magit-mode-setup #'magit-staging-mode))

  ;; Protect against accident pushes to upstream
  (defadvice magit-push-current-to-upstream
      (around my-protect-accidental-magit-push-current-to-upstream)
    "Protect against accidental push to upstream.

Causes `magit-git-push' to ask the user for confirmation first."
    (let ((my-magit-ask-before-push t))
      ad-do-it))

  (defadvice magit-git-push (around my-protect-accidental-magit-git-push)
    "Maybe ask the user for confirmation before pushing.

Advice to `magit-push-current-to-upstream' triggers this query."
    (if (bound-and-true-p my-magit-ask-before-push)
        ;; Arglist is (BRANCH TARGET ARGS)
        (if (yes-or-no-p (format "Push %s branch upstream to %s? "
                                 (ad-get-arg 0) (ad-get-arg 1)))
            ad-do-it
          (error "Push to upstream aborted by user"))
      ad-do-it))

  (ad-activate 'magit-push-current-to-upstream)
  (ad-activate 'magit-git-push)

  :bind (:map magit-mode-map
              ("O" . endless/visit-pull-request-url))
  )

(req-package magit-filenotify
  :require magit
  :commands magit-filenotify-mode
  :init (add-hook 'magit-status-mode-hook 'magit-filenotify-mode)
  :functions system-type-is-gnu
  :if (system-type-is-gnu))

(req-package bug-reference-github
  :if (system-type-is-gnu)
  :functions system-type-is-gnu)

(req-package github-notifier
  :if (system-type-is-gnu)
  :functions system-type-is-gnu)

(req-package git-gutter-fringe
  :init
  (setq git-gutter-fr:side 'right-fringe)
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode 1))

(provide 'magit-settings)
