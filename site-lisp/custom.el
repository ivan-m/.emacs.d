;;; -*- no-byte-compile: t -*-
;; Don't byte-compile, as it will cause issues (loading older
;; byte-compiled file, etc.)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-verbatim-environments (quote ("verbatim" "verbatim*" "comment")))
 '(TeX-PDF-mode t)
 '(ac-use-fuzzy t)
 '(after-save-hook (quote (executable-make-buffer-file-executable-if-script-p)))
 '(ahs-case-fold-search nil)
 '(ahs-modes (quote (actionscript-mode apache-mode bat-generic-mode c++-mode c-mode csharp-mode css-mode dos-mode emacs-lisp-mode html-mode ini-generic-mode java-mode javascript-mode js-mode lisp-interaction-mode lua-mode latex-mode makefile-mode makefile-gmake-mode markdown-mode moccur-edit-mode nxml-mode nxhtml-mode outline-mode perl-mode cperl-mode php-mode python-mode rc-generic-mode reg-generic-mode ruby-mode sgml-mode sh-mode squirrel-mode text-mode tcl-mode visual-basic-mode haskell-mode)))
 '(apropos-do-all t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(calendar-christian-all-holidays-flag t)
 '(calendar-holidays nil)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(completion-ignored-extensions (quote (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".hi")))
 '(custom-enabled-themes (quote (alect-dark)))
 '(custom-safe-themes (quote ("a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "abfb5aa2ffb629678d90a5883a1902f63dfa6270447d3c0da6130c1250e5cb46" default)))
 '(default-frame-alist (quote ((font . "DejaVu Sans Mono-10") (vertical-scroll-bars . left))))
 '(delete-selection-mode t)
 '(desktop-base-file-name "desktop")
 '(desktop-base-lock-name "desktop.lock")
 '(desktop-file-name-format (quote tilde))
 '(desktop-save t)
 '(desktop-save-mode nil)
 '(diary-show-holidays-flag nil)
 '(dynamic-fonts-preferred-monospace-fonts (quote ("DejaVu Sans Mono" "Monaco" "Consolas" "Menlo" "Droid Sans Mono Pro" "Droid Sans Mono" "Inconsolata" "Source Code Pro" "Lucida Console" "Envy Code R" "Andale Mono" "Lucida Sans Typewriter" "monoOne" "Lucida Typewriter" "Panic Sans" "Bitstream Vera Sans Mono" "HyperFont" "PT Mono" "Ti92Pluspc" "Excalibur Monospace" "Courier New" "Courier" "Cousine" "Fira Mono" "Lekton" "Ubuntu Mono" "Liberation Mono" "BPmono" "Free Mono" "Anonymous Pro" "ProFont" "ProFontWindows" "Latin Modern Mono" "Code 2002" "ProggyCleanTT" "ProggyTinyTT")))
 '(dynamic-fonts-preferred-monospace-point-size 10)
 '(dynamic-fonts-preferred-proportional-point-size 10)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(exheres-skeleton-realname "Ivan Lazar Miljenovic <Ivan.Miljenovic@gmail.com>" t)
 '(flycheck-check-syntax-automatically (quote (save)))
 '(flycheck-disabled-checkers (quote (emacs-lisp-checkdoc haskell-ghc)))
 '(flycheck-mode-hook (quote (flycheck-color-mode-line-mode)))
 '(flyspell-issue-welcome-flag nil)
 '(font-utils-less-feedback t)
 '(global-auto-highlight-symbol-mode t)
 '(global-flycheck-mode t nil (flycheck))
 '(haskell-interactive-mode-eval-mode (quote haskell-mode))
 '(haskell-literate-default (quote bird))
 '(haskell-notify-p t)
 '(haskell-process-check-cabal-config-on-load t)
 '(haskell-process-log t)
 '(haskell-process-prompt-restart-on-cabal-change t)
 '(haskell-process-suggest-language-pragmas t)
 '(haskell-process-suggest-no-warn-orphans t)
 '(haskell-process-suggest-overloaded-strings t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(history-delete-duplicates t)
 '(hs-checkers-replace-with-suggestions t)
 '(icomplete-mode t)
 '(ido-all-frames t)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-confirm-unique-completion t)
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-enable-last-directory-history t)
 '(ido-enable-tramp-completion nil)
 '(ido-everywhere t)
 '(ido-hacks-mode t)
 '(ido-ignore-directories (quote ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "_darcs")))
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "_darcs/")))
 '(ido-mode (quote both) nil (ido))
 '(ido-read-file-name-non-ido (quote (TeX-master-file-ask)))
 '(ido-rotate-file-list-default t)
 '(ido-save-directory-list-file "~/.emacs.d/ido.last")
 '(ido-ubiquitous-mode t)
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-virtual-buffers t)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(ispell-highlight-p t)
 '(lpr-add-switches nil)
 '(lpr-command "gtklp")
 '(magit-diff-use-overlays nil)
 '(markdown-code-modes (quote (("haskell" . literate-haskell-mode) ("dot" . graphviz-dot-mode) ("bash" . sh-mode))))
 '(markdown-command "pandoc -Ss")
 '(markdown-enable-math t)
 '(markdown-italic-underscore t)
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-window-setup (quote other-window))
 '(preview-auto-cache-preamble t)
 '(preview-default-option-list (quote ("displaymath" "floats" "graphics" "textmath" "sections" "footnotes" "showlabels")))
 '(recentf-save-file "~/.emacs.d/recentf")
 '(rw-hunspell-default-dictionary "en_AU")
 '(rw-hunspell-dicpath-list (quote ("/usr/share/myspell")))
 '(rw-hunspell-make-dictionary-menu t)
 '(rw-hunspell-use-rw-ispell t)
 '(safe-local-variable-values (quote ((eval defun cell-to-list (s) (concat "[" (mapconcat (quote cell-quote) (split-string s) ", ") "]")) (eval defun cell-to-maybe (s) (if (string= "" s) "Nothing" (cell-parens (concat "Just " (cell-quote s))))) (eval defun cell-parens (s) (concat "(" s ")")) (eval defun cell-quote (s) (concat "\"" s "\"")) (eval turn-on-orgtbl))))
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/saveplace")
 '(scroll-bar-mode (quote left))
 '(shm-auto-insert-skeletons t)
 '(shm-use-hdevtools t)
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis))
 '(size-indication-mode t)
 '(smex-auto-update nil)
 '(smex-save-file "~/.emacs.d/smex-items")
 '(tab-width 4)
 '(text-mode-hook (quote (turn-on-auto-fill table-recognize text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(use-dialog-box nil)
 '(wdired-allow-to-change-permissions t)
 '(woman-imenu t)
 '(woman-use-own-frame nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil :family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 113 :width normal))))
 '(woman-bold ((t (:inherit bold :foreground "#00aff5"))) t)
 '(woman-italic ((t (:inherit italic :foreground "#3cb370"))) t))

; '(TeX-view-program-list (quote (("okular" "okular -unique %o#src:%n%b"))))
