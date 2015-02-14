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
 '(after-save-hook
   (quote
    (executable-make-buffer-file-executable-if-script-p)))
 '(ahs-case-fold-search nil)
 '(ahs-modes
   (quote
    (actionscript-mode apache-mode bat-generic-mode c++-mode c-mode csharp-mode css-mode dos-mode emacs-lisp-mode html-mode ini-generic-mode java-mode javascript-mode js-mode lisp-interaction-mode lua-mode latex-mode makefile-mode makefile-gmake-mode markdown-mode moccur-edit-mode nxml-mode nxhtml-mode outline-mode perl-mode cperl-mode php-mode python-mode rc-generic-mode reg-generic-mode ruby-mode sgml-mode sh-mode squirrel-mode text-mode tcl-mode visual-basic-mode haskell-mode)))
 '(apropos-do-all t)
 '(auto-image-file-mode t)
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(browse-url-browser-function (quote eww-browse-url))
 '(calendar-christian-all-holidays-flag t)
 '(calendar-holidays nil)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(completion-ignored-extensions
   (quote
    (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".hi")))
 '(custom-enabled-themes (quote (alect-dark)))
 '(custom-safe-themes
   (quote
    ("a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" default)))
 '(default-frame-alist (quote ((font . "DejaVu Sans Mono-10"))))
 '(delete-old-versions t)
 '(delete-selection-mode t)
 '(desktop-base-file-name "desktop")
 '(desktop-base-lock-name "desktop.lock")
 '(desktop-file-name-format (quote tilde))
 '(desktop-save t)
 '(desktop-save-mode nil)
 '(diary-show-holidays-flag nil)
 '(dynamic-fonts-preferred-monospace-fonts
   (quote
    ("DejaVu Sans Mono" "Monaco" "Consolas" "Menlo" "Droid Sans Mono Pro" "Droid Sans Mono" "Inconsolata" "Source Code Pro" "Lucida Console" "Envy Code R" "Andale Mono" "Lucida Sans Typewriter" "monoOne" "Lucida Typewriter" "Panic Sans" "Bitstream Vera Sans Mono" "HyperFont" "PT Mono" "Ti92Pluspc" "Excalibur Monospace" "Courier New" "Courier" "Cousine" "Fira Mono" "Lekton" "Ubuntu Mono" "Liberation Mono" "BPmono" "Free Mono" "Anonymous Pro" "ProFont" "ProFontWindows" "Latin Modern Mono" "Code 2002" "ProggyCleanTT" "ProggyTinyTT")))
 '(dynamic-fonts-preferred-monospace-point-size 10)
 '(dynamic-fonts-preferred-proportional-point-size 10)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(exheres-skeleton-realname "Ivan Lazar Miljenovic <Ivan.Miljenovic@gmail.com>" t)
 '(flycheck-check-syntax-automatically (quote (save)))
 '(flycheck-mode-hook (quote (flycheck-color-mode-line-mode)))
 '(flyspell-issue-welcome-flag nil)
 '(font-utils-less-feedback t)
 '(global-auto-highlight-symbol-mode t)
 '(global-whitespace-mode t)
 '(haskell-ask-also-kill-buffers nil)
 '(haskell-interactive-mode-eval-mode (quote haskell-mode))
 '(haskell-interactive-mode-hide-multi-line-errors t)
 '(haskell-literate-default (quote bird))
 '(haskell-notify-p t)
 '(haskell-process-args-cabal-repl nil)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-check-cabal-config-on-load t)
 '(haskell-process-log t)
 '(haskell-process-prompt-restart-on-cabal-change t)
 '(haskell-process-suggest-haskell-docs-imports t)
 '(haskell-process-suggest-language-pragmas t)
 '(haskell-process-suggest-no-warn-orphans t)
 '(haskell-process-suggest-overloaded-strings t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(history-delete-duplicates t)
 '(history-length 1000)
 '(hs-checkers-replace-with-suggestions t)
 '(icomplete-mode t)
 '(ido-all-frames t)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-case-fold t)
 '(ido-confirm-unique-completion t)
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-enable-last-directory-history t)
 '(ido-enable-tramp-completion nil)
 '(ido-everywhere t)
 '(ido-hacks-mode t)
 '(ido-ignore-directories (quote ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "_darcs")))
 '(ido-ignore-files
   (quote
    ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "_darcs/")))
 '(ido-max-work-file-list 100)
 '(ido-mode (quote both) nil (ido))
 '(ido-read-file-name-non-ido (quote (TeX-master-file-ask)))
 '(ido-rotate-file-list-default t)
 '(ido-save-directory-list-file "~/.emacs.d/ido.last")
 '(ido-ubiquitous-mode t)
 '(ido-use-filename-at-point (quote guess))
 '(ido-use-virtual-buffers t)
 '(imenu-auto-rescan t)
 '(indent-tabs-mode nil)
 '(inhibit-default-init t)
 '(ispell-highlight-p t)
 '(ispell-program-name "hunspell")
 '(lpr-add-switches nil)
 '(lpr-command "gtklp")
 '(magit-completing-read-function (quote magit-ido-completing-read))
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(markdown-code-modes
   (quote
    (("haskell" . literate-haskell-mode)
     ("dot" . graphviz-dot-mode)
     ("bash" . sh-mode))))
 '(markdown-command "pandoc -Ss")
 '(markdown-indent-on-enter nil)
 '(markdown-italic-underscore t)
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-window-setup (quote other-window))
 '(paradox-automatically-star nil)
 '(paradox-github-token t)
 '(paradox-homepage-button-string "⌂")
 '(preview-auto-cache-preamble t)
 '(preview-default-option-list
   (quote
    ("displaymath" "floats" "graphics" "textmath" "sections" "footnotes" "showlabels")))
 '(recentf-max-saved-items 1000)
 '(recentf-save-file "~/.emacs.d/recentf")
 '(require-final-newline t)
 '(rw-hunspell-default-dictionary "en_AU")
 '(rw-hunspell-dicpath-list (quote ("~/.emacs.d/dictionaries")))
 '(rw-hunspell-make-dictionary-menu t)
 '(rw-hunspell-use-rw-ispell t)
 '(safe-local-variable-values
   (quote
    ((eval defun cell-to-list
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
     (eval turn-on-orgtbl))))
 '(save-place t nil (saveplace))
 '(save-place-file "~/.emacs.d/saveplace")
 '(scroll-bar-mode (quote left))
 '(shm-auto-insert-skeletons t)
 '(shm-indent-point-after-adding-where-clause t)
 '(shm-use-hdevtools t)
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis))
 '(size-indication-mode t)
 '(smex-auto-update nil)
 '(smex-save-file "~/.emacs.d/smex-items")
 '(tab-width 4)
 '(text-mode-hook
   (quote
    (turn-on-auto-fill table-recognize text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(unicode-fonts-block-font-mapping
   (quote
    (("Alchemical Symbols"
      ("Symbola"))
     ("Alphabetic Presentation Forms"
      ("DejaVu Sans:width=condensed" "FreeMono"))
     ("Arrows"
      ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
     ("Block Elements"
      ("DejaVu Sans Mono" "FreeMono" "DejaVu Sans:width=condensed" "Symbola"))
     ("Box Drawing"
      ("DejaVu Sans Mono" "FreeMono" "DejaVu Sans" "Symbola"))
     ("Combining Diacritical Marks Supplement"
      ("FreeSerif" "DejaVu Sans:width=condensed"))
     ("Combining Diacritical Marks for Symbols"
      ("Cambria Math" "Symbola"))
     ("Combining Diacritical Marks"
      ("DejaVu Sans:width=condensed" "DejaVu Sans Mono" "FreeMono"))
     ("Combining Half Marks"
      ("Symbola"))
     ("Control Pictures"
      ("Symbola" "FreeMono"))
     ("Currency Symbols"
      ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
     ("Cyrillic Supplement"
      ("DejaVu Sans:width=condensed" "Symbola"))
     ("Cyrillic"
      ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
     ("Dingbats"
      ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola"))
     ("Emoticons"
      ("Symbola"))
     ("General Punctuation"
      ("DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
     ("Geometric Shapes"
      ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
     ("Gothic"
      ("FreeSerif"))
     ("Greek Extended"
      ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "FreeMono"))
     ("Greek and Coptic"
      ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola"))
     ("IPA Extensions"
      ("DejaVu Sans Mono" "Symbola" "FreeMono"))
     ("Latin Extended-C"
      ("DejaVu Sans:width=condensed"))
     ("Latin Extended-D"
      ("FreeMono" "DejaVu Sans Mono" "DejaVu Sans:width=condensed"))
     ("Letterlike Symbols"
      ("DejaVu Sans:width=condensed" "Symbola"))
     ("Mathematical Alphanumeric Symbols"
      ("Symbola"))
     ("Mathematical Operators"
      ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
     ("Miscellaneous Mathematical Symbols-A"
      ("Symbola"))
     ("Miscellaneous Mathematical Symbols-B"
      ("Symbola"))
     ("Miscellaneous Symbols and Pictographs"
      ("Symbola"))
     ("Miscellaneous Symbols and Arrows"
      ("Symbola"))
     ("Miscellaneous Symbols"
      ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola"))
     ("Miscellaneous Technical"
      ("Symbola"))
     ("Musical Symbols"
      ("Symbola"))
     ("Number Forms"
      ("DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
     ("Optical Character Recognition"
      ("Symbola" "FreeMono"))
     ("Playing Cards"
      ("DejaVu Sans:width=condensed" "Symbola"))
     ("Specials"
      ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
     ("Superscripts and Subscripts"
      ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
     ("Supplemental Arrows-A"
      ("DejaVu Sans:width=condensed" "Symbola" "FreeMono"))
     ("Supplemental Arrows-B"
      ("Symbola"))
     ("Supplemental Mathematical Operators"
      ("Symbola"))
     ("Supplemental Punctuation"
      ("DejaVu Sans Mono" "Symbola"))
     ("Transport and Map Symbols"
      ("Symbola")))))
 '(unicode-fonts-existence-checks (quote first))
 '(unicode-fonts-skip-font-groups
   (quote
    (chinese-simplified chinese-traditional low-quality-glyphs microsoft-only multicolor non-free)))
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(use-dialog-box nil)
 '(user-mail-address "Ivan.Miljenovic@gmail.com")
 '(wdired-allow-to-change-permissions t)
 '(whitespace-style (quote (face tabs trailing empty)))
 '(woman-imenu t)
 '(woman-use-own-frame nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-type-face ((t (:foreground "#be59d8"))))
 '(woman-bold ((t (:inherit bold :foreground "#00aff5"))) t)
 '(woman-italic ((t (:inherit italic :foreground "#3cb370"))) t))

;; '(TeX-view-program-list (quote (("okular" "okular -unique %o#src:%n%b"))))
