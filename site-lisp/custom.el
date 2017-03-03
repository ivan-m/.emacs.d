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
 '(calendar-christian-all-holidays-flag t)
 '(diary-show-holidays-flag nil)
 '(ebal-completing-read-function (quote ebal-ido-completing-read))
 '(ensime-graphical-tooltips t)
 '(haskell-ask-also-kill-buffers nil)
 '(haskell-import-mapping
   (quote
    (("Data.Text" . "import qualified Data.Text as T
import Data.Text (Text)
")
     ("Data.Text.Lazy" . "import qualified Data.Text.Lazy as LT
")
     ("Data.ByteString" . "import qualified Data.ByteString as B
import Data.ByteString (ByteString)
")
     ("Data.ByteString.Lazy" . "import qualified Data.ByteString.Lazy as L
")
     ("Data.Map" . "import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
")
     ("Data.Map.Strict" . "import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
")
     ("Data.Set" . "import qualified Data.Set as S
import Data.Set (Set)
")
     ("Data.Vector" . "import qualified Data.Vector as V
import Data.Vector (Vector)
")
     ("Data.HashMap" . "import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict(HashMap)
"))))
 '(haskell-interactive-mode-eval-mode (quote haskell-mode))
 '(haskell-interactive-mode-hide-multi-line-errors t)
 '(haskell-literate-default (quote bird))
 '(haskell-notify-p t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-check-cabal-config-on-load t)
 '(haskell-process-load-or-reload-prompt t)
 '(haskell-process-log t)
 '(haskell-process-prompt-restart-on-cabal-change t)
 '(haskell-process-suggest-haskell-docs-imports t)
 '(haskell-process-suggest-language-pragmas t)
 '(haskell-process-suggest-no-warn-orphans t)
 '(haskell-process-suggest-overloaded-strings t)
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(holiday-other-holidays
   (quote
    ((holiday-fixed 1 1 "New Year's Day")
     (holiday-fixed 1 26 "Australia Day")
     (holiday-fixed 2 14 "Valentine's Day")
     (holiday-fixed 3 17 "St. Patrick's Day")
     (holiday-fixed 4 1 "April Fools' Day")
     (holiday-fixed 4 25 "ANZAC Day")
     (holiday-float 5 0 2 "Mother's Day")
     (holiday-float 6 1 2 "Queen's Birthday")
     (holiday-float 10 1 1 "Labour Day")
     (holiday-easter-etc)
     (holiday-fixed 12 25 "Christmas")
     (holiday-fixed 12 26 "Boxing Day")
     (if calendar-christian-all-holidays-flag
         (append
          (holiday-fixed 1 6 "Epiphany")
          (holiday-julian 12 25 "Christmas (Julian calendar)")
          (holiday-greek-orthodox-easter)
          (holiday-fixed 8 15 "Assumption")
          (holiday-advent 0 "Advent")))
     (holiday-hebrew-passover)
     (holiday-hebrew-rosh-hashanah)
     (holiday-hebrew-hanukkah)
     (if calendar-hebrew-all-holidays-flag
         (append
          (holiday-hebrew-tisha-b-av)
          (holiday-hebrew-misc)))
     (holiday-islamic-new-year)
     (holiday-islamic 9 1 "Ramadan Begins")
     (if calendar-islamic-all-holidays-flag
         (append
          (holiday-islamic 1 10 "Ashura")
          (holiday-islamic 3 12 "Mulad-al-Nabi")
          (holiday-islamic 7 26 "Shab-e-Mi'raj")
          (holiday-islamic 8 15 "Shab-e-Bara't")
          (holiday-islamic 9 27 "Shab-e Qadr")
          (holiday-islamic 10 1 "Id-al-Fitr")
          (holiday-islamic 12 10 "Id-al-Adha")))
     (holiday-bahai-new-year)
     (holiday-bahai-ridvan)
     (holiday-fixed 5 23 "Declaration of the Báb")
     (holiday-fixed 5 29 "Ascension of Bahá'u'lláh")
     (holiday-fixed 7 9 "Martyrdom of the Báb")
     (holiday-fixed 10 20 "Birth of the Báb")
     (holiday-fixed 11 12 "Birth of Bahá'u'lláh")
     (if calendar-bahai-all-holidays-flag
         (append
          (holiday-fixed 11 26 "Day of the Covenant")
          (holiday-fixed 11 28 "Ascension of `Abdu'l-Bahá")))
     (holiday-chinese-new-year)
     (if calendar-chinese-all-holidays-flag
         (append
          (holiday-chinese 1 15 "Lantern Festival")
          (holiday-chinese-qingming)
          (holiday-chinese 5 5 "Dragon Boat Festival")
          (holiday-chinese 7 7 "Double Seventh Festival")
          (holiday-chinese 8 15 "Mid-Autumn Festival")
          (holiday-chinese 9 9 "Double Ninth Festival")
          (holiday-chinese-winter-solstice)))
     (solar-equinoxes-solstices)
     (holiday-sexp calendar-daylight-savings-starts
                   (format "Daylight Saving Time Begins %s"
                           (solar-time-string
                            (/ calendar-daylight-savings-starts-time
                               (float 60))
                            calendar-standard-time-zone-name)))
     (holiday-sexp calendar-daylight-savings-ends
                   (format "Daylight Saving Time Ends %s"
                           (solar-time-string
                            (/ calendar-daylight-savings-ends-time
                               (float 60))
                            calendar-daylight-time-zone-name))))))
 '(hs-checkers-replace-with-suggestions t)
 '(lpr-add-switches nil)
 '(lpr-command "gtklp")
 '(markdown-code-modes
   (quote
    (("haskell" . literate-haskell-mode)
     ("dot" . graphviz-dot-mode)
     ("bash" . sh-mode))))
 '(markdown-command "pandoc -Ss")
 '(markdown-indent-on-enter nil)
 '(markdown-italic-underscore t)
 '(mmm-global-mode (quote maybe) nil (mmm-mode))
 '(mmm-pandoc-prefer-backticks t)
 '(mmm-parse-when-idle t)
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-window-setup (quote other-window))
 '(org-completion-use-ido t t)
 '(paradox-automatically-star nil)
 '(paradox-github-token t)
 '(paradox-homepage-button-string "⌂")
 '(preview-auto-cache-preamble t)
 '(preview-default-option-list
   (quote
    ("displaymath" "floats" "graphics" "textmath" "sections" "footnotes" "showlabels")))
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
     (eval turn-on-orgtbl))))
 '(scala-indent:indent-value-expression t)
 '(shm-auto-insert-skeletons t)
 '(shm-indent-point-after-adding-where-clause t)
 '(shm-use-hdevtools t))
