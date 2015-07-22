;;; mmm-pandoc.el --- Multi-mode Markdown          -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Ivan Lazar Miljenovic

;; Author: Ivan Lazar Miljenovic <Ivan.Miljenovic@gmail.com>
;; Keywords: languages, files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'mmm-mode)
(require 'markdown-mode)
(require 'yaml-mode)

;;; Customisation

(defgroup mmm-pandoc nil
  "Handling of Pandoc-compatible sub-modes in Markdown."
  :group 'mmm)

(defcustom mmm-pandoc-language-modes
  '()
  "User preferences about what major modes to use.
Each element has the form \(LANGUAGE . MODES) where LANGUAGE is the
name of a programming language such as `perl' as a symbol, and MODES
is a list of possible major modes to use, such as `cperl-mode' or
`perl-mode'.  The first element of MODES which is `fboundp' is used
for submodes of LANGUAGE.  The last element of MODES should be a mode
which will always be available."
  :type '(repeat (cons symbol
                       (repeat
                        (restricted-sexp :match-alternatives
                                         (fboundp)))))
  :group 'mmm-pandoc)

(defcustom mmm-pandoc-prefer-backticks nil
  "Non-nil uses backticks for code blocks rather than tildas.
Used for inserting the template for code blocks."
  :type 'boolean
  :group 'mmm-pandoc)

;;; Helper functions/variables

(defun mmm-check-pandoc-yaml ()
  "Verify that this is indeed that start of a Pandoc YAML block.
It is assumed that the '---' delimiter starting a YAML block will
have a non-empty line after it, and that any other use (header,
horizontal rule, etc.) will have an empty line."
  ; . doesn't match newline
  (looking-at "."))

(defconst mmm-pandoc-code-identifier
  "\\([[:alpha:]][[:alnum:]-_:\.]*\\)"
  "A pandoc attribute identifier.")

(defconst mmm-pandoc-code-class-attr
  (concat "\\." mmm-pandoc-code-identifier)
  "The regex to match a pandoc class attribute.")

(defun mmm-pandoc-code-insert-delim ()
  (make-string 3 (if mmm-pandoc-prefer-backticks ?` ?~)))

(defun regexp-alternatives (args)
  (concat "\\(" (mapconcat 'identity args "\\|") "\\)"))

;; Currently not used, but kept here in case the `{...}' detection
;; needs to be made smarter.

;; (defconst mmm-pandoc-code-ident-attr
;;   (concat "#" mmm-pandoc-code-identifier)
;;   "Matches an identifier attribute.")

;; (defconst mmm-pandoc-code-keyval-attr
;;   (concat mmm-pandoc-code-identifier
;;           "="
;;           (regexp-alternatives
;;            (list
;;             (concat "\"" mmm-pandoc-code-lit "\"")
;;             (concat "'" mmm-pandoc-code-lit "'")
;;             (concat "\\(?:" mmm-pandoc-code-esc "\\|[^ \t\n\r]\\)*")
;;             )))
;;   "Matches a key=val pair.")

;; (defconst mmm-pandoc-code-esc
;;   "\\\\[^[:alnum:]]"
;;   "Matches an escaped character.")

;; (defconst mmm-pandoc-code-lit
;;   (concat (regexp-alternatives '("[^\"']" "\\\"" "\\'")) "+")
;;   "Match a literal string.
;; Simplified from the original definition: will accept any
;; non-empty string that doesn't contain unescaped `\"' or `\''
;; characters.")

(defcustom mmm-pandoc-code-default-mode 'fundamental-mode
  "Default mode chosen when none specified or recognised."
  :group 'mmm-pandoc
  :type 'function)

(defun mmm-pandoc-code-get-mode (string)
  "Determine which mode to set the code block."
  (or (cond
       ;; No language specified
       ((= 0 (length string)) mmm-pandoc-code-default-mode)
       ;; Attribute list
       ((string-prefix-p "{" string) (mmm-pandoc-code-identify-mode-from-attributes string))
       ;; Must just be a language on its own.
       (t (mmm-pandoc-code-identify-mode string)))
      ;; Couldn't match it, so use the default
      mmm-pandoc-code-default-mode))

(defun mmm-pandoc-code-identify-mode (string)
  "Try to match the specified language to a mode."
  (or
   (mmm-ensure-modename
    ;; First try the user override.
    (some #'(lambda (pair)
              (if (string-match (car pair) string) (cdr pair) nil))
          mmm-pandoc-language-modes))
   (mmm-ensure-modename
    ;; Try to see if it's already a valid language in the default list.
    (intern string))
   (mmm-ensure-modename
    ;; Try tacking on -mode and see if it works.
    (intern (concat string "-mode")))
   ;; None of them matched, so give up
   ))

(defun mmm-pandoc-code-identify-mode-from-attributes (string)
  "Determine the mode from attributes.
It tries to do so by finding every class in the attribute list
and seeing if it's a valid language mode."
  (catch 'code-mode
    (save-match-data
      (let ((pos 0)
            (md nil)
            ;; Note the usage of "{": means it's at the beginning of
            ;; the string.  This could theoretically still get
            ;; misleading results by having a class-like value inside
            ;; a key=val pair.
            (reg (concat "\\(?:{\\|[[:space:]]\\)" mmm-pandoc-code-class-attr)))
        (while (string-match reg string pos)
          (setq pos (match-end 0))
          (setq md (mmm-pandoc-code-identify-mode (match-string 1 string)))
          (if md
              ;; Found a valid mode, so quit the loop.
              (throw 'code-mode md)))))))

(mmm-add-mode-ext-class 'markdown-mode nil 'pandoc-markdown)

(defconst mmm-pandoc-code-front
  (concat "\\(?:\\`\\|^\n\\)" ; blank line before
          "\\(~\\|`\\)\\(\\1\\{2,\\}\\)\\s-*" ; ~ or backticks at least three times
          (regexp-alternatives
           (list
            mmm-pandoc-code-identifier
            "{[^}]*}" ; not quite right: will possibly parse illegal things
            ""
            ))
          "\n")
  "Parse the beginning of a code block.")

(mmm-add-group
 'pandoc-markdown
 `(
   ;; YAML metadata blocks
   (pandoc-yaml
    :submode yaml-mode
    :face mmm-comment-submode-face
    :front "\\(?:\\`\\|^\n\\)---\n"
    :front-delim 0
    :front-verify mmm-check-pandoc-yaml
    :back "^\\(?:\\.\\.\\.\\|---\\)$"
    :back-delim 0
    :insert ((?y YAML "YAML element: "
                 @ ?\n "---" ?\n @ str _ ?\n @ "..." @)))

   ;; code blocks
   (pandoc-code
    :match-submode mmm-pandoc-code-get-mode
    :face mmm-code-submode-face
    :front ,mmm-pandoc-code-front
    :front-delim 0
    :front-form (lambda () (match-string 3))
    :save-matches t
    :save-name t
    :back "^~1~2~1*$" ;; need at least the same number of characters
    :back-delim 0
    :match-name "~3"
    :insert ((?c Code "Language: "
                 @ ?\n (mmm-pandoc-code-insert-delim) str @
                   ?\n _ ?\n
                 @ (mmm-pandoc-code-insert-delim) @))
   )))

;; (setq mmm-classes-alist (cdr mmm-classes-alist))

(provide 'mmm-pandoc)
;;; mmm-pandoc.el ends here
