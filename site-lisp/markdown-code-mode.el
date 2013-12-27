;;; markdown-code-mode.el

;; Copyright (C) 2003, 2007, 2009  Free Software Foundation, Inc.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This provides a mode for editing literate Haskell with the LaTeX
;; \begin{code}...\end{code} convention (but not the `Bird tracks'
;; `inverse comment' convention).
;;
;; It originally was meant to use my simple Haskell mode rather than
;; the one from haskell.org.  Versions of the haskell.org
;; haskell-indent.el prior to v2.1 (?) need to be modified to work
;; with this, due to the narrowing used by multi-mode around the
;; indentation command.  That has been fixed subsequently, so the
;; current version can be used with this code.  However, note that the
;; recommended way of installing it adds `literate-haskell-mode' to
;; `auto-mode-alist' for .lhs files, whereas we want `markdown-code'.
;; Thus you probably want to do something like this, assuming
;; markdown-code is on `load-path' and $haskell_mode_path is the path
;; name of the markdown-code directory:
;;
;;   (load "$haskell_mode_path/haskell-site-file.el")
;;   (add-to-list 'auto-mode-alist '("\\.l[gh]s\\'" . markdown-code-mode))
;;   (autoload 'markdown-code-mode "markdown-code")
;;
;; Alternatively, you can customize `haskell-mode-hook' by adding
;; `haskell-markdown-maybe' to it.
;;
;; You can use the Emacs Markdown mode or AUCTeX.  The latter causes
;; trouble which is hacked around in the multi-mode.el (revision
;; 1.6+).  There are problems with font-locking in Emacs 21, both with
;; AUCTeX and the built-in mode, which seem to be fixed in Emacs 22 --
;; single `$'s in Haskell parts cause subsequent Markdown to be fontified
;; as maths.

;;; Code:

(require 'multi-mode)
(require 'markdown-mode)
(require 'haskell-mode)

(defcustom markdown-code-modes
  '(
    ("haskell" . literate-haskell-mode)
    )
  "Mapping from name in markdown code block to major mode."
  :type '(alist :key-type (string :tag "Block name") :value-type function)
  :group 'markdown-code)

(defvar markdown-code-block-start-re "^~~~+"
  "Regular expression to detect a code block in markdown.")

(defvar markdown-code-block-re
  (concat markdown-code-block-start-re "[ \\t]*$")
  "Regular expression to detect a code block on its own")

(defvar markdown-end-code-block-re ; "^~~~+[ \\t]*$"
  (concat markdown-code-block-start-re "[ \\t]*$")
  "blah")

(defvar markdown-code-block-name-spec-re
  "[ \\t]*{\\.\\(\\w+\\)\\(?:[ \\t]+\\.\\w+\\)*}[ \\t]*$"
  "Regular expression to determine the name for a named code block in markdown.")

(defvar markdown-named-code-block-re
  (concat markdown-code-block-start-re markdown-code-block-name-spec-re)
  "Regular expression to detect a named code block in markdown.")

(defvar markdown-code-block-boundary-re
  ; have to do named one first
  ; FIXME: can be expressed better.
  (concat "\\(?:" markdown-named-code-block-re
          "\\|\\(" markdown-end-code-block-re "\\)\\)")
  "Regular expression to detect either the beginning or an end of a code block.")

(defun markdown-code-chunk-region (pos)
  "Determine type and limit of current chunk at POS.
Return (MODE START END), where MODE is `haskell-mode' or `markdown-mode'
and START and END are the limits of the chunk."
  (let ((mode 'markdown-mode)
        (start (point-min))
        (end (point-max)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char pos)
        ;; Look for the beginning or end of a code block.
        (cond
         ((save-excursion
            (beginning-of-line)
            (looking-at markdown-code-block-boundary-re))
          (if (match-beginning 2)	; end of block
              (progn
                (setq start (point))
                (if (re-search-forward markdown-named-code-block-re nil t)
                    (setq end (line-end-position))))
            ;; beginning of code block
            (setq end (1- (line-beginning-position 2)))
            (if (re-search-backward markdown-end-code-block-re nil t)
                (setq start (match-beginning 0)))))
         ;; Between \begin and \end (in either order).
         ((re-search-backward markdown-code-block-boundary-re
                              nil t)
          (if (match-beginning 2)	; end of block
              (progn
                (setq start (match-beginning 0))
                (if (re-search-forward markdown-named-code-block-re nil t)
                    (setq end (line-end-position))))
            ;; beginning of line
            (let ((exc-mode 'markdown-mode))
              (let ((lang (match-string 1)))
                (when lang
                  (setq exc-mode (catch 'major
                                   (dolist (rec markdown-code-modes)
                                     (when (string-match (car rec) lang)
                                       (throw 'major (cdr rec))))
                                   nil))))
              (setq start (1- (line-beginning-position 2))
                    mode exc-mode)
              (if (re-search-forward markdown-end-code-block-re nil t)
                  (setq end (1- (match-beginning 0)))))))
         ;; Doc chunk at start.
         (t
          (beginning-of-line)
          (if (re-search-forward markdown-named-code-block-re nil t)
              (setq end (point))
            (setq end (point-max)
                  mode 'literate-haskell-mode))))
        (multi-make-list mode start end)))))

(defun add-chunk-mode (mode-list)
  "Takes a list of (ModeName . Mode) values, and converts them to
a form suitable for `multi-mode-alist'."
  (if (not mode-list)
      nil
    (cons
     (cons (cdar mode-list) 'markdown-code-chunk-region)
     (add-chunk-mode (cdr mode-list)))))

;;;###autoload
(defun markdown-code-mode ()
  "Mode for editing `literate Haskell' with Markdown conventions."
  (interactive)
  (set (make-local-variable 'multi-mode-alist)
       (cons '(markdown-mode . nil)
             (add-chunk-mode markdown-code-modes)))
  (multi-mode-install-modes))

(provide 'markdown-code-mode)
;;; markdown-code-mode.el ends here
