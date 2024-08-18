;;; czm-tex-fold.el --- Extensions for tex-fold.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-tex-fold.el
;; Package-Requires: ((emacs "29.1") (czm-tex-util "0.0") (auctex "14.0.5"))
;; Keywords: tex

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package extends AUCTeX's `TeX-fold-mode' to give improved
;; folding of begin{...} and end{...}, references and citations.  The
;; basic idea is illustrated as follows:
;;
;; - \begin{theorem} is folded as "Theorem."
;;
;; - \begin{theorem}[Foo] is folded as "Theorem (Foo)."
;;
;; - \label{thm:foo} is folded as "[1]", with the label number ("1" in
;; this case) drawn from the accompanying .aux file.
;;
;; - \begin{theorem}\label{thm:foo} is folded as "Theorem [1]".
;;
;; - \end{theorem} is folded as "◼".
;;
;; - \end{proof} is folded as "□".
;;
;; - \ref{thm:foo} and \eqref{eq:bar} are folded as "[1]".
;;
;; - \cite[Section 1]{foo} is folded as "[CN84, Section 1]", using
;; last name initials and 2-digit years.  The citation keys are
;; extracted from the bib file specified by the customization variable
;; `czm-tex-fold-bib-file' (rather than from the file being visited --
;; indeed, this package works fine in non-file buffers).
;;
;; To use this package, run the commands `czm-tex-fold-set-defaults'
;; and `czm-tex-fold-install', then use `TeX-fold-mode' as usual
;; (restarting it if it was started after running the setup commands).

;;; Code:

(require 'latex)
(require 'tex-fold)
(require 'bibtex)
(require 'cl-lib)
(require 'czm-tex-util)

(defgroup czm-tex-fold nil
  "Customizations for folding LaTeX documents."
  :group 'tex)

(defcustom czm-tex-fold-bib-file
  nil
  "BibTeX file from which to extract citation keys."
  :type 'string
  :group 'czm-tex-fold)

(defun czm-tex-fold-set-defaults ()
  "Set default values for `czm-tex-fold'."
  (interactive)
  (setq
   TeX-fold-macro-spec-list
   '(("[f]" ("footnote" "marginpar"))
     (czm-tex-fold-label-display ("label"))
     (czm-tex-fold-cite-display ("cite"))
     (czm-tex-fold-textcolor-display ("textcolor"))
     (czm-tex-fold-alert-display ("alert"))
     ("[r]" ("pageref" "footref"))
     (czm-tex-fold-ref-display ("ref"))
     (czm-tex-fold-eqref-display ("eqref"))
     (czm-tex-fold-href-display ("href"))
     (czm-tex-fold-texorpdfstring ("texorpdfstring"))
     ("[i]" ("index" "glossary"))
     ("[1]:||*" ("item"))
     ("..." ("dots"))
     ("(C)" ("copyright"))
     ("(R)" ("textregistered"))
     ("TM" ("texttrademark"))
     (czm-tex-fold-begin-display ("begin"))
     (czm-tex-fold-end-display ("end"))
     ("🌱" ("documentclass"))
     ("🌌" ("input"))
     ("📚" ("bibliography"))
     ("📖" ("bibliographystyle"))
     ("✅" ("leanok"))
     (1 ("section" "part" "chapter" "subsection" "subsubsection" "paragraph" "subparagraph" "part*" "chapter*" "\nsection*" "subsection*" "subsubsection*" "paragraph*" "\nsubparagraph*" "emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt" "textbf" "textsc" "textup" "underline")))))

(defcustom czm-tex-fold-exclude-list
  '("equation" "equation*" "align" "align*" "multline" "multline*")
  "List of types to be excluded by `czm-tex-fold-helper-display'."
  :type '(repeat string)
  :group 'czm-tex-fold)

(defun czm-tex-fold-install ()
  "Install `czm-tex-fold'."
  (interactive)
  (add-hook 'TeX-fold-region-functions #'czm-tex-fold-quotes)
  (add-hook 'TeX-fold-region-functions #'czm-tex-fold-dashes)
  (add-hook 'TeX-fold-region-functions #'czm-tex-fold-verbs))

(defun czm-tex-fold-uninstall ()
  "Uninstall `czm-tex-fold'."
  (interactive)
  (remove-hook 'TeX-fold-region-functions #'czm-tex-fold-quotes)
  (remove-hook 'TeX-fold-region-functions #'czm-tex-fold-dashes)
  (remove-hook 'TeX-fold-region-functions #'czm-tex-fold-verbs))

(defcustom czm-tex-fold-begin-default
  "↴"
  "Default fold display for a \\begin{...} macro."
  :type 'string
  :group 'czm-tex-fold)

(defcustom czm-tex-fold-end-default
  "↲"
  "Default fold display for an \\end{...} macro."
  :type 'string
  :group 'czm-tex-fold)

(defcustom czm-tex-fold-environment-delimiter-spec-list
  '((("🌅" . "🌇")
     ("document"))
    (("⚡" . "⚡")
     ("minted" "minted*"))
    (("♣" . "♣")
     ("results" "results*"))
    ((czm-tex-fold-standard-display . "□")
     ("proof"))
    ((czm-tex-fold-standard-display . "◼")
     ("abstract" "lemma" "lemma*" "exercise" "example" "proposition" "corollary" "remark" "definition" "theorem" "proof" "conjecture" "notation" "terminology" "fact" "note" "problem" "ass" "acknowledgment" "algorithm" "question" "questions" "assumptions" "answer" "claim" "conclusion" "criterion" "summary" "thm" "prop" "rem" "cor" "lem" "lemmy" "def" "defn" "ex" "exer" "conj" "not" "term" "prob" "ques" "ans" "conc" "crit" "sum" "commentary"))
    ((czm-tex-fold-block-display . "◼")
     ("block")))
  "List of specifications for `czm-tex-fold-begin-display'.

Each element of the list is a specification for a fold display.
Each specification is a list of two elements.  The first element
is a cons cell, with car and cdr corresponding to \\begin{...} and
\\end{...} macros, each of which is either

  - a string, which is used as the fold display string, or

  - a function, which is called with the required macro arguments
    and should return a string for the fold display.

The second element of the specification is a list of environment
types (e.g., \"theorem\" or \"proof\") to which the specification
applies."
  :type '(repeat (group (choice (string :tag "Display String")
                                (function :tag "Function to execute"))
                        (repeat :tag "Types" (string))))
  :group 'czm-tex-fold)

(defun czm-tex-fold--optional-args ()
  "Return the optional arguments to the current macro."
  (let ((beg (point))
        (end (TeX-fold-item-end (point) 'macro))
        (n 1) result)
    (while-let ((arg (TeX-fold-macro-nth-arg
                      n beg end '(?\[ . ?\]))))
      (push (substring-no-properties (car arg)) result)
      (setq n (1+ n)))
    (nreverse result)))

(defcustom czm-tex-fold--environment-abbreviations
  '(("thm" . "theorem")
    ("prop" . "proposition")
    ("rem" . "remark")
    ("cor" . "corollary")
    ("lem" . "lemma")
    ("lemmy" . "lemma")
    ("def" . "definition")
    ("defn" . "definition")
    ("ex" . "example")
    ("exer" . "exercise")
    ("conj" . "conjecture")
    ("not" . "notation")
    ("term" . "terminology")
    ("prob" . "problem")
    ("ques" . "question")
    ("ans" . "answer")
    ("conc" . "conclusion")
    ("crit" . "criterion")
    ("ass" . "assumption")
    ("sum" . "summary"))
  "Lookup table for LaTeX environment abbreviations."
  :group 'czm-tex-fold
  :type '(alist :key-type (string) :value-type (string)))

(defun czm-tex-fold--full-latex-env-name (env-name)
  "Fetch the full environment name identified by ENV-NAME.
If there is no such abbreviation, return ENV-NAME as is."
  (or (cdr (assoc env-name czm-tex-fold--environment-abbreviations)) env-name))

(defun czm-tex-fold-standard-display (env &rest _args)
  "Format fold display for tex environment \\begin{ENV}.
Return \"Env.\" except or \"Env (Description).\" except when a
label occurs on the same line; in that case, omit the period."
  (let* ((props (text-properties-at 0 env))
         (env-expanded (czm-tex-fold--full-latex-env-name env))
         (env-uppercase (concat (upcase (substring env-expanded 0 1))
                            (substring env-expanded 1)))
         (description (car (czm-tex-fold--optional-args)))
         (has-label (save-excursion (re-search-forward
                                     "\\label{\\([^}]+\\)}" (line-end-position) t))))
    (set-text-properties 0 (length env-uppercase) props env-uppercase)
    (concat
     (format "%s" env-uppercase)
     (when description
       (format " (%s)" description))
     (if has-label " " "."))))

(defun czm-tex-fold-block-display (env &rest args)
  "Format fold display for tex environment \\begin{ENV}.
Return \"Env.\" or \"Env (Description).\" except when a label
occurs on the same line; in that case, omit the period.  ARGS is
nested list whose caaar is the block label."
  (let* ((props (text-properties-at 0 env))
         (uppercase (caaar args))
         (has-label (save-excursion (re-search-forward
                                     "\\label{\\([^}]+\\)}" (line-end-position) t))))
    (set-text-properties 0 (length uppercase) props uppercase)
    (concat
     (format "%s" uppercase)
     (if has-label " " "."))))

(defun czm-tex-fold-helper-display (type env &rest args)
  "Fold display string for \\begin{ENV} or \\end{ENV} macro.
TYPE should be either \='begin or \='end.  ARGS are the remaining
{} arguments to the macro."
  (if (member env czm-tex-fold-exclude-list)
      'abort
    (let ((default-fold (if (eq type 'begin)
                            czm-tex-fold-begin-default
                          czm-tex-fold-end-default)))
      (cl-dolist (spec czm-tex-fold-environment-delimiter-spec-list
                       default-fold)
        (let* ((display-rule (car spec))
               (display-string
                (if (eq type 'begin)
                    (car display-rule)
                  (cdr display-rule)))
               (envs (cadr spec)))
          (when (member env envs)
            (if (functionp display-string)
                (cl-return (funcall display-string env args))
              (cl-return display-string))))))))

(defun czm-tex-fold-begin-display (env &rest args)
  "Fold display for a \\begin{ENV}.
ARGS is the list of {} arguments supplied to the macro."
  (czm-tex-fold-helper-display 'begin env args))

(defun czm-tex-fold-end-display (env &rest args)
  "Fold display for a \\end{ENV} macro.
ARGS is the list of {} arguments supplied to the macro."
  (czm-tex-fold-helper-display 'end env args))

(defun czm-tex-fold-ref-helper (label default)
  "Helper function for `czm-tex-fold-ref-display'.
LABEL is the label name.
DEFAULT is the default fold display string for the environment."
  (format "[%s]" (or (czm-tex-util-get-label-number label)
                     default)))

(defun czm-tex-fold-ref-display (label &rest _args)
  "Fold display for a \\ref{LABEL} macro."
  (czm-tex-fold-ref-helper label "r"))

(defun czm-tex-fold-eqref-display (label &rest _args)
  "Fold display for a \\eqref{LABEL} macro."
  (czm-tex-fold-ref-helper label "e"))

(defun czm-tex-fold-href-display (_link name &rest _args)
  "Fold display for a \\href{LINK}{NAME} macro."
  (format "[%s]" (or name "href")))

(defun czm-tex-fold-texorpdfstring (tex _plain &rest _args)
  "Fold display for a \\texorpdfstring{TEX}{PLAIN} macro."
  (format "%s" tex))

(defun czm-tex-fold-label-display (label &rest _args)
  "Fold display for a \\label{LABEL} macro."
  (czm-tex-fold-ref-helper label "l"))

(defun czm-tex-fold-textcolor-display (color text &rest _args)
  "Fold display for a \\textcolor{COLOR}{TEXT} macro."
  (with-temp-buffer
    (insert text)
    (put-text-property (point-min) (point-max)
                       'face `(:foreground ,color)
                       (current-buffer))
    (buffer-string)))

(defun czm-tex-fold-alert-display (text &rest _args)
  "Fold display for a \\alert{TEXT} macro."
  (with-temp-buffer
    (insert text)
    (put-text-property (point-min) (point-max)
                       'face `(:foreground "red")
                       (current-buffer))
    (buffer-string)))

(defun czm-tex-fold--last-initial-of-name (name)
  "Return last initial of NAME.
NAME should be a name in the format \"Last, First\" or \"First
Last\".  Returns first alphanumeric letter before last space
before first comma, if any."
  (let* ((first-comma (string-match "," name))
         (name (if first-comma
                   (substring name 0 first-comma)
                 name))
         (last-space (string-match " " name))
         (name (if last-space
                   (substring name last-space)
                 name)))
    (when-let
        ((index (string-match "[[:alpha:]]" name)))
      (substring name index (1+ index)))))

(defun czm-tex-fold-bibtex-abbrev ()
  "Abbreviate the current bibtex entry.
Use first letter of each author's last name and 2-digit year."
  (when-let* ((entry (bibtex-parse-entry))
              (author (bibtex-text-in-field "author" entry))
              (year (bibtex-text-in-field "year" entry)))
    (let* ((initials
            (mapconcat
             #'czm-tex-fold--last-initial-of-name
             (string-split author " and ")))
           (year-XX (when year (substring year -2))))
      (concat initials year-XX))))

(defun czm-tex-fold-cite-display (text &rest _args)
  "Fold display for a \\cite{TEXT} macro."
  (let* ((citation (car (czm-tex-fold--optional-args)))
         (references
          (mapcar (lambda (cite)
                    (let ((trimmed-cite (string-trim cite)))
                      (when-let* ((bib-file czm-tex-fold-bib-file)
                                  (case-fold-search t))
                                        ; else bibtex-parse-entry breaks
                        (when (file-exists-p bib-file)
                          (with-current-buffer (find-file-noselect bib-file)
                            (save-excursion
                              (goto-char (point-min))
                              (if
                                  (search-forward (concat "{" trimmed-cite ",")
                                                  nil t)
                                  (save-excursion
                                    (bibtex-beginning-of-entry)
                                    (czm-tex-fold-bibtex-abbrev))
                                cite)))))))
                  (split-string text ",")))
         (joined-references (string-join references ", ")))
    (concat
     "["
     (if (string-empty-p joined-references)
         "c" joined-references)
     (when citation
       (format ", %s" citation))
     "]")))

(defun czm-tex-fold-quotes (start end)
  "Fold quotes in the region between START and END using overlays."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "``\\|''" end t)
      (let ((quote-start (match-beginning 0))
            (quote-end (match-end 0))
            (str (if (string= (match-string 0) "``") "“" "”")))
        (when (save-excursion
                (goto-char quote-start)
                (= (let ((math-faces '(tex-math font-latex-math-face))
                         (face (plist-get (text-properties-at (point))
                                          'face)))
                     (cond
                      ((memq face math-faces)
                       1)
                      ((listp face)
                       (let ((total 0))
                         (dolist (f face)
                           (when (memq f math-faces)
                             (setq total (1+ total))))
                         total))
                      (t 0)))
                   0))
          (czm-tex-fold--create-misc-overlay quote-start quote-end str str))))))

(defun czm-tex-fold-dashes (start end)
  "Fold dashes in the region between START and END using overlays."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\b\\(---\\)\\b" end t)
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0))
            (str "—"))
        (czm-tex-fold--create-misc-overlay match-start match-end str str)))
    (goto-char start)
    (while (re-search-forward "\\b\\(--\\)\\b" end t)
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0))
            (str "–"))
        (czm-tex-fold--create-misc-overlay match-start match-end str str)))))

(defvar czm-tex-fold--verb-regex
  "\\\\verb|\\([^|]*\\)|")

(defun czm-tex-fold-verbs (start end)
  "Fold `\\verb|...|' in between START and END using overlays."
  (save-excursion
    (goto-char start)
    (while (re-search-forward czm-tex-fold--verb-regex end t)
      (let ((verb-start (match-beginning 0))
            (verb-end (match-end 0))
            (str (match-string 1))
            (spec (lambda (&rest _args)
                    (when (looking-at czm-tex-fold--verb-regex)
                      (match-string 1)))))
        (czm-tex-fold--create-misc-overlay verb-start verb-end str spec)))))

(defun czm-tex-fold--create-misc-overlay (start end str spec)
  "Create an overlay to fold quotes between START and END with STR and SPEC."
  (let ((priority (TeX-overlay-prioritize start end))
        (ov (make-overlay start end)))
    (overlay-put ov 'category 'TeX-fold)
    (overlay-put ov 'priority priority)
    (overlay-put ov 'evaporate t) ; Remove the overlay when the text is modified.
    (overlay-put ov 'display str)
    (overlay-put ov 'TeX-fold-display-string-spec spec)))

(provide 'czm-tex-fold)
;;; czm-tex-fold.el ends here
