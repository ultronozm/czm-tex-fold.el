;;; czm-tex-fold.el --- Extensions for tex-fold.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-tex-fold.el
;; Package-Requires: ((emacs "29.1") (auctex-label-numbers "0.2") (auctex "14.0.6"))
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
;; folding of begin{...} and end{...}, references, citations and more.
;; The basic idea is illustrated as follows:
;;
;; - \begin{theorem} is folded as "Theorem."
;;
;; - \begin{theorem}[Foo] is folded as "Theorem (Foo)."
;;
;; - \label{thm:foo} is folded as "[1]", with the label number ("1" in
;; this case) drawn from the accompanying .aux file, courtesy of the
;; `auctex-label-numbers' package.
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
;; obtained via reftex or, as a backup, from the bib file specified by
;; the customization variable `czm-tex-fold-bib-file' (so that package
;; works also in non-file buffers).
;;
;; Folding support is provided for dashes, quotes and verbatim
;; environments via the minor mode `czm-tex-fold-misc-mode'.
;;
;; To use this package, add the following to your config:
;;
;; (with-eval-after-load 'latex
;;   (czm-tex-fold-set-defaults)
;;   (add-hook 'LaTeX-mode-hook #'czm-tex-fold-misc-mode))
;;
;; Then, use `TeX-fold-mode' as usual, restarting it if it was started
;; after running the setup commands.  See the README for further
;; information.

;;; Code:

(require 'latex)
(require 'tex-fold)
(require 'bibtex)
(require 'auctex-label-numbers)
(require 'reftex)

(defgroup czm-tex-fold nil
  "Customizations for folding LaTeX documents."
  :group 'tex)

(defun czm-tex-fold-set-defaults ()
  "Set default values for `czm-tex-fold'."
  (interactive)
  (setq
   TeX-fold-macro-spec-list
   '((czm-tex-fold-begin-display ("begin"))
     (czm-tex-fold-end-display ("end"))
     (czm-tex-fold-textcolor-display ("textcolor"))
     (czm-tex-fold-alert-display ("alert"))
     (czm-tex-fold-cite-display ("cite"))
     (auctex-label-numbers-ref-display ("ref"))
     (auctex-label-numbers-eqref-display ("eqref"))
     (auctex-label-numbers-label-display ("label"))
     ("[f]" ("footnote" "marginpar"))
     ("[r]" ("pageref" "footref"))
     ("[i]" ("index" "glossary"))
     ("[1]:||*" ("item"))
     ("[{2}]||[href]" ("href"))
     ("..." ("dots"))
     ("(C)" ("copyright"))
     ("(R)" ("textregistered"))
     ("TM" ("texttrademark"))
     ("🌱" ("documentclass"))
     ("🌌" ("input"))
     ("📚" ("bibliography"))
     ("📖" ("bibliographystyle"))
     ("✅" ("leanok"))
     (1 ("section" "part" "chapter" "subsection" "subsubsection" "paragraph" "subparagraph" "part*" "chapter*" "section*" "subsection*" "subsubsection*" "paragraph*" "\nsubparagraph*" "emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt" "textbf" "textsc" "textup" "underline")))))

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

;;; Begin/End

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
    ((czm-tex-fold-format-theorem-environment . "□")
     ("proof"))
    ((czm-tex-fold-format-theorem-environment . "◼")
     ("abstract" "lemma" "lemma*" "exercise" "example" "proposition" "corollary" "remark" "definition" "theorem" "proof" "conjecture" "notation" "terminology" "fact" "note" "problem" "ass" "acknowledgment" "algorithm" "question" "questions" "assumptions" "answer" "claim" "conclusion" "criterion" "summary" "thm" "prop" "rem" "cor" "lem" "lemmy" "def" "defn" "ex" "exer" "conj" "not" "term" "prob" "ques" "ans" "conc" "crit" "sum" "commentary"))
    ((czm-tex-fold-format-titled-block . "◼")
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

(defcustom czm-tex-fold-exclude-list
  '("equation" "equation*" "align" "align*" "multline" "multline*")
  "List of types to be excluded by `czm-tex-fold-helper-display'."
  :type '(repeat string)
  :group 'czm-tex-fold)

(defun czm-tex-fold--full-environment-name (name)
  "Fetch the full environment name identified by NAME.
If there is no such abbreviation, return NAME as is."
  (or (cdr (assoc name czm-tex-fold--environment-abbreviations)) name))

(defun czm-tex-fold-format-theorem-environment (env &rest _args)
  "Format fold display for theorem-like LaTeX environments.
ENV is the environment name, ARGS are ignored.  Returns a string of the
form \"Environment.\" or \"Environment (Description).\" If a \\label
occurs on the same line, the trailing period is omitted.  The
environment name is capitalized and expanded if it's an abbreviation."
  (let* ((props (text-properties-at 0 env))
         (env-expanded (czm-tex-fold--full-environment-name env))
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

(defun czm-tex-fold-format-titled-block (env &rest args)
  "Format fold display for beamer block environments.
ENV is ignored.  ARGS is a nested list where (caaar args) is expected to
be the capitalized block title.  Returns the capitalized block title,
followed by a period unless a \\label command is found on the same line."
  (let* ((props (text-properties-at 0 env))
         (label-re
          (concat "\\(?:" (mapconcat #'identity reftex-label-regexps "\\|") "\\)"))
         (uppercase (caaar args))
         (has-label (save-excursion (re-search-forward
                                     label-re (line-end-position) t))))
    (set-text-properties 0 (length uppercase) props uppercase)
    (concat
     (format "%s" uppercase)
     (if has-label " " "."))))

(defun czm-tex-fold-helper-display (type env &rest args)
  "Generate fold display string for \\begin{ENV} or \\end{ENV} macro.
TYPE should be either `begin' or `end'.
ENV is the environment name.
ARGS are the remaining {} arguments to the macro.
Returns `abort' if ENV is in `czm-tex-fold-exclude-list',
otherwise returns a string or function based on
`czm-tex-fold-environment-delimiter-spec-list'."
  (if (member env czm-tex-fold-exclude-list)
      'abort
    (let ((default-fold (if (eq type 'begin)
                            czm-tex-fold-begin-default
                          czm-tex-fold-end-default)))
      (catch 'result
        (dolist (spec czm-tex-fold-environment-delimiter-spec-list)
          (let* ((display-rule (car spec))
                 (display-string
                  (if (eq type 'begin)
                      (car display-rule)
                    (cdr display-rule)))
                 (envs (cadr spec)))
            (when (member env envs)
              (throw 'result
                     (if (functionp display-string)
                         (funcall display-string env args)
                       display-string)))))
        default-fold))))

(defun czm-tex-fold-begin-display (env &rest args)
  "Fold display for a \\begin{ENV}.
ARGS is the list of {} arguments supplied to the macro."
  (czm-tex-fold-helper-display 'begin env args))

(defun czm-tex-fold-end-display (env &rest args)
  "Fold display for a \\end{ENV} macro.
ARGS is the list of {} arguments supplied to the macro."
  (czm-tex-fold-helper-display 'end env args))

;;; Colored text

(defun czm-tex-fold-textcolor-display (color text &rest _args)
  "Fold display for a \\textcolor{COLOR}{TEXT} macro."
  (with-temp-buffer
    (insert text)
    (put-text-property (point-min) (point-max)
                       'face `(:foreground ,color)
                       (current-buffer))
    (buffer-string)))

(defconst czm-tex-fold-alert-color "red"
  "Color for alert text.")

(defun czm-tex-fold-alert-display (text &rest _args)
  "Fold display for a \\alert{TEXT} macro."
  (with-temp-buffer
    (insert text)
    (put-text-property (point-min) (point-max)
                       'face `(:foreground ,czm-tex-fold-alert-color)
                       (current-buffer))
    (buffer-string)))

;;; Citations

(defun czm-tex-fold--last-initial-of-name (name)
  "Return last initial of NAME.
NAME should be a name in the format \"Last, First\" or \"First Last\",
possibly with some braces.  Returns first alphanumeric letter before
last space before first comma, if any."
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
  "Abbreviate the BibTeX entry at point.
Returns a string composed of:
- The first letter of each author's last name
- The last two digits of the publication year
Returns nil if either author or year information is missing."
  (when-let* ((case-fold-search t)
              (entry (bibtex-parse-entry))
              (author (bibtex-text-in-field "author" entry))
              (year (bibtex-text-in-field "year" entry)))
    (let* ((initials
            (mapconcat
             #'czm-tex-fold--last-initial-of-name
             (string-split author " and ")))
           (year-XX (when year (substring year -2))))
      (concat initials year-XX))))

(defun czm-tex-fold--find-bib-entry (key files)
  "Find BibTeX entry for KEY in FILES."
  (condition-case nil
      (reftex-pop-to-bibtex-entry key files nil nil nil t)
    (error nil)))

(defcustom czm-tex-fold-bib-file nil
  "Backup BibTeX file from which to extract citation keys.
This is used in case for the file being visited, reftex can't find the
citation keys.  If nil, no backup is used."
  :type 'string
  :group 'czm-tex-fold)

(defun czm-tex-fold--get-citation-info (key)
  "Get citation information for KEY using RefTeX internals.
Returns a string in the format \"AuthorYear\" or the key if not found."
  (when-let* ((entry (or
                      (when-let (files
                                 (condition-case nil
                                     (reftex-get-bibfile-list)
                                   (error nil)))
                        (czm-tex-fold--find-bib-entry key files))
                      (and czm-tex-fold-bib-file
                           (czm-tex-fold--find-bib-entry
                            key (list czm-tex-fold-bib-file))))))
    (with-temp-buffer
      (insert entry)
      (goto-char (point-min))
      (czm-tex-fold-bibtex-abbrev))))

(defun czm-tex-fold-cite-display (keys &rest _args)
  "Generate fold display for a \\cite{KEYS} macro.
KEYS are the citation key(s).  Uses RefTeX to look up citation
information.  Returns a string of the form \"[AuthorYear, Optional
Citation Text]\"."
  (let* ((citation (car (czm-tex-fold--optional-args)))
         (key-list (split-string keys ","))
         (references
          (mapcar #'czm-tex-fold--get-citation-info key-list))
         (joined-references (string-join references ", ")))
    (concat
     "["
     (if (string-empty-p joined-references)
         "c" joined-references)
     (when citation
       (format ", %s" citation))
     "]")))

;;; Miscellaneous overlays

(defun czm-tex-fold--create-misc-overlay (start end str spec)
  "Create an overlay to fold quotes between START and END with STR and SPEC."
  (let ((priority (TeX-overlay-prioritize start end))
        (ov (make-overlay start end)))
    (overlay-put ov 'category 'TeX-fold)
    (overlay-put ov 'priority priority)
    (overlay-put ov 'evaporate t) ; Remove the overlay when the text is modified.
    (overlay-put ov 'display str)
    (overlay-put ov 'TeX-fold-display-string-spec spec)))

(defun czm-tex-fold-quotes (start end)
  "Fold LaTeX quotes between START and END.
Ignores quotes within math environments."
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
  "Fold LaTeX dashes between START and END."
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
  "\\\\\\(Verb\\|verb\\)\\(?:\\[[^]]*\\]\\)?[|{]\\([^|}]*\\)[|}]")

(defun czm-tex-fold-verbs (start end)
  "Fold `\\verb|...|', `\\Verb|...|', and `\\Verb{...}' macros between START and END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward czm-tex-fold--verb-regex end t)
      (let* ((verb-start (match-beginning 0))
             (verb-end (match-end 0))
             (verb-content (match-string 2))
             (spec (lambda (&rest _args)
                     (when (looking-at czm-tex-fold--verb-regex)
                       (match-string 2)))))
        (czm-tex-fold--create-misc-overlay verb-start verb-end verb-content spec)))))

(define-minor-mode czm-tex-fold-misc-mode
  "Minor mode for folding miscellaneous LaTeX constructs.
This includes quotes, dashes, and verbatim environments."
  :lighter nil
  :group 'czm-tex-fold
  (if czm-tex-fold-misc-mode
      (progn
        (add-hook 'TeX-fold-region-functions #'czm-tex-fold-quotes nil t)
        (add-hook 'TeX-fold-region-functions #'czm-tex-fold-dashes nil t)
        (add-hook 'TeX-fold-region-functions #'czm-tex-fold-verbs nil t))
    (remove-hook 'TeX-fold-region-functions #'czm-tex-fold-quotes t)
    (remove-hook 'TeX-fold-region-functions #'czm-tex-fold-dashes t)
    (remove-hook 'TeX-fold-region-functions #'czm-tex-fold-verbs t)))

(provide 'czm-tex-fold)
;;; czm-tex-fold.el ends here
