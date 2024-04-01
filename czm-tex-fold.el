;;; czm-tex-fold.el --- Extensions for tex-fold.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-tex-fold.el
;; Package-Requires: ((emacs "29.1") (czm-tex-util "0.0"))
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
;;
;; As a miscellaneous feature, this package includes the section
;; folding commands `czm-tex-fold-fold-section' and
;; `czm-tex-fold-clearout-section'.


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
  "List of types to be excluded by the advice for `TeX-fold-hide-item'."
  :type '(repeat string)
  :group 'czm-tex-fold)

(defcustom czm-tex-fold-fold-quotes t
  "Whether to fold quotes."
  :type 'boolean
  :group 'czm-tex-fold)

(defcustom czm-tex-fold-fold-dashes t
  "Whether to fold dashes."
  :type 'boolean
  :group 'czm-tex-fold)

(defcustom czm-tex-fold-fold-verbs t
  "Whether to fold `\\verb|...|' patterns."
  :type 'boolean
  :group 'czm-tex-fold)

(defun czm-tex-fold-install ()
  "Install `czm-tex-fold'."
  (interactive)
  (advice-add 'TeX-fold-hide-item :override #'czm-tex-fold--override-hide-item)
  (when czm-tex-fold-fold-quotes
    (advice-add 'TeX-fold-region :after #'czm-tex-fold-quotes))
  (when czm-tex-fold-fold-dashes
    (advice-add 'TeX-fold-region :after #'czm-tex-fold-dashes))
  (when czm-tex-fold-fold-verbs
    (advice-add 'TeX-fold-region :after #'czm-tex-fold-verbs))
  (advice-add 'TeX-fold-clearout-buffer :after #'czm-tex-fold--clear-misc-overlays))

(defun czm-tex-fold-uninstall ()
  "Uninstall `czm-tex-fold'."
  (interactive)
  (advice-remove 'TeX-fold-hide-item #'czm-tex-fold--override-hide-item)
  (advice-remove 'TeX-fold-region #'czm-tex-fold-quotes)
  (advice-remove 'TeX-fold-region #'czm-tex-fold-dashes)
  (advice-remove 'TeX-fold-region #'czm-tex-fold-verbs)
  (advice-remove 'TeX-fold-clearout-buffer #'czm-tex-fold--clear-misc-overlays))

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
     ("abstract" "lemma" "exercise" "example" "proposition" "corollary" "remark" "definition" "theorem" "proof" "conjecture" "notation" "terminology" "fact" "note" "problem" "ass" "acknowledgment" "algorithm" "question" "answer" "claim" "conclusion" "criterion" "summary" "thm" "prop" "rem" "cor" "lem" "lemmy" "def" "defn" "ex" "exer" "conj" "not" "term" "prob" "ques" "ans" "conc" "crit" "sum"))
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
            (replacement (if (string= (match-string 0) "``") "“" "”")))
        (czm-tex-fold--create-misc-overlay quote-start quote-end replacement)))))

(defun czm-tex-fold-dashes (start end)
  "Fold dashes in the region between START and END using overlays."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\b\\(---\\)\\b" end t)
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0))
            (replacement "—"))
        (czm-tex-fold--create-misc-overlay match-start match-end replacement)))
    (goto-char start)
    (while (re-search-forward "\\b\\(--\\)\\b" end t)
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0))
            (replacement "–"))
        (czm-tex-fold--create-misc-overlay match-start match-end replacement)))))

(defvar czm-tex-fold--verb-regex
  "\\\\verb\\(.\\)\\([^\\1]*\\)\\1")

(defun czm-tex-fold-verbs (start end)
  "Fold `\\verb|...|' in between START and END using overlays."
  (save-excursion
    (goto-char start)
    (while (re-search-forward czm-tex-fold--verb-regex end t)
      (let ((verb-start (match-beginning 0))
            (verb-end (match-end 0))
            (replacement (match-string 2)))
        (czm-tex-fold--create-misc-overlay verb-start verb-end replacement)))))

(defun czm-tex-fold--create-misc-overlay (start end replacement)
  "Create an overlay to fold quotes between START and END with REPLACEMENT."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'display replacement)
    (overlay-put overlay 'evaporate t) ; Remove the overlay when the text is modified.
    (overlay-put overlay 'czm-tex-fold-misc t)))

(defun czm-tex-fold--clear-misc-overlays ()
  "Remove all quote overlays in the current buffer."
  (remove-overlays nil nil 'czm-tex-fold-misc t))

;; miscellaneous: fold the contents of a section.  These could have
;; just as well been included in `tex-fold.el'.

;;;###autoload
(defun czm-tex-fold-fold-section ()
  "Hide all configured macros and environments in the current section.
The relevant macros are specified in the variable `TeX-fold-macro-spec-list'
and `TeX-fold-math-spec-list', and environments in `TeX-fold-env-spec-list'."
  (interactive)
  (save-mark-and-excursion
    (LaTeX-mark-section)
    (let ((start (point))
          (end (mark)))
      (TeX-fold-clearout-region start end)
      (TeX-fold-region start end))))


;;;###autoload
(defun czm-tex-fold-clearout-section ()
  "Permanently show all macros in the section point is located in."
  (interactive)
  (save-mark-and-excursion
    (LaTeX-mark-section)
    (let ((start (point))
          (end (mark)))
      (TeX-fold-clearout-region start end))))

(defun czm-tex-fold--override-hide-item (ov)
  "Hide a single macro or environment.
That means, put respective properties onto overlay OV.

OVERRIDE DIFFERENCE: if the function object returns `abort',
then the overlay is deleted."
  (let* ((ov-start (overlay-start ov))
         (ov-end (overlay-end ov))
         (spec (overlay-get ov 'TeX-fold-display-string-spec))
         (computed (cond
                    ((stringp spec)
                     (TeX-fold-expand-spec spec ov-start ov-end))
                    ((functionp spec)
                     (let (arg arg-list
                               (n 1))
                       (while (setq arg (TeX-fold-macro-nth-arg
                                         n ov-start ov-end))
                         (unless (member (car arg) arg-list)
                           (setq arg-list (append arg-list (list (car arg)))))
                         (setq n (1+ n)))
                       (or (condition-case nil
                               (apply spec arg-list)
                             (error nil))
                           "[Error: No content or function found]")))
                    (t (or (TeX-fold-macro-nth-arg spec ov-start ov-end)
                           "[Error: No content found]"))))
         (display-string (if (listp computed) (car computed) computed))
         ;; (face (when (listp computed) (cadr computed)))
         )
    (if (eq computed 'abort)
        (progn (delete-overlay ov)
               t ; so that `TeX-fold-dwim' "gives up"
               )
      ;; Do nothing if the overlay is empty.
      (when (and ov-start ov-end)
        ;; Cater for zero-length display strings.
        (when (string= display-string "") (setq display-string TeX-fold-ellipsis))
        ;; Add a linebreak to the display string and adjust the overlay end
        ;; in case of an overfull line.
        (when (TeX-fold-overfull-p ov-start ov-end display-string)
          (setq display-string (concat display-string "\n"))
          (move-overlay ov ov-start (save-excursion
                                      (goto-char ov-end)
                                      (skip-chars-forward " \t")
                                      (point))))
        (overlay-put ov 'mouse-face 'highlight)
        (when font-lock-mode
          ;; Add raise adjustment for superscript and subscript.
          ;; (bug#42209)
          (setq display-string
                (propertize display-string
                            'display (get-text-property ov-start 'display))))
        (overlay-put ov 'display display-string)
        (when font-lock-mode
          (overlay-put ov 'face TeX-fold-folded-face))
        (unless (zerop TeX-fold-help-echo-max-length)
          (overlay-put ov 'help-echo (TeX-fold-make-help-echo
                                      (overlay-start ov) (overlay-end ov))))))))

(provide 'czm-tex-fold)
;;; czm-tex-fold.el ends here
