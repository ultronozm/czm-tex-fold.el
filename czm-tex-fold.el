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

;; This package provides a minor mode, `czm-tex-fold-mode', that wraps
;; AUCTeX's `tex-fold-mode' and provides additional folding
;; functionality.  The additional functionality includes improved
;; folding of \begin{...} and \end{...} declarations, references,
;; citations, and sections.  When possible, the fold display
;; incorporates label numbers extracted from the accompanying .aux
;; file.
;; 
;; Let's start with \begin{...} and \end{...}.  `tex-fold-mode' folds
;; these simply as [begin] and [end], respectively.  This package


;; The new features include improved
;; folding of \begin{...} and \end{...} declarations, references,
;; citations, and sections.  When possible, the fold display
;; incorporates label numbers extracted from the accompanying .aux
;; file.
;;
;; The package
;;
;; My use-package declaration:
;; 
;; (use-package czm-tex-fold
;;     :after tex-fold
;;     :vc (:url "https://github.com/ultronozm/czm-tex-fold.el.git"
;;               :rev :newest)
;;     :demand
;;     :bind
;;     (:map TeX-fold-mode-map
;; 	  ("C-c C-o C-s" . czm-tex-fold-fold-section)
;; 	  ("C-c C-o s" . czm-tex-fold-clearout-section))
;;     :config
;;     (czm-tex-fold-setup)
;;     :custom
;;     (czm-tex-fold-bib-file . "~/doit/refs.bib")
;;     :hook
;;     (LaTeX-mode . czm-tex-fold-mode))
;;
;; Replace "~/doit/refs.bib" with your favorite bib file.  To
;; customize the fold display, replace `czm-tex-fold-setup' in the
;; above config with your own function.


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

;; TODO should probably redesign this to be a major mode that you
;; activate, rather than something that modifies stuff

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
     (1 ("section" "part" "chapter" "subsection" "subsubsection" "paragraph" "subparagraph" "part*" "chapter*" "\nsection*" "subsection*" "subsubsection*" "paragraph*" "\nsubparagraph*" "emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt" "textbf" "textsc" "textup")))))

(defun czm-tex-fold-misc-install ()
  "Install miscellaneous folding features."
  (advice-add 'TeX-fold-region :after #'czm-tex-fold-quotes)
  (advice-add 'TeX-fold-region :after #'czm-tex-fold-dashes)
  (advice-add 'TeX-fold-clearout-buffer :after #'czm-tex-fold--clear-misc-overlays))

(defun czm-tex-fold-misc-uninstall ()
  "Uninstall miscellaneous folding features."
  (advice-remove 'TeX-fold-region #'czm-tex-fold-quotes)
  (advice-remove 'TeX-fold-region #'czm-tex-fold-dashes)
  (advice-remove 'TeX-fold-clearout-buffer #'czm-tex-fold--clear-misc-overlays))

(defcustom czm-tex-fold-begin-default
  "↴"
  "Default fold display for a \begin{...} macro."
  :type 'string
  :group 'czm-tex-fold)

(defcustom czm-tex-fold-end-default
  "↲"
  "Default fold display for an \end{...} macro."
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
     ("lemma" "exercise" "example" "proposition" "corollary" "remark" "definition" "theorem" "proof" "conjecture" "notation" "terminology" "note" "problem" "acknowledgment" "algorithm" "question" "answer" "claim" "conclusion" "criterion" "summary")))
  "List of specifications for `czm-tex-fold-begin-display'.

Each element of the list is a specification for a fold display.
Each specification is a list of two elements.  The first element
is a cons cell, with car and cdr corresponding to \begin{...} and
\end{...} macros, each of which is either

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

(defun czm-tex-fold-standard-display (env &rest _args)
  "Format fold display for tex environment \begin{ENV}.
Return \"Env.\" except or \"Env (Description).\" except when a
label occurs on the same line; in that case, omit the period."
  (let ((uppercase (concat (upcase (substring env 0 1)) (substring env 1)))
        (description (car (czm-tex-fold--optional-args)))
        (has-label (save-excursion (re-search-forward
                                    "\\label{\\([^}]+\\)}" (line-end-position) t))))
    (concat
     (format "%s" uppercase)
     (when description
       (format " (%s)" description))
     (if has-label " " "."))))

(defun czm-tex-fold-helper-display (type env &rest args)
  "Fold display string for \begin{ENV} or \end{ENV} macro.
TYPE should be either \='begin or \='end.  ARGS are the remaining
{} arguments to the macro."
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
            (cl-return display-string)))))))

(defun czm-tex-fold-begin-display (env &rest args)
  "Fold display for a \begin{ENV}.
ARGS is the list of {} arguments supplied to the macro."
  (czm-tex-fold-helper-display 'begin env args))

(defun czm-tex-fold-end-display (env &rest args)
  "Fold display for a \end{ENV} macro.
ARGS is the list of {} arguments supplied to the macro."
  (czm-tex-fold-helper-display 'end env args))

(defun czm-tex-fold-ref-helper (label default)
  "Helper function for `czm-tex-fold-ref-display'.
LABEL is the label name.
DEFAULT is the default fold display string for the environment."
  (format "[%s]" (or (czm-tex-util-get-label-number label)
                     default)))

(defun czm-tex-fold-ref-display (label &rest _args)
  "Fold display for a \ref{LABEL} macro."
  (czm-tex-fold-ref-helper label "r"))

(defun czm-tex-fold-eqref-display (label &rest _args)
  "Fold display for a \eqref{LABEL} macro."
  (czm-tex-fold-ref-helper label "e"))

(defun czm-tex-fold-href-display (_link name &rest _args)
  "Fold display for a \\href{LINK}{NAME} macro."
  (format "[%s]" (or name "href")))

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

(defun czm-tex-fold-bibtex-abbrev ()
  "Abbreviate the current bibtex entry.
Use first letter of each author's last name and 2-digit year."
  (when-let* ((entry (bibtex-parse-entry))
              (author (bibtex-text-in-field "author" entry))
              (year (bibtex-text-in-field "year" entry)))
    (let* ((initials
            (mapconcat
             (lambda (x)
               (when-let
                   ((index (string-match "[[:alpha:]]" x)))
                 (substring x index (1+ index))))
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
                                  (case-fold-search t)) ; else bibtex-parse-entry breaks
                        (when (file-exists-p bib-file)
                          (with-current-buffer (find-file-noselect bib-file)
                            (save-excursion
                              (goto-char (point-min))
                              (search-forward (concat "{" trimmed-cite ",") nil t)
                              (save-excursion
                                (bibtex-beginning-of-entry)
                                (czm-tex-fold-bibtex-abbrev))))))))
                  (split-string text ",")))
         (joined-references (string-join references ", ")))
    (concat
     "["
     (if (string-empty-p joined-references) "c" joined-references)
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


(provide 'czm-tex-fold)
;;; czm-tex-fold.el ends here
