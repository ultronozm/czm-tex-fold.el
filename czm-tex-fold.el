;; czm-tex-fold.el --- Extensions for tex-fold.el  -*- lexical-binding: t; -*-

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

(defcustom czm-tex-fold-macro-spec-list
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
    (1 ("section" "part" "chapter" "subsection" "subsubsection" "paragraph" "subparagraph" "part*" "chapter*" "\nsection*" "subsection*" "subsubsection*" "paragraph*" "\nsubparagraph*" "emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt" "textbf" "textsc" "textup"))))

(defun czm-tex-fold-setup ()
  "Default setup for `czm-tex-fold'."
  (interactive)
  (setq TeX-fold-macro-spec-list czm-tex-fold-macro-spec-list))

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
  (let ((beg (point))
        (end (TeX-fold-item-end (point) 'macro))
        (n 1) result)
    (while-let ((arg (TeX-fold-macro-nth-arg
                      n beg end '(?\[ . ?\]))))
      (push (substring-no-properties (car arg)) result)
      (setq n (1+ n)))
    (nreverse result)))

(defun czm-tex-fold-standard-display (type &rest args)
  "Format fold display for tex environment TYPE.
TYPE, ARGS and PLIST are described in the
documentation for `czm-tex-fold-begin-display'."
  (let ((uppercase (concat (upcase (substring type 0 1)) (substring type 1)))
        (description (car (czm-tex-fold--optional-args)))
        (has-label (re-search-forward
                    "\\label{\\([^}]+\\)}" (line-end-position) t)))
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
  (czm-tex-fold-helper-display 'begin env args plist))

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

(defun czm-tex-fold-href-display (link name &rest _args)
  "Fold display for a \\href{LINK}{NAME} macro."
  (format "[%s]" (or name "href")))

(defun czm-tex-fold-label-display (label &rest _args)
  "Fold display for a \\label{LABEL} macro."
  (czm-tex-fold-ref-helper type "l"))

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
The abbreviation is the first letter of each author's last name
followed by the last two digits of the year."
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



(defun czm-tex-fold-cite-display (reference-names &rest _args)
  "Format the fold display for a \\cite macro.
TYPE, ARGS and PLIST are described in the documentation for
`czm-tex-fold-begin-display'."
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
                  (split-string reference-names ",")))
         (joined-references (string-join references ", ")))
    (concat
     "["
     (if (string-empty-p joined-references) "c" joined-references)
     (when citation
       (format ", %s" citation))
     "]")))



;; (defun czm-tex-fold-override-TeX-fold-hide-item (ov)
;;   "Hide a single macro or environment.
;; That means, put respective properties onto overlay OV."
;;   (let* ((ov-start (overlay-start ov))
;;          (ov-end (overlay-end ov))
;;          (spec (overlay-get ov 'TeX-fold-display-string-spec))
;;          (type (TeX-fold-macro-nth-arg 1 ov-start ov-end)))
;;     (if (and (functionp spec)
;;              (memq spec '(czm-tex-fold-begin-display czm-tex-fold-end-function))
;;              (member (car type) czm-tex-fold-exclude-list))
;;         t
;;       (let* (
;;              (computed (cond
;;                          ((stringp spec)
;;                           (TeX-fold-expand-spec spec ov-start ov-end))
;;                          ((functionp spec)
;;                           (let (arg arg-list
;;                                     (n 1)
;;                                     (m 1))
;;                             (while (setq arg (TeX-fold-macro-nth-arg
;;                                               n ov-start ov-end))
;;                               (unless (member (car arg) arg-list)
;;                                 (setq arg-list (append arg-list (list (car arg)))))
;;                               (setq n (1+ n)))
;;                             (let* ((description
;;                                     (car
;;                                      (TeX-fold-macro-nth-arg
;;                                       m ov-start ov-end
;;                                       '(?\[ . ?\]))))
;;                                    (label
;;                                     (save-excursion
;;                                       (goto-char ov-start)
;;                                       (when
;;                                           (re-search-forward
;;                                            "\\label{\\([^}]+\\)}" (line-end-position) t)
;;                                         (let ((name
;;                                                (match-string-no-properties 1)))
;;                                           (czm-tex-util-get-label-number name)))))
;;                                    (plist `(:description ,description
;;                                                          :label ,label
;;                                                          :default ,(TeX-fold-macro-nth-arg 1 ov-start ov-end))))
;;                               (funcall spec (car arg-list) (cdr arg-list) plist)
;;                               ;; (or (condition-case nil
;;                               ;;              (funcall spec (car arg-list) (cdr arg-list) plist)
;;                               ;;            (error nil))
;;                               ;;          "[Error: No content or function found]")
;;                               )))
;;                          (t (or (TeX-fold-macro-nth-arg spec ov-start ov-end)
;;                                 "[Error: No content found]"))))
;;              (display-string (if (listp computed) (car computed) computed))
;;              ;; (face (when (listp computed) (cadr computed)))
;;              )
;;         ;; Do nothing if the overlay is empty.
;;         (when (and ov-start ov-end)
;;           ;; Cater for zero-length display strings.
;;           (when (string= display-string "") (setq display-string TeX-fold-ellipsis))
;;           ;; Add a linebreak to the display string and adjust the overlay end
;;           ;; in case of an overfull line.
;;           (when (TeX-fold-overfull-p ov-start ov-end display-string)
;;             (setq display-string (concat display-string "\n"))
;;             (move-overlay ov ov-start (save-excursion
;;                                         (goto-char ov-end)
;;                                         (skip-chars-forward " \t")
;;                                         (point))))
;;           (overlay-put ov 'mouse-face 'highlight)
;;           (when font-lock-mode
;;             ;; Add raise adjustment for superscript and subscript.  (bug#42209)
;;             (setq display-string
;;                   (propertize display-string
;;                               'display (get-text-property ov-start 'display))))
;;           (overlay-put ov 'display display-string)
;;           (when font-lock-mode
;;             (overlay-put ov 'face TeX-fold-folded-face))
;;           (unless (zerop TeX-fold-help-echo-max-length)
;;             (overlay-put ov 'help-echo (TeX-fold-make-help-echo
;;                                         (overlay-start ov) (overlay-end ov)))))))))




(defun czm-tex-fold--init ()
  (advice-add 'TeX-fold-clearout-buffer :after #'czm-tex-fold--clear-misc-overlays)
  (advice-add 'TeX-fold-region :after #'czm-tex-fold-quotes)
  (advice-add 'TeX-fold-region :after #'czm-tex-fold-dashes)
  (advice-add #'TeX-fold-hide-item :override #'czm-tex-fold-override-TeX-fold-hide-item))

(defun czm-tex-fold--close ()
  (advice-remove 'TeX-fold-clearout-buffer #'czm-tex-fold--clear-misc-overlays)
  (advice-remove 'TeX-fold-region #'czm-tex-fold-quotes)
  (advice-remove 'TeX-fold-region #'czm-tex-fold-dashes)
  (advice-remove #'TeX-fold-hide-item #'czm-tex-fold-override-TeX-fold-hide-item))

(defvar czm-tex-fold--aux-files-revert-without-query-orig nil
  "Stores whether \"\\\\.aux\" is in `revert-without-query'")

(defvar czm-tex-fold--tex-fold-mode-orig nil
  "Stores the value of `TeX-fold-mode'")

(define-minor-mode czm-tex-fold-mode
  "Minor mode for hiding and revealing macros and environments."
  :init-value nil
  :lighter nil
  :keymap (list (cons TeX-fold-command-prefix TeX-fold-keymap))
  (if czm-tex-fold-mode
      (progn
        (czm-tex-fold--init)

        (setq czm-tex-fold--TeX-fold-macro-spec-list-orig TeX-fold-macro-spec-list)
        (setq TeX-fold-macro-spec-list czm-tex-fold-macro-spec-list)

        (setq czm-tex-fold--tex-fold-mode-orig TeX-fold-mode)

        (when TeX-fold-mode
          (TeX-fold-mode 0))
        (TeX-fold-mode 1)

        (setq czm-tex-fold--aux-files-revert-without-query-orig
              (member "\\.aux$" revert-without-query))
        (add-to-list 'revert-without-query "\\.aux$"))

    (czm-tex-fold--close)

    (setq TeX-fold-macro-spec-list czm-tex-fold--TeX-fold-macro-spec-list-orig)

    (unless czm-tex-fold--aux-files-revert-without-query-orig
      (setq revert-without-query
            (remove "\\.aux$" revert-without-query)))

    (unless czm-tex-fold--tex-fold-mode-orig
      (TeX-fold-mode 0))))

;; miscellaneous: fold quotes and dashes

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
