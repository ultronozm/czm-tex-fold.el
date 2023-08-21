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

;; This package extends tex-fold.el by overriding
;; `TeX-fold-hide-item'.  The new features include improved folding of
;; \begin{...} and \end{...} declarations, references, citations, and
;; sections.  When possible, the fold display incorporates label
;; numbers extracted from the accompanying .aux file.
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
    (czm-tex-fold-label-function ("label"))
    (czm-tex-fold-cite-function ("cite"))
    (czm-tex-fold-textcolor-function ("textcolor"))
    (czm-tex-fold-alert-function ("alert"))
    ("[r]" ("pageref" "footref"))
    (czm-tex-fold-ref-function ("ref"))
    (czm-tex-fold-eqref-function ("eqref"))
    (czm-tex-fold-href-function ("href"))
    ("[i]" ("index" "glossary"))
    ("[1]:||*" ("item"))
    ("..." ("dots"))
    ("(C)" ("copyright"))
    ("(R)" ("textregistered"))
    ("TM" ("texttrademark"))
    (czm-tex-fold-begin-function ("begin"))
    (czm-tex-fold-end-function ("end"))
    (czm-tex-fold-section-function ("section" "part" "chapter" "subsection" "subsubsection"))
    ("🌱" ("documentclass"))
    ("🌌" ("input"))
    ("📚" ("bibliography"))
    ("📖" ("bibliographystyle"))
    (1 ("paragraph" "subparagraph" "part*" "chapter*" "\nsection*" "subsection*" "subsubsection*" "paragraph*" "\nsubparagraph*" "emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt" "textbf" "textsc" "textup")))
  "List of replacement specifiers and macros to fold.
TODO: docs"
  :type '(repeat (choice (group (string :tag "Replacement Specifier")
                                (repeat :tag "Macros" (string)))
                         (group (integer :tag "Replacement Specifier")
                                (repeat :tag "Macros" (string)))
                         (group (function :tag "Function to execute")
                                (repeat :tag "Macros" (string)))))
  :group 'czm-tex-fold)

(defvar czm-tex-fold--TeX-fold-macro-spec-list-orig nil)

(defun czm-tex-fold-setup ()
  "Default setup for `czm-tex-fold'."
  )

(defcustom
  czm-tex-fold-exclude-list
  '("equation" "equation*" "align" "align*" "multline" "multline*")
  "List of types to be excluded by the advice for `TeX-fold-hide-item'."
  :type '(repeat string)
  :group 'czm-tex-fold)

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

(defcustom czm-tex-fold-spec-list
  '((("🌅" . "🌇")
     ("document"))
    (("⚡" . "⚡")
     ("minted" "minted*"))
    (("♣" . "♣")
     ("results" "results*"))
    ((czm-tex-fold-standard-display-function . "□")
     ("proof"))
    ((czm-tex-fold-standard-display-function . "◼")
     ("lemma" "exercise" "example" "proposition" "corollary" "remark" "definition" "theorem" "proof" "conjecture" "notation" "terminology" "note" "problem" "acknowledgment" "algorithm" "question" "answer" "claim" "conclusion" "criterion" "summary")))
  "List of specifications for `czm-tex-fold-begin-function'.

Each element of the list is a specification for a fold display.
Each specification is a list of two elements: the first element
is either

  - a string, which is used as the fold display string, or

  - a function, which is called with the arguments TYPE, ARGS and
    PLIST (see the documentation for
    `czm-tex-fold-begin-function` for a description of these
    arguments) and should return a string to be used as the fold
    display string.

The second element of the specification is a list of types for
which the fold display string should be used.  The types are the
first argument to the \begin{...} macro (e.g., \"theorem\")."
  :type '(repeat (group (choice (string :tag "Display String")
                                (function :tag "Function to execute"))
                        (repeat :tag "Types" (string))))
  :group 'czm-tex-fold)

(defun czm-tex-fold-standard-display-function (type &optional _args plist)
  "Format fold display for tex environment TYPE.
TYPE, ARGS and PLIST are described in the
documentation for `czm-tex-fold-begin-function'."
  (let ((uppercase (concat (upcase (substring type 0 1)) (substring type 1)))
	(description (plist-get plist :description))
	(label (plist-get plist :label)))
    ;; (concat
    ;;  (format "%s" uppercase)
    ;;  (when label
    ;;    (format " %s" label))
    ;;  (when description
    ;;    (format " (%s)" description))
    ;;  ".")
    (concat
     (format "%s" uppercase)
     (when description
       (format " (%s)" description))
     (if label " " "."))))

(defun czm-tex-fold-helper-function (begin-p type &optional args plist)
  "Fold display string for \begin{...} or \end{...} macro.
If BEGIN-P is non-nil, this is a \begin{...} macro.  Otherwise,
it is an \end{...} macro.  TYPE, ARGS and PLIST are described in the
documentation for `czm-tex-fold-begin-function'."
  (let ((default-fold (if begin-p czm-tex-fold-begin-default czm-tex-fold-end-default)))
    (cl-dolist (spec czm-tex-fold-spec-list default-fold)
      (let ((display-string
	     (if begin-p
		 (caar spec)
	       (cdar spec)))
            (types (cadr spec)))
        (when (member type types)
          (if (functionp display-string)
              (cl-return (funcall display-string type args plist))
            (cl-return display-string)))))))

(defun czm-tex-fold-begin-function (type &optional args plist)
  "Format the fold display for a \begin{...} macro.

TYPE is the first argument to the macro (e.g., \"theorem\").

ARGS is the list of optional {} arguments supplied to the macro.

PLIST contains additional properties:

:description - A description of the environment (e.g., \"The
  fundamental theorem of arithmetic\").

:label - The label of the environment (e.g., \"thm:fundamental\").

:default - The default fold display string for the environment.
  If this is a string, it is used as the fold display string.  If
  it is a list, the first element is used as the fold display
  string."
  (czm-tex-fold-helper-function t type args plist))

(defun czm-tex-fold-end-function (type &optional args plist)
  "Format the fold display for an \end{...} macro.
TYPE, ARGS and PLIST are described in the
documentation for `czm-tex-fold-begin-function'."
  (czm-tex-fold-helper-function nil type args plist))

(defun czm-tex-fold-section-function (_type &optional _args plist)
  "Format the fold display for a sectioning macro.
TYPE, ARGS and PLIST are described in the documentation for
`czm-tex-fold-begin-function'."
  (let* ((default-plain (plist-get plist :default))
	 (default (if (listp default-plain) (car default-plain) default-plain))
	 (label (plist-get plist :label)))
    (setq label nil) 			; decided not to use it
    (concat
     (when label
       (let ((label-string (format "§%s. " label)))
	 (apply #'propertize (append `(,label-string) (text-properties-at 0 default)))))
     default)))

(defun czm-tex-fold-ref-helper (type default)
  "Helper function for `czm-tex-fold-ref-function'.
TYPE is as described in the documentation for `czm-tex-fold-begin-function'.
DEFAULT is the default fold display string for the environment."
  (format "[%s]" (or (czm-tex-util-get-label-number type) default)))

(defun czm-tex-fold-ref-function (type &optional _args _plist)
  "Format the fold display for a \ref{...} macro.
TYPE, ARGS and PLIST are described in the documentation for
`czm-tex-fold-begin-function'."
  (czm-tex-fold-ref-helper type "r"))

(defun czm-tex-fold-eqref-function (type &optional _args _plist)
  "Format the fold display for a \eqref{...} macro.
TYPE, ARGS and PLIST are described in the documentation for
`czm-tex-fold-begin-function'."
  (czm-tex-fold-ref-helper type "e"))

(defun czm-tex-fold-href-function (_type &optional args _plist)
  "Format the fold display for a \\href{...}{...} macro.
TYPE, ARGS and PLIST are described in the documentation for
`czm-tex-fold-begin-function'."
  (format "[%s]"
	  (or (car args)
	      "href")))

(defun czm-tex-fold-label-function (type &optional _args _plist)
  "Format the fold display for a \\label{...} macro.
TYPE, ARGS and PLIST are described in the documentation for
`czm-tex-fold-begin-function'."
  (czm-tex-fold-ref-helper type "l"))

(defun czm-tex-fold-textcolor-function (type &optional args _plist)
  "Format the fold display for a \\textcolor{...}{...} macro.
TYPE, ARGS and PLIST are described in the documentation for
`czm-tex-fold-begin-function'."
  (let ((str   (car args)))
    (with-temp-buffer
      (insert str)
      (put-text-property (point-min) (point-max)
			 'face `(:foreground ,type)
			 (current-buffer))
      (buffer-string))))

(defun czm-tex-fold-alert-function (type &optional _args _plist)
  "Format the fold display for a \\alert{...} macro.
TYPE, ARGS and PLIST are described in the documentation for
`czm-tex-fold-begin-function'."
  (with-temp-buffer
    (insert type)
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
    (let* (
	   (initials
	    (mapconcat
	     (lambda (x)
	       (when-let
		   ((index (string-match "[[:alpha:]]" x)))
		 (substring x index (1+ index))))
	     (string-split author " and ")))
	   (year-XX (when year (substring year -2))))
      (concat initials year-XX))))



(defun czm-tex-fold-cite-function (type &optional _args plist)
  "Format the fold display for a \\cite macro.
TYPE, ARGS and PLIST are described in the documentation for
`czm-tex-fold-begin-function'."
  (let* ((citation (plist-get plist :description))
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
                  (split-string type ",")))
         (joined-references (string-join references ", ")))
    (concat
     "["
     (if (string-empty-p joined-references) "c" joined-references)
     (when citation
       (format ", %s" citation))
     "]")))



(defun czm-tex-fold-override-TeX-fold-hide-item (ov)
  "Hide a single macro or environment.
That means, put respective properties onto overlay OV."
  (let* ((ov-start (overlay-start ov))
	 (ov-end (overlay-end ov))
	 (spec (overlay-get ov 'TeX-fold-display-string-spec))
	 (type (TeX-fold-macro-nth-arg 1 ov-start ov-end)))
    (if (and (functionp spec)
	     (memq spec '(czm-tex-fold-begin-function czm-tex-fold-end-function))
	     (member (car type) czm-tex-fold-exclude-list))
	t
      (let* (
	     (computed (cond
			((stringp spec)
			 (TeX-fold-expand-spec spec ov-start ov-end))
			((functionp spec)
			 (let (arg arg-list
				   (n 1)
				   (m 1))
			   (while (setq arg (TeX-fold-macro-nth-arg
					     n ov-start ov-end))
			     (unless (member (car arg) arg-list)
			       (setq arg-list (append arg-list (list (car arg)))))
			     (setq n (1+ n)))
			   (let* ((description
				   (car
				    (TeX-fold-macro-nth-arg
				     m ov-start ov-end
				     '(?\[ . ?\]))))
				  (label
				   (save-excursion
				     (goto-char ov-start)
				     (when
					 (re-search-forward
					  "\\label{\\([^}]+\\)}" (line-end-position) t)
				       (let ((name
					      (match-string-no-properties 1)))
					 (czm-tex-util-get-label-number name)))))
				  (plist `(:description ,description
							:label ,label
							:default ,(TeX-fold-macro-nth-arg 1 ov-start ov-end))))
			     (funcall spec (car arg-list) (cdr arg-list) plist)
			     ;; (or (condition-case nil
			     ;; 	     (funcall spec (car arg-list) (cdr arg-list) plist)
			     ;; 	   (error nil))
			     ;; 	 "[Error: No content or function found]")
			     )))
			(t (or (TeX-fold-macro-nth-arg spec ov-start ov-end)
			       "[Error: No content found]"))))
	     (display-string (if (listp computed) (car computed) computed))
	     ;; (face (when (listp computed) (cadr computed)))
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
	    ;; Add raise adjustment for superscript and subscript.  (bug#42209)
	    (setq display-string
		  (propertize display-string
			      'display (get-text-property ov-start 'display))))
	  (overlay-put ov 'display display-string)
	  (when font-lock-mode
	    (overlay-put ov 'face TeX-fold-folded-face))
	  (unless (zerop TeX-fold-help-echo-max-length)
	    (overlay-put ov 'help-echo (TeX-fold-make-help-echo
					(overlay-start ov) (overlay-end ov)))))))))




(defun czm-tex-fold--init ()
  (advice-add 'TeX-fold-clearout-buffer :after #'czm-tex-fold-clear-quote-overlays)
  (advice-add 'TeX-fold-region :after #'czm-tex-fold-quotes)
  (advice-add 'TeX-fold-region :after #'czm-tex-fold-dashes)
  (advice-add #'TeX-fold-hide-item :override #'czm-tex-fold-override-TeX-fold-hide-item))

(defun czm-tex-fold--close ()
  (advice-remove 'TeX-fold-clearout-buffer #'czm-tex-fold-clear-quote-overlays)
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

        (when TeX-fold-mode
          (TeX-fold-mode 0))
        (TeX-fold-mode 1)
        
        (setq czm-tex-fold--TeX-fold-macro-spec-list-orig TeX-fold-macro-spec-list)
        (setq TeX-fold-macro-spec-list czm-tex-fold-macro-spec-list)
        
        (setq czm-tex-fold--aux-files-revert-without-query-orig
              (member "\\.aux$" revert-without-query))
        (add-to-list 'revert-without-query "\\.aux$")

        (setq czm-tex-fold--tex-fold-mode-orig TeX-fold-mode)

        )
    
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
        (czm-tex-fold-create-quote-overlay quote-start quote-end replacement)))))

(defun czm-tex-fold-dashes (start end)
  "Fold dashes in the region between START and END using overlays."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\b\\(---\\)\\b" end t)
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0))
            (replacement "—"))
        (czm-tex-fold-create-quote-overlay match-start match-end replacement)))
    (goto-char start)
    (while (re-search-forward "\\b\\(--\\)\\b" end t)
      (let ((match-start (match-beginning 0))
            (match-end (match-end 0))
            (replacement "–"))
        (czm-tex-fold-create-quote-overlay match-start match-end replacement)))))


(defun czm-tex-fold-create-quote-overlay (start end replacement)
  "Create an overlay to fold quotes between START and END with REPLACEMENT."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'display replacement)
    (overlay-put overlay 'evaporate t) ; Remove the overlay when the text is modified.
    (overlay-put overlay 'quote-fold t)))

(defun czm-tex-fold-clear-quote-overlays ()
  "Remove all quote overlays in the current buffer."
  (remove-overlays nil nil 'quote-fold t))


;; miscellaneous: fold the contents of a section

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
