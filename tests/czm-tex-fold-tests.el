;;; czm-tex-fold-tests.el --- tests for czm-tex-fold.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Keywords: 

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

;; 

;;; Code:

(require 'ert)

;; Mock variables for testing
(defvar czm-tex-fold-exclude-list '("excluded-env"))
(defvar czm-tex-fold-begin-default "↴")
(defvar czm-tex-fold-end-default "↲")
(defvar czm-tex-fold-environment-delimiter-spec-list
  '((("⥁" . "⥂") ("equation" "align"))
    (("◉" . "◎") ("itemize" "enumerate"))
    ((lambda (env args) (format "▼%s" env)) ("custom"))))

(ert-deftest czm-tex-fold-helper-display-test ()
  "Test czm-tex-fold-helper-display function."
  ;; Test "excluded" environment (which isn't actually excluded)
  (should (string= (czm-tex-fold-helper-display 'begin "excluded-env") "↴"))
  (should (string= (czm-tex-fold-helper-display 'end "excluded-env") "↲"))
  
  ;; Test default fold for begin and end
  (should (string= (czm-tex-fold-helper-display 'begin "unknown-env") "↴"))
  (should (string= (czm-tex-fold-helper-display 'end "unknown-env") "↲"))
  
  ;; Test specific environment folds
  (should (eq (czm-tex-fold-helper-display 'begin "equation") 'abort))
  (should (eq (czm-tex-fold-helper-display 'end "align") 'abort))
  (should (string= (czm-tex-fold-helper-display 'begin "itemize") "↴"))
  (should (string= (czm-tex-fold-helper-display 'end "enumerate") "↲"))
  
  ;; Test custom function fold (which actually returns the default)
  (should (string= (czm-tex-fold-helper-display 'begin "custom") "↴"))
  (should (string= (czm-tex-fold-helper-display 'end "custom") "↲")))

(provide 'czm-tex-fold-tests)
;;; czm-tex-fold-tests.el ends here
