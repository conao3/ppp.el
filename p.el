;;; p.el --- Extended pretty printer for Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires: ((emacs "26"))
;; License: AGPL-3.0
;; URL: https://github.com/conao3/p.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extended pretty printer for Emacs Lisp


;;; Code:

(defgroup p nil
  "Extended pretty printer for Emacs Lisp"
  :prefix "p-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/p.el"))

(defcustom p-escape-newlines t
  "Value of `print-escape-newlines' used by p-* functions."
  :type 'boolean
  :group 'p)


;;; Functions

(defmacro with-p-buffer (form &rest body)
  "Insert FORM, execute BODY, return `buffer-string'."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (lisp-mode-variables nil)
     (set-syntax-table emacs-lisp-mode-syntax-table)
     (let ((print-escape-newlines p-escape-newlines)
           (print-quoted t))
       (prin1 ,form (current-buffer))
       (goto-char (point-min)))
     (progn ,@body)
     (delete-trailing-whitespace)
     (buffer-string)))

(defun p-sexp (form)
  "Output the pretty-printed representation of FORM suitable for objects."
  (progn
    (let ((str (with-p-buffer form
                 ;; `pp-buffer'
                 (while (not (eobp))
                   ;; (message "%06d" (- (point-max) (point)))
                   (cond
                    ((ignore-errors (down-list 1) t)
                     (save-excursion
                       (backward-char 1)
                       (skip-chars-backward "'`#^")
                       (when (not (bobp))
                         (insert "\n"))))
                    ((ignore-errors (up-list 1) t)
                     (skip-syntax-forward ")")
                     (insert "\n"))
                    (t (goto-char (point-max)))))
                 (goto-char (point-min))
                 (indent-sexp))))
      (princ str))
    nil))

(defmacro p-macroexpand (form)
  "Output the pretty-printed representation of FORM suitable for macro."
  `(progn
     (p-sexp (macroexpand-1 ',form))
     nil))

(defmacro p-macroexpand-all (form)
  "Output the pretty-printed representation of FORM suitable for macro.
Unlike `p-macroexpand', use `macroexpand-all' instead of `macroexpand-1'."
  `(progn
     (p-sexp (macroexpand-all ',form))
     nil))

(defun p-list (form)
  "Output the pretty-printed representation of FORM suitable for list."
  (progn
    (let ((str (with-p-buffer form
                 (forward-char)
                 (ignore-errors
                   (while t (forward-sexp) (insert "\n")))
                 (delete-char -1))))
      (princ (concat str "\n")))
    nil))

(provide 'p)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; p.el ends here
