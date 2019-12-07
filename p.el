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

(defmacro p-sexp (form &optional stream)
  "Output the pretty-printed representation of FORM.
If specify STREAM, change `standard-output'."
  `(progn
     (pp ,form)
     nil))

(defmacro p-macroexpand (form &optional stream)
  "Output the pretty-printed `macroexpand-1' representation of FORM.
If specify STREAM, change `standard-output'."
  `(progn
     (pp (macroexpand-1 ',form))
     nil))

(defmacro p-list (form &optional stream)
  "Output the pretty-printed representation of FORM.
If specify STREAM, change `standard-output'."
  `(progn
     (princ
      (with-temp-buffer
        (insert (prin1-to-string ,form))
        (goto-char (point-min))
        (forward-char)
        (ignore-errors
          (while t (forward-sexp) (insert "\n")))
        (delete-char -1)
        (buffer-substring-no-properties (point-min) (point-max)))
      (or ,stream standard-output))
     (princ "\n" (or ,stream standard-output))
     nil))

(provide 'p)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; p.el ends here
