;;; ppp.el --- Extended pretty printer for Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 1.0.4
;; Keywords: tools
;; Package-Requires: ((emacs "25.1"))
;; License: AGPL-3.0
;; URL: https://github.com/conao3/ppp.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extended pretty printer for Emacs Lisp


;;; Code:

(require 'warnings)
(require 'seq)

(defgroup ppp nil
  "Extended pretty printer for Emacs Lisp"
  :prefix "ppp-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/ppp.el"))

(defcustom ppp-escape-newlines t
  "Value of `print-escape-newlines' used by ppp-* functions."
  :type 'boolean
  :group 'ppp)

(defcustom ppp-debug-buffer-template "*PPP Debug buffer - %s*"
  "Buffer name for debugging."
  :group 'ppp
  :type 'string)

(defcustom ppp-minimum-warning-level-base :warning
  "Minimum level for debugging.
It should be either :debug, :warning, :error, or :emergency.
Every minimul-earning-level variable initialized by this variable.
You can customize each variable like ppp-minimum-warning-level--{{pkg}}."
  :group 'ppp
  :type 'symbol)


;;; Helpers

(defmacro with-ppp--working-buffer (form &rest body)
  "Insert FORM, execute BODY, return `buffer-string'."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (lisp-mode-variables nil)
     (set-syntax-table emacs-lisp-mode-syntax-table)
     (let ((print-escape-newlines ppp-escape-newlines)
           (print-quoted t))
       (prin1 ,form (current-buffer))
       (goto-char (point-min)))
     (progn ,@body)
     (delete-trailing-whitespace)
     (buffer-substring-no-properties (point-min) (point-max))))


;;; Macros

;;;###autoload
(defmacro ppp-sexp-to-string (form)
  "Output the pretty-printed representation of FORM suitable for objects.
See `ppp-sexp' to get more info."
  `(with-output-to-string
     (ppp-sexp ,form)))

;;;###autoload
(defmacro ppp-macroexpand-to-string (form)
  "Output the pretty-printed representation of FORM suitable for macro.
See `ppp-macroexpand' to get more info."
  `(with-output-to-string
     (ppp-macroexpand ,form)))

;;;###autoload
(defmacro ppp-macroexpand-all-to-string (form)
  "Output the pretty-printed representation of FORM suitable for macro.
Unlike `ppp-macroexpand', use `macroexpand-all' instead of `macroexpand-1'.
See `ppp-macroexpand-all' to get more info."
  `(with-output-to-string
     (ppp-macroexpand-all ,form)))

;;;###autoload
(defmacro ppp-list-to-string (form)
  "Output the pretty-printed representation of FORM suitable for list.
See `ppp-list' to get more info."
  `(with-output-to-string
     (ppp-list ,form)))

;;;###autoload
(defmacro ppp-plist-to-string (form)
  "Output the pretty-printed representation of FORM suitable for plist.
See `ppp-plist' to get more info."
  `(with-output-to-string
     (ppp-plist ,form)))


;;; Functions

;;;###autoload
(defun ppp-sexp (form)
  "Output the pretty-printed representation of FORM suitable for objects."
  (prog1 nil
    (let ((str (with-ppp--working-buffer form
                 ;; `pp-buffer'
                 (while (not (eobp))
                   ;; (message "%06d" (- (point-max) (point)))
                   (cond
                    ((ignore-errors (down-list 1) t)
                     (save-excursion
                       (backward-char 1)
                       (skip-chars-backward "'`#^")
                       (when (and (not (bobp)) (memq (char-before) '(?\s ?\t ?\n)))
                         (delete-region
                          (point)
                          (progn (skip-chars-backward " \t\n") (point)))
                         (insert "\n"))))
                    ((ignore-errors (up-list 1) t)
                     (skip-syntax-forward ")")
                     (delete-region
                      (point)
                      (progn (skip-chars-forward " \t\n") (point)))
                     (insert "\n"))
                    (t (goto-char (point-max)))))
                 (goto-char (point-min))
                 (indent-sexp))))
      (princ str))))

;;;###autoload
(defmacro ppp-macroexpand (form)
  "Output the pretty-printed representation of FORM suitable for macro."
  `(ppp-sexp (macroexpand-1 ',form)))

;;;###autoload
(defmacro ppp-macroexpand-all (form)
  "Output the pretty-printed representation of FORM suitable for macro.
Unlike `ppp-macroexpand', use `macroexpand-all' instead of `macroexpand-1'."
  `(ppp-sexp (macroexpand-all ',form)))

;;;###autoload
(defun ppp-list (form)
  "Output the pretty-printed representation of FORM suitable for list."
  (prog1 nil
    (let ((str (with-ppp--working-buffer form
                 (when (and form (listp form))
                   (forward-char)
                   (ignore-errors
                     (while t (forward-sexp) (newline)))
                   (delete-char -1)))))
      (princ (concat str "\n")))))

;;;###autoload
(defun ppp-plist (form)
  "Output the pretty-printed representation of FORM suitable for plist."
  (prog1 nil
    (let ((str (with-ppp--working-buffer form
                 (when (and form (listp form))
                   (forward-char)
                   (ignore-errors
                     (while t (forward-sexp 2) (newline)))
                   (delete-char -1)))))
      (princ (concat str "\n")))))

(defun ppp--define-warning-level-symbol (sym pkg)
  "Define SYM as variable if not defined for PKG."
  (unless (boundp sym)
    (eval
     `(defcustom ,sym ppp-minimum-warning-level-base
        ,(format "Minimum level for debugging %s.
It should be either :debug, :warning, :error, or :emergency." pkg)
        :group 'ppp
        :type 'symbol))))

(defun ppp--get-caller (&optional level)
  "Get caller function and arguments from backtrace.
Optional arguments LEVEL is pop level for backtrace."
  (let ((trace-str (format "(%s)" (with-output-to-string (backtrace))))
        trace)
    (setq trace (cdr (read trace-str)))   ; drop `backtrace' symbol
    (let (tmp)
      (dotimes (_i (or level 1))
        (while (listp (setq tmp (pop trace)))))
      `(,tmp ,(car-safe trace)))))

;;;###autoload
(defmacro ppp-debug (&rest args)
  "Output debug message to `flylint-debug-buffer'.

ARGS accepts (KEYWORD-ARGUMENTS... PKG FORMAT &rest FORMAT-ARGS).
 
Auto arguments:
  PKG is symbol.
  FORMAT and FORMAT-ARGS passed `format'.

Keyword arguments:
  If LEVEL is specified, output higher than
  `ppp-minimum-warning-level--{{PKG}}' initialized `ppp-minimum-warning-level'.
  LEVEL should be one of :debug, :warning, :error, or :emergency.
  If LEVEL is omitted, assume :debug.
  If BUFFER is specified, output that buffer.
  If POPUP is non-nil, `display-buffer' debug buffer.
  If BREAK is non-nil, output page break before output string.

Note:
  If use keyword arguments, must specified these before auto arguments.

\(fn &key buffer level break PKG FORMAT &rest FORMAT-ARGS)"
  (declare (indent defun))
  (let (prop)
    (cl-loop for (key val) on args by #'cddr
             for rest on args by #'cddr
             for key* = (eval key)
             while (keywordp key*)
             do (if (memq key* '(:level :buffer :popup :break))
                    (setf (alist-get key* prop) (eval val))
                  (error "Unknown keyword: %s" key*))
             finally (progn
                       (setf (alist-get :pkg prop) (eval (pop rest)))
                       (setf (alist-get :format-raw prop) (pop rest))
                       (setf (alist-get :format-args-raw prop) rest)))
    (let* ((pkg             (alist-get :pkg prop))
           (format-raw      (alist-get :format-raw prop))
           (format-args-raw (alist-get :format-args-raw prop))
           (level           (or (alist-get :level prop) :debug))
           (buffer          (or (alist-get :buffer prop)
                                (format ppp-debug-buffer-template pkg)))
           (popup           (alist-get :popup prop))
           (break           (alist-get :break prop))
           (min-level       (intern
                             (format "ppp-minimum-warning-level--%s" pkg))))
      (ppp--define-warning-level-symbol min-level pkg)
      `(with-current-buffer (get-buffer-create ,buffer)
         (special-mode)
         (emacs-lisp-mode)
         (when (<= (warning-numeric-level ,min-level)
                   (warning-numeric-level ,level))
           (prog1 t
             (let ((inhibit-read-only t)
                   (msg (format ,format-raw ,@format-args-raw))
                   (scroll (equal (point) (point-max))))
               (seq-let (caller caller-args) (ppp--get-caller 2)
                 (save-excursion
                   (goto-char (point-max))
                   (insert
                    (concat
                     ,(and break "\n")
                     (format
                      ,(concat
                        (format (cadr (assq level warning-levels))
                                (format warning-type-format pkg))
                        "%s %s\n%s")
                      caller caller-args
                      msg)))
                   (unless (and (bolp) (eolp))
                     (newline))))
               (when scroll
                 (goto-char (point-max))
                 (set-window-point
                  (get-buffer-window (current-buffer)) (point-max)))
               ,(when popup
                  '(display-buffer (current-buffer))))))))))

(provide 'ppp)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; ppp.el ends here
