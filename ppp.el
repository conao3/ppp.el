;;; ppp.el --- Extended pretty printer for Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 1.2.4
;; Keywords: tools
;; Package-Requires: ((emacs "25.1"))
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
(require 'cl-lib)

(defgroup ppp nil
  "Extended pretty printer for Emacs Lisp."
  :prefix "ppp-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/conao3/ppp.el"))

(defcustom ppp-indent-spec
  '((0 . (_ unwind-protect))
    (1 . (_ lambda if condition-case not null car cdr 1+ 1-
            goto-char goto-line))
    (2 . (_ closure defcustom))
    (3 . (_ macro)))
  "Special indent specification.
Element at the top of the list takes precedence.

Format:
  FORMAT  := (SPEC*)
  SPEC    := (LEVEL . SYMBOLS)
  LEVEL   := <integer>
  SYMBOLS := (<symbol>*)

Duplicate LEVEL is accepted."
  :group 'ppp
  :type 'sexp)

(defcustom ppp-escape-newlines t
  "Value of `print-escape-newlines' used by ppp-* functions."
  :type 'boolean
  :group 'ppp)

(defcustom ppp-debug-buffer-template "*PPP Debug buffer - %s*"
  "Buffer name for `ppp-debug'."
  :group 'ppp
  :type 'string)

(defcustom ppp-minimum-warning-level-base :warning
  "Minimum level for `ppp-debug'.
It should be either :debug, :warning, :error, or :emergency.
Every minimul-earning-level variable initialized by this variable.
You can customize each variable like ppp-minimum-warning-level--{{pkg}}."
  :group 'ppp
  :type '(choice (const :tag ":debug"     :debug)
                 (const :tag ":warning"   :warning)
                 (const :tag ":error"     :error)
                 (const :tag ":emergency" :emergency)))


;;; Helpers

(defmacro with-ppp--working-buffer (form &rest body)
  "Insert FORM, execute BODY, return `buffer-string'."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (lisp-mode-variables nil)
     (set-syntax-table emacs-lisp-mode-syntax-table)
     (let ((print-escape-newlines ppp-escape-newlines)
           (print-quoted t))
       (prin1 ,form (current-buffer)))
     (goto-char (point-min))
     (save-excursion
       ,@body)
     (delete-trailing-whitespace)
     (while (re-search-forward "^ *)" nil t)
       (delete-region (line-end-position 0) (1- (point))))
     (buffer-substring-no-properties (point-min) (point-max))))

(defvar-local ppp-buffer-using nil
  "If non-nil, curerntly using *ppp-debug* buffer.")

(defmacro with-ppp--working-buffer-debug (form &rest body)
  "Insert FORM, execute BODY, return `buffer-string'.
Unlike `with-ppp--working-buffer', use existing buffer instead of temp buffer."
  (declare (indent 1) (debug t))
  `(let ((bufname "*ppp-debug*")
         newbuf)
     (with-current-buffer
         (if ppp-buffer-using
             (get-buffer (setq newbuf (generate-new-buffer-name bufname)))
           (get-buffer-create bufname))
       (erase-buffer)
       (unwind-protect
           (let ((ppp-buffer-using t))
             ;; see `with-ppp--working-buffer'
             (lisp-mode-variables nil)
             (set-syntax-table emacs-lisp-mode-syntax-table)
             (let ((print-escape-newlines ppp-escape-newlines)
                   (print-quoted t))
               (prin1 ,form (current-buffer))
               (goto-char (point-min)))
             (progn ,@body)
             (delete-trailing-whitespace)
             (while (re-search-forward "^ *)" nil t)
               (delete-region (line-end-position 0) (1- (point))))
             (buffer-substring-no-properties (point-min) (point-max)))
         (when newbuf
           (kill-buffer newbuf))))))


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

;;;###autoload
(defmacro ppp-alist-to-string (form)
  "Output the pretty-printed representation of FORM suitable for alist.
See `ppp-plist' to get more info."
  `(with-output-to-string
     (ppp-alist ,form)))

;;;###autoload
(defmacro ppp-symbol-function-to-string (form)
  "Output the pretty-printed representation of FORM suitable for symbol-function.
See `ppp-symbol-funciton' to get more info."
  `(with-output-to-string
     (ppp-symbol-function ,form)))

;;;###autoload
(defmacro ppp-symbol-value-to-string (form)
  "Output the pretty-printed representation of FORM suitable for symbol-value.
See `ppp-symbol-value' to get more info."
  `(with-output-to-string
     (ppp-symbol-value ,form)))



;;; Functions

(defun ppp--delete-spaces-at-point ()
  "Delete spaces near point."
  (let ((spaces " \t\n"))
    (delete-region
     (progn (skip-chars-backward spaces) (point))
     (progn (skip-chars-forward spaces) (point)))))

(defun ppp--delete-last-newline (str)
  "Delete last newline character for STR."
  (replace-regexp-in-string "\n$" "" str))

(defun ppp--space-before-p ()
  "Return non-nil if before point is spaces."
  (memq (char-before) '(?\s ?\t ?\n)))

;;;###autoload
(defun ppp-buffer ()
  "Prettify the current buffer with printed representation of a Lisp object.
ppp version of `pp-buffer'."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      ;; (message "%06d" (- (point-max) (point)))
      (let* ((sexp (sexp-at-point))
             (indent (or (car
                          (cl-find-if
                           (lambda (elm) (memq sexp (cdr elm)))
                           ppp-indent-spec))
                         (when (symbolp sexp)
                           (plist-get (symbol-plist sexp)
                                      'lisp-indent-function)))))
        (cond
         ((integerp indent)
          (forward-sexp)
          (when (not (eobp))
            (condition-case _
                (dotimes (_ indent)
                  (skip-chars-forward " \t\n")
                  (let ((child (ppp--delete-last-newline
                                (ppp-sexp-to-string
                                 (sexp-at-point)))))
                    (delete-region (point) (progn (forward-sexp) (point)))
                    (insert child)))
              (scan-error nil)))
          (insert "\n"))
         ((ignore-errors (down-list) t)
          (save-excursion
            (backward-char)
            (skip-chars-backward "'`#^")
            (when (and (not (bobp)) (ppp--space-before-p))
              (ppp--delete-spaces-at-point)
              (insert "\n"))))
         ((ignore-errors (up-list) t)
          (skip-syntax-forward ")")
          (ppp--delete-spaces-at-point)
          (insert "\n"))
         (t (goto-char (point-max)))))))
  (let ((inhibit-message t))
    (indent-region (point-min) (point-max)))

  ;; with-ppp-working-buffer post process (could use ppp-buffer only)
  (delete-trailing-whitespace)
  (while (re-search-forward "^ *)" nil t)
    (delete-region (line-end-position 0) (1- (point)))))

;;;###autoload
(defun ppp-sexp (form)
  "Output the pretty-printed representation of FORM suitable for objects."
  (prog1 nil
    (let ((str (with-ppp--working-buffer form
                 (ppp-buffer))))
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

;;;###autoload
(defun ppp-alist (form)
  "Output the pretty-printed representation of FORM suitable for alist."
  (prog1 nil
    (ppp-plist (ppp-alist-to-plist form))))

;;;###autoload
(defmacro ppp-symbol-function (form)
  "Output `symbol-function' for FORM."
  (let ((form* (if (symbolp form) form (eval form))))
    `(ppp-sexp (symbol-function ',form*))))

;;;###autoload
(defmacro ppp-symbol-value (form)
  "Output `symbol-value' for FORM."
  (let ((form* (if (symbolp form) form (eval form))))
    `(ppp-sexp (symbol-value ',form*))))

;;;###autoload
(defun ppp-alist-to-plist (alist)
  "Convert ALIST to plist."
  (mapcan
   (lambda (elm)
     (let ((keyname (prin1-to-string (car elm))))
       (list (intern
              (concat (unless (string-prefix-p ":" keyname) ":") keyname))
             (cdr elm))))
   alist))

(defmacro ppp--define-warning-level-symbol (sym pkg)
  "Define SYM as variable if not defined for PKG."
  `(defcustom ,sym ppp-minimum-warning-level-base
     ,(format "Minimum level for debugging %s.
It should be either :debug, :warning, :error, or :emergency." pkg)
     :group 'ppp
     :type 'symbol))

(defun ppp--get-caller (&optional level)
  "Get caller function and arguments from backtrace.
Optional arguments LEVEL is pop level for backtrace."
  (let ((trace-str (format "(%s)" (with-output-to-string (backtrace))))
        trace)
    (condition-case _err
        (progn
          (setq trace (cdr (read trace-str)))   ; drop `backtrace' symbol
          (let (tmp)
            (dotimes (_i (or level 1))
              (while (listp (setq tmp (pop trace)))))
            `(,tmp ,(car-safe trace))))
      (invalid-read-syntax
       '(:error invalid-read-syntax)))))

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
      `(with-current-buffer (get-buffer-create ,buffer)
         (ppp--define-warning-level-symbol ,min-level ,pkg)
         (special-mode)
         (emacs-lisp-mode)
         (when (<= (warning-numeric-level ,min-level)
                   (warning-numeric-level ,level))
           (prog1 t
             (let ((inhibit-read-only t)
                   (msg (format ,format-raw ,@format-args-raw))
                   (scroll (equal (point) (point-max))))
               (save-excursion
                 (goto-char (point-max))
                 (insert
                  (concat
                   ,(and break "\n")
                   ,(format (cadr (assq level warning-levels))
                           (format warning-type-format pkg))
                   ;; (seq-let (caller caller-args) (ppp--get-caller 2)
                   ;;   (prin1-to-string caller)
                   ;;   " "
                   ;;   (prin1-to-string caller-args))
                   "\n"
                   msg))
                 (unless (and (bolp) (eolp))
                   (newline)))
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
