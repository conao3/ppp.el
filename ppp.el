;;; ppp.el --- Extended pretty printer for Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
;; Version: 2.0.8
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
  '((0 . (unwind-protect))
    (1 . (lambda if condition-case not null car cdr 1+ 1-
           goto-char goto-line))
    (2 . (closure defcustom))
    (3 . (macro))
    ((lambda () (ppp--add-newline-per-sexp 2)) . (setq))
    ((lambda () (down-list) (ppp--add-newline-per-sexp 1)) . (let let*)))
  "Special indent specification.
Element at the top of the list takes precedence.

Format:
  FORMAT  := (SPEC*)
  SPEC    := (LEVEL . SYMBOLS)
  LEVEL   := <integer> | <lambda>
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

(defcustom ppp-minimum-warning-level-alist '((t . :warning))
  "Minimum level for `ppp-debug'.
The key is package symbol.
The value should be either :debug, :warning, :error, or :emergency.
The value its key is t, is default minimum-warning-level value."
  :group 'ppp
  :type 'sexp)


;;; Helpers

(defvar ppp-debug nil
  "If non-nil, show debug overlay.")

(defvar-local ppp-buffer-using nil
  "If non-nil, curerntly using *ppp-debug* buffer.")

(defvar-local ppp-debug-ovs (make-list 5 nil)
  "Debug overlay.")

(defvar ppp-debug-palette '("SeaGreen3" "khaki3" "brown3" "aquamarine3" "plum3")
  "Debug overlay palette.")

(defun ppp-debug-ov-make ()
  "Make debug overlay at point."
  (when ppp-debug
    (ppp-debug-ov-remove)
    (dotimes (i 5)
      (let ((ov (make-overlay (point) (1+ (point)))))
        (setf (nth i ppp-debug-ovs) ov)
        (move-overlay ov (point) (1+ (point)))
        (overlay-put ov 'ppp-debug-overlay t)
        (overlay-put ov 'priority (- 10 i))))))

(defun ppp-debug-ov-move (&optional inx)
  "Move INXth debug overlay at PTR."
  (when ppp-debug
    (let* ((inx* (or inx 0))
           (ov (nth inx* ppp-debug-ovs)))
      (overlay-put ov 'face `(t :foreground "black"
                                :background ,(nth inx* ppp-debug-palette)))
      (move-overlay ov (point) (1+ (point))))))

(defun ppp-debug-ov-remove ()
  "Remove ppp-debug-overlay in buffer."
  (when ppp-debug
    (dolist (ov (cl-remove-if-not
                 (lambda (ov)
                   (overlay-get ov 'ppp-debug-overlay))
                 (overlays-in (point-min) (point-max))))
      (delete-overlay ov))))

(defun ppp--add-newline-this-sexp ()
  "Add new line this pointed sexp."
  (save-restriction
    (save-excursion
      (let ((beg (point))
            (end (progn (forward-sexp) (point))))
        (narrow-to-region beg end)
        (ppp-buffer 'nonewline 'noindent)))))

(defun ppp--add-newline-after-sexp (nsexp)
  "Add new line after NSEXP.
Return t if scan succeeded and return nil if scan failed."
  (ignore-errors
    (dotimes (_ nsexp)
      (forward-sexp) (ppp-debug-ov-move 4)
      (skip-chars-forward " \t\n")
      (ppp--add-newline-this-sexp))
    (ignore-errors
      (forward-sexp)
      (backward-sexp)
      (skip-chars-backward " \t\n")
      (insert "\n"))
    t))

(defun ppp--add-newline-per-sexp (nsexp)
  "Add new line per NSEXP."
  (while (ppp--add-newline-after-sexp nsexp)))

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

;;;###autoload
(defun ppp-buffer (&optional notailnewline noindent)
  "Prettify the current buffer with printed representation of a Lisp object.
IF NOTAILNEWLINE is non-nil, add no last newline.
If NOINDENT is non-nil, don't perform indent sexp.
ppp version of `pp-buffer'."
  (interactive)
  (goto-char (point-min))
  (ppp-debug-ov-make)
  (while (not (eobp))
    (let* ((op (sexp-at-point))
           (indent (or (car
                        (cl-find-if
                         (lambda (elm) (memq op (cdr elm)))
                         ppp-indent-spec))
                       (when (symbolp op)
                         (plist-get (symbol-plist op) 'lisp-indent-function)))))
      (cond
       ((functionp indent)
        (forward-sexp)
        (funcall indent)
        (ignore-errors (forward-sexp) (backward-sexp) (insert "\n")))
       ((integerp indent)
        (dotimes (_ (1+ indent))
          (ignore-errors
            (forward-sexp) (ppp-debug-ov-move)))
        (if (not (eobp))
            (ignore-errors (forward-sexp) (backward-sexp) (insert "\n"))
          (unless notailnewline (insert "\n") (ppp-debug-ov-move))))
       ((ignore-errors (down-list 1) (ppp-debug-ov-move) t)
        (save-excursion
          (backward-char 1) (ppp-debug-ov-move 1)
          (skip-chars-backward "'`#^") (ppp-debug-ov-move 1)
          (when (and (not (bobp)) (memq (char-before) '(?\s ?\t ?\n)))
            (delete-region
             (point)
             (progn (skip-chars-backward " \t\n") (point)))
            (insert "\n") (ppp-debug-ov-move 1))))
       ((ignore-errors (up-list 1) (ppp-debug-ov-move) t)
        (skip-syntax-forward ")") (ppp-debug-ov-move)
        (delete-region
         (point)
         (progn (skip-chars-forward " \t\n") (point)))
        (unless notailnewline (insert "\n") (ppp-debug-ov-move)))
       (t (goto-char (point-max)) (ppp-debug-ov-move)))))
  (unless noindent
    (goto-char (point-min)) (ppp-debug-ov-move)
    (indent-sexp)))

(defun ppp-pp-buffer ()
  "Prettify the current buffer with printed representation of a Lisp object.
`pp-buffer' with debug marker."
  (ppp-debug-ov-make)
  (goto-char (point-min)) (ppp-debug-ov-move)
  (while (not (eobp))
    ;; (message "%06d" (- (point-max) (point)))
    (cond
     ((ignore-errors (down-list 1) (ppp-debug-ov-move) t)
      (save-excursion
        (backward-char 1) (ppp-debug-ov-move 1)
        (skip-chars-backward "'`#^") (ppp-debug-ov-move 1)
        (when (and (not (bobp)) (memq (char-before) '(?\s ?\t ?\n)))
          (delete-region
           (point)
           (progn (skip-chars-backward " \t\n") (point)))
          (insert "\n") (ppp-debug-ov-move 1))))
     ((ignore-errors (up-list 1) (ppp-debug-ov-move) t)
      (skip-syntax-forward ")") (ppp-debug-ov-move)
      (delete-region
       (point)
       (progn (skip-chars-forward " \t\n") (point)))
      (insert ?\n) (ppp-debug-ov-move))
     (t (goto-char (point-max)) (ppp-debug-ov-move))))
  (goto-char (point-min)) (ppp-debug-ov-move)
  (indent-sexp))


;;; String functions

;;;###autoload
(defun ppp-sexp-to-string (form &optional notailnewline)
  "Output the pretty-printed representation of FORM suitable for objects.
If NOTAILNEWLINE is non-nil, add no newline at tail newline.
See `ppp-sexp' to get more info."
  (with-ppp--working-buffer form
    (ppp-buffer notailnewline)))

;;;###autoload
(defmacro ppp-macroexpand-to-string (form &optional notailnewline)
  "Output the pretty-printed representation of FORM suitable for macro.
If NOTAILNEWLINE is non-nil, add no newline at tail newline.
See `ppp-macroexpand' to get more info."
  `(ppp-sexp-to-string (macroexpand-1 ',form) ,notailnewline))

;;;###autoload
(defmacro ppp-macroexpand-all-to-string (form &optional notailnewline)
  "Output the pretty-printed representation of FORM suitable for macro.
Unlike `ppp-macroexpand', use `macroexpand-all' instead of `macroexpand-1'.
If NOTAILNEWLINE is non-nil, add no newline at tail newline.
See `ppp-macroexpand-all' to get more info."
  `(ppp-sexp-to-string (macroexpand-all ',form) ,notailnewline))

;;;###autoload
(defun ppp-list-to-string (form &optional _notailnewline)
  "Output the pretty-printed representation of FORM suitable for list.
If NOTAILNEWLINE is non-nil, add no newline at tail newline.
See `ppp-list' to get more info."
  (with-ppp--working-buffer form
    (when (and form (listp form))
      (forward-char)
      (ignore-errors
        (while t (forward-sexp) (newline)))
      (delete-char -1))))

;;;###autoload
(defun ppp-plist-to-string (form &optional _notailnewline)
  "Output the pretty-printed representation of FORM suitable for plist.
If NOTAILNEWLINE is non-nil, add no newline at tail newline.
See `ppp-plist' to get more info."
  (with-ppp--working-buffer form
    (when (and form (listp form))
      (forward-char)
      (ignore-errors
        (while t (forward-sexp 2) (newline)))
      (delete-char -1))))

;;;###autoload
(defun ppp-alist-to-string (form &optional _notailnewline)
  "Output the pretty-printed representation of FORM suitable for alist.
If NOTAILNEWLINE is non-nil, add no newline at tail newline.
See `ppp-plist' to get more info."
  (ppp-plist-to-string (ppp-alist-to-plist form)))

;;;###autoload
(defun ppp-symbol-function-to-string (symbol &optional notailnewline)
  "Output the pretty-printed representation of SYMBOL `symbol-function'.
If NOTAILNEWLINE is non-nil, add no newline at tail newline.
See `ppp-symbol-funciton' to get more info."
  (ppp-sexp-to-string (symbol-function symbol) notailnewline))

;;;###autoload
(defun ppp-symbol-value-to-string (symbol &optional notailnewline)
  "Output the pretty-printed representation of SYMBOL `symbol-value'.
If NOTAILNEWLINE is non-nil, add no newline at tail newline.
See `ppp-symbol-value' to get more info."
  (ppp-sexp-to-string (symbol-value symbol) notailnewline))


;;; Princ functions

;;;###autoload
(defun ppp-sexp (form &optional notailnewline)
  "Output the pretty-printed representation of FORM suitable for objects.
If NOTAILNEWLINE is non-nil, add no newline at tail newline."
  (prog1 nil
    (princ (ppp-sexp-to-string form notailnewline))))

;;;###autoload
(defmacro ppp-macroexpand (form &optional notailnewline)
  "Output the pretty-printed representation of FORM suitable for macro.
If NOTAILNEWLINE is non-nil, add no newline at tail newline."
  `(prog1 nil
     (princ (ppp-macroexpand-to-string ,form ,notailnewline))))

;;;###autoload
(defmacro ppp-macroexpand-all (form &optional notailnewline)
  "Output the pretty-printed representation of FORM suitable for macro.
If NOTAILNEWLINE is non-nil, add no newline at tail newline.
Unlike `ppp-macroexpand', use `macroexpand-all' instead of `macroexpand-1'."
  `(prog1 nil
     (princ (ppp-macroexpand-all-to-string ,form ,notailnewline))))

;;;###autoload
(defun ppp-list (form &optional _notailnewline)
  "Output the pretty-printed representation of FORM suitable for list.
If NOTAILNEWLINE is non-nil, add no newline at tail newline."
  (prog1 nil
    (princ (concat (ppp-list-to-string form) "\n"))))

;;;###autoload
(defun ppp-plist (form &optional _notailnewline)
  "Output the pretty-printed representation of FORM suitable for plist.
If NOTAILNEWLINE is non-nil, add no newline at tail newline."
  (prog1 nil
    (princ (concat (ppp-plist-to-string form) "\n"))))

;;;###autoload
(defun ppp-alist (form &optional _notailnewline)
  "Output the pretty-printed representation of FORM suitable for alist.
If NOTAILNEWLINE is non-nil, add no newline at tail newline."
  (prog1 nil
    (princ (concat (ppp-alist-to-string form)))))

;;;###autoload
(defun ppp-symbol-function (symbol &optional notailnewline)
  "Output `symbol-function' for SYMBOL.
If NOTAILNEWLINE is non-nil, add no newline at tail newline."
  (prog1 nil
    (princ (ppp-sexp-to-string symbol notailnewline))))

;;;###autoload
(defun ppp-symbol-value (symbol &optional notailnewline)
  "Output `symbol-value' for SYMBOL.
If NOTAILNEWLINE is non-nil, add no newline at tail newline."
  (prog1 nil
    (princ (ppp-symbol-value-to-string symbol notailnewline))))

;;;###autoload
(defun ppp-alist-to-plist (alist &optional _notailnewline)
  "Convert ALIST to plist.
If NOTAILNEWLINE is non-nil, add no newline at tail newline."
  (mapcan
   (lambda (elm)
     (let ((keyname (prin1-to-string (car elm))))
       (list (intern
              (concat (unless (string-prefix-p ":" keyname) ":") keyname))
             (cdr elm))))
   alist))

(defun ppp--get-caller (&optional level)
  "Get caller function and arguments from backtrace.
If NOTAILNEWLINE is non-nil, add no newline at tail newline.
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
           (break           (alist-get :break prop)))
      `(with-current-buffer (get-buffer-create ,buffer)
         (special-mode)
         (emacs-lisp-mode)
         (when (<= (warning-numeric-level
                    ,(alist-get pkg ppp-minimum-warning-level-alist
                                (alist-get t ppp-minimum-warning-level-alist)))
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
