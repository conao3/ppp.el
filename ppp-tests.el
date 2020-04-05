;;; ppp-tests.el --- Test definitions for ppp  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>
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

;; Test definitions for `ppp'.


;;; Code:

(require 'cort-test)
(require 'ppp)

(defmacro cort-deftest-with-equal (name form)
  "Return `cort-deftest' compare by `equal' for NAME, FORM.

Example:
  (p (cort-deftest-with-equal leaf/disabled
       '((asdf asdf-fn)
         (uiop uiop-fn))))
   => (cort-deftest leaf/disabled
        '((:equal 'asdf asdf-fn)
          (:equal 'uiop uiop-fn)))"
  (declare (indent 1))
  `(cort-deftest ,name
     ',(mapcar (lambda (elm)
                 `(:equal ,(cadr elm) ,(car elm)))
               (cadr form))))


;;; test definitions

(setq-default indent-tabs-mode nil)

(cort-deftest-with-equal ppp/ppp-sexp--simple
  '(
    ((ppp-sexp-to-string
      '(a b c)
      'nonewline)
     "\
(a b c)")

    ((ppp-sexp-to-string
      '(a
        (some-function a b)
        c)
      'nonewline)
     "\
(a
 (some-function a b)
 c)")

    ((ppp-sexp-to-string
      'closure
      'nonewline)      ; issue#38
     "\
closure")))

(cort-deftest-with-equal ppp/ppp-sexp--indent
  '(
    ((ppp-sexp-to-string
      '(a
        (some-function
         (some-function a b)
         (some-function a b))
        c)
      'nonewline)
     "\
(a
 (some-function
  (some-function a b)
  (some-function a b))
 c)")

    ((ppp-sexp-to-string
      '(a
        (when (some-function a b)
          (some-function a b))
        c)
      'nonewline)
     "\
(a
 (when (some-function a b)
   (some-function a b))
 c)")

    ((ppp-sexp-to-string
      '(while (and
               (or
                (null limit)
                (< count limit))
               (setq tem (file-symlink-p newname)))
         (setq tem (substring tem 3)))
      'nonewline)
     "\
(while (and
        (or
         (null limit)
         (< count limit))
        (setq tem (file-symlink-p newname)))
  (setq tem (substring tem 3)))")

    ((ppp-sexp-to-string
      '(if (functionp indent)
           indent
         (symbol-function indent))
      'nonewline)
     "\
(if (functionp indent)
    indent
  (symbol-function indent))")

    ((ppp-sexp-to-string
      '(save-match-data
         (if (and
              (null limit)
              (= count 100))
             (error "Apparent cycle of symbolic links for %s" filename)))
      'nonewline)
     "\
(save-match-data
  (if (and
       (null limit)
       (= count 100))
      (error \"Apparent cycle of symbolic links for %s\" filename)))")))

(cort-deftest-with-equal ppp/ppp-sexp--let
  '(
    ((ppp-sexp-to-string
      '(let ((name (copy-sequence filename))
             (start 0))
         (list name start))
      'nonewline)
     "\
(let ((name (copy-sequence filename))
      (start 0))
  (list name start))")

    ((ppp-sexp-to-string
      '(let ((name (copy-sequence filename))
             (start (when (some-function a b)
                      (some-function a b))))
         (list name start))
      'nonewline)
     "\
(let ((name (copy-sequence filename))
      (start (when (some-function a b)
               (some-function a b))))
  (list name start))")

    ((ppp-sexp-to-string
      '(let* (backupname
              targets
              (old-versions (and targets
                                 (booleanp delete-old-versions)
                                 targets))
              old-versions
              modes)
         setmodes)
      'nonewline)
     "(let* (backupname
       targets
       (old-versions (and targets
                          (booleanp delete-old-versions)
                          targets))
       old-versions
       modes)
  setmodes)")))

(cort-deftest-with-equal ppp/ppp-sexp--setq
  '(
    ((ppp-sexp-to-string
      '(setq tem (substring tem 3)
             newname (expand-file-name newname)
             newname (file-chase-links
                      (directory-file-name
                       (file-name-directory newname)))
             newname (file-name-directory newname))
      'nonewline)
     "\
(setq tem (substring tem 3)
      newname (expand-file-name newname)
      newname (file-chase-links
               (directory-file-name
                (file-name-directory newname)))
      newname (file-name-directory newname))")))

(cort-deftest-with-equal ppp/ppp-list
  '(
    ((ppp-list-to-string
      '((0 . (unwind-protect progn))
        (1 . (lambda if condition-case not null))
        (2 . (closure defcustom))
        (3 . (macro))
        (ppp--add-newline-for-let . (let let*))
        (ppp--add-newline-for-setq . (setq setf)))
      'nonewline)
     "\
((0 unwind-protect progn)
 (1 lambda if condition-case not null)
 (2 closure defcustom)
 (3 macro)
 (ppp--add-newline-for-let let let*)
 (ppp--add-newline-for-setq setq setf))")

    ((ppp-list-to-string
      '((0 . (unwind-protect progn))
        (1 . ((lambda) if condition-case not null))
        (2 . (closure defcustom))
        (3 . (macro))
        (ppp--add-newline-for-let . (let let*))
        (ppp--add-newline-for-setq . (setq setf)))
      'nonewline)
     "\
((0 unwind-protect progn)
 (1
  (lambda)
  if condition-case
  not null)
 (2 closure defcustom)
 (3 macro)
 (ppp--add-newline-for-let let let*)
 (ppp--add-newline-for-setq setq setf))")))

(cort-deftest-with-equal ppp/ppp-plist
  '(
    ((ppp-plist-to-string
      '(:last-capture "org-capture-last-stored"
                      :last-refile "org-refile-last-stored"
                      :last-capture-marker "org-capture-last-stored-marker")
      t)
     "\
(:last-capture \"org-capture-last-stored\"
 :last-refile \"org-refile-last-stored\"
 :last-capture-marker \"org-capture-last-stored-marker\")")))

(cort-deftest-with-equal ppp/ppp-leaf
  '(
    ((ppp-leaf-to-string
      (leaf leaf
        :load-path "~/.emacs.d/elpa-archive/leaf.el/"
        :require t
        :config
        (leaf-init)
        (when (some-function a b)
          (some-function a b))
        (some-function
         (some-function a b)
         (some-function a b)))
      'nonewline)
     "\
(leaf leaf
  :load-path \"~/.emacs.d/elpa-archive/leaf.el/\"
  :require t
  :config
  (leaf-init)
  (when (some-function a b)
    (some-function a b))
  (some-function
   (some-function a b)
   (some-function a b)))")

    ((ppp-leaf-to-string
      (leaf color-moccur
        :bind (("M-s O" . moccur)
               (:isearch-mode-map
                ("M-o" . isearch-moccur)
                ("M-O" . isearch-moccur-all))))
      'nonewline)
     "\
(leaf color-moccur
  :bind ((\"M-s O\" . moccur)
         (:isearch-mode-map
          (\"M-o\" . isearch-moccur)
          (\"M-O\" . isearch-moccur-all))))")))

;; (provide 'ppp-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; ppp-tests.el ends here
