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

(cort-deftest-with-equal ppp/ppp-sexp
  '(
    ((ppp-sexp-to-string '(a b c) 'nonewline)
     "\
(a b c)")

    ((ppp-sexp-to-string '(a (some-function a b) c) 'nonewline)
     "\
(a
 (some-function a b)
 c)")

    ((ppp-sexp-to-string '(a (when a (some-function a b)) c) 'nonewline)
     "\
(a
 (when a
   (some-function a b))
 c)")

    ((ppp-sexp-to-string '(lambda (a b) (message a b)) 'nonewline)
     "\
(lambda (a b)
  (message a b))")))

(cort-deftest-with-equal ppp/misc
  '(((ppp-sexp-to-string '(defcustom dummy-variable) 'nonewline)
     "\
(defcustom dummy-variable)")

    ((ppp-sexp-to-string 'closure 'nonewline)      ; issue#38
     "\
closure")))

;; (provide 'p-test)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; ppp-tests.el ends here
