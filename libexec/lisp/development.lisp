;;;; development.lisp — Development System for GitHub Actions Support

;;;; GitHub Actions Support (https://github.com/melusina-org/cl-github-actions)
;;;; This file is part of GitHub Actions Support.
;;;;
;;;; Copyright © 2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.github-actions/development
  (:use #:common-lisp)
  (:export
   #:lint
   #+quicklisp
   #:reload))

(in-package #:org.melusina.github-actions/development)

(defun system-relative-pathname (&rest pathnames)
  (labels ((system-source-directory ()
	     (asdf:system-source-directory #.(string-downcase (package-name *package*))))
	   (merge-one (pathname)
	     (merge-pathnames pathname (system-source-directory))))
    (mapcar #'merge-one pathnames)))

(defparameter *parameter-bindings*
  '((:copyright-holder . "Michaël Le Barbier")
    (:copyright-year . "2023")
    (:project-filename . "org.melusina.github-actions")
    (:project-name . "GitHub Actions Support")
    (:project-description . "System for the development of GitHub Actions with Common Lisp")
    (:project-long-description . "This system provides tools that support the development of GitHub Actions with Common Lisp.")
    (:homepage . "https://github.com/melusina-org/cl-github-actions")
    (:license . :MIT)))

(defun lint ()
  (let ((atelier:*parameter-bindings* *parameter-bindings*))
    (atelier:lint
     (system-relative-pathname
      #p"org.melusina.github-actions.asd"
      #p"development"
      #p"doc"
      #p"src"
      #p"testsuite"
      #p"libexec/lisp/development.lisp"))))

#+quicklisp
(defun reload ()
  (ql:quickload '("org.melusina.github-actions"
		  "org.melusina.github-actions/testsuite"
		  "org.melusina.github-actions/development")))


;;;;
;;;; Command Stock
;;;;

#+nil
(org.melusina.github-actions/development:reload)

;;;; End of file `development.lisp'
