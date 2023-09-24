;;;; package.lisp — Package for GitHub Actions Support tests

;;;; GitHub Actions Support (https://github.com/melusina-org/cl-github-actions)
;;;; This file is part of GitHub Actions Support.
;;;;
;;;; Copyright © 2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.github-actions/testsuite
  (:use #:common-lisp)
  (:local-nicknames
   (#:core #:org.melusina.github-actions))
  (:import-from #:org.melusina.confidence
   #:define-testcase
   #:define-assertion
   #:assert-t
   #:assert-eq
   #:assert-set-equal
   #:assert-string=
   #:assert-type))

(in-package #:org.melusina.github-actions/testsuite)

;;;; End of file `package.lisp'
