;;;; entrypoint.lisp — Entrypoint for GitHub Actions Support

;;;; GitHub Actions Support (https://github.com/melusina-org/cl-github-actions)
;;;; This file is part of GitHub Actions Support.
;;;;
;;;; Copyright © 2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(in-package #:org.melusina.github-actions/testsuite)

(define-testcase unit-tests ()
  (assert-t t))

(define-testcase all-tests ()
  (unit-tests))

;;;; End of file `entrypoint.lisp'
