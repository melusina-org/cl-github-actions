;;;; package.lisp — Package for GitHub Actions Support

;;;; GitHub Actions Support (https://github.com/melusina-org/cl-github-actions)
;;;; This file is part of GitHub Actions Support.
;;;;
;;;; Copyright © 2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(defpackage #:org.melusina.github-actions
  (:use #:common-lisp)
  (:export
   #:is-debug-p
   #:add-path
   #:set-secret
   #:set-output
   #:set-debug
   #:set-notice
   #:set-warning
   #:set-error
   #:with-output-to-summary
   #:auscultate
  ))

(in-package #:org.melusina.github-actions)

;;;; End of file `package.lisp'
