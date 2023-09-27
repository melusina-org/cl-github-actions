;;;; entry-point.lisp — Entry point for GitHub Actions Support

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

(defun line (string)
  (concatenate 'string string '(#\Newline)))

(define-testcase unit-tests ()
  (let ((core::*github-output* nil)
	(core::*github-path* nil)
	(core::*github-step-summary* nil))
    (assert-string=
     (line "::debug::Set the Octocat variable")
     (with-output-to-string (*standard-output*)
       (core:set-debug "Set the ~A variable" "Octocat")))

    (assert-string=
     (line "::notice file=app.js,line=1,col=5,endColumn=7::Missing semicolon")
     (with-output-to-string (*standard-output*)
       (core:set-notice "Missing semicolon" :file "app.js" :start-line 1 :start-column 5 :end-column 7)))

    (assert-string=
     (line "::warning file=app.js,line=1,col=5,endColumn=7::Missing semicolon")
     (with-output-to-string (*standard-output*)
       (core:set-warning "Missing semicolon" :file "app.js" :start-line 1 :start-column 5 :end-column 7)))

    (assert-string=
     (line "::error file=app.js,line=1,col=5,endColumn=7::Missing semicolon")
     (with-output-to-string (*standard-output*)
       (core:set-error "Missing semicolon" :file "app.js" :start-line 1 :start-column 5 :end-column 7)))

    (assert-string=
     (line "::add-mask::Mona the Octocat")
     (with-output-to-string (*standard-output*)
       (core:set-secret "Mona the Octocat")))
  
    (assert-string=
     (line "SELECTED_COLOR=green")
     (with-output-to-string (*standard-output*)
       (core:set-output :key "SELECTED_COLOR" :value "green")))
  
    (assert-string=
     (line "/root/.local/bin")
     (with-output-to-string (*standard-output*)
       (core:add-path #p"/root/.local/bin")))

    (assert-string=
     (line "### Hello world! :rocket:")
     (with-output-to-string (*standard-output*)
       (core:with-output-to-summary (*standard-output*)
	 (format t "### Hello world! :rocket:~%"))))))

(define-testcase all-tests ()
  (unit-tests))

;;;; End of file `entry-point.lisp'
