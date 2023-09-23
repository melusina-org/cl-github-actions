;;;; org.melusina.github-actions.asd — System definition for GitHub Actions Support

;;;; GitHub Actions Support (https://github.com/melusina-org/cl-github-actions)
;;;; This file is part of GitHub Actions Support.
;;;;
;;;; Copyright © 2023 Michaël Le Barbier
;;;; All rights reserved.

;;;; This file must be used under the terms of the MIT License.
;;;; This source file is licensed as described in the file LICENSE, which
;;;; you should have received as part of this distribution. The terms
;;;; are also available at https://opensource.org/licenses/MIT

(asdf:defsystem #:org.melusina.github-actions
  :description "System for the development of GitHub Actions with Common Lisp"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:alexandria)
  :components
  ((:module "src"
    :components ((:file "package")
                 (:file "utilities")
		 (:file "entrypoint")))))

(asdf:defsystem #:org.melusina.github-actions/testsuite
  :description "System for the development of GitHub Actions with Common Lisp"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:alexandria
	       #:org.melusina.confidence
	       #:org.melusina.github-actions)
  :components
  ((:module "testsuite"
    :components ((:file "package")
		 (:file "utilities")
		 (:file "entrypoint")))))

(asdf:defsystem #:org.melusina.github-actions/development
  :description "Development tools for GitHub Actions Support"
  :author "Michaël Le Barbier"
  :license "MIT License"
  :depends-on (#:alexandria
	       #:org.melusina.atelier)
  :components
  ((:module "libexec/lisp"
    :components ((:file "development")))))

;;;; End of file `org.melusina.github-actions.asd'
