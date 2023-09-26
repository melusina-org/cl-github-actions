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

(in-package #:org.melusina.github-actions)

(defun prompt-value (control-string &rest format-arguments)
  "Print message with format and read value.
This function is to be used as interactive slot for restarts."
  (apply #'format *trace-output* control-string format-arguments)
  (list (read-line)))

(defun set-secret (value)
  "Masking a value prevents a string or variable from being printed in the log."
  (format t "~&::add-mask::~A~&" value))

(defun set-output (&key key value)
  "Set output KEY to VALUE."
  (unless value
    (restart-case (error "The value for ~A is not defined." key)
      (use-value (new-value)
	:report "Specify a value to  use instead of the missing value definition."
	:interactive (lambda () (prompt-value "~&Specify a value to  use instead of the missing value definition.~%"))
	(setf value new-value))))
  (flet ((write-output (stream)
	   (format stream "~&~A=~A~&" key value)))
    (if (uiop:getenv "GITHUB_OUTPUT")
	(with-open-file (output (uiop:getenv "GITHUB_OUTPUT")
				:direction :output
				:if-exists :append
				:if-does-not-exist :create)
	  (write-output output))
	(write-output *standard-output*))))

(defun add-path (pathname)
  "Prepends PATHNAME to the system PATH variable.
This automatically makes it available to all subsequent actions in the
current job; the currently running action cannot access the updated path
variable."
  (let ((pathname
	  (etypecase pathname
	    (string
	     pathname)
	    (pathname
	     (namestring pathname)))))
    (flet ((write-path (stream)
	     (format stream "~&~A~&" pathname)))
      (if (uiop:getenv "GITHUB_PATH")
	  (with-open-file (output (uiop:getenv "GITHUB_PATH")
				  :direction :output
				  :if-exists :append
				  :if-does-not-exist :create)
	    (write-path output))
	  (write-path *standard-output*)))))

(defun set-debug (control-string &rest format-arguments)
  "Set a debug message."
  (format t "~&::debug::")
  (apply #'format t control-string format-arguments)
  (format t "~&"))

(defun set-notice (message &key title file start-column end-column start-line end-line)
  "Set a notice message."
  (format t "~&::notice")
  (loop :with separator = " "
	:for (argument value)
	:in (list
	     (list "title" title)
	     (list "file" file)
	     (list "line" start-line)
	     (list "endLine" end-line)
	     (list "col" start-column)
	     (list "endColumn" end-column))
	:do
	(when value
	  (format t "~A~A=~A" separator argument value)
	  (setf separator ",")))
  (format t "::~A~&" message))

(defun set-warning (message &key title file start-column end-column start-line end-line)
  "Set a warning message."
  (format t  "~&::warning")
  (loop :with separator = " "
	:for (argument value)
	:in (list
	     (list "title" title)
	     (list "file" file)
	     (list "line" start-line)
	     (list "endLine" end-line)
	     (list "col" start-column)
	     (list "endColumn" end-column))
	:do
	(when value
	  (format t "~A~A=~A" separator argument value)
	  (setf separator ",")))
  (format t "::~A~&" message))

(defun set-error (message &key title file start-column end-column start-line end-line)
  "Set a error message."
  (format t "~&::error")
  (loop :with separator = " "
	:for (argument value)
	:in (list
	     (list "title" title)
	     (list "file" file)
	     (list "line" start-line)
	     (list "endLine" end-line)
	     (list "col" start-column)
	     (list "endColumn" end-column))
	:do
	(when value
	  (format t "~A~A=~A" separator argument value)
	  (setf separator ",")))
  (format t "::~A~&" message))

(defmacro with-output-to-summary ((var) &body body)
  `(if (uiop:getenv "GITHUB_PATH")
       (with-open-file (,var (uiop:getenv "GITHUB_PATH")
			     :direction :output
			     :if-exists :append
			     :if-does-not-exist :create)
	 ,@body)
       (let ((,var *standard-output*))
	 ,@body)))

;;;; End of file `entrypoint.lisp'
