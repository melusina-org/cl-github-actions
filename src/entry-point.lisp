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

(in-package #:org.melusina.github-actions)

(defparameter *github-output*
  (uiop:getenv "GITHUB_OUTPUT")
  "The file to write step output parmameters to.")

(defparameter *github-path*
  (uiop:getenv "GITHUB_PATH")
  "The file to write path addtions to.")
  
(defparameter *github-step-summary*
  (uiop:getenv "GITHUB_STEP_SUMMARY")
  "The file to write step summary to.")
  
(defmacro with-open-file-or-standard-output ((var filespec) &body body)
  (alexandria:once-only (filespec)
    `(flet ((write-output (,var)
	      ,@body))
       (if ,filespec
	   (with-open-file (output ,filespec
				   :direction :output
				   :if-exists :append
				   :if-does-not-exist :create)
	     (write-output output))
	   (write-output *standard-output*)))))

(defun is-debug-p ()
  (when (uiop:getenv "RUNNER_DEBUG")
    (string= (uiop:getenv "RUNNER_DEBUG") "1")))

(defun prompt-value (control-string &rest format-arguments)
  "Print message with format and read value.
This function is to be used as interactive slot for restarts."
  (apply #'format *trace-output* control-string format-arguments)
  (list (read-line)))

(defun set-secret (value)
  "Masking a value prevents a string or variable from being printed in the log."
  (format t "~&::add-mask::~A~%" value))

(defun set-output (&key key value)
  "Set output KEY to VALUE.
Note that composiste actions need to declare their outputs, as mentionned
in

  https://docs.github.com/en/actions/creating-actions/metadata-syntax-for-github-actions#outputs

This behaviour is different from script steps, which can set
arbitrary output values."
  (unless value
    (restart-case (error "The value for ~A is not defined." key)
      (use-value (new-value)
	:report "Specify a value to  use instead of the missing value definition."
	:interactive (lambda () (prompt-value "~&Specify a value to use instead of the missing value definition.~%"))
	(setf value new-value))))
  (with-open-file-or-standard-output (stream *github-output*)
    (format stream "~&~A=~A~%" key value)))

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
    (with-open-file-or-standard-output (stream *github-path*)
      (format stream "~&~A~%" pathname))))

(defun set-debug (control-string &rest format-arguments)
  "Set a debug message."
  (format t "~&::debug::")
  (apply #'format t control-string format-arguments)
  (format t "~%"))

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
  (format t "::~A~%" message))

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
  (format t "::~A~%" message))

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
  (format t "::~A~%" message))

(defmacro with-output-to-summary ((var) &body body)
  `(with-open-file-or-standard-output (,var *github-step-summary*)
     ,@body))

(defun dump-file (header filespec)
  (flet ((dump-lines ()
	   (format t "~&---- BEGIN DUMP OF FILE ~A ----~%" header)
	   (with-open-file (stream filespec :direction :input)
	     (loop :for line = (read-line stream nil nil)
		   :while line
		   :do (format t "~A~%" line)))
	   (format t "~&---- END DUMP OF FILE ~A ----~%" header))
	 (dump-hexadecimal ()
	   (with-open-file (stream filespec
				   :direction :input
				   :element-type '(unsigned-byte 8))
	     (loop :for char = (read-byte stream nil nil)
		   :for count = 0 :then (1+ count)
		   :when (= count 16)
		   :do (progn (write-char #\Newline)
			      (setf count 0))
		   :while char
		   :do (format t "  ~2,'0X" char)))))
    (dump-lines)
    (dump-hexadecimal)))

(defun auscultate ()
  (flet ((show-environment (env)
	   (format t "~&~A: ~A~&" env (uiop:getenv env))))
    (dolist (env '("GITHUB_OUTPUT" "GITHUB_PATH" "GITHUB_STEP_SUMMARY"))
      (show-environment env)))
  (when *github-output*
    (dump-file "GITHUB_OUTPUT" *github-output*))
  (when *github-path*
    (dump-file "GITHUB_PATH" *github-path*))
  (when *github-step-summary*
    (dump-file "GITHUB_STEP_SUMMARY" *github-step-summary*)) )

;;;; End of file `entry-point.lisp'
