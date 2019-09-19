(defpackage md-lisp-utils
  (:use :cl)
  (:export :a-list-exec)
  (:export :sleepf)
  (:export :stream-print)
  (:export :stream-read)
  (:export :warning-log)
  (:export :read-log))
(in-package :md-lisp-utils)

(defun a-list-exec (ls ex &optional key)
  "takes an alist and a function identifier and executes the function at that location. Automatically handles the case where an alist is stored as the cdr of a cons,
   and interprets the optional key to be an index of an alist in an alist. This should smooth over the use of alists as complex objects."
  (let ((real-ls ls))
    (when (last ls) (setf real-ls (cdr ls)));handles case where alist is stored as a named cons
    (when key (setf real-ls (cdr (assoc key ls))));handle case where alist is stored in an alist
  (funcall (or (cdr (assoc ex real-ls)) (lambda () (warning-log "failed a-list-exec "))))))

(defun sleepf (tim) (when (> tim 0) (sleep tim)))

(defvar *log* "")
(defun warning-log (new) (setf *log* (concatenate 'string *log* new)))
(defun read-log () (format t "~a~%" *log*))

(defun stream-read (stream)
  "Reads from a usocket connected stream"
  (read (usocket:socket-stream stream)))
(defun stream-print (string stream)
  "Prints to a usocket connected stream"
  (print string (usocket:socket-stream stream))
  (force-output (usocket:socket-stream stream)))
