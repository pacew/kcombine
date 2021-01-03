#! /usr/bin/env exlisp

(defconstant *rt*
  (let ((rt (copy-readtable nil)))
    (setf (readtable-case rt) :preserve)
    rt))

(defun read-sexps-from-stream (stream)
  (let ((*readtable* *rt*))
    (read stream)))

(defun read-sexps (filename)
  (with-open-file (inf filename)
    (read-sexps-from-stream inf)))

(defun sch-to-string (sch)
  (with-output-to-string (out)
    (let ((*readtable* *rt*))
      (prin1 sch out))))

;(let ((val (read-sexps "byhand/led.kicad_sch")))
;  (let ((*readtable* *rt*))
;    (format t "~s~%" val)))

(defun get-stream ()
  (let ((filename (cadr sb-ext:*posix-argv*)))
    (if (null filename)
	*standard-input*
	(open filename))))

(defun soak (stream)
  (let (c)
    (loop
       (setq c (read-char stream))
       (print c))))

(format t "~s~%" (read-sexps-from-stream (get-stream)))

  




