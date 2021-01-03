#! /usr/bin/env exlisp

(defconstant *rt*
  (let ((rt (copy-readtable nil)))
    (setf (readtable-case rt) :preserve)
    rt))

(defun read-sexps (filename)
  (with-open-file (inf filename)
    (let ((*readtable* *rt*))
      (read inf))))

(defun sch-to-string (sch)
  (with-output-to-string (out)
    (let ((*readtable* *rt*))
      (prin1 sch out))))

(let ((val (read-sexps "top.sch")))
  (let ((*readtable* *rt*))
    (format t "~s~%" val)))





