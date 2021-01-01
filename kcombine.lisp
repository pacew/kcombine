#! /usr/bin/env exlisp

(defun read_sexps (filename)
  (with-open-file (inf filename)
    (let ((*readtable* (copy-readtable nil)))
      (setf (readtable-case *readtable*) :preserve)
      (read inf))))

(defvar *mod* (read_sexps "flat/flat.kicad_sch"))
(defvar *top* (read_sexps "empty/empty.kicad_sch"))

(print *mod*)






