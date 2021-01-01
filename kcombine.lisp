#! /usr/bin/env exlisp

(defconstant *case-preserving-readtable*
  (let ((rt (copy-readtable nil)))
    (setf (readtable-case rt) :preserve)
    rt))

(defun read-sexps (filename)
  (with-open-file (inf filename)
    (let ((*readtable* *case-preserving-readtable*))
      (setf (readtable-case *readtable*) :preserve)
      (read inf))))

(defvar *mod* (read-sexps "flat/flat.kicad_sch"))
(defvar *top* (read-sexps "empty/empty.kicad_sch"))

(defun filter-mod (sch)
  (remove-if (lambda (el) 
	       (and (listp el) 
		    (or (eq (car el) '|sheet_instances|)
			(eq (car el) '|symbol_instances|))))
	     sch))

(defconstant rstate (make-random-state t))
(defun rand-in-range (from to)
  (+ from (random (float (- to from)) rstate)))

; 123e4567-e89b-12d3-a456-426652340000
(defun uuid4 ()
  (with-output-to-string (uuid)
    (dotimes (i 16)
      (let ((val (random 256 rstate)))
	(if (or (= i 4) (= i 6) (= i 8) (= i 10))
	    (format uuid "-"))
	(if (= i 6)
	    (setq val (logior #x40 (logand val #x0f))))
	(format uuid "~2,'0x" val)))))

(defun mod-to-sheet (sch)
  (let* ((page-width (* 11 25.4))
	 (page-height (* 8.5 25.4))
	 (sheet-width 60)
	 (sheet-height 60))
    (let ((posx (rand-in-range (* 0.50 page-width) (* 0.75 page-width)))
	  (posy (rand-in-range (* 0.20 page-height) (* 0.80 page-height)))
	  (width 60)
	  (height 20)
	  (uuid (uuid4)))
      `(|sheet|
	(|at| ,posx, posy)
	(|size| ,width, height)
	(|stroke| (|width| 0.001) (|type| |solid|) (|color| 132 0 132 1))
	(|fill| (|color| 255 255 255 0.0000))
	(|uuid| ,uuid)
	(|property| "Sheet name" "MOD" 
	 (|id| 0) 
	 (|at| ,posx ,(- posy 1) 0)
	 (|effects|
	  (|font| (|size| 1.27 1.27))
	  (|justify| |left| |bottom|)))
	(|property| "Sheet file" "led.kicad_sch" 
	 (|id| 1)
	 (|at| ,posx ,(+ posy height 1) 0)
	 (|effects|
	  (|font| (|size| 1.27 1.27))
	  (|justify| |left| |top|))
	 )))))


(defvar mod-to-insert (filter-mod *mod*))

(defun write-board (sch)
  (let ((*readtable* *case-preserving-readtable*))
    (print sch)
    (format t "~%")))

(write-board (mod-to-sheet *mod*))


