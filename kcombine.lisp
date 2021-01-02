#! /usr/bin/env exlisp

(require :trivial-escapes)
(defvar *rt* 
  (let ((rt (named-readtables:find-readtable 'trivial-escapes:dq-readtable)))
    (setf (readtable-case rt) :preserve)
    rt))

(defconstant *case-preserving-readtable*
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

;"(kicad_sch (version 20200310) (page \"A\\\\n4\") (lib_symbols) (symbol_instances))")))

(print *rt*)
(print *readtable*)

(defvar *empty* 
  (let ((*readtable* *rt*))
    (print *readtable*)
    (read-from-string "\"foo\\nbar\"")))
(print *empty*)
(format t "~%")

(with-open-file (out "foo" :direction :output :if-exists :supersede)
  (write-string (sch-to-string *empty*) out)
  (write-char #\Newline out))

(defvar *mod* (read-sexps "flat/flat.kicad_sch"))
(defvar *top* (read-sexps "empty/empty.kicad_sch"))
;(defvar *top* (read-sexps "flat/flat.kicad_sch"))

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

(defun mod-to-sheet (sch uuid)
  (let* ((page-width (* 11 25.4))
	 (page-height (* 8.5 25.4))
	 (sheet-width 60)
	 (sheet-height 60))
    (let ((posx (rand-in-range (* 0.50 page-width) (* 0.75 page-width)))
	  (posy (rand-in-range (* 0.20 page-height) (* 0.80 page-height)))
	  (width 60)
	  (height 20))
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

(defun get-sheet-instances (sch)
  (let ((val (assoc '|sheet_instances| (cdr sch))))
    (cond ((null val)
	   (setq val (list '|sheet_instances|))
	   (setf (cdr (last sch)) (list val))))
    val))

(defun get-symbol-instances (sch)
  (let ((val (assoc '|symbol_instances| (cdr sch))))
    (cond ((null val)
	   (setq val (list (list '|symbol_instances|)))
	   (setf (cdr (last sch)) val)))
    val))

(defun path-page (path)
  (let ((val (assoc '|page| (cddr path))))
    (if val
	(parse-integer (cadr val))
	0)))

(defun max-page (sheet-instances)
  (loop for p in (cdr sheet-instances)
     maximize (path-page p)))

(defun paths (sheet-instances)
  (loop for p in (cdr sheet-instances)
       collect (cadr p)))

(defun add-path (sheet-instances path pagenum)
  (setf (cdr (last sheet-instances))
	`((|path| ,path (|page| ,(format nil "~d" pagenum))))))

(defun scan (l)
  (cond ((null l) nil)
	((eq (caar l) '|lib_symbols|)
	 l)
	(t
	 (scan (cdr l)))))

(defun find-pos-for-sheets (sch)
  (let ((pos (scan (cdr sch))))
    (or pos (last sch))))


(defun add-sheets ()
  (let* ((sheet-instances (get-sheet-instances *top*))
	 (paths (paths sheet-instances)))
    (if (not (member "/" paths :test #'equal))
	(add-path sheet-instances "/" 1))
    (let ((max-page (max-page sheet-instances)))
      (let ((sheet-page (+ max-page 1))
	    (uuid (uuid4))
	    )
	(let ((sheet (mod-to-sheet *mod* uuid)))
	  (add-path sheet-instances (format nil "/~a/" uuid) sheet-page)

	  (let ((pos (find-pos-for-sheets *top*))
		(slist (list sheet)))
	    (setf (cdr slist) (cdr pos))
	    (setf (cdr pos) slist)
	    ))))))

(add-sheets)



(defvar mod-to-insert (filter-mod *mod*))

(defun write-output ()
  (with-open-file (outf "combined/combined.kicad_sch" 
			:direction :output
			:if-exists :supersede)
    (write-string (sch-to-string *top*) outf)
    (write-char #\linefeed outf)))

(write-output)



