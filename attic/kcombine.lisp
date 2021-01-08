;#! /usr/bin/env exlisp

(require :trivial-escapes)
(defvar *rt* 
  (let ((rt (named-readtables:find-readtable 'trivial-escapes:dq-readtable)))
    (setf (readtable-case rt) :preserve)
    rt))

(defun read-sexps (filename)
  (with-open-file (inf filename)
    (let ((*readtable* *rt*))
      (read inf))))

(defclass cstring ()
  ((val :initarg :val)))

(defmethod print-object ((obj cstring) stream)
  (write-char #\" stream)
  (loop for c across (slot-value obj 'val)
	do (cond ((eql c #\Newline)
		  (write-string "\\n" stream))
		 ((member c '(#\" #\\) :test 'eql)
		  (write-char #\\ stream)
		  (write-char c stream))
		 (t
		  (write-char c stream))))
  (write-char #\" stream))

(defun cstring-replace (tree)
  (cond ((null tree) nil)
	((stringp tree) (make-instance 'cstring :val tree))
	((atom tree) tree)
	(t (cons (cstring-replace (car tree))
		 (cstring-replace (cdr tree))))))

(defun sch-to-string (sch)
  (with-output-to-string (out)
    (let ((*readtable* *rt*))
      (prin1 sch (cstring-replace out)))))

(defun write-sch (sch filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (write-string (sch-to-string sch) out)
    (write-char #\Newline out)))


(defun filter-subsheet (sch)
  (remove-if (lambda (el) 
	       (and (listp el) 
		    (or (eq (car el) '|sheet_instances|)
			(eq (car el) '|symbol_instances|))))
	     sch))

(defvar *rstate* (make-random-state t))
(defun rand-in-range (from to)
  (+ from (random (float (- to from)) *rstate*)))

; 123e4567-e89b-12d3-a456-426652340000
(defun uuid4 ()
  (with-output-to-string (uuid)
    (dotimes (i 16)
      (let ((val (random 256 *rstate*)))
	(if (or (= i 4) (= i 6) (= i 8) (= i 10))
	    (format uuid "-"))
	(if (= i 6)
	    (setq val (logior #x40 (logand val #x0f))))
	(write-string (string-downcase (format nil "~2,'0x" val)) uuid)))))

(defun find-in-list (items key)
  (cond ((null items) nil)
	((and (listp (car items))
	      (eq (car items) key))
	 (cdr items))
	(t
	 (find-in-list (cdr items) key))))

(defun make-empty ()
  (copy-tree
   '(|kicad_sch|
     (|version| 20200310)
     (|page| "A4"))))

(defstruct sheet
  (filename)
  (sch)
  )

(defun get-sheet (filename)
  (let ((sheet (make-sheet)))
    (setf (sheet-filename sheet) filename)
    (setf (sheet-sch sheet) (read-sexps filename))
    sheet))

(defun add-to-end (list val)
  (setf (cdr (last list)) (list val)))

(defun xassoc (item xalist)
  (cond ((null xalist) nil)
	((and (listp (car xalist))
	      (eq (caar xalist) item))
	 (car xalist))
	(t
	 (xassoc item (cdr xalist)))))

(defun max-property-id (sch-item)
  (let ((maxval -1))
    (loop for item in (cdr sch-item)
	  when (eq (car item) '|property|)
	    do (let ((id (cadr (xassoc '|id| item))))
		 (if (and id (> id maxval))
		     (setq maxval id))))
    maxval))

(defun add-sch-item-property (sch-item pname)
  (let* ((max-id (max-property-id sch-item))
	 (item (list '|property|
		     pname 
		     "" 
		     (list '|id| (+ max-id 1)))))
    (add-to-end sch-item item)
    item))

(defun get-sch-item-property (sch-item pname)
  (labels ((walk (l)
	     (cond ((null l) nil)
		   ((and (listp l)
			 (>= (length (car l)) 2)
			 (eq (caar l) '|property|)
			 (string-equal (cadar l) pname))
		    (car l))
		   (t (walk (cdr l))))))
    (walk (cdr sch-item))))

(defun get-property (sch-item pname)
  (caddr (get-sch-item-property sch-item pname)))

(defun set-property (sch-item pname pval)
  (let ((prop (get-sch-item-property sch-item pname)))
    (if (null prop)
	(setq prop (add-sch-item-property sch-item pname)))
    (setf (caddr prop) pval)))

(defun make-sheet-inst (sheet-filename inst-name)
  (let ((uuid (uuid4))
	(elt (list '|sheet|)))
    (add-to-end elt '(|at| 50 50))
    (add-to-end elt '(|size| 20 20))
    (add-to-end elt (list '|uuid| (intern uuid)))
    (set-property elt "Sheet name" inst-name)
    (set-property elt "Sheet file" sheet-filename)
    elt))
    
(defun find-sheet-insts-used (sch)
  (loop for sch-item in (cdr sch)
	when (eq (car sch-item) '|sheet|)
	  collect (let ((uuid (cadr (xassoc '|uuid| sch-item)))
			(sheet-name (get-property sch-item "Sheet name"))
			(sheet-filename (get-property sch-item "Sheet file")))
		    (list uuid sheet-name sheet-filename))))

(defun make-sheet-path (uuid pagenum)
  (let ((path (if (null uuid)
		  "/"
		  (format nil "/~a/" uuid))))
    (list '|path| path (list '|page| (format nil "~d" pagenum)))))
      
		  
(defun find-max-pagenum (sch)
  (let ((max-pagenum 0))
    (loop for path in (cdr (xassoc '|sheet_instances| sch))
	  do (let ((pagenum-str (cadr (xassoc '|page| path))))
	       (if pagenum-str
		   (let ((pagenum (parse-integer pagenum-str)))
		     (if (> pagenum max-pagenum)
			 (setq max-pagenum pagenum))))))
    max-pagenum))


;    (cond ((null slist)
;	   (setq slist (list '|sheet_instances| (make-sheet-path nil 1)))
;	   (add-to-end sch slist)))


(defun find-sheet-insts-declared (sch)
  (loop for path-info in (cdr (xassoc '|sheet_instances| sch))
	collect (let* ((path (cadr path-info))
		       (page-str (cadr (xassoc '|page| path-info)))
		       (page (if page-str (parse-integer page-str) nil)))
		  (list path page))))
	     


(defun build ()
  (let ((top (make-empty))
	(led-sheet (get-sheet "/home/pace/kcombine/byhand/led.kicad_sch")))
    (let ((inst1 (make-sheet-inst (sheet-filename led-sheet) "led1"))
	  (inst2 (make-sheet-inst (sheet-filename led-sheet) "led2")))
      (add-to-end top inst1)
      (add-to-end top inst2)
      )

    (let ((used (find-sheet-insts-used top)))
      (print used))

    (write-sch top "/home/pace/kcombine/top.sch")
    (format t "output in top.sch~%")
    top))

(build)

