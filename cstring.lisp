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


(print (cstring-replace (format nil "foo~%bar")))
