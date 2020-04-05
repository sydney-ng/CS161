(defun TREE-CONTAINS (N TREE)
  (cond ((eql nil TREE) nil)
  		((atom TREE) (if (= N TREE) t nil))
        (t (or (TREE-CONTAINS N (car TREE)) (TREE-CONTAINS N (cdr TREE))))
  )
 )

(defun TREE-MIN (TREE)
	(cond ((atom TREE) TREE)
		  (t (TREE-MIN (car TREE)))))

; (first element -> (car TREE)
; middle element -> (cadr TREE)
; (rest -> (cddr TREE)

(defun TREE-ORDER (TREE)
	(cond ((atom TREE) TREE)
		  (t (cons (TREE-ORDER (cadr TREE)) (cons (TREE-ORDER(car TREE)) (TREE-ORDER (cddr TREE)))))
	)
)

(defun SUB-LIST (L START LEN)
	(cond ((= 0 START) (if (= 0 LEN) nil (cons (car L) (SUB-LIST(cdr L) 0 (- LEN 1)))))
		  (t (SUB-LIST (cdr L) (- START 1) LEN))
	)
)

; 	(cond (eql 0 START) ((if (eql 0 LEN)) nil (SUB-LIST ((cdr L) 0 (LEN-1))))


