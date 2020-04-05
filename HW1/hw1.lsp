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

(defun IS-SAME (E1 E2)
	(cond ((and (eql E1 nil) (eql nil E2)) t)
		  ((and (numberp E1) (numberp E2)) (if (= E1 E2) t nil))
		  ((and (numberp E1) (not (numberp E2))) nil)
		  ((and (numberp E2) (not (numberp E1))) nil)
		  (t (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2))))
	)
)

(defun BTREE2LIST (TREE)
	(cond ((numberp TREE) (list TREE))

		  (t (cons (BTREE2LIST (car TREE)) (BTREE2LIST (cdr TREE))))
))


;(defun SPLIT-LIST (L)
;	(cond ((evenp (length L)) (cons (SUB-LIST (L 0 2)) (SUB-LIST (L 2 2)))
;		  )
		  ;((eql nil L) nil)
		  ;((evenp (length L)) (cons (SUB-LIST (L 0 len)) (SUB-LIST L len len)))
;	)
;)



; 	(cond (eql 0 START) ((if (eql 0 LEN)) nil (SUB-LIST ((cdr L) 0 (LEN-1))))


