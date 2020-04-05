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

; adds a nil 

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
	(cond ((atom TREE) (cons TREE nil))
		  (t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))
	)
)

(defun SPLIT-LIST (L)
	(cond ((evenp (length L)) (let* ((split_length (/ (length L) 2))) (append (list (SUB-LIST L 0 split_length)) (list (SUB-LIST L split_length split_length)))))
		  ((oddp (length L)) (let* ((split_length (/ (- (length L) 1) 2))) (append (list (SUB-LIST L 0 split_length)) (list (SUB-LIST L split_length (+ split_length 1))))))
	)
)

(defun BTREE-HEIGHT (TREE)
	(cond ((atom TREE) 0) ; we are at the bottom, return 1 to add that element
		  ((> (BTREE-HEIGHT (first TREE)) (BTREE-HEIGHT (second TREE))) (+ (BTREE-HEIGHT (second TREE) 1)))
		  (t (+ (BTREE-HEIGHT (second TREE)) 1))	
	)
)

(defun LIST2BTREE (LEAVES)
	(cond ((= (length LEAVES) 1) (first LEAVES))
		  ((= (length LEAVES) 2) LEAVES)
		  (t (append (list (LIST2BTREE (first (SPLIT-LIST LEAVES)))) (list(LIST2BTREE (cadr (SPLIT-LIST LEAVES))))))
	)
)
; is it okay for the tree to be right biased 




 
