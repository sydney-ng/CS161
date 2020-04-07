; 1. Write a single Boolean LISP function, called TREE-CONTAINS, which takes two arguments N and TREE, and 
; checks whether number N appears in the ordered tree TREE.

(defun TREE-CONTAINS (N TREE)
  (cond ((eql nil TREE) nil) ; if tree empty, return 
  		((atom TREE) (if (= N TREE) t nil)) ; if tree only has 1 atom, check if that's equal to N 
        (t (or (TREE-CONTAINS N (car TREE)) (TREE-CONTAINS N (cdr TREE)))) ; otherwise, break tree down and check
  )
 )


; test cases: 
	; (TREE-CONTAINS 3 '((1 2 3) 7 8)) returns T 
	; (TREE-CONTAINS 4 '((1 2 3) 7 8)) returns NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 2. Write a single LISP function, called TREE-MIN, which takes one argument TREE, and returns the 
;    minimum number appearing in the ordered tree TREE.

(defun TREE-MIN (TREE)
	(cond ((atom TREE) TREE) ; if it's one element, return it
		  (t (TREE-MIN (car TREE))))) ; grab first element (will always be the smallest element)
; test cases:
	; (TREE-MIN '((1 2 3) 7 8)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 3. Write a single LISP function, called TREE-ORDER, which takes one argument TREE, and returns an 
;    pre-ordered list of the numbers appearing in the ordered tree TREE. 

(defun TREE-ORDER (TREE)
	(cond ((atom TREE) (list TREE)) ; return the list of that element
		  (t (append (TREE-ORDER (second TREE)) (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE)))) 
	) ; tree order always goes 2nd, 1st, 3rd (then recombine recursively)
) 

; Notes from TA OH: 
	; 1. cons -> 2 args, will always return result (put first argument inside the 2nd)
	; 	 append does concatenation 
	; 2. don't use (first element -> (car TREE), middle element -> (cadr TREE), (rest -> (cddr TREE)
	;    use first, second, third 

; test cases: 
	; (TREE-ORDER 3) returns (3)
	; (TREE-ORDER '((1 2 3) 7 8)) returns (7 2 1 3 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 4. Write a single LISP function, called SUB-LIST, that takes a list L and two non-negative integers START 
;    and LEN, and returns the sub-list of L starting at position START and having length LEN. Assume that 
;    the first element of L has position 0. 

(defun SUB-LIST (L START LEN)
	(cond ((= 0 START) (if (= 0 LEN) nil (cons (car L) (SUB-LIST(cdr L) 0 (- LEN 1))))) ;add to list, decrement LEN 
		  (t (SUB-LIST (cdr L) (- START 1) LEN)) ; decrement start if not at 0 
	)
)

; test cases: 
	; (SUB-LIST '(a b c d) 0 3) returns (a b c) 
	; (SUB-LIST '(a b c d) 3 1) returns (d) 
	; (SUB-LIST '(a b c d) 2 0) return s NIL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 5. Write a single LISP function, called SPLIT-LIST, that takes a list L, and returns a list of two lists L1 and L2, in that order, such that
;    - L is the result of appending L1 and L2;
;	 - Length of L1 minus length of L2 is 0 or 1.

(defun SPLIT-LIST (L)
	(cond ((evenp (length L)) (let* ((split_length (/ (length L) 2))) (append (list (SUB-LIST L 0 split_length)) (list (SUB-LIST L split_length split_length)))))
		  ((oddp (length L)) (let* ((split_length (/ (- (length L) 1) 2))) (append (list (SUB-LIST L 0 (+ 1 split_length))) (list (SUB-LIST L (+ split_length 1)  split_length)))))
	)
)
; even case -> get length, split into 2 even parts, call sub-list 
; odd case -> get length, split into 2 parts (left biased), call sub-list

; test cases: 
	; (SPLIT-LIST '(a b c d)) returns ((a b) (c d)) 
	; (SPLIT-LIST '(a b c d e)) returns ((a b c) (d e)) 
	; (SPLIT-LIST '(a b c d e f)) returns ((a b c) (d e f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 6. Write a single LISP function, called BTREE-HEIGHT, which takes a binary tree TREE, and returns the height of TREE. 
;    Note that the height of a binary tree is defined as the length of the longest path from the root node to the farthest leaf node.

(defun BTREE-HEIGHT (TREE)
	(cond ((atom TREE) 0) ; we are at the bottom, return that element
		  ((> (BTREE-HEIGHT (first TREE)) (BTREE-HEIGHT (second TREE))) (+ (BTREE-HEIGHT (first TREE) 1))) ; left side deeper, count it 
		  (t (+ (BTREE-HEIGHT (second TREE)) 1)) ; right side is deeper, count it 
	)
)

; test cases: 
	; (BTREE-HEIGHT 1) returns 0
	; (BTREE-HEIGHT '(1 2)) returns 1
	; (BTREE-HEIGHT '(1 (2 3))) returns 2 
	; (BTREE-HEIGHT '((1 2) (3 4))) returns 2 
	; (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7)))) returns 3 
	; (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8)))) returns 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 7. Write a single LISP function, called LIST2BTREE, that takes a non-empty list of atoms LEAVES, and 
;    returns a binary tree such that
; 	 - The tree leaves are the elements of LEAVES;
; 	 - For any internal (non-leaf) node in the tree, the number of leaves in its left branch minus the
; 	 number of leaves in its right branch is 0 or 1.

(defun LIST2BTREE (LEAVES)
	(cond ((= (length LEAVES) 1) (first LEAVES)) ; take the first one if only one 
		  ((= (length LEAVES) 2) LEAVES) ; return both items if there's 2 
		  (t (append (list (LIST2BTREE (first (SPLIT-LIST LEAVES)))) (list(LIST2BTREE (cadr (SPLIT-LIST LEAVES))))))
		  ; recursively add with split list + combine with append for left/right
	)
)

; test cases: 
	; (LIST2BTREE '(1)) returns 1
	; (LIST2BTREE '(1 2)) returns (1 2)
	; (LIST2BTREE '(1 2 3)) returns ((1 2) 3)
	; (LIST2BTREE '(1 2 3 4)) returns ((1 2) (3 4))
	; (LIST2BTREE '(1 2 3 4 5 6 7)) returns (((1 2) (3 4)) ((5 6) 7))
	; (LIST2BTREE '(1 2 3 4 5 6 7 8)) returns (((1 2) (3 4)) ((5 6) (7 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 8. Write a single LISP function, called BTREE2LIST, that takes a binary tree TREE as input, and returns a list of atoms 
;    (assume TREE follows the constraints we defined earlier).
;    - As the input is a binary tree, each node has at most 2 children;
; 	 - This function is the inverse of LIST2BTREE. That is, (BTREE2LIST (LIST2BTREE X)) = X for all lists of atoms X.

(defun BTREE2LIST (TREE)
	(cond ((atom TREE) (cons TREE nil)) ; if there's just one atom, add it and be done 
		  (t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE)))) ; otherwise break tree up into left and right, 
		  																	; add recursively 
	)
)

; test cases: 
	; (BTREE2LIST 1) returns (1)
	; (BTREE2LIST '(1 2)) returns (1 2)
	; (BTREE2LIST '((1 2) 3)) returns (1 2 3)
	; (BTREE2LIST '((1 2) (3 4))) returns (1 2 3 4)
	; (BTREE2LIST '(((1 2) (3 4)) ((5 6) 7))) returns (1 2 3 4 5 6 7) 
	; (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))) returns (1 2 3 4 5 6 7 8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 9. Write a single Boolean LISP function, called IS-SAME, that takes two LISP expressions E1 and E2 whose atoms 
;    are all numbers, and checks whether the expressions are identical. In this question, you can only use ‘=‘ to 
;	 test equality (you cannot use ‘equal’). Recall that a LISP expression is either an atom or a list of LISP expressions.

(defun IS-SAME (E1 E2)
	(cond ((and (eql E1 nil) (eql nil E2)) t) ; both equal, done 
		  ((and (numberp E1) (numberp E2)) (if (= E1 E2) t nil)) ; one is a list, one isn't
		  ((and (numberp E1) (not (numberp E2))) nil) ; one not number
		  ((and (numberp E2) (not (numberp E1))) nil) ; one not number
		  (t (and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2)))) ; recursively break down the lists 
	)
)
; test cases: 
	; (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)) returns T 
	; (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8)) returns NIL

