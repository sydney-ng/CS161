
(defun TREE-CONTAINS (N TREE)
  ;(cond (and (atom TREE) (= N TREE)) )
  (cond ((atom TREE) (if (= N TREE) (t) (nil)))))
  		  ; if it's 1 element & element = N
	;(print "nah"))
	;(if (list TREE) (and (TREE-CONTAINS N car TREE ) (TREE-CONTAINS N cdr TREE)) (print "this is nothing")))

;(defun TREE-CONTAINS (n TREE)
;	(cond ((= nil car(TREE) (nil)))
;		  ((= atom car(TREE)) 
;			(if (= car(TREE) n)
;				(t))
;				(nil)) 
;		  ((= list car(TREE)) (or (TREE-CONTAIINS (car(TREE)) (TREE-CONTAINS (cdr(TREE))))))
;
;)

 ;(defun TREE-CONTAINS (n TREE)
 	;(print ("hello world"))
 ;)


;(defun TREE-MIN (TREE)
;	(if (= atom (TREE)) 
;			(TREE)
;			(TREE-MIN (car(TREE))
;		)
;	)
;)

;(defun TREE-ORDER (TREE)
	;()
;)
