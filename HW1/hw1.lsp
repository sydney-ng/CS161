;(defun TREE-CONTAINS (N TREE)
;  (cond ((atom TREE) (if (= N TREE) (t)) (nil))

(defun TREE-MIN (TREE)
	(cond ((atom TREE) TREE)
		 (t(TREE-MIN (car TREE)))))

;(defun TREE-ORDER (TREE)
	;()
;)
