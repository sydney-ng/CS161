
(defun condition-checker (clause symbol type)
	( if (null-checker clause)
      nil  
      (cond ((string= type "remove-var") 
      			(if (not (= (- 0 symbol) (car clause))) 
		          (append (list (car clause)) (condition-checker (cdr clause) symbol "remove-var"))
		          (condition-checker (cdr clause) symbol "remove-var")
      			)
      		)
      		
      		((string= type "is-true-clause") 
      			(if (= (first clause) symbol) 
		          t 
		          (condition-checker (rest clause) symbol "is-true-clause")
		      	)
      		)
      		((string= type "remove-true-clause") 
      			(if (not (condition-checker (first clause) symbol "is-true-clause"))
	          		(append (list (condition-checker (first clause) symbol "remove-var")) (condition-checker (rest clause) symbol "remove-true-clause")) ; not true, so keep 
	          		(condition-checker (rest clause) symbol "remove-true-clause")
      			)
      		)
      		((string= type "contain-val") 
      			(cond ((null-checker clause) nil)
					  ((= (car clause) symbol) t)
					  ((= (car clause) (- 0 symbol)) t)
					  (t (condition-checker (cdr clause) symbol "contain-val"))
				)
      		)
      ) 
    )
)

(defun clause-iterator (clause type)
	(if (null-checker clause) 
      nil  
      (cond ((string= type "contain-not-number") 
      						(if (numberp (first clause))  
		          				(clause-iterator (rest clause) "contain-not-number")
		          				t))

      		((string= type "contain-null") 
      						(if (not (null (first clause)))  
		          				(clause-iterator (rest clause) "contain-null")
		          				t)
      		)
    	)
    )
)

(defun find-next-clause (delta)
  (cond ((null-checker delta) nil)
        (t (let* ((head (first delta)) (tail (find-next-clause (rest delta))))
            (cond ((= (length tail) 0) head)
                  ((> (length head) (length tail)) tail)
                  (t head) 
            )
          )
        )
  )
)

(defun null-checker (delta)
	(if (= (length delta) 0) 
      t 
      nil)
)

(defun try-to-match (delta clause)
	(cond ((and (null-checker clause) (null-checker delta)) nil) ; clause & delta are null 
		  ((and (not (null-checker delta)) (null-checker clause)) (list '())) ; only clause is null 
		  (t (if (clause-iterator (condition-checker delta (first clause) "remove-true-clause") "contain-null")
		  		 (try-to-match delta (cdr clause))
		  		 (append (list (car clause)) 
		  		 	     (try-to-match (condition-checker delta (first clause) "remove-true-clause")
		  		 	     (find-next-clause (condition-checker delta (first clause) "remove-true-clause"))))
		  	 )
		  )
	)
)

(defun generate-answer (n result)
	(if (null-checker result) 
		nil
		(if (= n 0) 
			result 
			(if (condition-checker result n "contain-val")
				(generate-answer (- n 1) result)
				(cons n (generate-answer (- n 1) result))
			)
		)
	)
)

(defun sat? (n delta)
	(cond ((null-checker delta) nil)
		  ((clause-iterator (try-to-match delta (find-next-clause delta)) "contain-not-number") nil)
		  (t (generate-answer n (try-to-match delta (find-next-clause delta))))
	)
)


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

