; checks if what we are passing in still has anything to check
; is used as a helper fx for many of the other fx
(defun null-checker (delta)
  (if (= (length delta) 0) 
      t 
      nil)
)

; this is the parent function that will remove elements from the clauses as we parse them 
(defun discard-symbol-function-parser (symbol clause fx-name)
	(if (null-checker clause)
		nil
		(cond ((string= fx-name "prune-element") ; tries to prune symbol from the clause 
		      (if (eq (car clause) symbol) ; match with first element
		        (discard-symbol-function-parser symbol (cdr clause) "prune-element") 
		        ; then remove it and continue with rest 
		        (append (list (car clause)) (discard-symbol-function-parser symbol (cdr clause) "prune-element"))
		      	; else keep and then do rest 
		      ))
							
			  ((string= fx-name "discard-symbol-top-level") ; going through clauses 
			    (append (list (discard-symbol-function-parser symbol (car clause) "prune-element")) ; call the sub function 
			    (discard-symbol-function-parser symbol (cdr clause) "discard-symbol-top-level"))) ; plus do it for the rest of the clauses 
		)
	)
)

; checks to make sure the clause is still valid post-discarding
(defun top_level_validity_parser (symbol clause type)
	(cond ((string= "top-level-valid-check" type) 
				(if (top_level_validity_parser symbol clause "clause-validity-check")
				; need to go one level deeper into clauses 
			    (discard-symbol-function-parser symbol clause "discard-symbol-top-level")
			    ; do the discarding 
			    nil))
		  ((string= "clause-validity-check" type) 
		  		(if (null clause) 
	    			T
			    	(if (discard-symbol-function-parser symbol (car clause) "prune-element") ; if you can prune the element 
			      		(top_level_validity_parser symbol (cdr clause) "clause-validity-check") 
			      		; keep checking for the rest to see if the rest of clauses also still will be valid 
			      		nil)	
  				))
	)
)

; checks -> does a symbol exist in this clause? 
(defun exists (symbol clause)
  (if (null-checker clause) ; null clause 
    nil 
    (if (= (first clause) symbol) ; found symbol 
        t 
        (exists symbol (rest clause)) ; didn't find symbol, keep checking 
    )
  )
)

; is the parent function that will send the clauses to be parsed for removal of target element 
(defun top-level-removal-fx (symbol clause output_clause)
  (if (null-checker clause) ; if there is nothing in the clause, send output 
    output_clause
    ; else send it to be parsed 
    (continue-removing symbol clause (car clause) (cdr clause) output_clause)
  )
)

; separates head and tail of list to make it easy for parsing 
(defun continue-removing (symbol clause clause_head clause_tail output_clause)
	(if (exists symbol clause_head) ; check if clause head is target 
      (top-level-removal-fx symbol clause_tail output_clause) ; yes, so try same w/ tail 
      (top-level-removal-fx symbol clause_tail (append output_clause (list clause_head))) 
      ; else add head and do rest for tail 
    )
)

; parent level function that will try 
(defun iterate-through-branch (symbol clause)
  (if (confirm_branch_validity symbol clause) 
  		nil
  		(if (null (top-level-removal-fx symbol clause nil))
  			symbol
  			(begin-removal symbol (- symbol) clause 
  						    (top-level-removal-fx symbol clause nil) 
  						    (top-level-removal-fx (- symbol) clause nil))
  		)
  )
)

(defun begin-removal (symbol neg_symbol clause remove-pos-sym remove-neg-sym)
	 (if (equal remove-pos-sym clause) 
	 	 (if (equal remove-neg-sym clause)
	 	 	clause
	 	 	(top_level_validity_parser neg_symbol remove-pos-sym "top-level-valid-check")
	 	 ) 
         (top_level_validity_parser neg_symbol remove-pos-sym "top-level-valid-check")
      )  
)

; the element was removed, can it still work? return t if yes, nil if no 
(defun confirm_branch_validity (symbol clause)
  (if (or (or (null (top_level_validity_parser (- symbol) clause "top-level-valid-check"))  ; results in null clause
      (equal (top-level-removal-fx symbol clause nil) clause)) ; removing symbol didn't make a diff 
      (null-checker clause) ; null clause 
    )
      t 
      nil
  )
)

; before parsing, check validity of clause 
(defun SAT-parser-prechecks (clause symbol n)
  (if (or (null clause) (> symbol n))
    t
    nil)
) 

; see how many symbols are left to parse through 
(defun answer-generator (cur_sol vars_remaining)
  (if (= vars_remaining 0) ; we are done! 
    cur_sol
    ; not done yet, keep going, decrement number to parse left by one and add what we have
    (answer-generator (append cur_sol (list (+ (length cur_sol) 1))) (- vars_remaining 1))
  )
)

; parent function that will check what level of clauses we are on 
(defun SAT-parser (clause symbol n)
    (cond ((SAT-parser-prechecks clause symbol n) nil)
        ((atom clause) (list clause)) ; there is only one clause 
        (t (go-through-branches clause symbol n (iterate-through-branch symbol clause) (iterate-through-branch (- symbol) clause)))
    	; try parsing t/f assignment of branches through children fx 
    )
)

; continue parsing & pruning 
(defun null-branch-parse (temp_T_val T_branch symbol)
  (cond ((null T_branch) nil) ; precondition checks 
      (t (cond ((atom T_branch) (list symbol)) ; append the last one 
             (t (cond ((null temp_T_val) nil)
                  (t (append (list symbol) temp_T_val)) ; otherwise add to branch 
                )
             )

          )
      )
  )
)

; this does the actual iterating through branch 
(defun go-through-branches (clause symbol n T_branch F_branch)
    (if (null F_branch)
      (null-branch-parse (SAT-parser T_branch (+ symbol 1) n) T_branch symbol) ; true branch 
      (null-branch-parse2 F_branch (SAT-parser F_branch (+ symbol 1) n) (SAT-parser T_branch (+ symbol 1) n)symbol) ; false branch 
         
  )
)

; parse the false branch 
(defun null-branch-parse2 (F_branch temp_F_val temp_T_val symbol)
  (cond ((atom F_branch)(list (- symbol))) ; last item add 
  		(t (cond ((null temp_F_val) (cond ((null temp_T_val) nil)
  									 (t (append (list symbol) temp_T_val))) ; add to branch if not nil 
  								) 
  				  (t (append (list (- symbol)) temp_F_val)) ; add false 
  		   )
  		)
  )
)

; original function given, calls all children function 
(defun sat? (n delta) 
	(if (= n 0) ; data validation 
		nil 
		(solve-sat? n 1 delta) ; validated, pass to children 
	)
)

; child function to aggregate answer 
(defun solve-sat? (n num delta)
	(do-work (SAT-parser delta num n) n) ; pass to child 
)

; cumulate the solution + keep appending it 
(defun do-work (sol_so_far n)
	(if (null-checker sol_so_far)
		nil 
		(do-work-2 sol_so_far (- n (length sol_so_far))) ; pass to child with negative sym
	)       

)

; parent to begin the parsing -> high level 
(defun do-work-2 (sol_so_far num_still_to_parse)
	(answer-generator sol_so_far num_still_to_parse) ; call child to parse 
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
  