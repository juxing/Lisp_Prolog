;
; p2opt.lsp:    A Simple Puzzle Solver
;
;      - reads a series of puzzle descriptions of the form:
;             (  ( (a b ... ) (x y ...) )   ; puzzle symbols
;                ( (a b) (c d) ... )        ; puzzle constraints
;                )
;      - produces all legal product permutations of all but the first symbol set 
;      - invoked via:  (p2opt)


;
; Top level control loop:
;
(defun p2opt ()          

    (let (dat_file_name dat_file puzzle)

       (loop
         (format t "Enter puzzle file name: ")      ; dynamically specify puzzle files
         (setf dat_file_name  (read-line))
         (if (string-equal dat_file_name "") (return))

         (setf dat_file
               (open (concatenate 'string "~/kdj/CS580/homework/HW2/" dat_file_name)))
;         (setf dat_file   (open dat_file_name :direction :input))
          (setf *print_max* 5)     ; not declared in a LET => global variable

          (loop 
	 (setf puz_list (read dat_file nil nil))
	 (if (null puz_list)	   (return))
	 (gen_puz puz_list)
	 )
          )
       (close dat_file)
       '***ALL_DONE***
       )
    )


(defun gen_puz (puz_list)
    (let ( (sym_list (first puz_list)) )

         (setf *constraints* (second puz_list) )
         (format t "~%Generating permutation products of: ~A~%" sym_list)
         (format t "~%subject to constraints: ~A~%" *constraints*)

         (setf *soln_cnt* 0)
         (setf *node_cnt* 0)
         (permute (cddr sym_list) (second sym_list) '() (list (first sym_list)))

         (format t "~%  Node count: ~D" *node_cnt*)
         (format t "~%  Solution count: ~D~%~%" *soln_cnt*)
         )
    )


;
; permute is invoked with four args: 
;     (unused sets) (unused symbols) (partial permutation) (partial product)
;
; If the unused symbol list is null, the partial permutation is a complete 
; one and ready to be added to the partial product;
;
; If there are no more used sets, then the recursion has bottomed out and
; the partial product is complete and ready to be printed.
;
; If there are unused symbols, each in turn added to the front of the partial
; permutation, creating a child node, and permute is recursively invoked
; to generate the grandchildren.

(defun permute (unused_sets unused_sym perm perm_prod)

  (let (new_perm)

    (setf *node_cnt* (1+ *node_cnt*))
    (cond 
         ( (null unused_sym)       ; have a permutation to add to product

           (setf perm_prod (cons perm perm_prod))
;           (if (legal perm_prod)    ; less effective pruning
               (cond
	         ( (null unused_sets)    ; bottomed out at leaf node 
                      (cond  
		     ( (< *soln_cnt* *print_max*)  (print (reverse perm_prod)) )
		       ( (= *soln_cnt* *print_max*)
                                         (format t "~%       suppressing printing ...") )
	                  )
		(setf *soln_cnt* (1+ *soln_cnt*))
	           )

	         ( t                    ; get next set & permute it
		  (permute (rest unused_sets) (first unused_sets) '() perm_prod)
                         )
	         )
;	    )
           )

       ( t                           ; generate children
	 (dolist (item unused_sym)
               (setf new_perm (append perm (list item)))
               (if (legal (cons new_perm perm_prod))      ; more effective pruning
                  (permute  unused_sets (remove item unused_sym) new_perm perm_prod)
                  )
               )
           )
         )
    )   
)

; Assumes perm_prod is of the form ( (a b ...) (c d ...) ... )
; Assumes *constraints* is a list of negative constraints of the form: ( (a b) (c d) ...)
; For each constraint, check if the first symbol is in the permutation product.
; If so, check if the second symbol is in the permuation product.
; If so and both symbols are at the same relative positions, it is a constraint violation.

(defun legal (perm_prod)

  (let (index index2 remainder (ans t))
    (block done                            ; allow breaks out of nested levels

      (dolist (constraint *constraints*)
        (dolist (perm perm_prod)
          (setf index (position (first constraint) perm))  ; first symbol in perm_prod?
          (cond ( (not (null index)) 
                  (setf remainder (remove perm perm_prod)) ; second sym must be in another perm
                  (dolist (rperm remainder)
                    (setf index2 (position (second constraint) rperm))  ; second symbol found?
                    (cond ( (not (null index2))
                            (cond ( (eq index index2)    ; same relative position?
                                    (setf ans nil)       ; constraint violation
                                    (return-from done)   ; no need to look further
                                    )
                                  )
                            )
                          )
                    )
                  )
                )
          )
        )
      ans
      )
   )
 )