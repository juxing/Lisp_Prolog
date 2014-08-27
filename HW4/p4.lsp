; Ming Zhang
; CS580
; Homework #4
;
; p4.lsp:    A Simple Puzzle Solver and could handle linear ordering constraints.
;
;      - reads a series of puzzle descriptions of the form:
;             (  ( (a b ... ) (x y ...) )       ; puzzle symbols
;                ( (above >) (below <) )        ; background definitions to specify meaning of relational words
;                ( (ordered (first second third ...) ) )    ; relatiional definitions. 
;                ( (not a b) (above j k) (below c (d e)) ... )        ; puzzle constraints
;                )
;      - produces all legal product permutations of all but the first symbol set 
;      - invoked via:  (p4)

; Some personal macro examples

(defmacro head (x)
    (list 'car x)
    )

(defmacro tail (x)
    (list 'cdr x)
    )

;
; Top level control loop:
;
(defun p4 ()          

    (let (dat_file_name dat_file puzzle)

	(loop
            (format t "Enter puzzle file name: ")
            (setf dat_file_name  (read-line))
            (if (string-equal dat_file_name "") (return))

            ;(setf dat_file (open (concatenate 'string "~/kdj/CS580/homework/HW3/" dat_file_name))) ; open file for input
	    (setf dat_file   (open dat_file_name :direction :input))
	    (setf *print_max* 10)     ; not declared in a LET => global variable

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
    (let ( (sym_list (first puz_list)) (defs (second puz_list) ))

         (setf *order* (second (first (third puz_list))))  ; *order* global variable is used to check linear relation. 
         (setf *constraints* (fourth puz_list))
         (format t "~%Generating permutation products of: ~A~%" sym_list)
         (format t "~%using the following definitions: ~A~%" defs)
         (format t "~%order relations: ~A~%" *order*)
         (format t "~%subject to constraints: ~A~%" *constraints*)
         (setf *constraints* (process_constraints defs))
         (format t "~%~%Expanded constraints: ~A~%~%" *constraints*)

         (setf *object_length* (length sym_list))
         (setf *soln_cnt* 0)
         (setf *node_cnt* 0)

         (new_permute (tail (head sym_list)) (tail sym_list) (list (first (first sym_list))) '())

         (format t "~%  Node count: ~D" *node_cnt*)
         (format t "~%  Solution count: ~D~%~%" *soln_cnt*)
         )
    )

; Process_constraints(defs) uses the information in defs to expand the symbols in *constraints*
; returning the expanded constraint list as its value.
; This version expands the ability to expand (above >) and (below <) defs.

(defun process_constraints (defs) 
  (let (new_constraints lhs rhs new_lhs new_rhs operator)

    (dolist (constraint *constraints* new_constraints)

      (if (atom (second constraint))       ; simplfy expansion code
          (setf lhs (list (second constraint)))
          (setf lhs (second constraint))
          )
      (setf new_lhs nil)

      (dolist (symb lhs)
        (if (null (setf def (assoc symb defs)))  ; no expansion
          (setf new_lhs (cons symb new_lhs))
          (dolist (item (second def))            ; expand
            (setf new_lhs (cons item new_lhs)) 
            )
          )
        )

      (if (atom (third constraint))     ; simplify expansion code
          (setf rhs (list (third constraint)))
          (setf rhs (third constraint))
          )
      (setf new_rhs nil)

      (dolist (symb rhs)
        (if (null (setf def (assoc symb defs)))   ; no expansion
          (setf new_rhs (cons symb new_rhs))
          (dolist (item (second def))             ; expand
            (setf new_rhs (cons item new_rhs))
            )
          )
        )

      ; expand the operator if it is "above" or "below"
      (setf operator (first constraint))
      (if (null (setf def (assoc operator defs)))
          (setf operator operator)
          (setf operator (second def))
      )

      (setf new_constraints (cons (list operator new_lhs new_rhs) new_constraints))
      )
  ; returned value specified by dolist
  )
 )

;
; new_permute is invoked with four args: 
;     (fixed symbols) (permuted symbols) (object) (partial solution)
;
; If the length of the object equals *obj_length*, then add it to the partial solution.
; Else, recurse to add more symbols.
; If the fixed symbol list is null, we're at a leaf node - a solution.
; Else, recurse to add the next object the partial solution.

(defun new_permute (fixed_syms permuted_syms object solution)

  (let (new_object slot_num new_permuted_syms)

    (setf *node_cnt* (1+ *node_cnt*))

    (cond 
         ( (eq (length object) *object_length*)      ; have a permutation to add to product
           (setf solution (cons (reverse object) solution))
               (cond
	         ( (null fixed_syms)    ; bottomed out at leaf node 
                   (cond  
		        ( (< *soln_cnt* *print_max*)  (print (reverse solution)) )
		        ( (= *soln_cnt* *print_max*)
                               (format t "~%       suppressing printing ...") )
	                )
		   (setf *soln_cnt* (1+ *soln_cnt*))
	           )

	         ( t                    ; start on the next object
		     (new_permute (tail fixed_syms) permuted_syms (list (head fixed_syms)) solution)
                     )
	         )
           )

         ( t             ; add more symbols to the current object
             (setf slot_num (1- (length object)))
             (dolist (slot_sym (nth slot_num permuted_syms))
               (setf new_object (cons slot_sym object))
               ;(print new_object)
               (cond ( (legal (cons new_object solution))      ; pruning check
                       (setf new_permuted_syms '())
                       (dotimes (x (length permuted_syms) )
                         (if (eq x slot_num)
                             (setf new_permuted_syms (cons (remove slot_sym (nth x permuted_syms)) new_permuted_syms))
                             (setf new_permuted_syms (cons (nth x permuted_syms) new_permuted_syms))
                             )
                         )
                       (new_permute fixed_syms (reverse new_permuted_syms) new_object solution)
                       )
                     )
               )
             )
         )
    )
  )


; Assumes solution is a list of puzzle objects of the form ( (a b ...) (c d ...) ... )
; Assumes *constraints* is a list of negative constraints of the form: ( (not (a ...) (b ...)) ...)
; For each constraint, for each symbol in the left-hand side, check if it is in an object in the solution.
; If so, for each symbol in the right-hand side, check if it is in the the same object.
; If so, it is a constraint violation.

; This version expands the ability to check (above (a b) (c d)) and (below (a b) (c d)) constraints.
; For each constraint, check if any symbol1 in the left-hand side is in one object, and that object has relation symbol, 
; then check if any symbol2 in the right-hand side is in another object, and that object has relation symbol,
; if both of above conditions are met, then check what positons are those two relation symbols in *order* list and check if 
; they comply with < or > relation, if not, it is a constraint violation.

(defun legal (solution)

  (block done                                    ; break out of nested levels
                                   
    (dolist (constraint *constraints* t)

      (cond 
          ((equal (first constraint) 'not)  ; Check not constraint.

               (dolist (object solution)
                   (dolist (sym1 (second constraint))         ; check for left-hand matches
                       (if (member sym1 object)
                           (dolist (sym2 (third constraint))     ; check for right-hand matches 
                               (cond 
                                   ((member sym2 object)
                                       ;(print constraint)
                                       (return-from done nil)
                                   )
                               )
                           )
                       )
                   )
               )

          )

          ((equal (first constraint) '<)  ; Check < constraint.
              (dolist (sym1 (second constraint))
                  (dolist (sym2 (third constraint))
                      (setf pos1 -1)
                      (setf pos2 -1)
                      (dolist (object solution)
                          (cond 
                              ((and (intersection *order* object) (member sym1 object))
                                  (setf floor (first (intersection *order* object)))
                                  (setf pos1 (position floor *order*))
                              )
                          )

                          (cond 
                              ((and (intersection *order* object) (member sym2 object))
                                  (setf floor (first (intersection *order* object)))
                                  (setf pos2 (position floor *order*))
                              )
                          )
                      )

                      (cond
                          ((and (not (equal pos1 -1)) (not (equal pos2 -1)) (not (< pos1 pos2)))
                              ;(print constraint)
                              (return-from done nil)
                          )
                      )
                  )
              )    
          )

          ((equal (first constraint) '>)  ; Check > constraint.
              (dolist (sym1 (second constraint))
                  (dolist (sym2 (third constraint))
                      (setf pos1 -1)
                      (setf pos2 -1)
                      (dolist (object solution)
                          (cond 
                              ((and (intersection *order* object) (member sym1 object))
                                  (setf floor (first (intersection *order* object)))
                                  (setf pos1 (position floor *order*))
                              )
                          )

                          (cond 
                              ((and (intersection *order* object) (member sym2 object))
                                  (setf floor (first (intersection *order* object)))
                                  (setf pos2 (position floor *order*))
                              )
                          )
                      )

                      (cond 
                          ((and (not (equal pos1 -1)) (not (equal pos2 -1)) (not (> pos1 pos2)))
                              ;(print constraint)
                              (return-from done nil)
                          )
                      )
                  )
              )    
          )
      )
      )
    )
  )




