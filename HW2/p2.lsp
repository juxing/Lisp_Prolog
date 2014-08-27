;
; p2.lsp:    A Logic Game Player
;
;      - reads lists of symbols and lists of negative constraints from p2.dat of the form:
;              ( ( (a b c) ... (1 2 3) ) ((a 1) (b 3)) ) 
;      - produces all legal combinations that comply with negative constraints
;      - invoked via:  (p2)


;
; Top level control loop:
;
(defun p2 ()          

    (let (dat_file)

	(setf dat_file   (open "p2.dat" :direction :input))
        ;(setf dat_file   (open "p2_bird.dat" :direction :input))
	(setf *print_max* 10)     ; not declared in a LET => global variable

	(loop 
	    (setf sym_and_neg_list (read dat_file nil nil))
	    (if (null sym_and_neg_list)	   (return))
            (setf sym_list (elt sym_and_neg_list 0))
            (setf *prefix* (elt sym_list 0))  ; In logic game, no need to permute the first list of symbols,
                                              ; just print this list of symbos in front of other permutations.
            (setf sym_list (rest sym_list))  ; Just permute the rest lists of symbols
            (setf *neg_cond* (elt sym_and_neg_list 1))
	    (gen_perm sym_list)
	    )

	(close dat_file)
	'***ALL_DONE***
    )
)


(defun gen_perm (sym_list)

    (format t "~%Generating permutation products of: ~A~%" sym_list)
    (format t "~%subject to constraints: ~A~%" *neg_cond*)

    (setf *leaf_cnt* 0)
    (setf *node_cnt* 0)
    (permute (rest sym_list) (first sym_list) '() '())

    (format t "~%  Node count: ~D" *node_cnt*)
    (format t "~%  Leaf count: ~D~%~%" *leaf_cnt*)
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

  (let ()

    (setf *node_cnt* (1+ *node_cnt*))
    (cond
	 ( (null unused_sym)       ; have a completed permutation to add to product

	      (setq perm_prod (cons perm perm_prod))
              ;(setf tmp_perm_prod (cons *prefix* perm_prod))
              (cond
                  ((legal (cons *prefix* perm_prod))  ; Check if this partial product comply with negative constraints.
                                                      ; If not, don't do anything, the recursion will stop here, means we cut
                                                      ; a part of the tree off here. 
                      (cond
                           ( (null unused_sets)    ; bottomed out at leaf node

                                (cond
                                     ( (< *leaf_cnt* *print_max*)
                                             (print (cons *prefix* (reverse perm_prod)) ) )
                                     ( (= *leaf_cnt* *print_max*)
                                             (format t "~%       suppressing printing ...") )
                                     )
                                (setf *leaf_cnt* (1+ *leaf_cnt*))
                                )

                           ( t                    ; get next set & permute it
                                (permute (rest unused_sets) (first unused_sets) '() perm_prod))  ; If this partial product is OK, keep going
                           )
                  )
              )
          )

;	      (cond
;		   ( (null unused_sets)    ; bottomed out at leaf node
;
;			(cond  
;		             ( (< *leaf_cnt* *print_max*)   
;			             (print (reverse perm_prod)) )
;		             ( (= *leaf_cnt* *print_max*)   
;			             (format t "~%       suppressing printing ...") )
;			     )
;		        (setf *leaf_cnt* (1+ *leaf_cnt*))
;			)
;
;		   ( t                    ; get next set & permute it
;		        (permute (rest unused_sets) (first unused_sets) '() perm_prod))
;		   )
;	      )

	 ( t                           ; generate children
	      (dolist (item unused_sym)
	         (permute  unused_sets (remove item unused_sym) (cons item perm) perm_prod) )
	      )
	 )
    )
  )

;; Check if a partial product comply with negative constraints
(defun legal (partial_sol)
    (setf res t)
    (dolist (neg *neg_cond*)
        (let (pos0 pos1)
            (dolist (per partial_sol)
                (cond
                    ((numberp (position (elt neg 0) per))
                        (setf pos0 (position (elt neg 0) per))
                    )
                    ((numberp (position (elt neg 1) per))
                        (setf pos1 (position (elt neg 1) per))
                    )
                )
                
                (cond
                    ((and (numberp pos0) (numberp pos1) (= pos0 pos1))  ; If the first and second element of negative constraint appear at
                                                                        ; the same position of this partial product, then this product is illegal.
                        (setf res nil)
                    )
                )
            )
        )
    )
    res
)

