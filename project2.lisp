

;(main pop-size generations)
;(setq pop-size 50)
;(setq args *args*)
;(setq x (car args))
;(setq y (car (cdr args)))
;(setq z (car (cdr(cdr args))))
;(setq output (car(cdr(cdr(cdr args)))))
;(setq pop-size (car(cdr(cdr(cdr(cdr args))))))
;(setq generations (car(cdr(cdr(cdr(cdr(cdr args)))))))
;(setq x (parse-integer x))
;(setq y (parse-integer y))
;(setq z (parse-integer z))
;(setq output (parse-integer output))
;(setq pop-size (parse-integer pop-size))
;(setq generations (parse-integer generations))
;(format t "~d ~d ~d ~d ~d ~d" x y z output pop-size generations))

(setq pop-size 50)
(setq generations 50)
(setq samples (list '(0 -2 1 -16)
                    '(-4 -5 -3 58)
                    '(9 8 -6 72)
                    '(9 -7 5 113)
                    '(-8 7 3 150)
                    '(5 4 -5 20)
                    '(6 -4 6 41)
                    '(-5 3 -7 -24)
                    '(-6 -5 9 -18)
                    '(1 0 2 2)))
(setq mutation_rate 75)
(setq vars '(x y z))
(setq nums '(-9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9))
(setq ops '(+ - *))

;Declare the random seed
(setf *random-state* (make-random-state t))

;;; PROVIDED HELPER FUNCTIONS ;;;

(defun flatten (obj)
  (do* ((result (list obj))
        (node result))
       ((null node) (delete nil result))
    (cond ((consp (car node))
           (when (cdar node) (push (cdar node) (cdr node)))
           (setf (car node) (caar node)))
          (t (setf node (cdr node))))))

(defun tree_nth_cell (rnth rtree)
  "Return the DFS N-th cell in the given tree: 1-based."
  ;; (declare (optimize (safety 3)))
  ;; (print "tree_nth_cell")
  ;; (print rnth)
  ;; (print rtree)
  (let ((size (cell_count rtree)))
    ;;(print (list :dbga rnth size (car rtree)))
    (cond
      ((not (integerp rnth)) nil)
      ((not (listp rtree)) nil) ;; Not a tree?
      ((null rtree) nil) ;; No tree elts?
      ((= 1 rnth) rtree) ;; 1st elt of list is the tree, itself.
      ((>= 0 rnth) nil) ;; Nth 0 or negative?
      ((> rnth size) nil) ;; N-th beyond tree's end?
      (t ;; Elt is in car subtree or cdr "subtree".
        (setq rnth (1- rnth)) ;;Account: Elt isn't the current (car+cdr's) node.
        (let ((size1 (cell_count (car rtree))))
          ;;(print (list :dbgb rnth size1 (car rtree)))
          (cond
          ((>= 0 size1) (tree_nth_cell ;; No car subtree.
            rnth
            (cdr rtree))) ;; Find elt in the cdr subtree.
          ((<= rnth size1) (tree_nth_cell ;; Elt is in car subtree.
            rnth
            (car rtree))) ;; Find elt in the car subtree.
          (t (tree_nth_cell ;; Elt is in cdr subtree.
            (- rnth size1) ;; Account for skipping car subtree.
            (cdr rtree))))))))) ;; Skip car subtree.

(defun tree_nth (rnth rtree)
  "Return the DFS N-th subtree/elt in the given tree."
  ;; (write "tree nth w/")
  ;; (print rnth)
  ;; (print rtree)
  (let ((size (cell_count rtree)))
    ;; (print (list :dbga rnth size (car rtree)))
    (cond
      ((not (integerp rnth)) nil)
      ((not (listp rtree)) nil) ;; Not a tree?
      ((null rtree) nil) ;; No tree elts?
      ((= 1 rnth) (car rtree)) ;; 1st elt of list is its car subtree.
      ((>= 0 rnth) nil) ;; Nth 0 or negative?
      ((> rnth size) nil) ;; N-th beyond tree's end?
      ((= 1 size) (car rtree)) ;; 1st elt is Tree's car.
      (t ;; Elt is in car subtree or cdr "subtree".
        (setq rnth (1- rnth)) ;;Account: Elt isn't the current (car+cdr's) node.
        (let ((size1 (cell_count (car rtree))))
          ;; (print (list :dbgb rnth size1 (car rtree)))
          (cond
            ((>= 0 size1) (tree_nth ;; No car subtree.
                           rnth
                           (cdr rtree))) ;; Find elt in the cdr subtree.
            ((<= rnth size1) (tree_nth ;; Elt is in car subtree.
                              rnth
                              (car rtree))) ;; Find elt in the car subtree.
            (t (tree_nth ;; Elt is in cdr subtree.
                (- rnth size1) ;; Account for skipping car subtree.
                (cdr rtree))))))))) ;; Skip car subtree.

(defun random_tree_cell (rtree)
 "Return random cell in the tree."
  (setf rtree (flatten rtree))
  ;(write "TREE:")
  ;(write rtree)
  ;(terpri)
 (let* ((size (cell_count rtree))
 (rx (1+ (random (1- size)))) ;; Avoid 1st cell (the whole tree).
 (nth (1+ rx)) ;; Incr cuz our fcn is 1-based, not 0-based.
 (spot (tree_nth_cell nth rtree)))
 ;; (print (list :dbg size nth spot))
 spot))

(defun pop_fitness ( rpop ) ;; Pop is a population.
 "Create Pop-Scored pairs (Score Critter) given Pop list of critters."
 (mapcar #'(lambda (critter)
 (let ((score (get_fitness critter)))
 (list score critter)))
 rpop))

(defun cell_count (rt)
  "Return the number of nodes in the tree. Skip non-cells."
  (cond
    ((null rt) 0)
    ((not (listp rt)) 0)
    (t (let ((cc (length rt)))
      (+ cc (apply #'+ (mapcar #'cell_count rt)))))))



(defun make_kid (rmom rtgt rnew)
  "Return kid: copy of mom with tgt cell replaced by given new cell or NIL."
  (if (not (and rmom rtgt rnew
                (listp rmom)
                (listp rtgt)
                (listp rnew)))
    rmom
    (if (eq rmom rtgt)
      rnew
      (cons (make_kid (car rmom) rtgt rnew)
            (make_kid (cdr rmom) rtgt rnew)))))

(defun get_front_upto_nth ( rn rlist )
 "Return list head from 0-th thru N-th elt. Assumes elt-n is unique."
 (let ((elt-n (nth rn rlist)))
 (reverse (member elt-n (reverse rlist))))) 

(defun safe_sort_scored_pop ( rscored-pop )
 "Return a sorted list of scored-critter elts. Don't change given list.
 NB, your Lisp's built-in sort fcn may damage the incoming list."
 (let ((sacrifice-list (copy-list rscored-pop)))
 (sort sacrifice-list
 #'(lambda (scored-critter-1 scored-critter-2)
 (< (car scored-critter-1) (car scored-critter-2))))))

(defun get_pop_from_scored (rscored-pop)
 "Return just the Pop of critters from the Scored Pop."
 ;;Alt: (mapcar #'(lambda (elt) (nth 1 elt)) rscored-pop)
 (mapcar #'cadr rscored-pop))

;;; END PROVIDED FUNCTIONS ;;;


(defun mutate_critter (critter)
    "We pass in a critter to mutate, changing either than operator or an operand"
    (setq new_critter critter) ;Create a copy of our critter
    (setq change_pos (random (length critter))) ;Randomly select the mutation point
    (setq op (random 3)) ;Randomly select the operation to change if selected
    (setq curnum (- (random 22) 9)) ;Randomly select the operand to replace old if selected
    (cond 
        ((= change_pos 0) ;If selected point is 0 we change the operator with a randomly selected one
        (if (= op 0) (setq newop '+))
        (if (= op 1) (setq newop '-))
        (if (= op 2) (setq newop '*))
        (setq new_critter (append (list newop) (cdr critter)))) ;Set the new critter to be prepended by the new operation
        ((> change_pos 0);If the position is not the operator we change with a number or variable at random
            (cond
            ((= curnum -10) (setf curnum 'x ))
            ((= curnum -11) (setf curnum 'y ))
            ((= curnum -12) (setf curnum 'z ))
            ((= curnum 10) (setf curnum 'x ))
            ((= curnum 11) (setf curnum 'y ))
            ((= curnum 12) (setf curnum 'z )))
            (setf (nth change_pos new_critter) curnum))) ;Set the new operand
    (return-from mutate_critter new_critter) ;Return the resulting new critter
    )

(defun create_random_child (passed)
    "Create a child at random"
    (setq child passed) ;Set our current child to the passed child
    (setq op (random 3)) ;Randomly select our operator
    (if (= op 0) (setq newop '+))
    (if (= op 1) (setq newop '-))
    (if (= op 2) (setq newop '*))
    (setf newlist (append '() (list newop))) ;Create a new list with our operator
    (setf numele (random 3)) ;Randomly select how many elements in our list
    (loop while(>= numele 0) do ;Insert random numbers/variables for each element
        (setq curnum (- (random 22) 9)) ;Get random number/variable
        (case curnum
            (-10 (setq curnum 'x ))
            (-11 (setq curnum 'y ))
            (-12 (setq curnum 'z ))
            (10 (setq curnum 'x ))
            (11 (setq curnum 'y ))
            (12 (setq curnum 'z )))
        (nconc newlist (list curnum)) ;Create a new list with our numbers
        (decf numele)) ;Decrement
    
    (if (not passed) ;If not NIL then we want to create a normal list else create a deep list
       (setq child (append child  newlist))
       (setq child (append child (list newlist))))
    (setf try-again (random 5)) ;Randomly try again to build a deeper list
    (if (= try-again 0)
        (create_random_child child))
    (return-from create_random_child child)) ;Return our critter

(defun random-el (seq)
    "Returns a random element from the given sequence using nth and random
    PARAMETERS:
        seq => sequence, accessible by nth, to choose an element from
    RETURNS:
        A single, randomly selected, element from the list"
    (nth (random (length seq)) seq))

(defun gen-expr (&optional (max-len 7) (max-sub 5) (sub-count 0))
    "Generates a random expression of a given maximum element length 
    (counting a sub-list as 1) and a maximum number of sub-lists.
    OPTIONAL PARAMETERS:
        max-len => number, represents max expression length
        max-sub => number, represents max number of sub-lists
        sub-count => number, counter for number of sub-lists
    RETURNS:
        A list which is a random expression which may include:
            X, Y, and/or Z
            the numbers -9 - 9
            the operators +, -, and/or *"
    (let ((expr-len (random max-len))
          (make-sub nil)
          (use-var nil)
          (expr-piece nil)
          (expr '()))
            (if (< expr-len 1)
                (setf expr-len (+ expr-len 2))) ; minimum length 2 (to ensure expression is valid)
            (loop while (<= (length expr) expr-len) 
                do (setf make-sub (random 10))
                do (setf use-var (random 10))
                do (cond
                        ((< (length expr) 1) (setf expr-piece (random-el ops))) ; only first elements are operators 
                        ((and (<= make-sub 4) (< sub-count max-sub))    ; 50% chance to insert sub-list
                            (setf expr-piece (gen-expr max-len max-sub (+ sub-count 1))))
                        ((<= use-var 4)     ; 50% chance to insert a variable
                            (setf expr-piece (random-el vars)))
                        (T                  ; default insert a number
                            (setf expr-piece (random-el nums))))
                collect expr-piece into expr
                finally 
                    (return-from gen-expr expr))))

(defun init-pop ()
  "Initialize a population of expressions"
  (let ((e-pop '())
        (n-expr '()))
    (loop while (< (length e-pop) pop-size)
      do (setf n-expr (gen-expr))
      collect n-expr into e-pop
      finally (return-from init-pop e-pop))))

(defun get_fitness (critter)
  "Get the fitness score for a given critter"
  (let ((fitness 0)
        (test-delta 0))
    (loop for test in samples
      do (setq x (nth 0 test))
         (setq y (nth 1 test))
         (setq z (nth 2 test))
         (setf test-delta (abs (- (nth 3 test) (eval critter))))
      sum test-delta into fitness
      finally (return-from get_fitness fitness))))

(defun count_lists (target)
  (print "count_lists")
  (let ((counter 1)
        (c_list 0))
    (loop for el in target
      do (if (listp el)
            (setq c_list 1)
          (setq c_list 0))
          (print el)
          (print counter)
          (setq counter (+ counter c_list))
      finally (return-from count_lists counter))))

(defun nth_list (nth_val target)
  ;; (print "nth_val")
  ;; (print nth_val)
  (if (= nth_val 1)
      (return-from nth_list target))
  (let ((counter 1))
    (loop for el in target
      do  (if (listp el)
            (setq counter (1+ counter)))
          (if (= counter nth_val)
            (return-from nth_list el)))))

(defun random_from_cdr (subject)
  "Get a random element that is not the first in the list"
  (let ((nth_val (random (length subject))))
    (if (<= nth_val 0)
      (setf nth_val (1+ nth_val)))
    (nth nth_val subject)))

(defun test_cross (parent_1 parent_2)
  ;; (print "test_cross")
  (let ((n_sub (random_from_cdr parent_2))
        (nth_val (random (length parent_1)))
        (n_kid (copy-tree parent_1)))
      (if (<= nth_val 0)
        (setf nth_val (1+ nth_val)))
      (setf (nth nth_val n_kid) n_sub)
      n_kid))

(defun deep-copy (source)
  "Get a deep copy of the given structure"
  (write "deep-copy")
  (cond ((not (listp source)) source)
        (T (let ((left (deep-copy (car source)))
                 (right (deep-copy (cdr source))))
              (cons left right)))))

(defun cross (source spot n_sub)
  "Cross the source expression with a new sub-expression at the spot"
  (cond ((not (listp source)) source)
        ((eq source spot) n_sub)
        (T (let ((left (cross (car source) spot n_sub))
                 (right (cross (cdr source) spot n_sub)))
              (cons left right)))))

(defun get_crossed (parent_1 parent_2)
  ;; (print "get_crossed")
  ;; (print parent_1)
  ;; (print parent_2)
  (let ((pt_1 (random_tree_cell parent_1))
        (pt_2 (random_tree_cell parent_2)))
    ;; (print pt_1)
    ;; (print pt_2)
    (make_kid parent_1 pt_1 pt_2)))
  ;; (test_cross parent_1 parent_2))

(defun test-cross ()
  "test function to view the crossover of random expressions"
  (let* ((pop-test (init-pop))
         (parent_1 (nth (random (length pop-test)) pop-test))
         (parent_2 (nth (random (length pop-test)) pop-test)))
      (print (crossover parent_1 parent_2))))

(defun new_kid (parent_1 parent_2)
  "Create a new kid and randomly apply mutation"
  (let ((kid (get_crossed parent_1 parent_2)))
    (setf kid (mutate_critter kid))
    kid))

(defun display_pop_extremes (scored_pop)
  (print "Best/Worst")
  (print (car scored_pop))
  (print (last scored_pop)))

(defun display_gen_average (scored_pop)
  (print "Average Score")
  (let ((summed_scores 0)
        (expr_score 0))
    (loop for expr in scored_pop
      do (setq expr_score (nth 0 expr))
      sum expr_score into summed_scores
      finally (print (handler-case ; if score is too high for float representation, leave as fraction
                        (float (/ summed_scores (length scored_pop)))
                        (floating-point-overflow () (/ summed_scores (length scored_pop))))))))

(defun test_fun ()
  "Test version of genetic programming main function"
  (let ((pop-curr (init-pop))
        (pop-next '())
        (parent_1 nil)
        (parent_2 nil)
        (pop-scored nil)
        (pop-top nil)
        (most-fit '())
        (generation-count 0))
    (loop while (< generation-count generations)
      do (incf generation-count)  ; increment generation counter
         (setq pop-scored (safe_sort_scored_pop (pop_fitness pop-curr)))  ; score population, sort by scores
         (push (car pop-scored) most-fit) ; save most fit
         (setq pop-top (get_pop_from_scored pop-scored))  ; take top 50% 
         (format T "Generation ~D" generation-count)
         (display_pop_extremes pop-scored)
         (display_gen_average pop-scored)
         (setq pop-top (subseq pop-top 0 (floor (length pop-top) 2)))
         (print "expressions")
         (print pop-scored)
         (setq pop-next '())  ; start with empty new pop
         (loop while (< (+ (length pop-next) 1) pop-size)
            do  (setf parent_1 (nth (random (length pop-top)) pop-top)) ; create two children 
                (setf parent_2 (nth (random (length pop-top)) pop-top)) ; parents from top 25%
                (loop while (equal parent_1 parent_2)   ; ensure that the parents are not the same
                  do (setq parent_2 (nth (random (length pop-top)) pop-top)))
                (push (new_kid parent_1 parent_2) pop-next) ; add kids to next pop
                (push (new_kid parent_2 parent_1) pop-next))
          (setq pop-curr pop-next)) ; set next pop as current pop
      (print "best expressions:")
      (print most-fit)))
