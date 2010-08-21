					;The Z system <--- Z???
;testtree.l
;Let's set up a function tree and evaluate it

;desired results
(setq f-g (list '(1 2) '(2 3) '(3 4) '(4 5) '(5 6) '(6 7) '(7 8)))

;the function we want
(defun want-func(x)
  (+ x 1))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A set of general math routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun range (s e)
  (unless (= s e)
      (cons s  (range (+ 1 s) e))))


(defun flatten (x)
  "descend into the supplied list until an atom is hit.
append the atom to the flattened rest"
  (if (endp x)
      x
    (if (atom (car x ))
	(append (list (car x)) (flatten (cdr x)))
      (append (flatten (car x)) (flatten (cdr x ))))))

;squares x
(defun sqr (x) (* x x))

;Sum the entire list
(defun sum-list(l)
  (reduce '+ l))

;(a + b) / 2
(defun average (a b) ( / (+ a b) 2))

;;;gets the average of a list
(defun average-list (l) 
  (let ((newlist (flatten l)))
    (/ (sum-list newlist)
       (list-length l))))

;;;component-wise addition I think...
;;TODO: Rewrite as one of the map* family
(defun addlists (x y)
 (setq retval nil)
  (loop 	
   (setq retval 
	 (append (list (+ (car x) (car y))) retval))
     (setf x (cdr x))
     (setf y (cdr y))	
     (when (endp  x) (return)))
  retval)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A set of list routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;exchanges the n'th element of a and b
;; Returns the modified a,b in a list
(defun exchange(n a b)
"Creates new values and swaps."
  (let ((temp (nth n b))
	(newa a)
	(newb b))	
    (setf (nth n newb) (nth n a))
    (setf (nth n newa) (nth n b))
    (list newa newb)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A set of fuzzy routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;fuzzy closeness is needed. [0, 1] 
;TODO: Check if this can be Reduced.
(defun fuzzy-equal (a b)
  (cond 
    ((and (numberp a) (numberp b))	;If both are numbers
     (fuzzy-close a b))
    ((and (listp a) (listp b))			;If both are lists
     (fuzzy-equal-lists a b))
    (t nil)))								;Otherwise nil out.


(defun euclidian (x1 x2)
  "euclidean distance"
  (sqrt 
   (reduce '+ 
	   (map 'list 
		#'(lambda (a b) (sqr (-  a  b)))
		x1
		x2))))

(defun distance (a b)
  "Standard 1-D euclidean distance"
  (sqrt (sqr (- a  b))))

;degree of equality of the two trees
;Will recurse.  ;;TODO: But doesn't!!! dun dun dun
(defun fuzzy-equal-trees (a b)
  (if (or 
       (not (equal (length a) (length b)))	;Length must be equal 
       (not (and (listp a) (listp b)))) 	;Both must be lists
      nil				      	;fails if condition is not met
    (progn       
	(average-list (mapcar 'fuzzy-equal a  b)))))

;;;Dissertating on the trees equality - 
;; Shape equality
;; shape-value equality
;; flattened, ordered, value equality 		
;; flattened-unordered equality

;;honking slow
(defun generic-le (a b)
  (let ((str-a (format nil "~a" a))
	(str-b (format nil "~a" b)))
    ( numberp (STRING< str-a str-b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A set of random routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;where n is the number of elements, m is the range
;;TODO: WHO AM I?
(defun generate-rand-list (n m)
    (setq temp nil)
    (loop      
     (setq temp (append (list (random m (make-random-state t))) temp))
     (setq n (- n 1))
     (when (= n 0) (return)))
    temp)

(make-random-state t)
(defun getrand (max)
  "Returns between 0 and max."
	(random max))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The actual GP routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun check-similarity ()
  (let 
      ((i 1) 
       (dummy-tree (new-tree 4))
       test-value)    
  (loop 
   (setq x i)
   (setq test-value (eval-tree dummy-tree))
   (format t "~a\n" test-value)
   (setq i (+ i 1))
   (when (eq i 100) (return)))))



;these are our operations
;binary operations
(setq bin-op-list '(+ - * /))
;unary operations
(setq un-op-list '(1+ sin cos log exp abs sqr sqrt))

;sets up a list with the n'th binary operation and params 
(defun bin-term-create (n p1 p2)
  (list (nth n bin-op-list) p1 p2))

;Like bin-term-create, but with unary operations
(defun un-term-create (n p1)
  (list (nth n un-op-list) p1))


(defmacro create-random-unary (var)
  `(list (nth (getrand (length un-op-list)) un-op-list) ,var))
(defmacro create-random-bin (var1 var2)
  `(list (nth (getrand (length bin-op-list)) bin-op-list) ,var1 ,var2))


;creates a random operation with unevaluated params p1, and p2
(defun rand-tree-node ()
  (if (shake-p)
      (create-random-bin 'x 'x)
    (create-random-unary 'x)))

;Sets the left leaf of parent to 'leaf
(defun set-left-leaf (parent leaf)
  (setf (nth 1 parent) leaf))

;Sets the right leaf of parent to 'leaf(
(defun set-right-leaf (parent leaf)
  (setf (nth 2 parent) leaf))


(defun shake-p ()
  "Shakes according to a specific predicate"
  (> (getrand 10) 7))

;;;uses x as an independant variable
;;Adds depth d to node node. Recursive
(defun make-tree (node depth)
  (if (eq (length node) 3) ;Is it of the form (op arg1 arg2)
      ;;Can we go deeper?
      (if (eq depth 0)
	  ;;this case is simple. Terminals only
	  (progn (setf (nth 1 node) 'x)      
		 (setf (nth 2 node) 'x))
	
	;;This case is the interesting one
	(progn
	  ;;The left node
	  (if (shake-p)
	      (setf (nth 1 node) 'x)	    
	    (setf (nth 1 node) (make-tree (rand-tree-node) (- depth 1))))	  
	  ;;the right node
	  (if (shake-p)
	      (setf (nth 2 node) 'x)
	    (setf (nth 2 node) (make-tree (rand-tree-node) (- depth 1))))))    

    ;;Is it of the form (op arg1)
    (progn 
      ;;Can we go deeper?
      (if (eq depth 0)
	  ;;this case is simple. Terminals only
	  (setf (nth 1 node) 'x)
	;;This case is the interesting one
	(progn
	  ;;The center node
	  (if (shake-p) ;;seventy percent.
	      (setf (nth 1 node) 'x)
	    (setf (nth 1 node) (make-tree (rand-tree-node) (- depth 1))))))))
  node);end of function- returns node

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The interface for Lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun new-tree ( x )
  (make-tree (rand-tree-node) x))

(defmacro generate-lambda-tree(n)
  "Generates a tree and returns the lambda of its function"
  `(lambda (X)
       ,(new-tree n)))

;;Turns a tree into a lambda
(defmacro tree-to-lambda(tree)
  "Takes a tree and turns it into the lambda of its function"
  (eval ``(lambda (X) 
     ,,tree)))

(defun make-trees (n)
  "Makes n trees"
  (if (> n 0) 
      (cons (new-tree 2) (make-trees (- n 1)))))

(defmacro make-functions-macro (tree)
  `(if ,tree
      (cons 
       (tree-to-lambda (car ,tree))
       (make-functions-macro (cdr ,tree)))))

(defun eval-trees (tau-list inputs)
  (if tau-list
      (cons
       (mapcar (car tau-list) inputs)
       (eval-trees (cdr tau-list) inputs))))

(defun rotate (&rest list-of-lists)
  "Suppose each list was a row in a matrix. Then rotate swaps the rows
and columns"
  (apply #'mapcar #'list list-of-lists))

(setq others '(((1 2 3) (4 5 6))
	       ('a 'b) 
	       (11 12)
	       (8 9)))






(setf dummy2 (make-trees 1))
(setf dummy3 (make-functions-macro dummy2))
;(setq samples
      (let* ((treelist (make-trees 2))
	     (taus (make-functions-macro treelist))
	     (tau-evaled (eval-trees taus (range 2 10))))
	(rotate (list treelist taus tau-evaled))))

(setq sample 
      (let* ((trees (make-trees 2))
	     (tree-bindings (bind-trees trees))
	     (evaluated-trees (eval-fitness tree-bindings))))
      (list (trees tree-bindings evaluated-trees)))
      
;(car samples)

;; (setq treelist2 (eval `(make-trees 2)))
;; (setq taus2 (make-functions-macro treelist2))
;; (setq tau-evaled2 (eval-trees taus2 (range 2 10)))
;; (rotate (list treelist2 taus2 tau-evaled2))

;(make-trees 2)
;(macroexpand-1 '(make-trees 2))
;(setq samples (return-sample 2))

;(rotate samples)



;; (setq a-result (mapcar (tree-to-lambda treea) (range 2 10)))
;; (setq b-result (mapcar (tree-to-lambda treeb) (range 2 10)))
;; (setq c-result (mapcar (tree-to-lambda treec) (range 2 10)))

;; (setq want-result (mapcar 'want-func (range 2 10)))

;; (euclidian want-result a-result)
;; (euclidian want-result b-result)
;; (euclidian want-result c-result)


 