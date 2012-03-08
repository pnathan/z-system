;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Paul Nathan
;;;; 2005-2012
;;;; Z system. Genetic algorithm playground
;;;; The Z system attempts to 'breed' a matching function to a specified set of values
;;;; Credit is due to Svante and Rainer Joswig for extensive assistence to a newbie.

;the function we want
(defun want-func (x)
  "Goal function"
  (+ x 1))


(defun range (start end)
  (assert (<= start end))
  (loop for n from start below end
        collect n))

(defvar *test-input-output* (mapcar #'(lambda (x)
					(list x (want-func x)))
				    (range 2 100))
  "The desired results")


(defvar *population* '()
  "The list of functions under examination")

(defvar *next-gen*
  "the next generation of population members")

(defun expected-inputs ()
  "List of the expected inputs"
  (mapcar #'car *test-input-output*))

(defun expected-outputs ()
  "List of the expected outputs"
  (mapcar #'cadr *test-input-output*))


(defun unzip (seq)
  "Takes an even-length list and breaks it apart by evens/odd index"
  (let ((oddresult '())
	(evenresult '()))
    (loop for n from 0 to (- (length seq) 1) do
	  (if (oddp n)
	      (push (nth n seq) oddresult)
	    (push (nth n seq) evenresult)))
    (list (reverse oddresult) (reverse evenresult))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A set of general list/tree routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun range (start end)
  (unless (<= start end)
    (error "Start (~a) must be smaller than the end (~a)" start end))
  (loop for n from start below end
        collect n))
(defun flatten (tree)
  "descend into the supplied list until an atom is hit.
append the atom to the flattened rest"
  (if (endp tree)
      x
    (if (atom (car tree ))
	(append (list (car tree)) (flatten (cdr tree)))
      (append (flatten (car tree)) (flatten (cdr tree ))))))

(defmacro swap (a b)
  ;;Parallel setf
  `(psetf ,a ,b
          ,b ,a))

(defun exchange (index list-a list-b)
  "Conses two new lists and swaps their respective elements at index."
  (let ((new-a (copy-list list-a))
        (new-b (copy-list list-b)))
    (swap (nth index new-a)
          (nth index new-b))
    (list new-a new-b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Give the difference between two lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sum-f-diff (f list1 list2)
  "Sums the differences between components of lists. Diff is computed
by the function f"
  (handler-case
      (reduce #'+
	      (mapcar
	       f
	       list1 list2))
    (floating-point-overflow () 1e19)))	; It's huge. Just make it huge
					; and run

(defun sum-sq-diff (list1 list2)
  (sum-f-diff
   #'(lambda (x y)
       (square (- x y)))
   list1 list2))

(defun sum-abs-diff (list1 list2)
  (sum-f-diff
	   #'(lambda (x y)
	       (abs (- x y)))
	   list1 list2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;A set of general math routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun square (x)
  "squares x"
  (* x x))

(defun sum-list(l)
  "Sum the entire list"
  (reduce '+ l))


(defun euclidean (x1 x2)
  "euclidean distance"
  ;; Meditate on the binary tree possibilities here.
  (sqrt
   (reduce '+
	   (mapcar
		#'(lambda (a b)
		    (square
		     (-  a  b)))
		x1
		x2))))


;(a + b) / 2
(defun average (a b)
  ( / (+ a b) 2))


(defun average-list (l)
  "gets the average of a list"
  (/ (sum-list l)
       (list-length l)))

;;;component-wise addition
(defun addlists (x y)
  (mapcar #'+ x y))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A set of routines to do an 'imprecise' analysis of the trees.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;fuzzy closeness is needed. [0, 1]
;TODO: Check if this can be Reduced.
(defun fuzzy-equal (a b)
  (cond
    ((and (numberp a) (numberp b))	;If both are numbers
     (fuzzy-close a b))
    ((and (listp a) (listp b))	        ;If both are lists
     (fuzzy-equal-lists a b))
    (t nil)))				;otherwise nil out.


;degree of equality of the two trees
;Will recurse.  ;;TODO: But doesn't!!! dun dun dun
(defun fuzzy-equal-trees (a b)
  (if (or
       (not (equal (length a) (length b)))	;Length must be equal
       (not (and (listp a) (listp b)))) 	;Both must be lists
      nil				      	;fails if condition is not met
    (progn
	(average-list (mapcar 'fuzzy-equal a  b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Dissertating on the trees equality -
;; Shape equality
;; shape-value equality
;; flattened, ordered, value equality
;; flattened-unordered equality

;;
(defun generic-le (a b)
  "Compares using the string representation of the objects.  Primarily
useful when some sort, any sort, is wanted"
  (let ((str-a (format nil "~a" a))
	(str-b (format nil "~a" b)))
    ( numberp (STRING< str-a str-b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A set of randomness routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun generate-random-list (length max)
  (loop for n from 0 below length
        for x = (random max)
        collect x))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Function construction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
(defparameter *expand-probability* 0.7
  "Parameter of random expansion")

(defun shake-p ()
  "General dice roll for determining whether something should be
expanded."
  (> (random 1.0) *expand-probability*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;these are the operations we can use in our functions
;binary operations
(defvar *binary-operator-list* '(+ - * /))
;unary operations
(defvar *unary-operator-list* '(1+ sin cos log exp abs square sqrt))


(defun create-binary-term (n p1 p2)
  "Creates a list with the n'th binary operation and parameters p1 and p2."
  (list (nth n *binary-operator-list*) p1 p2))

(defun create-unary-term (n p1)
  "Creates a list with the n'th unary operation and parameter p1"
  (list (nth n *unary-operator-list*) p1))

(defun create-random-unary (var)
  (create-unary-term (random (length *unary-operator-list*)) var))

(defun create-random-binary (var1 var2)
  (create-binary-term (random (length *binary-operator-list*)) var1 var2))


;creates a random operation with unevaluated params p1, and p2
(defun rand-tree-node ()
  (if (shake-p)
      (create-random-binary 'x 'x)
    (create-random-unary 'x)))

;Sets the left leaf of parent to 'leaf
(defun set-left-leaf (parent leaf)
  (setf (nth 1 parent) leaf))

;Sets the right leaf of parent to 'leaf
(defun set-right-leaf (parent leaf)
  (setf (nth 2 parent) leaf))

(defun make-tree (node depth)
  (if (eq (length node) 3) ;Is it of the form (op arg1 arg2)
      ;;Can we go deeper?
      (if (eq depth 0)
          ;;this case is simple. Terminals only
          (setf (nth 1 node) 'x
                (nth 2 node) 'x)
	;;This case is the interesting one
	(mapcar (lambda (leaf-setter)
		  (funcall leaf-setter node
			   (if (shake-p)
			       'x
			     (make-tree (rand-tree-node) (- depth 1)))))
		'(set-left-leaf  set-right-leaf)))
      ;;Is it of the form (op arg1)
      ;;Can we go deeper?
      (if (eq depth 0)
          ;;this case is simple. Terminals only
          (set-left-leaf node 'x)
          ;;This case is the interesting one
          (set-left-leaf node
                         (if (shake-p)
                             'x
                             (make-tree (rand-tree-node) (- depth 1))))))
  node);end of function- returns node


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Build a evaluation framework
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun new-tree ( x )
  (make-tree (rand-tree-node) x))


(defun make-tree-lambda (depth)
  (list 'lambda '(x)
        (new-tree depth)))


(defun make-tree-function (lambda-tree)
  (eval lambda-tree))

(defun eval-tree-function (lambda-tree val)
  "Evaluates a tree function with val"
  (funcall lambda-tree val))

(defclass genetic-function ()
  ((compiled
   :accessor compiled)
   (representation
    :accessor representation)
   (fitness
    :accessor fitness)))

(defmethod print-object ((obj genetic-function) (stream t))
  (format stream "<#generic-function ~a ~a>"   (fitness obj) (representation obj)))

(defun equal-gf (a b)
  (equal
   (representation a)
   (representation b)))

(defun construct-functions (depth)
  "Generates a Genetic Function object. Contains both the lambda and the compiled function"
  (let ((gf (make-instance 'genetic-function)))
    (setf (representation gf) (make-tree-lambda depth))
    (setf (compiled gf) (make-tree-function (representation gf)))
    gf))

(defmethod call ((obj genetic-function) arg)
  "Given object and input, generate f(x)"
  (handler-case
      (funcall (compiled obj) arg)
    (division-by-zero () 1e9) ;;Ideally NaN
    (floating-point-overflow () 1e9)
    (floating-point-underflow () 1e9)))


(defun execute-list (obj input-list)
  "Given obj and a list of inputs, generate list of outputs f(x[i])"
  (mapcar #'(lambda (x) (call obj x))
	  input-list))

(defun assign-fitness (obj)
  (setf (fitness obj)
	(sum-sq-diff (execute-list obj (expected-inputs))
		     (expected-outputs)))
  obj)

(defun compare-genetic (a b &optional (sort-function '<))
  (funcall sort-function (fitness a) (fitness b)))

(defun return-top-function (seq-objs)
  (first (sort seq-objs 'compare-genetic)))

(defun build-function-list (depth count)
  "Generates count function objects of tree-depth depth"
  (mapcar
   #'(lambda (x)
       (assign-fitness (construct-functions depth)))
   (range 0 count)))

(defun best-half (list-of-objs)
  "Takes the best half of the objects"
  (butlast
   (sort list-of-objs 'compare-genetic)
   (truncate (/ (length list-of-objs ) 2))))

(defun breed (parent1 parent2)
  "Breeds the two objects and takes the offspring"
  (let
      ((gf (make-instance 'genetic-function)))
    (setf (representation gf)
	  (breed-trees
	   (third (representation parent1) )
	   (third (representation parent2))))
    (setf (compiled gf) (make-tree-function (representation gf)))
    gf))

(defun breed-trees-hack (parent1 parent2)
  (if (and
       (consp parent1)
       (consp parent2))
      (if (or
	   (= (length parent2) 2)
	   (= (length parent1) 2))
	  ;; either the op or the arg
	  (if (and
	       (consp (third parent2))
	       (consp (third parent1)))
	      (progn (format t "swappin")
	      (exchange (random 2)
			(breed-trees (second parent1) (second parent2))
			(breed-trees (second parent1) (second parent2))))
	    )
	;;randomly pick op, arg1, arg2 to exchange
	(exchange (random 3) parent1 parent2))))

(defun breed-trees (parent-lambda-1 parent-lambda-2)
  "Takes (lambda (x) <list>"

  ;;don't recurse into a nil
  (if (and
	 (consp parent-lambda-1)
	 (consp parent-lambda-2))
      (let ((parent1 (third parent-lambda-1))
	    (parent2 (third parent-lambda-2)))

	(cond
	 ;;If one of the two has form (op arg)
	 ((or (= (length parent1) 2)
	      (= (length parent2) 2))
	  (progn
	    (format t "one of us are op arg")
	    ;;swap the first arg
	    (if (and (consp (second parent1))
		     (consp (second parent2)))
		(exchange 1 parent1 parent2))
	    ;;swap the op
	    (exchange 0 parent1 parent2)))
	 ;;if both have form (op arg1 arg2)
	 ((and (= (length parent1) 3)
	       (= (length parent2) 3))
	  (progn
	    (format t "both are op arg arg")
	    ;;swap the first arg
	    (if (and (consp (second parent1))
		     (consp (second parent2)))
		(exchange 1 parent1 parent2) )
	    ;;swap the second arg
	    (if (and (consp (third parent1))
		     (consp (third parent2)))
		(exchange 2 parent1 parent2)))))

	(exchange 0 parent1 parent2))))



;(setq f  '(LAMBDA (X) (cos (abs X) )) )
;(setq g '(lambda (x) (sin (exp x))))
;(setq g '(LAMBDA (X) (/ (/ X (1+ X)) X)))
;(let ((random (lambda (x) 0))) (breed-trees f g))

(defun truncate-if-odd-length (seq)
  (if (zerop (mod (length seq) 2))
      seq
    (butlast seq)))

(defun breed-list (list-of-objs)
  (mapcar 'assign-fitness
	  (let ((parent1s (first (unzip (truncate-if-odd-length list-of-objs))))
		(parent2s (second (unzip (truncate-if-odd-length list-of-objs)))))
	    (mapcar 'breed parent1s parent2s))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *average-goodness* '())

(defun evolve (generations-left &optional (pop-max 100))
  (loop for i from 1 to generations-left do
	(push (average-list (mapcar 'fitness *population*)) *average-goodness*)
	(setq *next-gen* (breed-list (best-half *population*)))
	(setq *population* (append *population* *next-gen*))
	(if (> (length *population*) pop-max)
	    (setq *population*
		  (butlast *population*
			   (- (length *population*) pop-max))))))

(defun evolve-no-trim (generations-left )
  (loop for i from 1 to generations-left do
	;; set the current average
	(push (average-list (mapcar 'fitness *population*)) *average-goodness*)
	;;do the breeding
	(setq *next-gen* (breed-list (best-half *population*)))
	;;add the bred functions to the population
	(setq *population*
	      (uniquize
	       (append *population* *next-gen*)
	       'equal-gf))))

(defun best-so-far ()
  (first (sort *population* 'compare-genetic)))

(defun uniquize (seq &optional (test-func 'equal))
 (if seq
     (if (not (position (car seq) (cdr seq) :test test-func)) ;if it's unique
	 (cons
	  (car seq)
	  (uniquize (cdr seq) test-func))
       (uniquize (cdr seq) test-func))))


;;; Now for the real work!
(defun seed-population()
  (setf *population* (build-function-list 3 10)))


(defmacro fast-add (seq)
  (reduce #'+ seq))