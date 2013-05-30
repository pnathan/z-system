;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Paul Nathan
;;;; 2005-2013
;;;; Z system. Genetic algorithm playground
;;;; The Z system attempts to 'breed' a matching function to a specified set of values
;;;;
;;;; Credit is due to Svante, Rainer Joswig, and other users of Stack
;;;; Overflow for extensive assistence to a newbie taking on a
;;;; too-hard project.
;;;;
;;;; License: AGPL3.


;;(ql:quickload :alexandria)
(defun shuffle (sequence &key (start 0) end)
  "Returns a random permutation of SEQUENCE bounded by START and END.
Original sequece may be destructively modified, and share storage with
the original one. Signals an error if SEQUENCE is not a proper
sequence."
  (declare (type fixnum start)
           (type (or fixnum null) end))
  (etypecase sequence
    (list
     (let* ((end (or end (list-length sequence)))
            (n (- end start)))
       (do ((tail (nthcdr start sequence) (cdr tail)))
           ((zerop n))
         (rotatef (car tail) (car (nthcdr (random n) tail)))
         (decf n))))
    (vector
     (let ((end (or end (length sequence))))
       (loop for i from start below end
             do (rotatef (aref sequence i)
                         (aref sequence (+ i (random (- end i))))))))
    (sequence
     (let ((end (or end (length sequence))))
       (loop for i from (- end 1) downto start
             do (rotatef (elt sequence i)
                         (elt sequence (+ i (random (- end i)))))))))
  sequence)

(defun range (start end)
  (assert (<= start end))
  (loop for n from start below end
        collect n))

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
    (values new-a new-b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Give the difference between two lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sum-f-diff (f list1 list2)
  "Sums the differences between components of lists. Diff is computed
by the function f"
  (unless (or (position nil list1)
              (position nil list1))

    (handler-case
        (reduce #'+
                (mapcar
                 f
	       list1 list2))
      (floating-point-overflow () nil))))

(defun sum-sq-diff (list1 list2)
  "Sum of the squares of the differences. "
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

(defun shake-p ()
  "General dice roll for determining whether something should be
expanded."
  (> (random 1.0) *expand-probability*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;these are the operations we can use in our functions
;binary operations
(defconstant +binary-operator-list+ '(+ - * /))
;unary operations
(defconstant +unary-operator-list+ '(sin cos log exp abs square sqrt))


(defun create-binary-term (n p1 p2)
  "Creates a list with the n'th binary operation and parameters p1 and p2."
  (list (nth n +binary-operator-list+) p1 p2))

(defun create-unary-term (n p1)
  "Creates a list with the n'th unary operation and parameter p1"
  (list (nth n +unary-operator-list+) p1))

(defun create-random-unary (var)
  (create-unary-term (random (length +unary-operator-list+)) var))

(defun create-random-binary (var1 var2)
  (create-binary-term (random (length +binary-operator-list+)) var1 var2))

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
  "Given object and input, generate f(x).
If an error occurs, :FAIL will be returned instead of a number"
  (let ((result
          (handler-case
              (funcall (compiled obj) arg)
            (division-by-zero () :FAIL) ;;Ideally NaN
            (floating-point-overflow () :FAIL)
            (FLOATING-POINT-INEXACT () :FAIL)
            (floating-point-underflow () :FAIL))))
    (if (complexp result)
        :FAIL
        result)))


(defun execute-list (obj input-list)
  "Given obj and a list of inputs, generate list of outputs f(x[i])"
  (mapcar #'(lambda (x) (call obj x))
	  input-list))

(defun assign-fitness (obj)
  (let ((presented-vector
          (execute-list obj (expected-inputs))))
    (if (position :FAIL presented-vector)
        (setf (fitness obj) :FAIL)
        (setf (fitness obj)
              (sum-sq-diff
               presented-vector
               (expected-outputs))))
    obj))

(defun compare-genetic (a b &optional (sort-function '<))
  (cond ((eq (fitness a) :fail)
         nil)
        ((eq (fitness b) :fail)
         t)

        ((eq (fitness a) nil)
         nil)
        ((eq (fitness b) nil)
         t)

        (t
         (funcall sort-function (fitness a) (fitness b)))))

(defun return-top-function (seq-objs)
  (first (sort seq-objs 'compare-genetic)))

(defun build-function-list (depth count)
  "Generates count function objects of tree-depth depth"
  (mapcar
   #'(lambda (x)
       (assign-fitness (construct-functions depth)))
   (range 0 count)))

(defun best-half (list-of-objs &optional (comparator #'compare-genetic))
  "Takes the best half of the objects"
  (butlast
   (sort list-of-objs comparator)
   (truncate (/ (length list-of-objs ) 2))))

(defun breed (parent1 parent2)
  "Breeds the two objects and takes the offspring"
  (let
      ((gf (make-instance 'genetic-function)))
    (setf (representation gf)
          `(lambda (x)
            ,(breed-functions
              (third (representation parent1) )
              (third (representation parent2))) )
	  )
    (setf (compiled gf) (make-tree-function (representation gf)))
    gf))

(defun available-breeeding-indicies (parent1 parent2)
  "Returns the available indicies for swapping"

  (when (and (consp parent1)
             (consp parent2))
    ;(format t "both of us are lists~&")
    ;;assumption of safely swapping the operation
    (let ((breeding-index-list))

      ;; if the arity is equal, we can swap ops.
      (when (and (= (length parent1)
                    (length parent2)))
     ;   (format t "pushing 0~&")
        (pushnew 0 breeding-index-list))

      ;;If either one of the two has form (op arg)
      (when (or (= (length parent1) 2)
                (= (length parent2) 2))
      ;  (format t "one of us are op arg~&")

        (when (not (equal (second parent1)
                          (second parent2)))
       ;   (format t "pushing 1~&")
          (pushnew 1 breeding-index-list)))


      ;;if both have form (op arg1 arg2)
      (when (and (= (length parent1) 3)
                 (= (length parent2) 3))
        ;(format t "both are op arg arg~&")

        ;(format t "pushing 0~&")
        (pushnew 0 breeding-index-list)

        ;; if the first argument is a form
        ;;swap the first arg
        (when (not (equal (second parent1)
                          (second parent2)))
         ; (format t "pushing 1~&")
          (pushnew 1 breeding-index-list) )

        ;;swap the second arg
        (when (not (equal (third parent1)
                          (third parent2)))
          ;(format t "pushing 2~&")
          (pushnew 2 breeding-index-list)))
      breeding-index-list)))

(defun breed-functions (parent1 parent2)
  "Takes 2 <list>s, where <list> is expected to be an expresion suitable
for being in a form (lambda (x) <list>).
Breeds them together, non-recursively. "
  (let ((indicies (available-breeeding-indicies parent1 parent2)))
    (loop for index in indicies do
      ;; randomly...
      (when (shake-p)

         (multiple-value-bind (p1 p2)
             (exchange index parent1 parent2 )
           (setf parent1 p1
                 parent2 p2)))))
  ;; practically, we only need to nab the first, so values is perfoect here
  (values parent1 parent2))

(defun truncate-if-odd-length (seq)
  (if (zerop (mod (length seq) 2))
      seq
    (butlast seq)))

(defun breed-list (list-of-objs)
  (mapcar 'assign-fitness
	  (let ((parent1s (first (unzip (truncate-if-odd-length list-of-objs))))
		(parent2s (second (unzip (truncate-if-odd-length list-of-objs)))))
	    (mapcar 'breed parent1s parent2s))))


(defun evolve-one-generation (population &optional pop-max)
  "Expects a sorted list of population beginning at best and ending
  with worst. If pop-max is set, then it should be an integer > 0
  which will be the size of the list returned.

The population will be filtered: fitness values of :FAIL will be
outright removed.  Then, the remainder will be divided in half; the
top half will be shuffled, then the top quarter will be bred against
the bottom quarter.. The bottom half will be discarded.

The children will be mixed with the top quarter
"
  (when pop-max
    (assert (integerp pop-max)))
  (let* ((reproducing (remove-if #'(lambda (v)
                                    (eq (fitness v) :FAIL))
                                 population))
         (elite (best-half reproducing))
         (shuffled-elite (shuffle elite))
         (midpoint (truncate (/ (length elite) 2))))
    (sort
     (append
      elite

     (mapcar
      #'assign-fitness
      (mapcar #'breed
              (subseq shuffled-elite 0 midpoint)
              (subseq shuffled-elite midpoint))))
     #'compare-genetic)))





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
(defun seed-population( &optional (population 20) )
  (sort
   (build-function-list 3 population)
   #'compare-genetic))


(defmacro fast-add (seq)
  (reduce #'+ seq))

(defun expected-inputs ()
  "List of the expected inputs"
  (mapcar #'car *test-input-output*))

(defun expected-outputs ()
  "List of the expected outputs"
  (mapcar #'cadr *test-input-output*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; workbench

;the function we want
(defun want-func (x)
  "Goal function"
  (+ x 1))


(defvar *test-input-output*
  (mapcar #'(lambda (x)
              (list x (want-func x)))
          (range 2 100))
  "The desired results")


(defparameter *expand-probability* 0.7
  "Parameter of random expansion")



(defun evolve (population generations-to-spin &optional pop-max)
  (if (= generations-to-spin 0)
      (evolve-one-generation population pop-max)
      (evolve-one-generation
       (evolve population (1- generations-to-spin) pop-max))))


(defun example-run ()
  ;; evolves using a seed population for 10 generations, capping the
  ;; population at 100
  (evolve (seed-population) 10 100))
