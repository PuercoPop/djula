;;;; Copyright 2015 Javier Olaechea <pirata@gmail.com>

(defpackage #:djula/iterator
  (:use #:cl)
  (:shadow #:step #:count)
  (:documentation "A Python-inspired iterator protocol"))
(in-package #:djula/iterator)


;; The Core of the Protocol
(define-condition stop-iteration (condition)
  ()
  (:documentation "Signals that the iterator has no more values to provide."))

(defgeneric iterator (object)
  (:documentation "Return the iterator for the OBJECT."))

(defgeneric next (iterator)
  (:documentation "Retrieve the next batch of values from the ITERATOR."))

(defgeneric iteratop (object)
  (:documentation "Is OBJECT an iterator?")
  (:method (object) nil))


(defun parse-clause (clause)
  "Split the CLAUSE into a cons cell where the `car' is the lists of names and
  the `cdr' the iterator."
  (let ((in-marker (position :in clause :from-end t)))
    ;; Todo, should the cdr be evaluated? For the time being assume that an
    ;; iterator is placed in the tail position
    (cons (subseq clause 0 in-marker)
          (car (subseq clause (1+ in-marker))))))

(defun parse-clauses (clauses)
  "Prepare the clauses for consumption by the `for' macro"
  (loop :for clause :in clauses
        :collect (parse-clause clause)))

(defun name-list (parsed-clauses)
  "Return a list of the names. To be sued to 'let-initialize' the names"
  (alexandria:flatten (mapcar #'car parsed-clauses)))

(defmacro for (clauses &body body)
  "Successively evaluate the body in the context of the variables
  provided. Iteration stops when one of the iterators in clauses signals the
  `stop-iteration' condition.

Each clause is of the form (names+ :in iterable). Where iterable is an object that
responds to the iterator method.
"
  ;; XXX: Should syntax sugar for built-in iterables be added?.
  ;; Implementation:
  ;;
  ;; 1) Create names contained in clauses
  ;; 2) Fetch the values by binding the names and multiple-value-call the iterator. Kept in an alist
  ;; 3) If stop-iteration is signaled go to the cleanup form, #6.
  ;; 4) Evaluate the body
  ;; 5) Go to 2)
  ;; 6) Clean up
  (let
      ((parsed-clauses (parse-clauses clauses)))
    `(let ,(name-list parsed-clauses)
       (tagbody
        step
          (handler-case 
              (progn
                ,@(loop :for parsed-clause :in parsed-clauses
                        ;; Should I quote/list this?
                        :collect `(multiple-value-setq ,(car parsed-clause)
                                    (multiple-value-call #'next ,(cdr parsed-clause)))))
            (stop-iteration ()
              (go end)))
        body
          ,@body
          (go step)
        end))))

#+example
(let ((counter (make-instance 'counter :start-from 0 :upto 8)))
  (for ((count :in counter))
    (format t "Current Count: ~A~%" count)))

#+desired-expasion
(let ((counter (make-instance 'counter :start-from 0 :upto 8)))
  (let (count)
    (tagbody
     step
       (handler-case
           (multiple-value-setq (count) (multiple-value-call #'next counter))
         (stop-iteration ()
           (go end)))
     body
       (format t "Current Count: ~A~%" count)
       (go step)
     end)))


;; Built-in iterators
;; (defun iterable-list (iterable)
;;   "Convert ITERABLE to an object that can be iterated in :parsed-for"
;;   (typecase iterable
;;     (array (coerce iterable 'list))
;;     (hash-table (alexandria:hash-table-alist iterable))
;;     (sequence iterable)
;;     (t (error "Cannot iterate on ~A" iterable))))

(defparameter +do-not-stop+ (gensym "DO-NOT-STOP")
  "A sigil that signals that there is no stop condition in the counter ")

(defclass counter ()
  ((start :initarg :start-from :initform 0
          :documentation "Where to start the count from")
   (step :initarg :step :initform 1 :reader step
         :documentation "Determines by how much is the count incremented.")
   (stop :initarg :upto :reader stop :initform +do-not-stop+
         :documentation "The number that when reached, or surpassed, the
         iterator should stop.")
   (%count :accessor count :initarg :start-from :initform 0
           :documentation "Keeps the current count"))
  (:documentation "An iterator that counts from `start' to `stop' using `step' as the _increment_."))

(defmethod print-object ((obj counter) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "with current count ~A and increment of ~A" (count obj) (step obj))))

(defmethod iterator ((obj counter))
  t)

(defmethod iterator ((counter counter))
  counter)

(defmethod next ((counter counter))
  "Update the current `count' and return it. If `stop' has been reached signal
stop iteration."
  (with-accessors
        ((count count)
         (step step)
         (stop stop)) counter
    (setf count (+ count step))
    (if (or (eql stop +do-not-stop+)
            (>= count stop))
        (signal 'stop-iteration)
        count)))


#| 

Variable	Description
forloop.counter	The current iteration of the loop (1-indexed)
forloop.counter0	The current iteration of the loop (0-indexed)
forloop.revcounter	The number of iterations from the end of the loop (1-indexed)
forloop.revcounter0	The number of iterations from the end of the loop (0-indexed)
forloop.first	True if this is the first time through the loop
forloop.last	True if this is the last time through the loop
forloop.parentloop	For nested loops, this is the loop surrounding the current one

|#
