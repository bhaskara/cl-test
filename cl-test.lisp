(defpackage :cl-test
  (:use :cl :cl-utils)
  (:export :check-true :check-equal :check-error :check-randomized :load-relative :*lhs* :*rhs* :*print-forms*
	   :srs :rrs :*saved-random-state* :*break-on-errors*))

(in-package :cl-test)

(defvar *out* t)
(defvars *lhs* *rhs*)
(defvar *print-forms* nil "Whether to print out what's being tested before testing it")
(defvar *break-on-errors* t "Whether to signal an error upon a failed test, or just print a message")

(defun signal-error (str &rest args)
  (if *break-on-errors* 
      (apply #'cerror "Continue with remaining tests" (first args) (rest args))
      (apply #'force-format str args)))

(defmacro check-true (form)
  `(progn
     (setq *lhs* ,form)
     (unless *lhs*
       (signal-error *out* "~&~a asserted true but equalled nil" ',form))))

(defmacro check-equal (f1 f2 &optional test)
  (orf test #'equal)
  `(progn
     (when *print-forms*
       (force-format *out* "~&Checking if ~a equals ~a..." ',f1 ',f2))
     (setq *lhs* ,f1 *rhs* ,f2)
     (unless (funcall ,test *lhs* *rhs*)
       (signal-error *out* "~&LHS ~a~& equalled ~a~&and RHS ~a~& equalled ~a~&which did not satisfy equality test ~a" ',f1 *lhs* ',f2 *rhs* ,test))
     (when *print-forms* (force-format *out* " ok"))))
       

(defmacro check-error (form &optional (error-type 'error))
  (with-gensyms (c)
    `(handler-case
	 (progn
	   (when *print-forms*
	     (force-format *out* "~&Checking if ~a signals error of type ~a" ',form ',error-type))
	   (setq *lhs* ,form)
	   (signal-error *out* "~&~a returned ~a instead of throwing an error of type ~a" 
		   ',form *lhs* ',error-type)
	   (when *print-forms* (force-format *out* " ok")))
       (,error-type (,c)
	 (declare (ignorable ,c))))))

(defun l1-distance (d1 d2 &key test)
  (orf test #'equal)
  (let ((dist 0))
    (dolist (pair d1 dist)
      (destructuring-bind (x . p) pair
	(let ((p2 (aif (assoc x d2 :test test) (cdr it) 0.0)))
	  (when (> p p2) (incf dist (* 2 (- p p2)))))))))
  

(defmacro check-randomized (num-trials tol form dist &key test conf)
  (orf test #'equal)
  (when (consp (first dist)) (setq dist `',dist))
  (with-gensyms (distance)
    `(progn
       (when *print-forms*
	 (force-format *out* "~&Checking distribution (~a trials) of ~a" ,num-trials ',form))
       (setq *lhs* (generate-histogram ,form ,num-trials :test ,test :normalize t)
	     *rhs* ,dist)
       (let ((,distance (l1-distance *lhs* *rhs* :test ,test)))
	 (when (> ,distance ,tol)
	   (signal-error *out* "~&Distribution of ~a was~& ~a~&which is at distance ~a > ~a from expected distribution:~& ~a~& ~:[~;(expected to happen ~:*~a of the time)~]."
		   ',form *lhs* ,distance ,tol *rhs* ,conf))
	 (when *print-forms* (force-format *out* " ok"))))))

(defun load-relative (pathname)
  "load-relative PATHNAME.  For example, if /foo/baz.lisp contains (load-relative qux/oof.lisp), this will result in loading /foo/qux/oof.lisp"
  (load (merge-pathnames pathname *load-pathname*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random states
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *saved-random-state*)

(defun srs ()
  "save the current random state, so it can be retrieved later by rrs.  Given a piece of randomized code FOO (that doesn't do things like check the time), calling (progn (srs) (foo)), and later (assuming no intervening calls to srs), (progn (rrs) (foo)) will yield the same result."
  (setf *saved-random-state* (make-random-state))
  (values))

(defun rrs ()
  "see srs"
  (setf *random-state* (make-random-state *saved-random-state*))
  (values))
