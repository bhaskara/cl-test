(defpackage :cl-test
  (:use :cl :cl-utils)
  (:export :check-true :check-equal :check-error :check-randomized :load-relative :*lhs* :*rhs*))

(in-package :cl-test)

(defvar *out* t)
(defvars *lhs* *rhs*)

(defmacro check-true (form)
  `(progn
     (setq *lhs* ,form)
     (unless *lhs*
       (format *out* "~&~a asserted true but equalled nil" ',form))))

(defmacro check-equal (f1 f2 &optional test)
  (orf test #'equal)
  `(progn
     (setq *lhs* ,f1 *rhs* ,f2)
     (unless (funcall ,test *lhs* *rhs*)
       (format *out* "~&~a equalled ~a, and ~a equalled ~a, which did not satisfy equality test ~a" ',f1 *lhs* ',f2 *rhs* ,test))))
       

(defmacro check-error (form &optional (error-type 'error))
  (with-gensyms (c)
    `(handler-case
	 (progn
	   (setq *lhs* ,form)
	   (format *out* "~&~a returned ~a instead of throwing an error of type ~a" 
		   ',form *lhs* ',error-type))
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
       (setq *lhs* (generate-histogram ,form ,num-trials :test ,test :normalize t)
	     *rhs* ,dist)
       (let ((,distance (l1-distance *lhs* *rhs* :test ,test)))
	 (when (> ,distance ,tol)
	   (format *out* "~&Distribution of ~a was~& ~a~&which is at distance ~a > ~a from expected distribution:~& ~a~& ~:[~;(expected to happen ~:*~a of the time)~]."
		   ',form *lhs* ,distance ,tol *rhs* ,conf))))))

(defun load-relative (pathname)
  "load-relative PATHNAME.  For example, if /foo/baz.lisp contains (load-relative qux/oof.lisp), this will result in loading /foo/qux/oof.lisp"
  (load (merge-pathnames pathname *load-pathname*)))