;;; These are the examples given in Problem 3 expressed in Common Lisp, which
;;; is the model of the language specified in Problem 3.
;;;
;;; To see them work, install a Common Lisp implementation like [GNU
;;; CLISP](http://www.clisp.org), enter the REPL and then load this file by
;;; evaluating `(load "test.cl")`.  Once it is loaded, just evaluate one of
;;; `test1` through `test5` to see the result.

(setf test1
  (progn (defun id (x) x)
         (let ((id 2))
           id ) ) )

(setf test2
  (progn (defun id (x) x)
         (let ((id 2))
           (id 1) ) ) )

(setf test3
  (progn (defun id (x) x)
         (defun id (x) 2)
         (id 1) ) )

(setf test4
  (progn (defun id (x) x)
         (let ((id 2))
           (funcall (function id) 1) ) ) )

(defun double (x) (+ x x))

(defun twice (f)
  (progn (defun f2 (x)
           (funcall f (funcall f x)) )
         (function f2) ) )

(setf test5
  (let ((y 1))
    (progn (defun f (x) (+ (double x) y))
           (let ((g (twice (function f))))
             (funcall g 1) ) ) ) )

(setf tests
  (and (= test1 2)
       (= test2 1)
       (= test3 2)
       (= test4 1)
       (= test5 7) ) )

