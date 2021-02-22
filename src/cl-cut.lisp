(defpackage :cl-cut
  (:documentation
   "Utilities to partially apply functions or expressions in the spirit of
`Scheme SRFI 26: Notation for Specializing Parameters without Currying'
http://srfi.schemers.org/srfi-26/srfi-26.html")
  (:use :cl)
  (:nicknames :cut)
  (:export :cut
           :cut*
           :cute
           :cute*))

(in-package :cl-cut)

(defparameter *slot-name* "<>")
(defparameter *rest-slot-name* "<...>")

(defun symbol-name-eq (form name)
  (and (symbolp form) (string= form name)))

(defun slotp (form)
  (symbol-name-eq form *slot-name*))

(defun rest-slotp (form)
  (symbol-name-eq form *rest-slot-name*))

(defun cut-internal (function arguments funcallp)
  (when (rest-slotp function)
    (error "~a cannot be the first argument of CUT." *rest-slot-name*))
  (let* ((function-slot-as-value (or funcallp (listp function) (slotp function)))
         (slot-names)
         ;; Turn slots into symbols for each argument.
         (fixed-arguments
          (loop :for argument :in (cons function arguments)
             :until (rest-slotp argument)
             :collect
             (cond ((slotp argument)
                    (car (push (gensym "SLOT-") slot-names)))
                   (t argument))))
         ;; Turn the rest-slot into a symbol if it exists
         ;; and make sure it's actually the last argument.
         (rest-argument
          (let ((pos (position-if 'rest-slotp arguments)))
            (cond ((null pos) nil)
                  ((= (1- (length arguments)) pos)
                   (gensym "REST-SLOT-"))
                  (t (error "~a not found at the end of CUT." *rest-slot-name*))))))
    ;; Fill out the lambda list.
    `(lambda (,@(reverse slot-names)
              ,@(when rest-argument `(&rest ,rest-argument)))
       ;; Decide how to call.
       (,@(cond (rest-argument '(apply))
                (function-slot-as-value '(funcall))
                (t nil))
          ;; Decide treatment of the called object.
          ,(cond (rest-argument
                  (if function-slot-as-value
                      (car fixed-arguments)
                      `(function ,(car fixed-arguments))))
                 (function-slot-as-value (car fixed-arguments))
                 (t function))
          ,@(cdr fixed-arguments)
          ,@(when rest-argument `(,rest-argument))))))

(defun cute-internal (function arguments function-as-value)
  (let* ((function-as-value (or function-as-value (listp function)))
         (let-bindings)
         ;; Create let bindings for non-slot arguments.
         (fixed-arguments
          (loop :for argument :in (if function-as-value
                                      (cons function arguments)
                                      arguments)
             :collect
             (if (or (slotp argument) (rest-slotp argument))
                 argument
                 (caar (push (list (gensym "LET-BINDING-") argument)
                             let-bindings))))))
    (setf let-bindings (nreverse let-bindings))
    `(let (,@let-bindings)
       ,(if function-as-value
            (cut-internal (car fixed-arguments)
                          (cdr fixed-arguments)
                          (equal (caar let-bindings)
                                 (car fixed-arguments)))
            (cut-internal function
                          fixed-arguments
                          nil)))))

(defmacro cut (slot-or-expr &rest slot-or-expr*)
  "CUT partially applies its first argument, which can be a symbol (which is
treated as a function) or an expression that evaluates to a function. Any
argument of CUT that appears as a slot `<>' will be a positional argument
of the resulting partially applied function, even for the first argument (the
function position). A special slot `<...>' can be the last argument of CUT
which is then treated as a REST argument for the partial application.
Example expansions:

(mapcar (cut member <> (list a b c) :test 'equal) things-to-find)
=>
(MAPCAR (LAMBDA (#:SLOT-1)
          (MEMBER #:SLOT-1 (LIST A B C) :TEST 'EQUAL))
        THINGS-TO-FIND)

(cut + <> (+ x 1) <>)
=>
(LAMBDA (#:SLOT-1 #:SLOT-2)
  (+ #:SLOT-1 (+ X 1) #:SLOT-2))

(cut <> 1 2 3 <>)
=>
(LAMBDA (#:SLOT-1 #:SLOT-2)
  (FUNCALL #:SLOT-1 1 2 3 #:SLOT-2))

(cut + 1 x <...>)
=>
(LAMBDA (&REST #:REST-SLOT-1)
  (APPLY #'+ 1 X #:REST-SLOT-1))

(cut <> 1 x <...>)
=>
(LAMBDA (#:SLOT-1 &REST #:REST-SLOT-2)
  (APPLY #:SLOT-1 1 X #:REST-SLOT-2))

(cut (if (= 0 (random 10)) #'+ #'-) 10 <...>)
=>
(LAMBDA (&REST #:REST-SLOT-1)
  (APPLY
   (IF (= 0 (RANDOM 10))
       #'+
       #'-)
   10 #:REST-SLOT-1))

(cut (cut <> <>) <> <>)
=>
(LAMBDA (#:SLOT-1 #:SLOT-2)
  (FUNCALL (LAMBDA (#:SLOT-3 #:SLOT-4) (FUNCALL #:SLOT-3 #:SLOT-4))
           #:SLOT-1 #:SLOT-2))"
  (cut-internal slot-or-expr slot-or-expr* nil))

(defmacro cut* (slot-or-expr &rest slot-or-expr*)
  "Like CUT, but treats a non-slot symbol in the first argument as a value
instead of a function. Examples:

(mapcar (cut* member <> (list a b c) :test 'equal) things-to-find)
=>
(MAPCAR (LAMBDA (#:SLOT-1195)
          (FUNCALL MEMBER #:SLOT-1195 (LIST A B C) :TEST 'EQUAL))
        THINGS-TO-FIND)

(let ((some-func (lambda (x y) (- x y))))
  (cut* some-func <> 1))
=>
(LET ((SOME-FUNC (LAMBDA (X Y) (- X Y))))
  (LAMBDA (#:SLOT-1) (FUNCALL SOME-FUNC #:SLOT-1 1)))

(cut* list 'a <...>)
=>
;; Notice LIST and not #'LIST, which may cause an error.
(LAMBDA (&REST #:REST-SLOT-1)
  (APPLY LIST 'A #:REST-SLOT-1))

(cut* <> 'a 'b)
=>
;; Same as (cut <> 'a 'b).
(LAMBDA (#:SLOT-1)
  (FUNCALL #:SLOT-1 'A 'B))"
  (cut-internal slot-or-expr slot-or-expr* t))

(defmacro cute (slot-or-expr &rest slot-or-expr*)
  "Like CUT, but evaluates all arguments except the first. Examples:

(mapcar (cute member <> (list a b c) :test 'equal) things-to-find)
=>
(MAPCAR (LET ((#:LET-BINDING-1 (LIST A B C))
              (#:LET-BINDING-2 :TEST)
              (#:LET-BINDING-3 'EQUAL))
          (LAMBDA (#:SLOT-4)
            (MEMBER #:SLOT-1199 #:LET-BINDING-1196 #:LET-BINDING-1197
                    #:LET-BINDING-1198)))
        THINGS-TO-FIND)

(cute list (if flag 'a 'b) <...>)
=>
(LET ((#:LET-BINDING-1
       (IF FLAG 'A 'B)))
  (LAMBDA (&REST #:REST-SLOT-2)
    (APPLY #'LIST #:LET-BINDING-1 #:REST-SLOT-2)))

(cute <> 1 (complex-computation))
=>
(LET ((#:LET-BINDING-1 1)
      (#:LET-BINDING-2 (COMPLEX-COMPUTATION)))
  (LAMBDA (#:SLOT-3)
    (FUNCALL #:SLOT-3 #:LET-BINDING-1 #:LET-BINDING-2)))"
  (cute-internal slot-or-expr slot-or-expr* nil))

(defmacro cute* (slot-or-expr &rest slot-or-expr*)
  "Like CUTE, but evaluates all arguments as values, including the first
argument. Examples:

(mapcar (cute* member <> (list a b c) :test 'equal) things-to-find)
=>
(MAPCAR (LET ((#:LET-BINDING-1 MEMBER) ;; Notice how MEMBER is bound here.
              (#:LET-BINDING-2 (LIST A B C))
              (#:LET-BINDING-3 :TEST)
              (#:LET-BINDING-4 'EQUAL))
          (LAMBDA (#:SLOT-5)
            (FUNCALL #:LET-BINDING-1 #:SLOT-5 #:LET-BINDING-2 #:LET-BINDING-3
                     #:LET-BINDING-4)))
        THINGS-TO-FIND)

(cute* some-value <> 10 <...>)
=>
(LET ((#:LET-BINDING-1 SOME-VALUE)
      (#:LET-BINDING-2 10))
  (LAMBDA (#:SLOT-3 &REST #:REST-SLOT-4)
    (APPLY #:LET-BINDING-1 #:SLOT-3 #:LET-BINDING-2 #:REST-SLOT-4)))

(cute* <> 'foo <>)
=>
(LET ((#:LET-BINDING-1 'FOO))
  (LAMBDA (#:SLOT-2 #:SLOT-3)
    (FUNCALL #:SLOT-2 #:LET-BINDING-1 #:SLOT-3)))"
  (cute-internal slot-or-expr slot-or-expr* t))
