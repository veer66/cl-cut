# cl-cut
[![Build Status](https://travis-ci.org/cromachina/cl-cut.svg)](https://travis-ci.org/cromachina/cl-cut)
[![Quicklisp](http://quickdocs.org/badge/cl-cut.svg)](http://quickdocs.org/cl-cut/)

This package provides a set of macros for partially applying functions and expressions like in [SRFI 26](http://srfi.schemers.org/srfi-26/srfi-26.html). In addition to the two macros that the Scheme document specifies, two more are added for Common Lisp to accomodate the separate function and value namespaces.

This is best explained by examples (modified from the SRFI 26 document):
```lisp
(cut cons (+ a 1) <>)    =>  (lambda (x2) (cons (+ a 1) x2))
(cut list 1 <> 3 <> 5)   =>  (lambda (x2 x4) (list 1 x2 3 x4 5))
(cut list)               =>  (lambda () (list))
(cut list 1 <> 3 <...>)  =>  (lambda (x2 &rest xs) (apply #'list 1 x2 3 xs))
(cut <> a b)             =>  (lambda (f) (funcall f a b))
```
Special forms like `IF` can work too, but only when in the function positon and without a rest argument.
```lisp
(cut if <> a b)          =>  (lambda (x2) (if x2 a b))
```
`CUT*` treats the function argument as a value holding a function.
```lisp
(cut* cons (+ a 1) <>)    =>  (lambda (x2) (funcall cons (+ a 1) x2))
(cut* list 1 <> 3 <> 5)   =>  (lambda (x2 x4) (funcall list 1 x2 3 x4 5))
(cut* list)               =>  (lambda () (funcall list))
(cut* list 1 <> 3 <...>)  =>  (lambda (x2 &rest xs) (apply list 1 x2 3 xs))
(cut* <> a b)             =>  (lambda (f) (funcall f a b))
```
`CUTE` evaluates arguments of the application only once, except the function argument.
```lisp
(cute cons (+ a 1) <>)    =>  (let ((z1 (+ a 1))) (lambda (x2) (cons z1 x2)))
(cute list 1 <> 3 <> 5)   =>  (let ((z1 1) (z2 3) (z3 5)) (lambda (x2 x4) (list z1 x2 z2 x4 z3)))
(cute list)               =>  (lambda () (list))
(cute list 1 <> 3 <...>)  =>  (let ((z1 1) (z2 3)) (lambda (x2 &rest xs) (apply #'list z1 x2 z2 xs)))
(cute <> a b)             =>  (let ((z1 a) (z2 b)) (lambda (f) (funcall f z1 z2)))
```
`CUTE*` is like `CUTE` but treats the function argument as a value as well.
```lisp
(cute* cons (+ a 1) <>)    =>  (let ((f1 cons) (z1 (+ a 1))) (lambda (x2) (funcall f1 z1 x2)))
(cute* list 1 <> 3 <> 5)   =>  (let ((f1 list) (z1 1) (z2 3) (z3 5)) (lambda (x2 x4) (funcall f1 z1 x2 z2 x4 z3)))
(cute* list)               =>  (let ((f1 list)) (lambda () (funcall f1)))
(cute* list 1 <> 3 <...>)  =>  (let ((f1 list) (z1 1) (z2 3)) (lambda (x2 &rest xs) (apply f1 z1 x2 z2 xs)))
(cute* <> a b)             =>  (let ((z1 a) (z2 b)) (lambda (f) (funcall f z1 z2)))
```

## Similar projects
- [cl-op](https://code.google.com/p/cl-op/)
- [fn](https://github.com/cbaggers/fn)
- [positional-lambda](http://quickdocs.org/positional-lambda/)
- [curry-compose-reader-macros](https://github.com/eschulte/curry-compose-reader-macros)

## Dependencies
Unit tests depend on [prove](https://github.com/fukamachi/prove).

## Installation
Cl-cut can be installed via Quicklisp:
```lisp
(ql:quickload :cl-cut)
```

## License
    Copyright (c) 2015 Cromachina
    
    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:
    
    The above copyright notice and this permission notice shall be included in
    all copies or substantial portions of the Software.
    
    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
    THE SOFTWARE
