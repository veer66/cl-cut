(defpackage :cl-cut.test
  (:use :cl :cut :prove))

(in-package :cl-cut.test)

(diag "Test cl-cut")
(plan 6)

(subtest "Cut"
  (plan 6)
  (is-expand (cut cons (+ a 1) <>)   (lambda ($x2) (cons (+ a 1) $x2)))
  (is-expand (cut list 1 <> 3 <> 5)  (lambda ($x2 $x4) (list 1 $x2 3 $x4 5)))
  (is-expand (cut list)              (lambda () (list)))
  (is-expand (cut list 1 <> 3 <...>) (lambda ($x2 &rest $xs) (apply #'list 1 $x2 3 $xs)))
  (is-expand (cut <> a b)            (lambda ($f) (funcall $f a b)))
  (is-expand (cut if <> a b)         (lambda ($x2) (if $x2 a b))))

(subtest "Cut*"
  (plan 5)
  (is-expand (cut* cons (+ a 1) <>)   (lambda ($x2) (funcall cons (+ a 1) $x2)))
  (is-expand (cut* list 1 <> 3 <> 5)  (lambda ($x2 $x4) (funcall list 1 $x2 3 $x4 5)))
  (is-expand (cut* list)              (lambda () (funcall list)))
  (is-expand (cut* list 1 <> 3 <...>) (lambda ($x2 &rest $xs) (apply list 1 $x2 3 $xs)))
  (is-expand (cut* <> a b)            (lambda ($f) (funcall $f a b))))

(subtest "Cute"
  (plan 5)
  (is-expand (cute cons (+ a 1) <>)   (let (($z1 (+ a 1))) (lambda ($x2) (cons $z1 $x2))))
  (is-expand (cute list 1 <> 3 <> 5)  (let (($z1 1) ($z2 3) ($z3 5)) (lambda ($x2 $x4) (list $z1 $x2 $z2 $x4 $z3))))
  (is-expand (cute list)              (let () (lambda () (list))))
  (is-expand (cute list 1 <> 3 <...>) (let (($z1 1) ($z2 3)) (lambda ($x2 &rest $xs) (apply #'list $z1 $x2 $z2 $xs))))
  (is-expand (cute <> a b)            (let (($z1 a) ($z2 b)) (lambda ($f) (funcall $f $z1 $z2)))))

(subtest "Cute*"
  (plan 5)
  (is-expand (cute* cons (+ a 1) <>)   (let (($f1 cons) ($z1 (+ a 1))) (lambda ($x2) (funcall $f1 $z1 $x2))))
  (is-expand (cute* list 1 <> 3 <> 5)  (let (($f1 list) ($z1 1) ($z2 3) ($z3 5)) (lambda ($x2 $x4) (funcall $f1 $z1 $x2 $z2 $x4 $z3))))
  (is-expand (cute* list)              (let (($f1 list)) (lambda () (funcall $f1))))
  (is-expand (cute* list 1 <> 3 <...>) (let (($f1 list) ($z1 1) ($z2 3)) (lambda ($x2 &rest $xs) (apply $f1 $z1 $x2 $z2 $xs))))
  (is-expand (cute* <> a b)            (let (($z1 a) ($z2 b)) (lambda ($f) (funcall $f $z1 $z2)))))

(subtest "Cut function position expressions"
  (plan 2)
  (is-expand (cut (if x '+ '-) 10 <...>) (lambda (&rest $xs) (apply (if x '+ '-) 10 $xs)))
  (is-expand (cut (if x '+ '-) 10 <>)    (lambda ($x3) (funcall (if x '+ '-) 10 $x3))))

(subtest "Cut errors"
  (plan 2)
  (is-error (macroexpand-1 '(cut cons <...> 'a)) 'simple-error)
  (is-error (macroexpand-1 '(cut <...>)) 'simple-error))

(finalize)
