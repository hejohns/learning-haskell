#!/usr/bin/guile -s
coding: utf-8
!#

(define-module (hejohns two)
 #:version (0 2021 02 20))
(use-modules (ice-9 regex))
(define call/cc
 call-with-current-continuation)
(define-syntax set/cc!
 (syntax-rules ()
  ((set/cc! f)
   (call/cc (lambda (x)
             (set! f x)
             (x x))))))
(define label
 #f)
(define volatile
 0)
(set/cc! label)
(display volatile)
(newline)
(set! volatile
 (+ volatile 1))
(display "hi")
(newline)
(label #f)
