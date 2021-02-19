#!/usr/bin/guile -s
coding: utf-8
!#

(use-modules (ice-9 regex))
(define input "")
(set! input (object->string (read)))
(if (string-match "[0-9]" input)
    (begin
        (display "Hello, world")
        (newline)
        (system "ls")
    )
    (begin
        (display "Sad")
        (newline)
    )
)
