#lang info

(define collection "fig")
(define scribblings '(("scribblings/fig.scrbl")))
(define deps '("rackunit-lib"
               "base" "brag"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
(define license 'MIT)