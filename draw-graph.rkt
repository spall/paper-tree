#lang racket

(require "semantics.rkt")
(require racket/gui/base)

(provide show-graph)

(define show-graph
  (lambda (canvas dc graph)
    (let* ([nodes (graph-nodes graph)]
           [len (length nodes)]
           [posns (build-list len (lambda (x)
                                    (cons (* x 10) (* x 10))))])
      (map (lambda (node pos)
             (send dc draw-rectangle (car pos) (cdr pos) 5 5))
           nodes
           posns))))
