#lang racket

(require "semantics.rkt")

(provide create-node
         create-edge
         current-graph
         graph
         (except-out (all-from-out racket) #%module-begin)
         (rename-out [my-module-begin #%module-begin]))

#|
A guard procedure takes ONE argument. Whenever the parameter procedure is applied to
an argument, the argument is passed on to the guard procedure. The result returned by
the guard procedure is used as the new parameter value.
|#

;; global param of our graph
(define current-graph 
  (make-parameter (graph '())
                  (lambda (f)
                    f)))

(define-syntax-rule (my-module-begin body ...)
   (#%plain-module-begin
    (parameterize ([current-graph (graph '())])
      body ...)))


;; create a new node
(define-syntax-rule (create-node name content)
  (current-graph (add-node (node name content '()) (current-graph))))

;; add an edge
(define-syntax-rule (create-edge n1 n2 relationship)
  (current-graph (add-edge (edge n2 n2 relationship) (current-graph))))




  

