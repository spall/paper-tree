#lang racket

(require graph)
(provide (all-defined-out))

(struct node (id content edges) #:transparent)

(struct edge (node1 node2 [directed #:auto])
  #:auto-value #f
  #:transparent)

(struct graph (nodes edges)
  #:methods gen:graph
  [(define (has-vertex? g v)
     (define helper
       (lambda (nodes)
         (cond
           [(empty? nodes)
            #f]
           [(equal? (node-id v) (node-id (car nodes)))
            #t]
           [else
            (helper (cdr nodes))])))
     (helper (graph-nodes g)))
   
   (define (has-edge? g u v)
     (define helper
       (lambda (e)
         (or (and (vertex=? g (edge-node1 e) v)
                  (vertex=? g (edge-node2 e) u))
             (and (vertex=? g (edge-node1 e) u)
                  (vertex=? g (edge-node2 e) v)))))
     (and (ormap helper (node-edges u))
          (ormap helper (node-edges v))))

   (define (vertex=? g u v)
     (equal? (node-id u) (node-id v)))
   
   (define (add-vertex! g v)
     (displayln "this function is not implemented"))
   
   (define (remove-vertex! g v) (displayln "this function is not implemented"))
   (define (add-edge! g u v [weight 1]) (displayln "this function is not implemented"))
   (define (add-directed-edge! g u v [weight 1]) (displayln "this function is not implemented"))
   (define (remove-edge! g u v) (displayln "this function is not implemented"))
   (define (remove-directed-edge! g u v) (displayln "this function is not implemented"))
   
   (define (get-vertices g)
     (graph-nodes g))
   
   (define (in-vertices g)
     (graph-nodes g))
   
   (define (get-neighbors g v)
     (define helper
       (lambda (e)
         (cond
           [(equal? (node-id (edge-node1 e))
                    (node-id v))
            (edge-node2 e)]
           [else
            (edge-node1 e)])))
     (map helper (node-edges v)))
   
   (define (in-neighbors g v)
     (get-neighbors g v))
   
   (define (get-edges g)
     (graph-edges g))
   
   (define (in-edges g)
     (graph-edges g))
   
   (define (edge-weight g u v)
     (define helper
       (lambda (edges)
         (cond
           [(empty? edges)
            +inf.0]
           [(or (and (vertex=? g (edge-node1 (car edges)) v)
                      (vertex=? g (edge-node2 (car edges)) u))
                 (and (vertex=? g (edge-node1 (car edges)) u)
                      (vertex=? g (edge-node2 (car edges)) v)))
            1]
           [else
            (helper (cdr edges))])))
     (helper (graph-edges g)))
   
   (define (transpose g) g)
   
   (define (graph-copy g) g)]
  #:transparent)


(define create-graph (lambda (n) (graph n)))
 ;; add a direction. if direction it is always from node1 to node2, otherwise undirected.

(define add-node
  (lambda (n g)
    (let* ([nodes (graph-nodes g)]
           [new-nodes (append nodes (list n))])
      (graph new-nodes))))

;; replace node in graph
(define update-node
  (lambda (node g)
    (define helper
      (lambda (nodes)
        (cond
          [(empty? nodes)
           (error 'update-node "failed because node ~s does not exist" (node-id node))]
          [(equal? (node-id node) (node-id (car nodes)))
           (cons node (cdr nodes))]
          [else
           (cons (car nodes) (helper (cdr nodes)))])))
    (graph (helper (graph-nodes g)))))

;; add edge to node
(define add-edge-to-node
  (lambda (e n) 
    (let ([edges (append (node-edges n) (list e))])
      (node (node-id n) (node-content n) edges))))

;; adds edge to the two nodes in edge. errors if cna't find nodes.
(define add-edge
  (lambda (e g)
    (let ([new-node1 (add-edge-to-node 
                      e
                      (edge-node1 e))]
          [new-node2 (add-edge-to-node
                      e
                      (edge-node2 e))])
      (update-node new-node2
                   (update-node new-node1 g)))))

(define find-node
  (lambda (n g)
    (define helper
      (lambda (ns)
        (cond
          [(empty? ns)
           (error 'find-node "could not find node")]
          [(equal? (node-id n) (node-id (car ns)))
           (car ns)]
          [else
           (helper (cdr ns))])))
    (helper (graph-nodes g))))

(define construct-graph
  (lambda (nodes need-edge? make-edge)
    (define helper
      (lambda (node other-nodes)car
        (cond
          [(empty? other-nodes)
           '()]
          [(need-edge? node (car other-nodes))
           (cons (make-edge node (car other-nodes))
                 (helper node (cdr other-nodes)))]
          [else
           (helper node (cdr other-nodes))])))
    (define all-edges
      (lambda (ns)
        (if (empty? ns)
            '()
            (append (helper (car ns) (cdr ns))
                    (all-edges (cdr ns))))))
    (let ([edges (all-edges nodes)]
          [new-graph (graph nodes)])  
      (foldl add-edge new-graph edges))))




