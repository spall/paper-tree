#lang racket

(provide (all-defined-out))

(struct graph (nodes)
  #:transparent)

(define create-graph (lambda (n) (graph n)))

(struct node (id content edges) #:transparent)

(struct edge (node1 node2 [directed #:auto])
  #:auto-value #f
  #:transparent) ;; add a direction. if direction it is always from node1 to node2, otherwise undirected.

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




