#lang racket

(provide (all-defined-out))

(struct graph (nodes)
  #:transparent)

(define create-graph (lambda (n) (graph n)))

(struct node (content edges) #:transparent)

(struct edge (node1 node2 [directed #:auto])
  #:auto-value #f
  #:transparent) ;; add a direction. if direction it is always from node1 to node2, otherwise undirected.

;; replace node in graph
(define update-node
  (lambda (node g)
    (define helper
      (lambda (nodes)
        (cond
          [(empty? nodes)
           (error 'update-node "failed because node ~s does not exist" (node-content node))]
          [(equal? (node-content node) (node-content (car nodes)))
           (cons node (cdr nodes))]
          [else
           (helper (cdr nodes))])))
    (graph (helper (graph-nodes g)))))

;; add node to graph
;; make sure node with name doesnt already exist
(define add-node
  (lambda (node g)
    (let ([nodes (graph-nodes g)])
      (if (empty? (filter (lambda (n) (equal? (node-content n) (node-content node)))
                          nodes))
          (graph (append nodes (list node)))
          (error 'add-node "failed because node ~s already exists" (node-content node))))))

;; add edge to node
(define add-edge-to-node
  (lambda (e n) 
    (let ([edges (append (node-edges n) (list e))])
      (node (node-content n) edges))))

;; adds edge to the two nodes in edge. errors if cna't find nodes.
(define add-edge
  (lambda (e g)
    (define helper
      (lambda (name ns)
        (cond
          [(empty? ns)
           (error 'add-edge "failed because node ~s could not be found" name)]
          [(equal? name (node-content (car ns)))
           (car ns)]
          [else
           (helper name (cdr ns))])))
    (let* ([nodes (graph-nodes g)]
           [new-node1 (add-edge-to-node 
                       e
                       (helper (node-content (edge-node1 e)) nodes))]
           [new-node2 (add-edge-to-node
                       e
                       (helper (node-content (edge-node2 e)) nodes))])
      (update-node new-node2 
                   (update-node new-node1 g)))))

(define find-node
  (lambda (n g)
    (define helper
      (lambda (ns)
        (cond
          [(empty? ns)
           (error 'find-node "could not find node")]
          [(equal? n (node-content (car ns)))
           (car ns)]
          [else
           (helper (cdr ns))])))
    (helper (graph-nodes g))))




