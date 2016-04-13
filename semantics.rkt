#lang racket

(provide (all-defined-out))

(struct graph (nodes))

(define create-graph (lambda (n) (graph n)))

(struct node (name val edges))

(struct edge (node1 node2 info))

;; replace node in graph
(define update-node
  (lambda (node g)
    (define helper
      (lambda (nodes)
        (cond
          [(empty? nodes)
           (error 'update-node "failed because node ~s does not exist" (node-name node))]
          [(equal? (node-name node) (node-name (car nodes)))
           (cons node (cdr nodes))]
          [else
           (helper (cdr nodes))])))
    (graph (helper (graph-nodes g)))))

;; add node to graphadd-e
;; make sure node with name doesnt already exist
(define add-node
  (lambda (node g)
    (let ([nodes (graph-nodes g)])
      (if (empty? (filter (lambda (n) (equal? (node-name n) (node-name node)))
                          nodes))
          (graph (append nodes (list node)))
          (error 'add-node "failed because node ~s already exists" (node-name node))))))

;; add edge to node
(define add-edge-to-node
  (lambda (edge n)
    (let ([edges (append (node-edges n) (list edge))])
      (node (node-name n) (node-val n) edges))))

;; adds edge to the two nodes in edge. errors if cna't find nodes.
(define add-edge
  (lambda (e g)
    (define helper
      (lambda (name ns)
        (cond
          [(empty? ns)
           (error 'add-edge "failed because node ~s could not be found" name)]
          [(equal? name (node-name (car ns)))
           (car ns)]
          [else
           (helper name (cdr ns))])))
    (let* ([nodes (graph-nodes g)]
           [new-node1 (add-edge-to-node 
                       e
                       (helper (edge-node1 e) nodes))]
           [new-node2 (add-edge-to-node
                       e
                       (helper (edge-node2 e) nodes))])
      (update-node new-node2 
                   (update-node new-node1 g)))))




