#lang racket

(require graph)
(provide (all-defined-out))

(struct node (id content) #:transparent)

(struct edge (node1 node2 weight [directed #:auto])
  #:auto-value #f
  #:transparent)

(define (is-edge? g u v e)
         (or (and (vertex=? g (edge-node1 e) v)
                  (vertex=? g (edge-node2 e) u))
             (and (vertex=? g (edge-node1 e) u)
                  (vertex=? g (edge-node2 e) v))))

(struct graph (nodes edges)
  #:methods gen:graph
  [(define (has-vertex? g v)
     (define (helper nodes)
         (cond
           [(empty? nodes)
            #f]
           [(equal? (node-id v) (node-id (car nodes)))
            #t]
           [else
            (helper (cdr nodes))]))
     (helper (graph-nodes g)))
   
   (define (has-edge? g u v)
     (ormap (curry is-edge? g u v) (graph-edges g)))

   (define (vertex=? g u v)
     (equal? (node-id u) (node-id v)))
   
   (define (add-vertex! g v)
     (set-graph-nodes! g (append (graph-nodes g) (list v))))
   
   (define (remove-vertex! g v)
     (define (helper nodes)
       (cond
         [(empty? nodes)
          '()]
         [(vertex=? g v (car nodes))
          (cdr nodes)]
         [else
          (cons (car nodes) (helper (cdr nodes)))]))
     (define (remove-edge edges)
       (cond
         [(empty? edges)
          '()]
         [(or (vertex=? g (edge-node1 (car edges))
                        v)
              (vertex=? g (edge-node2 (car edges))
                        v))
          (helper (cdr edges))]
         [else
          (cons (car edges) (remove-edge (cdr edges)))]))
     (set-graph-nodes! g (helper (graph-nodes g)))
     (set-graph-edges! g (remove-edge (graph-edges g))))

   (define (rename-vertex! g old new)
     (define (helper nodes)
       (cond
         [(empty? nodes)
          (error 'rename-vertex! "Error: graph does not contain old vertex")]
         [(vertex=? g old (car nodes))
          (car new (cdr nodes))]
         [else
          (cons (car nodes) (helper (cdr nodes)))]))
     (define (update-edge e)
       (cond
         [(vertex=? g (edge-node1 e) old)
          (edge new (edge-node2 e) (edge-weight e) (edge-directed e))]
         [(vertex=? g (edge-node2 e) old)
          (edge (edge-node1 e) new (edge-weight e) (edge-directed e))]
         [else
          e]))
     (if (has-vertex? g new)
         (error 'rename-vertex! "Error: graph already contains new vertex")
         (begin
           (set-graph-nodes! g (helper (graph-nodes g)))
           (set-graph-edges! g (map update-edge (graph-edges g))))))
   
   (define (add-edge! g u v [weight 1])
     (let ([new-edge (edge u v weight)])
       (set-graph-edges! g (cons new-edge (graph-edges g)))))
   
   (define (add-directed-edge! g u v [weight 1])
     (let ([new-edge (edge u v weight #t)])
       (set-graph-edges! g (cons new-edge (graph-edges g)))))
   
   (define (remove-edge! g u v)
     (define (helper edges)
       (cond
         [(empty? edges)
          '()]
         [(and (is-edge? g u v (car edges))
               (not (edge-directed (car edges))))
          (cdr edges)]
         [else
          (cons (car edges) (helper (cdr edges)))]))
     (set-graph-edges! g (helper (graph-edges g))))
     
   (define (remove-directed-edge! g u v)
     (define (helper edges)
       (cond
         [(empty? edges)
          '()]
         [(and (is-edge? g u v (car edges))
               (edge-directed (car edges)))
          (cdr edges)]
         [else
          (cons (car edges) (helper (cdr edges)))]))
     (set-graph-edges! g (helper (graph-edges g))))
   
   (define (get-vertices g)
     (graph-nodes g))
   
   (define (in-vertices g)
     (graph-nodes g))
   
   (define (get-neighbors g v)
     (define helper
       (lambda (e)
         (cond
           [(vertex=? g (edge-node1 e) v)
            (edge-node2 e)]
           [(vertex=? g (edge-node2 e) v)
            (edge-node1 e)]
           [else
            #f])))
     (filter helper (graph-edges g)))
   
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
           [(is-edge? g u v (car edges))
            (edge-weight (car edges))]
           [else
            (helper (cdr edges))])))
     (helper (graph-edges g)))
   
   (define (transpose g)
     (define (helper edges)
       (cond
         [(empty? edges)
          '()]
         [(edge-directed (car edges))
          (edge (edge-node2 (car edges))
                (edge-node1 (car edges))
                (edge-weight (car edges))
                (edge-directed (car edges)))]
         [else
          (cons (car edges) (helper (cdr edges)))]))
     (let ([new-edges (helper (graph-edges g))])
       (graph (graph-nodes g) new-edges)))
   
   (define (graph-copy g)
     (graph (graph-nodes g) (graph-edges g)))

   ;; extra functions that are not part of graph generics
   ;; define functional versions of each imperitive function defined by graph
   (define (add-vertex g v)
     (let ([new-nodes (append (graph-nodes g) (list v))])
       (graph new-nodes (graph-edges g))))
   
   (define (remove-vertex g v)
     (define (helper nodes)
       (cond
         [(empty? nodes)
          '()]
         [(vertex=? g v (car nodes))
          (cdr nodes)]
         [else
          (cons (car nodes) (helper (cdr nodes)))]))
     (define (local-remove-edge edges)
       (cond
         [(empty? edges)
          '()]
         [(or (vertex=? g (edge-node1 (car edges))
                        v)
              (vertex=? g (edge-node2 (car edges))
                        v))
          (helper (cdr edges))]
         [else
          (cons (car edges) (local-remove-edge (cdr edges)))]))
     (let ([new-nodes (helper (graph-nodes g))]
           [new-edges (local-remove-edge (graph-edges g))])
       (graph new-nodes new-edges)))

   (define (rename-vertex g old new)
     (define (helper nodes)
       (cond
         [(empty? nodes)
          (error 'rename-vertex! "Error: graph does not contain old vertex")]
         [(vertex=? g old (car nodes))
          (car new (cdr nodes))]
         [else
          (cons (car nodes) (helper (cdr nodes)))]))
     (define (update-edge e)
       (cond
         [(vertex=? g (edge-node1 e) old)
          (edge new (edge-node2 e) (edge-weight e) (edge-directed e))]
         [(vertex=? g (edge-node2 e) old)
          (edge (edge-node1 e) new (edge-weight e) (edge-directed e))]
         [else
          e]))
     (if (has-vertex? g new)
         (error 'rename-vertex! "Error: graph already contains new vertex")
         (let ([new-nodes (helper (graph-nodes g))]
               [new-edges (map update-edge (graph-edges g))])
           (graph new-nodes new-edges))))

   (define (add-edge g u v [weight 1])
     (let ([new-edge (edge u v weight)])
       (graph (graph-nodes g) (cons new-edge (graph-edges g)))))

   (define (add-directed-edge g u v [weight 1])
     (let ([new-edge (edge u v weight #t)])
       (graph (graph-nodes g) (cons new-edge (graph-edges g)))))

   (define (remove-edge g u v)
     (define (helper edges)
       (cond
         [(empty? edges)
          '()]
         [(and (is-edge? g u v (car edges))
               (not (edge-directed (car edges))))
          (cdr edges)]
         [else
          (cons (car edges) (helper (cdr edges)))]))
     (graph (graph-nodes g) (helper (graph-edges g))))

   (define (remove-directed-edge g u v)
     (define (helper edges)
       (cond
         [(empty? edges)
          '()]
         [(and (is-edge? g u v (car edges))
               (edge-directed (car edges)))
          (cdr edges)]
         [else
          (cons (car edges) (helper (cdr edges)))]))
     (graph (graph-nodes g) (helper (graph-edges g))))]
  #:mutable
  #:transparent)

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
      (lambda (node other-nodes)
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
      (graph nodes (all-edges nodes))))




