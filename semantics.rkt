#lang racket

(require graph)
;; all defined out doesn't work for functions defined in a structure? :(
(provide (struct-out my-graph)
         (all-defined-out)
         graphviz)

(struct node (id content) #:transparent)

(struct edge (node1 node2 [directed #:auto])
  #:auto-value #f
  #:transparent)

(define (is-edge? g u v e)
  (or (and (vertex=? g (edge-node1 e) v)
           (vertex=? g (edge-node2 e) u))
      (and (vertex=? g (edge-node1 e) u)
           (vertex=? g (edge-node2 e) v))))

;; this function converts edges because graph generics expects edges to be lists. but afaik does not state this in docs!
(define (convert-edge e)
  (list (edge-node1 e) (edge-node2 e)))
  
(struct my-graph (nodes edges)
  #:methods gen:graph
  [(define (has-vertex? g v)
     (define (helper nodes)
         (cond
           [(empty? nodes)
            #f]
           [(vertex=? g v (car nodes))
            #t]
           [else
            (helper (cdr nodes))]))
     (helper (my-graph-nodes g)))
   
   (define (has-edge? g u v)
     (ormap (curry is-edge? g u v) (my-graph-edges g)))

   (define (vertex=? g u v)
     (equal? (node-id u) (node-id v)))
   
   (define (add-vertex! g v)
     (set-my-graph-nodes! g (append (my-graph-nodes g) (list v))))
   
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
     (begin (set-my-graph-nodes! g (helper (my-graph-nodes g)))
            (set-my-graph-edges! g (remove-edge (my-graph-edges g)))))

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
          (edge new (edge-node2 e) (edge-directed e))]
         [(vertex=? g (edge-node2 e) old)
          (edge (edge-node1 e) new  (edge-directed e))]
         [else
          e]))
     (if (has-vertex? g new)
         (error 'rename-vertex! "graph already contains new vertex")
         (begin
           (set-my-graph-nodes! g (helper (my-graph-nodes g)))
           (set-my-graph-edges! g (map update-edge (my-graph-edges g))))))
   
   (define (add-edge! g u v [weight 1])
     (let ([new-edge (edge u v weight)])
       (set-my-graph-edges! g (cons new-edge (my-graph-edges g)))))
   
   (define (add-directed-edge! g u v [weight 1])
     (let ([new-edge (edge u v weight #t)])
       (set-my-graph-edges! g (cons new-edge (my-graph-edges g)))))
   
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
     (set-my-graph-edges! g (helper (my-graph-edges g))))
     
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
     (set-my-graph-edges! g (helper (my-graph-edges g))))
   
   (define (get-vertices g)
     (my-graph-nodes g))
   
   (define (in-vertices g)
     (my-graph-nodes g))
   
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
     (filter helper (my-graph-edges g)))
   
   (define (in-neighbors g v)
     (get-neighbors g v))
   
   (define (get-edges g)
     (map convert-edge (my-graph-edges g)))
   
   (define (in-edges g)
     (map convert-edge (my-graph-edges g)))
   
   (define (edge-weight g u v)
     +inf.0)
   
   (define (transpose g)
     (define (helper edges)
       (cond
         [(empty? edges)
          '()]
         [(edge-directed (car edges))
          (edge (edge-node2 (car edges))
                (edge-node1 (car edges))
                (edge-directed (car edges)))]
         [else
          (cons (car edges) (helper (cdr edges)))]))
     (let ([new-edges (helper (my-graph-edges g))])
       (my-graph (my-graph-nodes g) new-edges)))
   
   (define (my-graph-copy g)
     (my-graph (my-graph-nodes g) (my-graph-edges g)))

   ;; extra functions that are not part of graph generics
   ;; define functional versions of each imperitive function defined by graph
 ]
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
    (helper (my-graph-nodes g))))

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
      (my-graph nodes (all-edges nodes))))

  (define (add-vertex g v)
     (let ([new-nodes (append (my-graph-nodes g) (list v))])
       (my-graph new-nodes (my-graph-edges g))))
   
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
     (let ([new-nodes (helper (my-graph-nodes g))]
           [new-edges (local-remove-edge (my-graph-edges g))])
       (my-graph new-nodes new-edges)))

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
          (edge new (edge-node2 e) (edge-directed e))]
         [(vertex=? g (edge-node2 e) old)
          (edge (edge-node1 e) new (edge-directed e))]
         [else
          e]))
     (if (has-vertex? g new)
         (error 'rename-vertex! "Error: graph already contains new vertex")
         (let ([new-nodes (helper (my-graph-nodes g))]
               [new-edges (map update-edge (my-graph-edges g))])
           (my-graph new-nodes new-edges))))

   (define (add-edge g u v [weight 1])
     (let ([new-edge (edge u v weight)])
       (my-graph (my-graph-nodes g) (cons new-edge (my-graph-edges g)))))

   (define (add-directed-edge g u v [weight 1])
     (let ([new-edge (edge u v weight #t)])
       (my-graph (my-graph-nodes g) (cons new-edge (my-graph-edges g)))))

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
     (my-graph (my-graph-nodes g) (helper (my-graph-edges g))))

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
     (my-graph (my-graph-nodes g) (helper (my-graph-edges g))))


(define (write-graph-to-dot g)
  (define f (open-output-file "out.dot" #:mode 'text #:exists'replace))
  (define dot (graphviz g ))
  (display dot f)
  (close-output-port f))
  
;;(add-vertex! (my-graph '() '()) (node (gensym) 2))


;; neato -T pdf 


