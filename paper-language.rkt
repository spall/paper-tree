#lang racket

(require "semantics.rkt")
(require racket/gui/base)
(require "autobib-copy.rkt")

#| (define-struct auto-bib (author date title location url note is-book? key specific)) |#

#|
cannot use scriblib/autobib struct definition for a bib because it does not provide
enough access to the structure.  basically copy autobib.rkt and provide more functions.
|#

(provide
 draw-graph
 define-paper
 size
 define-graph
 authors
 techrpt-location
 (all-from-out "autobib-copy.rkt")
 (except-out (all-from-out racket) #%module-begin)
 (rename-out [my-module-begin #%module-begin]))

(define current-graph
  (make-parameter (my-graph '() '())
                  (lambda (f)
                    f)))

(define-syntax-rule (my-module-begin body ...)
   (#%plain-module-begin
    (parameterize ([current-graph (my-graph '() '())])
      body ...)))

(define-syntax-rule (define-paper id bib)
  (begin
    (define id (node (string->symbol (symbol->string (gensym))) bib))
    (let ([g (current-graph)])
      (current-graph (add-vertex g id)))))

(define-syntax-rule (size)
  (displayln (length (my-graph-nodes (current-graph)))))

(define-syntax-rule (draw-graph g)
  (write-graph-to-dot g))

(define-syntax define-graph
  (syntax-rules (author authors)
    [(_ id author name)
     (define id
       (construct-graph
        (my-graph-nodes (current-graph))
        (lambda (n1 n2)
          (and (member (author-element-names (parse-author name)) (authors-list (node-content n1)))
               (member (author-element-names (parse-author name)) (authors-list (node-content n2)))))
        (lambda (n1 n2)
          (edge n1 n2))))]
    [(_ id authors (names ...+ . name))
     (define id
           (construct-graph
            (my-graph-nodes (current-graph))
            (lambda (n1 n2)
              (and (andmap (lambda (x) (member (author-element-names (parse-author x))
                                               (authors-list (node-content n1))))
                           (names ...+ . name))
                   (andmap (lambda (x) (member (author-element-names (parse-author x))
                                               (authors-list (node-content n2))))
                           (names ...+ . name))))
            (lambda (n1 n2)
              (edge n1 n2))))]
    ))

(define authors-list
  (lambda (paper)
    (map string-trim
         (string-split (author-element-names* (extract-bib-author paper))
                       "/"))))





  

                     


  