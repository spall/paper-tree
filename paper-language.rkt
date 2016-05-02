#lang racket

(require "semantics.rkt")
(require "draw-graph.rkt")
(require racket/gui/base)
(require "autobib-copy.rkt")
;;(require scriblib/autobib)

#| (define-struct auto-bib (author date title location url note is-book? key specific)) |#

#|
cannot use scriblib/autobib struct definition for a bib because it does not provide
enough access to the structure.  basically copy autobib.rkt and provide more functions.
|#

(provide
 define-paper
 draw-graph
 size
 define-graph
 authors
 techrpt-location
 (all-from-out "autobib-copy.rkt")
 (except-out (all-from-out racket) #%module-begin)
 (rename-out [my-module-begin #%module-begin]))

(define current-graph
  (make-parameter (graph '())
                  (lambda (f)
                    f)))

(define-syntax-rule (my-module-begin body ...)
   (#%plain-module-begin
    (parameterize ([current-graph (graph '())])
      body ...)))

#|
(define-paper paper1
  (make-bib
    #:title    "Reference: Racket"
    #:author   (authors "Mathew Flatt" "PLT")
    #:date     "2010"
    #:location (techrpt-location #:institution "PLT Inc."
                                 #:number "PLT-TR-2010-1")
    #:url      "http://racket-lang.org/tr1/"))

|#

(define-syntax-rule (define-paper id bib)
  (begin
    (define id (node (string->symbol (symbol->string (gensym))) bib '()))
    (let ([g (current-graph)])
      (current-graph (add-node id g)))))

(define-syntax-rule (size)
  (displayln (length (graph-nodes (current-graph)))))
(define-syntax-rule (draw-graph)
  (let* ([g (current-graph)]
         [frame (new frame%
                     [label "paper tree"]
                     [width 1000]
                     [height 1000])]
         [canvas (new canvas% [parent frame]
                      [paint-callback
                       (lambda (canvas dc)
                         (displayln (length (graph-nodes g)))
                         (show-graph canvas dc g))])])
   (send frame show #t)
))


(define-syntax define-graph
  (syntax-rules (author authors)
    [(_ id author name)
     (define id
       (construct-graph
        (graph-nodes (current-graph))
        (lambda (n1 n2)
          (and (member (author-element-names (parse-author name)) (authors-list (node-content n1)))
               (member (author-element-names (parse-author name)) (authors-list (node-content n2)))))
        (lambda (n1 n2)
          (edge n1 n2))))]
    [(_ id authors (names ...+ . name))
     (define id
           (construct-graph
            (graph-nodes (current-graph))
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
    






  

                     


  