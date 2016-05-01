#lang s-exp "paper-language.rkt"

(define-paper paper1
  (make-bib
    #:title    "Reference: Racket"
    #:author   (authors "Matthew Flatt" "PLT")
    #:date     "2010"
    #:location (techrpt-location #:institution "PLT Inc."
                                 #:number "PLT-TR-2010-1")
    #:url      "http://racket-lang.org/tr1/"))


(define-paper paper2
  (make-bib
    #:title    "Racket"
    #:author   (authors "Matthew Flatt" "PLT")
    #:date     "2011"
    #:location (techrpt-location #:institution "PLT Inc."
                                 #:number "PLT-TR-2010-1")
    #:url      "http://racket-lang.org/tr1/"))

(define-paper paper3
  (make-bib
   #:title    "Paper"
    #:author   (authors "Sarah Spall" "Scott Bauer")
    #:date     "2011"
    #:location (techrpt-location #:institution "PLT Inc."
                                 #:number "PLT-TR-2010-1")
    #:url      "http:fake.com"))


(define-graph author-graph authors (list "Matthew Flatt" "PLT"));; take all of the nodes in the current graph and draw edges between the nodes which share the author(s)

(displayln author-graph)

;;(draw-graph)

