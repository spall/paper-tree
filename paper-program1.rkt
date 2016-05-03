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


(define-paper paper4
  (make-bib
    #:title    "Racketeering"
    #:author   (authors "Matthew Flatt")
    #:date     "2015"
    #:location (techrpt-location #:institution "PLT Inc."
                                 #:number "PLT-TR-2015-1")
    #:url      "http://racket-lang.org/tr1/"))

(define-graph author-graph authors (list "Matthew Flatt"));; take all of the nodes in the current graph and draw edges between the nodes which share the author(s)

(displayln author-graph)

(draw-graph author-graph)

;;(draw-graph)

