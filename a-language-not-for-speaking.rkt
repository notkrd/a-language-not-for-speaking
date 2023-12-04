#lang racket
(require json)

(define (random-elt a-list) (list-ref a-list (random (length a-list))))
(define harvard-sentences (hash-ref (read-json
                           (open-input-file "harvard_sentences.json")) 'data))
(define burroughs-instructions (map (lambda (inst) (hash-ref inst 'instruction)) (hash-ref (read-json
                           (open-input-file "burroughsinstructionset.json")) 'burroughsinstructionset)))

(define (say-something) (string-append (random-elt burroughs-instructions) ": " (string-downcase(random-elt harvard-sentences))))

(map (lambda (_) (say-something)) (range 10))
