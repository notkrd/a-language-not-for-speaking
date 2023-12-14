#lang racket
(require json)
(require racket/set)
(require megaparsack)

(define (cat-nested a-cat a-json) (map (lambda (an-entry) (hash-ref an-entry a-cat)) a-json))
(define (file->json a-file) (read-json (open-input-file a-file)))
(define (read-cat a-cat a-file) (hash-ref (file->json a-file) a-cat))
(define (random-elt a-list) (list-ref a-list (random (length a-list))))

(define harvard-sentences 
  (read-cat 'data "harvard_sentences.json"))
(define burroughs-instructions (cat-nested 'instruction
                                    (read-cat 'burroughsinstructionset "burroughsinstructionset.json")))
(define fragments (file->lines "fragments.txt"))
(define containers (read-cat 'containers "containers.json"))
(define all-ultraconserved (read-cat 'ultraconserved_words "ultraconserved.json"))
(define ultraconserved (flatten (hash-ref all-ultraconserved 'english)))
(define ultraconserved-langs (hash-keys all-ultraconserved))
(define stopwords (read-cat 'stopWords "stopwords_en.json"))
(define stopconserved (set-intersect (list->set stopwords) (list->set ultraconserved)))
  
(define (say-something) (string-append (random-elt burroughs-instructions) ": " (string-downcase(random-elt harvard-sentences))))

(define (handle-sentence sent)
  (cond
    [(string? sent) (say-something)]
    [else (say-something)]))

(define (translate word) (let ([word-index (index-of (hash-ref all-ultraconserved 'english) (list word))])
                           (if word-index
                               (let ([a-lang (random-elt ultraconserved-langs)])
                                 (let (
                                     [an-entry (list-ref (hash-ref all-ultraconserved a-lang) word-index)])
                                 (if (not (empty? an-entry))
                                     (first an-entry)
                                     word)))
                               word)))

(define (translate-phrase phrase) (string-join (map translate (string-split phrase))))


(map (lambda (_) (translate-phrase (say-something))) (range 10))
