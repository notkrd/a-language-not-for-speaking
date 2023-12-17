#lang racket
(require json)
(require racket/set)
(require megaparsack)

; Utility functions
(define (cat-nested a-cat a-json) (map (lambda (an-entry) (hash-ref an-entry a-cat)) a-json))
(define (file->json a-file) (read-json (open-input-file a-file)))
(define (read-cat a-cat a-file) (hash-ref (file->json a-file) a-cat))
(define (random-elt a-list) (list-ref a-list (random (length a-list))))
(define (replace-nth lst n x)
  (if (and (>= n 0) (< n (length lst)))
      (if (= n 0)
          (cons x (cdr lst))
          (cons (car lst) (replace-nth (cdr lst) (- n 1) x)))
      lst))
(define (cycle-n lst start change) (list-ref lst (modulo (+ start change) (length lst))))
(define (cycle-from lst word change) (let (
                                           [the-index (index-of lst word)])
                                       (if the-index
                                           (cycle-n lst the-index change)
                                           0)))


; Reading corpora
(define harvard-sentences 
  (read-cat 'data "phrases/harvard_sentences.json"))
(define burroughs-instructions (cat-nested 'instruction
                                           (read-cat 'burroughsinstructionset "phrases/burroughsinstructionset.json")))
(define fragments (file->lines "phrases/fragments.txt"))
(define containers (read-cat 'containers "phrases/containers.json"))
(define all-ultraconserved (read-cat 'ultraconserved_words "phrases/ultraconserved.json"))
(define ultraconserved (flatten (hash-ref all-ultraconserved 'english)))
(define ultraconserved-langs (hash-keys all-ultraconserved))
(define stopwords (read-cat 'stopWords "phrases/stopwords_en.json"))
(define stopconserved (set-intersect (list->set stopwords) (list->set ultraconserved)))
(define grammatical-cats (list "adjs" "nouns" "adverbs" "prepositions"))
(define adjs (read-cat 'adjs "phrases/adjs.json"))
(define nouns (read-cat 'nouns "phrases/nouns.json"))
(define adverbs (read-cat 'adverbs "phrases/adverbs.json"))
(define ergative-verbs (read-cat 'ergative_verbs "phrases/ergative_verbs.json"))
(define prepositions (read-cat 'prepositions "phrases/prepositions.json"))
(define list-names (list "containers" "ultraconserved" "stopwords" "adjs" "nouns" "adverbs" "prepositions" "ergative_verbs"))
(define memory-lists (list containers ultraconserved stopwords adjs nouns adverbs prepositions))

; Other assorted constants
(define vowels (string->list "aeiou"))
(define consonants (string->list "bcdfghjklmnpqrstvwxyz"))

; Language operations

(define (say-something) (string-append (random-elt burroughs-instructions) ": " (string-downcase(random-elt harvard-sentences))))

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

; Lexicons

(struct lexicon (wordlists words active composed))
(define (new-lexicon) (lexicon
                       memory-lists
                       (map (lambda (l) (random-elt l)) memory-lists)
                       (random (length memory-lists))
                       ""))

(define (append-phrase a-lexicon a-phrase) (struct-copy lexicon a-lexicon
                                                        [composed (string-append (lexicon-composed a-lexicon) " " a-phrase)]))

(define (active-word a-lexicon) (list-ref (lexicon-words a-lexicon) (lexicon-active a-lexicon)))

; Handling language

(define (say-active-word a-lexicon)
  (append-phrase a-lexicon (active-word a-lexicon)))

(define (increment-word a-lexicon n)
  (let (
        [active-list (list-ref (lexicon-wordlists a-lexicon) (lexicon-active a-lexicon))]
        [active-word (list-ref (lexicon-words a-lexicon) (lexicon-active a-lexicon))])
    (struct-copy lexicon a-lexicon
                 [words (replace-nth
                         (lexicon-words a-lexicon)
                         (lexicon-active a-lexicon)
                         (cycle-from active-list active-word n))])))

(define (shift-list a-lexicon n) (struct-copy lexicon a-lexicon
                                              [active (modulo (+ (lexicon-active a-lexicon) n) (length (lexicon-wordlists a-lexicon)))]))


(define (increment-and-say a-lexicon n)
  (let (
        [active-list (list-ref (lexicon-wordlists a-lexicon) (lexicon-active a-lexicon))]
        [old-word (list-ref (lexicon-words a-lexicon) (lexicon-active a-lexicon))])
    (struct-copy lexicon a-lexicon
                 [words (replace-nth
                         (lexicon-words a-lexicon)
                         (lexicon-active a-lexicon)
                         (cycle-from active-list old-word n))]
                 [composed (string-append (lexicon-composed a-lexicon) " "
                                          (list-ref (lexicon-words a-lexicon)
                                                    (lexicon-active a-lexicon)))])))

(define (handle-word a-word a-lexicon)
  (cond
    [(equal? (string-length a-word) 0) a-lexicon]
    [(equal? (modulo (string-length a-word) 2) 0) (cond
                                                    [(member (first (string->list a-word)) vowels)
                                                     (say-active-word (shift-list a-lexicon 1))]
                                                    [(member (first (string->list a-word)) consonants)
                                                     (say-active-word (shift-list a-lexicon -1))]
                                                    [else a-lexicon])]
    [(equal? (modulo (string-length a-word) 2) 1) (cond
                                                    [(member (last (string->list a-word)) vowels)
                                                     (say-active-word (increment-word a-lexicon 1))]
                                                    [(member (last (string->list a-word)) consonants)
                                                     (say-active-word (increment-word a-lexicon -1))]
                                                    [else a-lexicon])]
    [else a-lexicon]))

(define (handle-sentence sent)
  (cond
    [(string? sent) (lexicon-composed (foldl handle-word (new-lexicon) (string-split sent)))]
    [else (say-something)]))

(define (read-word a-sent a-lexicon)
  (let ([words (string-split a-sent)])
    (if (equal? (length words) 0)
        a-lexicon  
        (let ([a-word (first words)])
          (cond
            [(equal? (string-length a-word) 0) a-lexicon]
            [(equal? (modulo (string-length a-word) 2) 0)
             (cond
               [(member (first (string->list a-word)) vowels)
                (read-word
                 (rest words)
                 (say-active-word (shift-list a-lexicon 1)))]
               [(member (first (string->list a-word)) consonants)
                (read-word
                 (rest words)
                 (say-active-word (shift-list a-lexicon -1)))]
               [else (read-word
                      (rest words) a-lexicon)])]
            [(equal? (modulo (string-length a-word) 2) 1)
             (cond
               [(member (last (string->list a-word)) vowels)
                (read-word
                 (rest words)
                 (say-active-word (increment-word a-lexicon 1)))]
               [(member (last (string->list a-word)) consonants)
                (read-word
                 (rest words)
                 (say-active-word (increment-word a-lexicon -1)))]
               [else (read-word
                      (rest words) a-lexicon)])]
            [else (read-word
                   (rest words) a-lexicon)])))))


; Testing
(translate-phrase (say-something))
(define l (new-lexicon))
(lexicon-words l)
(lexicon-words (increment-word l 2))
(handle-sentence "this is a sentence I see of some old kind")
