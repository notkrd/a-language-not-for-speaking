#lang racket

(require json)
 
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
(define (cycle-from lst word change)
  (let ([the-index (index-of lst word)])
    (if the-index
        (cycle-n lst the-index change)
        word)))
(define (format-sentence str)
  (let ([chars-list (string->list str)])
    (if (equal? (length chars-list) 0)
        str
        (string-append (list->string (cons (char-upcase (first chars-list)) (rest chars-list))) "." ))))
(define (dashes str) (string-append "- " str " -"))


; Reading corpora
(define harvard-sentences 
  (read-cat 'data "phrases/harvard_sentences.json"))
(define burroughs-instructions (cat-nested 'instruction
                                           (read-cat 'burroughsinstructionset "phrases/burroughsinstructionset.json")))
(define authors (read-cat 'authors "phrases/authors.json"))
(define containers (read-cat 'containers "phrases/containers.json"))
(define all-ultraconserved (read-cat 'ultraconserved_words "phrases/ultraconserved.json"))
(define ultraconserved (flatten (hash-ref all-ultraconserved 'english)))
(define ultraconserved-langs (hash-keys all-ultraconserved))
(define stopwords (read-cat 'stopWords "phrases/stopwords_en.json"))
; (define stopconserved (set-intersect (list->set stopwords) (list->set ultraconserved)))
(define grammatical-cats (list "adjs" "nouns" "adverbs" "prepositions"))
(define adjs (read-cat 'adjs "phrases/adjs.json"))
(define nouns (read-cat 'nouns "phrases/nouns.json"))
(define adverbs (read-cat 'adverbs "phrases/adverbs.json"))
(define ergative-verbs (read-cat 'ergative_verbs "phrases/ergative_verbs.json"))
(define prepositions (read-cat 'prepositions "phrases/prepositions.json"))
(define interjections (read-cat 'interjections "phrases/interjections.json"))
(define personal-pronouns-subj (flatten (map (lambda (h) (hash-ref h 'word)) (file->json "phrases/personal_pronouns_subj.json"))))
(define personal-pronouns-obj (flatten (map (lambda (h) (hash-ref h 'word)) (file->json "phrases/personal_pronouns_obj.json"))))
(define determiners (list "a" "the" "that" "this" "one" "some" "each" "neither" "little" "much" "their" "his" "her" "zir" "your" "its"))
(define past-verbs (map (lambda (entry) (hash-ref entry 'past)) (read-cat 'verbs "phrases/verbs.json")))
(define infinitives (file->json "phrases/infinitive_verbs.json"))
(define environmental-hazards (read-cat 'entries "phrases/environmental_hazards.json"))
(define closed-pairs (read-cat 'pairs "phrases/closed_pairs.json"))
(define closed-left (map (lambda (p) (first p)) closed-pairs))
(define closed-right (map (lambda (p) (second p)) closed-pairs))
(define list-names (list "personal_pronouns_subj" "adverbs" "past_verbs" "determiners" "adjs" "nouns"))
(define memory-lists (list personal-pronouns-subj adverbs past-verbs determiners adjs nouns))


; Other assorted constants
(define vowels (string->list "aeiou"))
(define consonants (string->list "bcdfghjklmnpqrstvwxyz"))

; Language operations

(define (say-something) (string-append "(" (random-elt burroughs-instructions) ": " (string-downcase(random-elt harvard-sentences)) ")"))

(define (say-phrase cats) (string-join (map random-elt cats)))

(define (translate word)
  (let
      ([word-index (index-of (hash-ref all-ultraconserved 'english) (list word))])
    (if word-index
        (let ([a-lang (random-elt (remove 'english ultraconserved-langs))])
          (let (
                [an-entry (list-ref (hash-ref all-ultraconserved a-lang) word-index)])
            (if (not (empty? an-entry))
                (string-append "_" (first an-entry) "_")
                word)))
        word)))

(define (translate-phrase phrase) (string-join (map translate phrase)))

; Lexicons

(struct lexicon (active words zero-words composed wordlists))
(define (new-lexicon)
  (let ([zero-words (map (lambda (l) (random-elt l)) memory-lists)])
    (lexicon
     3
     zero-words
     zero-words
     "   "
     memory-lists)))

(define (append-phrase a-lexicon a-phrase) (struct-copy lexicon a-lexicon
                                                        [composed (string-append (lexicon-composed a-lexicon) " " a-phrase)]))

(define (active-word a-lexicon) (list-ref (lexicon-words a-lexicon) (lexicon-active a-lexicon)))

(define (say-active-word a-lexicon)
  (append-phrase a-lexicon (active-word a-lexicon)))

(define (get-of-cat a-lexicon a-cat)
  (let
      ([cat-index (index-of list-names a-cat)])
    (if cat-index
        (list-ref (lexicon-words a-lexicon) cat-index)
        "")))

(define noun-phrase-pattern '("adjs" "nouns"))
(define sentence-pattern '("personal_pronouns_subj" "adverbs" "past_verbs" "determiners" "adjs" "nouns"))

(define (say-sentence a-lexicon [a-pattern sentence-pattern]) (append-phrase a-lexicon (format-sentence (string-join (map (lambda (cat) (get-of-cat a-lexicon cat)) a-pattern)))))
(define (lexicon-state a-lexicon) (display (list (lexicon-active a-lexicon) ";" (lexicon-words a-lexicon) ";" (lexicon-zero-words a-lexicon) ";" (lexicon-composed a-lexicon))))

; Handling language


(define (increment-word a-lexicon n)
  (let ([active-list (list-ref (lexicon-wordlists a-lexicon) (lexicon-active a-lexicon))]
        [active-word (list-ref (lexicon-words a-lexicon) (lexicon-active a-lexicon))])
    (struct-copy lexicon a-lexicon
                 [words (replace-nth
                         (lexicon-words a-lexicon)
                         (lexicon-active a-lexicon)
                         (cycle-from active-list active-word n))])))

(define (shift-list a-lexicon n) (struct-copy lexicon a-lexicon
                                              [active (modulo (+ (lexicon-active a-lexicon) n) (length (lexicon-wordlists a-lexicon)))]))


(define (find-closing a-sent curr-index [curr-depth 1])
  (if (>= curr-index (length a-sent))
      (- curr-index 1)
      (let ([a-word (list-ref a-sent curr-index)])
        (cond
          [(member a-word closed-right)
           (if (equal? curr-depth 1)
               curr-index
               (find-closing a-sent (+ curr-index 1) (- curr-depth 1)))]
          [(member a-word closed-left)
           (find-closing a-sent (+ curr-index 1) (+ curr-depth 1))]
          [else
           (find-closing a-sent (+ curr-index 1) curr-depth)]))))


(define (find-opening a-sent curr-index [curr-depth 1])
  (if (<= curr-index 0)
      0
      (let ([a-word (list-ref a-sent curr-index)])
        (cond
          [(member a-word closed-left)
           (if (equal? curr-depth 1)
               curr-index
               (find-opening a-sent (- curr-index 1) (- curr-depth 1)))]
          [(member a-word closed-right)
           (find-opening a-sent (- curr-index 1) (+ curr-depth 1))]
          [else
           (find-opening a-sent (- curr-index 1) curr-depth)]))))

(define (get-block a-sent [previous '()])
  (cond [(equal? (length a-sent) 0) (reverse previous)]
        [(member (first a-sent) prepositions) (reverse previous)]
        [else (get-block (rest a-sent) (cons (first a-sent) previous))]))

(define (handle-text a-sent)
  (cond
    [(string? a-sent) (lexicon-composed (read-first-word (string-split a-sent) (new-lexicon)))]
    [else (say-something)]))
(define (? a-sent) (display (handle-text a-sent)))

(define (read-word a-sent a-lexicon original [loops 0])
  (if (or (equal? (length a-sent) 0) (>= loops 280))
      (say-sentence a-lexicon)
      (let ([a-word (first a-sent)])
        (cond
          [(equal? (string-length a-word) 0) a-lexicon]
          [(member (string-titlecase a-word) authors)
           (read-word (rest a-sent) (append-phrase a-lexicon (say-something)) original loops)]
          [(member a-word interjections)
           (read-word (rest a-sent) (append-phrase a-lexicon (string-append (translate-phrase (get-block a-sent)))) original loops)]
          [(member a-word closed-left)
           (if (equal? (active-word a-lexicon) (list-ref (lexicon-zero-words a-lexicon) (lexicon-active a-lexicon)))
               (let
                   ([close-index (find-closing original (+ 1 (- (length original) (length a-sent))))])
                 (read-word
                  (list-tail original (min (+ close-index 1) (length original)))
                  (append-phrase a-lexicon (say-phrase (list (list "-") prepositions determiners environmental-hazards '("-"))))
                  original
                  loops))
               (read-word (rest a-sent) a-lexicon original loops)
               )]
          [(member a-word closed-right)
           (let
               ([opening-index (find-opening original (- (length original) (length a-sent) 1))])
             (read-word
              (list-tail original opening-index)
              (append-phrase a-lexicon (say-phrase (list '("to") infinitives)))
              original
              (+ loops 1)))]
          [(equal? (modulo (string-length a-word) 2) 0)
           (cond
             [(member (last (string->list a-word)) consonants)
              (read-word
               (rest a-sent)
               (say-active-word (increment-word a-lexicon 1))
               original
               loops)]
             [(member (last (string->list a-word)) vowels)
              (read-word
               (rest a-sent)
               (increment-word a-lexicon -1)
               original)]
             [else (read-word (rest a-sent) a-lexicon original)])]
          [(equal? (modulo (string-length a-word) 2) 1)
           (cond
             [(member (first (string->list a-word)) consonants)
              (read-word
               (rest a-sent)
               (say-active-word (shift-list a-lexicon 1))
               original
               loops)]
             [(member (first (string->list a-word)) vowels)
              (read-word
               (rest a-sent)
               (shift-list a-lexicon -1)
               original
               loops)]
             [else (read-word
                    (rest a-sent) a-lexicon original loops)])]
          [else (read-word (rest a-sent) a-lexicon original loops)]))))

(define (read-first-word a-sent a-lexicon) (read-word a-sent (say-sentence a-lexicon) a-sent))

; Testing


(define l (new-lexicon))
(define s "an open sea is upon an artful Wolfe ghostly appearing in time mostly")

(define tests
  '((translate-phrase (string-split (say-something)))
    (lexicon-words l)
    (lexicon-words (increment-word l 2))
    (get-of-cat l "adverbs")
    (find-closing (string-split s) 7)
    (find-opening (string-split s) 10)
    (handle-text s)
    (handle-text "ab ab baa ab ab ab bairn aab ab baa ba cairn")
    (handle-text "What if I wrote a sentence and began again or an idea on this keyboard to start and begin to write like Austen writes or about the chair on which I sit quietly")
    (? "The voice I need for an us, that would say something
in the glossy words that might matter, requires flinching.
Black heads and beaks in the grass: surface, return.
How to express another way of perching, sinking into this chair.")
    (find-closing (string-split "here's some random exquisite ugly and so on facetious and specious but visit that place") 4)
    (find-opening (string-split "this alluvial plains has grown a great many pluvial grains") 7)))

; Rubble
; lexicon-composed (foldl handle-word (new-lexicon) (string-split sent))

;(define (handle-word a-word a-lexicon)
;  (cond
;    [(equal? (string-length a-word) 0) a-lexicon]
;    [(equal? (modulo (string-length a-word) 2) 0)
;     (cond
;       [(member (first (string->list a-word)) vowels)
;        (say-active-word (shift-list a-lexicon 1))]
;       [(member (first (string->list a-word)) consonants)
;        (say-active-word (shift-list a-lexicon -1))]
;       [else a-lexicon])]
;    [(equal? (modulo (string-length a-word) 2) 1)
;     (cond
;       [(member (last (string->list a-word)) vowels)
;        (say-active-word (increment-word a-lexicon 1))]
;       [(member (last (string->list a-word)) consonants)
;        (say-active-word (increment-word a-lexicon -1))]
;       [else a-lexicon])]
;    [else a-lexicon]))

;(define (get-block a-sent [curr-depth 1] [prior '()])
;  (if
;    (null? a-sent) (reverse prior)
;    (let ([a-word (first a-sent)]
;          [the-rest (rest a-sent)])
;      (cond
;        [(member a-word prepositions)
;         (if (equal? curr-depth 1)
;             (reverse prior)
;             (get-block the-rest (- curr-depth 1) (cons a-word prior)))]
;        [(member a-word personal-pronouns) (get-block the-rest (+ curr-depth 1) (cons a-word prior))]
;        [else (get-block the-rest curr-depth (cons a-word prior))]))))