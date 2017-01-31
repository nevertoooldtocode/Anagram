#lang racket

; Actual anagram procedure
; Takes a list of strings, each string a single word

(define (anagrams single-word-list)
  
  (define (remove-hyphen string)
    (list->string
     (filter (lambda (char) (not (char=? char #\-)))
             (string->list string))))
  
  (define (create-key string)
    (list->string
     (sort (filter (lambda (char) (not (char=? char #\-)))
                   (string->list string))
           char<?)))
  
  (define (squash sequence)
    (define (iter buffered-line previous-key sqnce)
      (if (null? sqnce)
          (list buffered-line)
          (let* ([current-key (caar sqnce)]
                 [current-word (cadar sqnce)]
                 [rest (cdr sqnce)])
            (if (string=? previous-key current-key)
                (iter (cons current-word buffered-line) previous-key rest)
                (cons buffered-line
                      (iter (list current-word) current-key rest))))))
    (cdr (iter '() "" sequence)))
  
  (let* ([ĳ->ij-single-word-list (map (lambda (string) (string-replace string "ĳ" "ij"))
                                      single-word-list)]
         [key-added-list (map (lambda (line) (list (create-key line) line))
                              ĳ->ij-single-word-list)]
         [sorted-list (sort key-added-list string<? #:key car)]
         [anagram-list (filter (lambda (entry) (> (length entry) 1))
                               (squash sorted-list))]
         [alist-without-hyphens (filter (lambda (entry)
                                          (not (string=? (remove-hyphen (car entry))
                                                         (remove-hyphen (cadr entry)))))
                                        anagram-list)]
         [alist-sorted-entries (map (lambda (entry) (sort entry string<?))
                                    alist-without-hyphens)])
    alist-sorted-entries))


;(define anagram-list (anagrams (file->lines "NL woordenlijst Gutenberg.txt")))
(define anagram-list (anagrams (file->lines "Dutch cleaned.txt")))

(define (anagram-tuples-longer-than anagram-list lower-bound)
  (sort
   (filter
    (lambda (entry) (> (length entry) lower-bound))
    anagram-list)
   >
   #:key length))

(define (anagrams-longer-than anagram-list lower-bound)
  (sort
   (filter
    (lambda (entry) (> (string-length (car entry)) lower-bound))
    anagram-list)
   >
   #:key (lambda (entry) (string-length (car entry)))))

(define (make-frequency-table input-list key . inc)
  
  (define (add-item table item)
    (define (iter-add-item result rest)
      (if (null? rest)
          (cons (list (key item) 1) result)
          (if (equal? (caar rest) (key item))
              (append result
                      (list (list (caar rest)
                                  (if (null? inc)
                                      (add1 (cadar rest))
                                      ((car inc) item (cadar rest)))))
                      (cdr rest))
              (iter-add-item (append result (list (car rest))) (cdr rest)))))
    (iter-add-item '() table))
  
  (define (iter-make-table result rest)
    (if (null? rest)
        result
        (iter-make-table (add-item result (car rest)) (cdr rest))))
  (iter-make-table '() input-list)
  )

(define anagram-wordlength-frequency-table
  (make-frequency-table
   anagram-list
   (lambda (item) (string-length (car item)))
   (lambda (item value) (+ (length item) value))))

(define wordlength-frequency-table
  (make-frequency-table (file->lines "Dutch cleaned.txt") string-length))

(define anagram-tuplelength-frequency-table
  (make-frequency-table
   anagram-list
   length))


;(anagram-tuples-longer-than anagram-list 4)
;(anagrams-longer-than anagram-list 9)

;anagram-wordlength-frequency-table
;(sort wordlength-frequency-table < #:key car)
;anagram-tuplelength-frequency-table

(define anagrams-with-ij-and-ji
  (filter (lambda (line)
            (for/or ([word line])
              (not (string-contains? word "ij"))))
          (filter (lambda (line)
                    (for/or ([word line])
                      (string-contains? word "ij")))
                  anagram-list)))

anagrams-with-ij-and-ji


