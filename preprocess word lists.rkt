#lang racket

(define (char-alphabetic-or-hyphen? char)
  (or (char-alphabetic? char) (char=? char #\-)))

(define (firstword line)
  (define (iter result rest)
    (if (or (null? rest) (not (char-alphabetic-or-hyphen? (car rest))))
        (string-downcase (list->string result))
        (iter (append result (list (car rest))) (cdr rest))))
  (iter '() (string->list line)))

(define (no-blank-lines line)
  (not (string=? line "")))

(define (pre-process file-name)
  (remove-duplicates (map firstword
                          (filter no-blank-lines
                                  (file->lines file-name
                                               #:mode 'text)))))

(call-with-output-file
    "Dutch cleaned.txt"
  (lambda (out) (for-each 
                 (lambda (line) (fprintf out "~a\n" line))
                 (pre-process "Dutch.dic")))
  #:mode 'text
  #:exists 'replace)