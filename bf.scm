;; Brainfuck Interpreter

(import (rnrs records syntactic (6)))

(define-record-type bf
  (fields
   code
   data
   (mutable code-ptr)
   (mutable data-ptr)
   (mutable stack)
   in
   out))

(define (bf-data-ptr-1+! bf)
  (bf-data-ptr-set! bf (1+ (bf-data-ptr bf))))

(define (bf-data-ptr-1-! bf)
  (bf-data-ptr-set! bf (1- (bf-data-ptr bf))))

(define (bf-code-ptr-1+! bf)
  (bf-code-ptr-set! bf (1+ (bf-code-ptr bf))))

(define (bf-code-ptr-1-! bf)
  (bf-code-ptr-set! bf (1- (bf-code-ptr bf))))

(define (bf-datum-1+! bf)
  (bf-datum-set! bf (1+ (bf-datum bf))))

(define (bf-datum-1-! bf)
  (bf-datum-set! bf (1- (bf-datum bf))))

(define (bf-datum bf)
  (hash-ref (bf-data bf) (bf-data-ptr bf) 0))

(define (bf-datum-set! bf v)
  (hash-set! (bf-data bf) (bf-data-ptr bf) v))

(define (bf-command bf)
  (vector-ref (bf-code bf) (bf-code-ptr bf)))

(define (bf-init code in out)
  (make-bf (list->vector (string->list code))
           (make-hash-table)
           0 0 '() in out))

(define (bf-run! bf)
  (when (not (bf-done? bf))
    (let* ([command (bf-command bf)]
           [action (bf-action bf command)])
      (action bf)
      (bf-code-ptr-1+! bf)
      (bf-run! bf))))

(define (bf-action bf command)
  (case command
    [(#\>) bf-data-ptr-1+!]
    [(#\<) bf-data-ptr-1-!]
    [(#\+) bf-datum-1+!]
    [(#\-) bf-datum-1-!]
    [(#\.) bf-print-char]
    [(#\,) bf-read-char!]
    [(#\:) bf-print-int]
    [(#\;) bf-read-int!]
    [(#\[) bf-next!]
    [(#\]) bf-prev!]
    [(#\#) bf-stack-push!]
    [(#\$) bf-stack-pop!]
    [else bf-ignore]))

(define (bf-print-char bf)
  (display (integer->char (bf-datum bf)) (bf-out bf)))

(define (bf-read-char! bf)
  (let ([c (read-char (bf-in bf))])
    (if (eof-object? c)
        (error "bf-read-char!: no chars")
        (bf-datum-set! bf (char->integer c)))))

(define (bf-print-int bf)
  (display (bf-datum bf) (bf-out bf))
  (display " " (bf-out bf)))

(define (bf-read-int! bf)
  (let loop ([digits '()])
    (cond
     [(can-read-digit? (bf-in bf))
      (loop (cons (read-char (bf-in bf)) digits))]
     [(null? digits)
      (error "bf-read-int!: no digits")]
     [else
      (skip-space (bf-in bf))
      (bf-datum-set!
       bf (string->number
           (list->string (reverse digits))))])))

(define (skip-space port)
  (let ([s (peek-char port)])
    (when (and (char? s) (char-whitespace? s))
      (read-char port)
      (skip-space port))))

(define (can-read-digit? port)
  (let ([d (peek-char port)])
    (and (char? d) (char-numeric? d))))

(define (bf-next! bf)
  (when (zero? (bf-datum bf))
    (bf-code-ptr-1+! bf)
    (bf-go-to-forward-bracket! bf)))

(define (bf-prev! bf)
  (bf-code-ptr-1-! bf)
  (bf-go-to-backward-bracket! bf)
  (bf-code-ptr-1-! bf))

(define (bf-stack-push! bf)
  (bf-stack-set!
   bf (cons (bf-datum bf) (bf-stack bf))))

(define (bf-stack-pop! bf)
  (bf-datum-set! bf (car (bf-stack bf)))
  (bf-stack-set! bf (cdr (bf-stack bf))))

(define (bf-ignore bf)
  #f)

(define (bf-go-to-forward-bracket! bf)
  (let loop ([level 0])
    (let ([command (bf-command bf)])
      (cond
       [(equal? command #\[)
        (bf-code-ptr-1+! bf)
        (loop (1+ level))]
       [(equal? command #\])
        (when (positive? level)
          (bf-code-ptr-1+! bf)
          (loop (1- level)))]
       [else
        (bf-code-ptr-1+! bf)
        (loop level)]))))

(define (bf-go-to-backward-bracket! bf)
  (let loop ([level 0])
    (let ([command (bf-command bf)])
      (cond
       [(equal? command #\])
        (bf-code-ptr-1-! bf)
        (loop (1+ level))]
       [(equal? command #\[)
        (when (positive? level)
          (bf-code-ptr-1-! bf)
          (loop (1- level)))]
       [else
        (bf-code-ptr-1-! bf)
        (loop level)]))))

(define (bf-done? bf)
  (>= (bf-code-ptr bf) (vector-length (bf-code bf))))

(define* (bf-exec program #:optional data)
  (with-output-to-string
    (lambda ()
      (bf-run!
       (bf-init program
                (open-input-string (or data ""))
                (current-output-port))))))
