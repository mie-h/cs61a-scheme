(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.

(define (cons-all first rests)
    (define fn (lambda (x) (append (list first) x)))
    (map fn rests)
)

(define (zip pairs)
    (define fn-first (lambda (x) (car x)))
    (define fn-second (lambda (x) (car (cdr x))))
    (list (map fn-first pairs) (map fn-second pairs))
)

;; Problem 16
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 16
  (define (helper lst count)
    (if (null? lst)
      nil
      (append (list (list count (car lst))) (helper (cdr lst) (+ count 1)))
    )
  )
  (helper s 0)
)
  ; END PROBLEM 16

;; Problem 17
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 17
  (cond
    ((null? denoms) nil)
    ((= total 0) (list (list (car denoms))))
    ((= total (car denoms))
                  (append (list-change (- total (car denoms)) denoms)
                          (list-change total (cdr denoms))))
    ((< total (car denoms)) (list-change total (cdr denoms)))
    (else (append (cons-all (car denoms) (list-change (- total (car denoms)) denoms))
                  (list-change total (cdr denoms))))
  )
)
  ; END PROBLEM 17

;; Problem 18
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr)
         ; BEGIN PROBLEM 18
           expr
         ; END PROBLEM 18
         )
        ((quoted? expr)
         ; BEGIN PROBLEM 18
           expr
         ; END PROBLEM 18
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
           (define ev-params (map let-to-lambda params))
           (define ev-body (map let-to-lambda body))
           (cons form (cons  ev-params ev-body))
           ; END PROBLEM 18
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN PROBLEM 18
           (define ev-vals (zip (let-to-lambda values)))
           (define ev-body (map let-to-lambda body))
           (cons (cons 'lambda (cons (car ev-vals) ev-body))
                 (cons (let-to-lambda (car (cadr ev-vals))) (cdr (cadr ev-vals))))


           ; END PROBLEM 18
           ))
        (else
         ; BEGIN PROBLEM 18
         (map let-to-lambda expr)
         ; END PROBLEM 18
         )))
