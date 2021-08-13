#lang racket

(provide (all-defined-out))

;C1
(define (sequence low high stride)
  (cond [(<= low high) (cons low (sequence (+ low stride) high stride))]
        [#t null]))
;C2
(define (string-append-map xs suffix)
  (map (lambda(x) (string-append x suffix)) xs))
;C3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t
         (define num (remainder n (length xs)))
         (define (find xs num)
           (if (= num 0) (car xs) (find (cdr xs) (- num 1))))
         (find xs num)]))
;C4
(define ones (lambda () (cons 1 ones)))
(define (stream-for-n-steps s n)
  (cond [(= n 0) null]
        [#t
         (define temp (s))
         (cons (car temp) (stream-for-n-steps (cdr temp) (- n 1)))]))

;C5
(define funny-number-stream
  (letrec([f (lambda (x)
              (cond
                [(= 0 (remainder x 5)) (cons (- x (* 2 x)) (lambda () (f (+ x 1))))]
                [#t (cons x ( lambda () (f (+ x 1))))]))])

  (lambda () (f 1))))
;C6
(define dan-then-dog
  (letrec([f (lambda (x)
               (cond
                 [(= 1) (cons 'dan.jpg' (lambda() (f 0)))]
                 [#t (cons 'dog.jpg' (lambda() (f 1)))]))])
    (lambda () (f 1))))
;C7

(define (stream-add-zero s)
  (letrec([f (lambda (x) (cons (cons 0 (car (x))) (lambda () (f (cdr (x))))))])
    (lambda () (f s))))

;C8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (xs ys)
                (cons (cons (car xs) (car ys)) (lambda () (f (append (cdr xs) (list (car xs))) (append (cdr ys) (list (car ys)))))))])
    (lambda () (f xs ys))))

;C9
(define (vector-assoc v vec)
  (define (f n)
  (cond [(= (vector-length vec) n) #f]
        [(pair? (vector-ref vec n)) (if (equal? v (car (vector-ref vec n))) (vector-ref vec n) (f (+ 1 n)))]
        [#t (f (+ 1 n))]))
  (f 0))
;C10
(define (cached-assoc xs n)
  (define cache (make-vector n #f))
  (define current-slot 0)

  (define (add-to-cache e)
    (vector-set! cache current-slot e)
    (if (= current-slot n) (set! current-slot 0) (set! current-slot (+ 1 current-slot))))

  (define (look-and-update v)
    (define val (assoc v xs))
     (if val (add-to-cache val) #f)
     val)
  
  (define (f v)
    (define cache-look (vector-assoc v cache))
     (if cache-look cache-look (look-and-update v)))
  (lambda(x) (f x)))
                 
      