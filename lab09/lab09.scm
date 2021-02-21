;; Scheme ;;

(define (over-or-under a b)
  (cond
      ((> a b) 1)
      ((< a b) -1)
      (else 0)
  )
)

;;; Tests
(over-or-under 1 2)
; expect -1
(over-or-under 2 1)
; expect 1
(over-or-under 1 1)
; expect 0

(define (filter-lst fn lst)
  (if (null? lst)
      nil
      (if (fn (car lst))
          (cons (car lst) (filter-lst fn (cdr lst)))
          (filter-lst fn (cdr lst))
      )
  )
)

;;; Tests
(define (even? x)
  (= (modulo x 2) 0))
(filter-lst even? '(0 1 1 2 3 5 8))
; expect (0 2 8)

(define (make-adder n)
  (lambda (x) (+ n x))
)

;;; Tests
(define adder (make-adder 5))
(adder 8)
; expect 13

;; Extra questions

(define lst
  (list (cons 1 nil) 2 (cons 3 (cons 4 nil)) 5)
)

(define (composed f g)
  (lambda (x) (f (g x)))
)

(define (remove item lst)
  (filter-lst (lambda (x) (not (eq? x item))) lst)
)


;;; Tests
(remove 3 nil)
; expect ()
(remove 3 '(1 3 5))
; expect (1 5)
(remove 5 '(5 3 5 5 1 4 5 4))
; expect (3 1 4 4)

(define (no-repeats s)
  (if (null? s)
      nil
      (cons (car s) (no-repeats (remove (car s) (cdr s))))
  )
)

(define (substitute s old new)
  (if (null? s)
      nil
      (if (pair? (car s))
          (cons (substitute (car s) old new) (substitute (cdr s) old new))
          (if (eq? (car s) old)
              (cons new (substitute (cdr s) old new))
              (cons (car s) (substitute (cdr s) old new))
          )
      ) 
  )
)


(define (sub-all s olds news)
  (if (or (null? s) (null? olds))
      s
      (sub-all (substitute s (car olds) (car news)) (cdr olds) (cdr news) )
  )
)