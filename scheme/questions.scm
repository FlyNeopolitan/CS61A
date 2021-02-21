(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))


;; Problem 15
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 15
    (define (enumerate_helper s index)
      (if (null? s) nil
                    (cons (list index (car s)) (enumerate_helper (cdr s) (+ index 1)))
      )
    )
    (enumerate_helper s 0)
  )
  ; END PROBLEM 15

;; Problem 16

;; Merge two lists LIST1 and LIST2 according to COMP and return
;; the merged lists.
(define (merge comp list1 list2)
  ; BEGIN PROBLEM 16
  (define (chooseFirst list1 list2)
    (and (not (null? list1)) 
         (or (null? list2) (comp (car list1) (car list2))) 
    ) 
  )
  (cond 
        ((chooseFirst list1 list2) (cons (car list1) (merge comp (cdr list1) list2)))
        ((chooseFirst list2 list1) (cons (car list2) (merge comp list1 (cdr list2))))
        (else nil)
  )
  )
  ; END PROBLEM 16


(merge < '(1 5 7 9) '(4 8 10))
; expect (1 4 5 7 8 9 10)
(merge > '(9 7 5 1) '(10 8 4 3))
; expect (10 9 8 7 5 4 3 1)

;; Problem 17

(define (nondecreaselist s)
    ; BEGIN PROBLEM 17
    (if (null? (cdr s)) (list s)
        (begin (define restPart (nondecreaselist (cdr s)))
                (if (<= (car s) (cadr s)) (cons (cons (car s) (car restPart)) (cdr restPart)) 
                    (cons (list (car s)) restPart)
                )
        )
    )
    
)
    ; END PROBLEM 17


