(define (flatmap f x)
  (define (flatHelper current f x)
          (if (null? x) current
                        (flatHelper (append current (f (car x))) f (cdr x))
          
          )
  )
  (flatHelper nil f x)
)

(define (expand lst)
  (define (h x)
          (cond ((eq? x 'x) '(x r y f r))
                ((eq? x 'y) '(l f x l y))    
                (else (list x))
          )
  )
  (flatmap h lst)
)

(define (interpret instr dist)
  (cond
    ((null? instr) (fd 0))
    ((equal? (car instr) 'f) (begin (fd dist)  (interpret (cdr instr) dist)))
    ((equal? (car instr) 'l) (begin (lt 90) (interpret (cdr instr) dist)))
    ((equal? (car instr) 'r) (begin (rt 90)  (interpret (cdr instr) dist)))
    (else (interpret (cdr instr) dist))
  )
)

(define (apply-many n f x)
  (if (zero? n)
      x
      (apply-many (- n 1) f (f x))))

(define (dragon n d)
  (interpret (apply-many n expand '(f x)) d))