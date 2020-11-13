#lang racket/base

(provide == conde fresh run run*
         conda condu project)

(define-syntax lambdag@
  (syntax-rules ()
    ((_ (s) e) (lambda (s) e))))

(define-syntax lambdaf@
  (syntax-rules ()
    ((_ () e) (lambda () e))))

(define unify
  (lambda (u v s)
    (let ((u (walk u s))
          (v (walk v s)))
      (cond
        ((eq? u v) s)
        ((var? u) 
         (cond
           ((var? v) (ext-s u v s))
           (else (ext-s-check u v s))))
        ((var? v) (ext-s-check v u s))
        ((and (pair? u) (pair? v))
         (let ((s (unify (car u) (car v) s)))
           (and s (unify (cdr u) (cdr v) s))))
        ((equal? u v) s)
        (else #f)))))                                                       

(define walk
  (lambda (v s)
    (cond
      ((var? v) 
       (let ((a (assq v s)))
         (cond
           (a (walk (cdr a) s))
           (else v))))
      (else v))))

(define ext-s-check                                                             
  (lambda (x v s)                                                               
    (cond                                                                       
      ((occurs-check x v s) #f)                                                 
      (else (ext-s x v s)))))

(define occurs-check
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) (eq? v x))
        ((pair? v)
         (or (occurs-check x (car v) s) (occurs-check x (cdr v) s)))
        (else #f)))))

(define ext-s                                                                   
  (lambda (x v s)                                                               
    (cons `(,x . ,v) s)))

(define empty-s '())                                                            

(define var vector)

(define var? vector?)

(define reify
  (letrec
      ((reify-s
        (lambda (v s)
          (let ((v (walk v s)))
            (cond
              ((var? v) (ext-s v (reify-name (length s)) s))
              ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
              (else s))))))
    (lambda (v s)
      (let ((v (walk* v s)))
        (walk* v (reify-s v empty-s))))))

(define walk*
  (lambda (w s)
    (let ((v (walk w s)))
      (cond
        ((var? v) v)
        ((pair? v) (cons (walk* (car v) s) (walk* (cdr v) s)))
        (else v)))))

(define reify-name
  (lambda (n)
    (string->symbol 
     (string-append "_" "."  (number->string n)))))

(define-syntax mzero 
  (syntax-rules () 
    ((_) #f)))

(define-syntax unit
  (syntax-rules ()
    ((_ a) a)))

(define-syntax choice
  (syntax-rules ()
    ((_ a f) (cons a f))))

(define-syntax inc
  (syntax-rules ()
    ((_ e) (lambdaf@ () e))))

(define-syntax case-inf
  (syntax-rules ()
    ((_ e on-zero ((a^) on-one) ((a f) on-choice) ((f^) on-inc))
     (let ((a-inf e))
       (cond
         ((not a-inf) on-zero)
         ((procedure? a-inf) (let ((f^ a-inf)) on-inc))
         ((and (pair? a-inf) (procedure? (cdr a-inf)))
          (let ((a (car a-inf)) (f (cdr a-inf))) on-choice))
         (else (let ((a^ a-inf)) on-one)))))))

(define-syntax ==
  (syntax-rules ()
    ((_ u v) 
     (lambdag@ (s)
               (unify u v s)))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (s)
               (inc
                (mplus* 
                 (bind* (g0 s) g ...)
                 (bind* (g1 s) g^ ...) ...))))))

(define-syntax mplus*
  (syntax-rules ()
    ((_ e) e)
    ((_ e0 e ...) (mplus e0 (lambdaf@ () (mplus* e ...))))))

(define mplus
   (lambda (a-inf f)
     (case-inf a-inf
        (f)
        ((a) (choice a f))
        ((a f^) (choice a (lambdaf@ () (mplus (f) f^))))
        ((f^) (inc (mplus (f) f^))))))  

(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (s)
               (let ((x (var 'x)) ...)
                 (bind* (g0 s) g ...))))))

(define-syntax bind*
  (syntax-rules ()
    ((_ e) e)
    ((_ e g0 g ...)
     (let ((a-inf e))
       (and a-inf (bind* (bind a-inf g0) g ...))))))

(define bind
  (lambda (a-inf g)
    (case-inf a-inf
              (mzero)
              ((a) (g a))
              ((a f) (mplus (g a) (lambdaf@ () (bind (f) g))))
              ((f) (inc (bind (f) g))))))

(define-syntax run
  (syntax-rules ()
    ((_ n (x) g ...)
     (run n x g ...))
    ((_ n (x0 x ...) g ...)
     (run n q (fresh (x0 x ...)
                     (== `(,x0 ,x ...) q) g ...)))
    
    ((_ n x g0 g^ ...)
     (take n (lambdaf@ ()
                       (let ((g (fresh (x)
                                  (lambdag@ (s)
                                            (bind* (g0 s) g^ ... 
                                                   (lambdag@ (s)
                                                             (list (reify x s))))))))
                         (g empty-s)))))))

(define-syntax run*
  (syntax-rules ()
    ((_ x g ...) (run #f x g ...))))

(define take
  (lambda (n f)
    (if (and n (zero? n))
        '()
        (case-inf (f)
                  '()
                  ((a) a)
                  ((a f) (cons (car a) (take (and n (- n 1)) f)))
                  ((f) (take n f))))))

(define-syntax conda
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (s)
               (if* (picka (g0 s) g ...) (picka (g1 s) g^ ...) ...)))))

(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (g1 g^ ...) ...)
     (lambdag@ (s)
               (if* (picku (g0 s) g ...) (picku (g1 s) g^ ...) ...)))))

(define-syntax if*
  (syntax-rules ()
    ((_) (mzero))
    ((_ (pick e g ...) b ...)
     (let loop ((a-inf e))
       (case-inf a-inf
                 (if* b ...)
                 ((a) (bind* a-inf g ...))
                 ((a f) (bind* (pick a a-inf) g ...))
                 ((f) (inc (loop (f)))))))))

(define-syntax picka
  (syntax-rules ()
    ((_ a a-inf) a-inf)))

(define-syntax picku
  (syntax-rules ()
    ((_ a a-inf) (unit a))))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (lambdag@ (s)
               (let ((x (walk* x s)) ...)
                 (bind* (g0 s) g ...))))))
