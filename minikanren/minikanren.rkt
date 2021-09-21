#lang racket/base

(provide succeed fail
         == fresh run run*
         conde all condi alli
         conda condu project)


(define-syntax lambdag@
  (syntax-rules ()
    [(_ (s) e) (lambda (s) e)]))

(define-syntax lambdaf@
  (syntax-rules ()
    [(_ () e) (lambda () e)]))


(define unify
  (lambda (u v s)
    (let ([u (walk u s)]
          [v (walk v s)])
      (cond
        [(eq? u v) s]
        [(var? u)
         (cond
           [(var? v) (ext-s u v s)]
           [else (ext-s-check u v s)])]
        [(var? v) (ext-s-check v u s)]
        [(and (pair? u) (pair? v))
         (let ([s (unify (car u) (car v) s)])
           (and s (unify (cdr u) (cdr v) s)))]
        [(equal? u v) s]
        [else #f]))))

(define walk
  (lambda (v s)
    (cond
      [(var? v)
       (let ([a (assq v s)])
         (cond
           [a (walk (cdr a) s)]
           [else v]))]
      [else v])))

(define ext-s-check
  (lambda (x v s)
    (cond
      [(occurs-check x v s) #f]
      [else (ext-s x v s)])))

(define occurs-check
  (lambda (x v s)
    (let ([v (walk v s)])
      (cond
        [(var? v) (eq? v x)]
        [(pair? v)
         (or (occurs-check x (car v) s) (occurs-check x (cdr v) s))]
        [else #f]))))

(define ext-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

(define empty-s '())

(define var vector)

(define var? vector?)

(define reify
  (letrec
      ([reify-s
        (lambda (v s)
          (let ([v (walk v s)])
            (cond
              [(var? v) (ext-s v (reify-name (length s)) s)]
              [(pair? v) (reify-s (cdr v) (reify-s (car v) s))]
              [else s])))])
    (lambda (v s)
      (let ([v (walk* v s)])
        (walk* v (reify-s v empty-s))))))

(define walk*
  (lambda (w s)
    (let ([v (walk w s)])
      (cond
        [(var? v) v]
        [(pair? v) (cons (walk* (car v) s) (walk* (cdr v) s))]
        [else v]))))

(define reify-name
  (lambda (n)
    (string->symbol
     (string-append "_" "."  (number->string n)))))

(define-syntax mzero
  (syntax-rules ()
    [(_) #f]))

(define-syntax unit
  (syntax-rules ()
    [(_ a) a]))

(define-syntax choice
  (syntax-rules ()
    [(_ a f) (cons a f)]))

(define-syntax inc
  (syntax-rules ()
    [(_ e) (lambdaf@ () e)]))

(define-syntax case-inf
  (syntax-rules ()
    [(_ e on-zero ((a^) on-one) ((a f) on-choice) ((f^) on-inc))
     (let ([a-inf e])
       (cond
         [(not a-inf) on-zero]
         [(procedure? a-inf) (let ([f^ a-inf]) on-inc)]
         [(and (pair? a-inf) (procedure? (cdr a-inf)))
          (let ([a (car a-inf)] [f (cdr a-inf)]) on-choice)]
         [else (let ([a^ a-inf]) on-one)]))]))

(define succeed (lambdag@ (s) (unit s)))
(define fail (lambdag@ (s) (mzero)))

(define-syntax ==
  (syntax-rules ()
    [(_ u v)
     (lambdag@ (s)
       (unify u v s))]))

(define-syntax fresh
  (syntax-rules ()
    [(_ (x ...) g0 g ...)
     (lambdag@ (s)
       (let ([x (var 'x)] ...)
         (bind* (g0 s) g ...)))]))

(define-syntax conde
  (syntax-rules (else)
    [(_) fail]
    [(_ [g]) g]
    [(_ [else g]) g]
    [(_ [else g0 g ...]) (conde [g0 g ...])]
    [(_ [g0 g ...] c ...)
     (lambdag@ (s)
       (inc
        (mplus*
         (bind* (g0 s) g ...)
         ((conde c ...) s))))]))

(define-syntax all
  (syntax-rules ()
    [(_) succeed]
    [(_ g) g]
    [(_ g0 g ...)
     (let ([g^ g0])
       (lambdag@ (s)
         (bind (g^ s)
               (lambdag@ (s)
                 ((all g ...) s)))))]))

(define-syntax mplus*
  (syntax-rules ()
    [(_ e) e]
    [(_ e0 e ...) (mplus e0 (lambdaf@ () (mplus* e ...)))]))

(define mplus
  (lambda (a-inf f)
    (case-inf a-inf
       (f)
       ((a) (choice a f))
       ((a f^) (choice a (lambdaf@ () (mplus (f^) f))))
       ((f^) (inc (mplus (f^) f))))))

(define-syntax bind*
  (syntax-rules ()
    [(_ e) e]
    [(_ e g0 g ...)
     (let ([a-inf e])
       (and a-inf (bind* (bind a-inf g0) g ...)))]))

(define bind
  (lambda (a-inf g)
    (case-inf a-inf
       (mzero)
       ((a) (g a))
       ((a f) (mplus (g a) (lambdaf@ () (bind (f) g))))
       ((f) (inc (bind (f) g))))))

(define-syntax alli
  (syntax-rules ()
    [(_) succeed]
    [(_ g) g]
    [(_ g0 g ...)
     (let ([g^ g0])
       (lambdag@ (s)
         (bindi (g^ s)
                (lambdag@ (s)
                  ((alli g ...) s)))))]))

(define-syntax condi
  (syntax-rules (else)
    [(_) fail]
    [(_ [g]) g]
    [(_ [else g]) g]
    [(_ [else g0 g ...]) (condi [g0 g ...])]
    [(_ [g0 g ...] c ...)
     (lambdag@ (s)
       (inc
        (mplusi*
         (bind* (g0 s) g ...)
         ((condi c ...) s))))]))

(define-syntax mplusi*
  (syntax-rules ()
    [(_ e) e]
    [(_ e0 e ...) (mplusi e0 (lambdaf@ () (mplusi* e ...)))]))

(define mplusi
  (lambda (a-inf f)
    (case-inf a-inf
       (f)
       ((a) (choice a f))
       ((a f^) (choice a (lambdaf@ () (mplusi (f) f^))))
       ((f^) (inc (mplusi (f^) f))))))

(define-syntax bindi*
  (syntax-rules ()
    [(_ e) e]
    [(_ e g0 g ...)
     (let ([a-inf e])
       (and a-inf (bindi* (bindi a-inf g0) g ...)))]))

(define bindi
  (lambda (a-inf g)
    (case-inf a-inf
       (mzero)
       ((a) (g a))
       ((a f) (mplusi (g a) (lambdaf@ () (bindi (f) g))))
       ((f) (inc (bindi (f) g))))))


(define-syntax run
  (syntax-rules ()
    [(_ n (x) g0 g^ ...)
     (take n (lambdaf@ ()
               (let ([g (fresh (x)
                               (lambdag@ (s)
                                 (bind* (g0 s) g^ ...
                                        (lambdag@ (s)
                                          (list (reify x s))))))])
                 (g empty-s))))]))

(define-syntax run*
  (syntax-rules ()
    [(_ (x) g ...) (run #f (x) g ...)]))

(define take
  (lambda (n f)
    (unless (or (not n) (exact-nonnegative-integer? n))
      (raise-arguments-error 'take
                             "n should be natural number or false."
                             "n" n))

    (if (and n (zero? n))
        '()
        (case-inf (f)
           '()
           ((a) a)
           ((a f) (cons (car a) (take (and n (- n 1)) f)))
           ((f) (take n f))))))

(define-syntax conda
  (syntax-rules (else)
    [(_) fail]
    [(_ [g]) g]
    [(_ [else g]) g]
    [(_ [else g0 g ...]) (conda [g0 g ...])]
    [(_ [g0 g ...] c ...)
     (lambdag@ (s) (if* (picka (g0 s) g ...) ((conda c ...) s)))]))

(define-syntax condu
  (syntax-rules (else)
    [(_) fail]
    [(_ [g]) g]
    [(_ [else g]) g]
    [(_ [else g0 g ...]) (condu [g0 g ...])]
    [(_ [g0 g ...] c ...)
     (lambdag@ (s) (if* (picku (g0 s) g ...) ((condu c ...) s)))]))

(define-syntax if*
  (syntax-rules ()
    [(_) (mzero)]
    [(_ g) g]
    [(_ (pick e g ...) c ...)
     (let loop ([a-inf e])
       (case-inf a-inf
          (if* c ...)
          ((a) (bind* a-inf g ...))
          ((a f) (bind* (pick a a-inf) g ...))
          ((f) (inc (loop (f))))))]))

(define-syntax picka
  (syntax-rules ()
    [(_ a a-inf) a-inf]))

(define-syntax picku
  (syntax-rules ()
    [(_ a a-inf) (unit a)]))

(define-syntax project
  (syntax-rules ()
    [(_ (x ...) g0 g ...)
     (lambdag@ (s)
       (let ([x (walk* x s)] ...)
         (bind* (g0 s) g ...)))]))
