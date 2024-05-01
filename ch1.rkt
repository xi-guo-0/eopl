#lang racket/base

(define (list-length lst)
  (if (null? lst)
      0
      (+ 1 (list-length (cdr lst)))))

(define (nth-element lst n)
  (if (null? lst)
      (error "List too short")
      (if (zero? n)
          (car lst)
          (nth-element (cdr lst) (- n 1)))))

(define (remove-first s los)
  (if (null? los)
      '()
      (if (eqv? (car los) s)
          (cdr los)
          (cons (car los)
                (remove-first s (cdr los))))))

(define (occurs-free? var exp)
  (cond
    ((symbol? exp) (eqv? var exp))
    ((eqv? (car exp) 'lambda)
     (and
      (not (eqv? var (car (cadr exp))))
      (occurs-free? var (caddr exp))))
    (else
     (or
      (occurs-free? var (car exp))
      (occurs-free? var (cadr exp))))))

(define (subst-in-s-exp new old sexp)
  (if (symbol? sexp)
      (if (eqv? sexp old) new sexp)
      (subst new old sexp)))

(define (subst new old slist)
  (if (null? slist)
      '()
      (cons (subst-in-s-exp new old (car slist))
            (subst new old (cdr slist)))))

(define (number-elements lst)
  (define (number-elements-from lst n)
    (if (null? lst) '()
        (cons
         (list n (car lst))
         (number-elements-from (cdr lst) (+ n 1)))))
  (number-elements-from lst 0))

(define (vector-sum v)
  (define (partial-vector-sum v n)
    (if (zero? n)
        (vector-ref v 0)
        (+ (vector-ref v n)
           (partial-vector-sum v (- n 1)))))
  (let ((n (vector-length v)))
    (if (zero? n)
        0
        (partial-vector-sum v (- n 1)))))

;; Exercise 1.15
(define (duple n x)
  (if (zero? n)
      '()
      (cons x (duple (- n 1) x))))

;; Exercise 1.16
(define (invert lst)
  (map reverse lst))

;; Exercise 1.17
(define (down lst)
  (map list lst))

;; Exercise1.18
(define (swapper s1 s2 slist)
  (define (swapper-in-s-exp s1 s2 sexp)
    (if (symbol? sexp)
        (cond [(eqv? sexp s1) s2]
              [(eqv? sexp s2) s1]
              [else sexp])
        (swapper s1 s2 sexp)))
  (if (null? slist)
      '()
      (cons (swapper-in-s-exp s1 s2 (car slist))
            (swapper s1 s2 (cdr slist)))))

;; Exercise1.19
(define (partial-list-set lst n x i len)
  (cond [(= i len) '()]
        [(= i n) (cons x (cdr lst))]
        [else (cons (car lst) (partial-list-set (cdr lst) n x (+ i 1) len))]))

(define (list-set lst n x)
  (partial-list-set lst n x 0 (length lst)))

;; Exercise 1.20
(define (count-occurrences s slist)
  (define (count-occurrences-in-s-exp s sexp)
    (if (symbol? sexp)
        (if (eqv? s sexp) 1 0)
        (count-occurrences s sexp)))
  (if (null? slist)
      0
      (+ (count-occurrences-in-s-exp s (car slist))
         (count-occurrences s (cdr slist)))))

;; Exercise 1.21
(define (product sos1 sos2)
  (if (null? sos1)
      '()
      (append (map (lambda (x) (list (car sos1) x)) sos2) (product (cdr sos1) sos2))))

;; Exercise 1.22
(define (filter-in pred lst)
  (cond [(null? lst) '()]
        [(pred (car lst)) (cons (car lst) (filter-in pred (cdr lst)))]
        [else (filter-in pred (cdr lst))]))

;; Exercise 1.23
(define (list-index pred lst)
  (define (list-index-helper pred lst idx)
    (cond [(null? lst) #f]
          [(pred (car lst)) idx]
          [else (list-index-helper pred (cdr lst) (+ idx 1))]))
  (list-index-helper pred lst 0))

;; Exercise1.24
(define (every? pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (every? pred (cdr lst)))))

;; Exercise1.25
(define (exists? pred lst)
  (and (not (null? lst))
       (or (pred (car lst))
           (exists? pred (cdr lst)))))

;; Exercise1.26
(define (up lst)
  (if (null? lst)
      '()
      (let ([x (car lst)])
        (append (if (list? x)
                    x
                    (list x))
                (up (cdr lst))))))
;; Exercise 1.27
(define (flatten slist)
  (if (null? slist)
      '()
      (append (if (list? (car slist))
                  (flatten (car slist))
                  (list (car slist)))
              (flatten (cdr slist)))))

;; Exercise1.28
(define (merge loi1 loi2)
  (cond [(null? loi1) loi2]
        [(null? loi2) loi1]
        [(<= (car loi1) (car loi2)) (cons (car loi1) (merge (cdr loi1) loi2))]
        [else (cons (car loi2) (merge loi1 (cdr loi2)))]))

;; Exercise 1.29
(define (quicksort loi)
  (cond [(null? loi) '()]
        [(null? (cdr loi)) loi]
        [else
         (let ([pivot (car loi)])
           (append (quicksort (filter (lambda (x) (< x pivot)) (cdr loi)))
                   (list pivot)
                   (quicksort (filter (lambda (x) (not (< x pivot))) (cdr loi)))))]))

;; Exercise1.30
(define (sort/predicate pred loi)
  (cond [(null? loi) '()]
        [(null? (cdr loi)) loi]
        [else
         (let ([pivot (car loi)])
           (append (sort/predicate pred (filter (lambda (x) (pred x pivot)) (cdr loi)))
                   (list pivot)
                   (sort/predicate pred (filter (lambda (x) (not (pred x pivot))) (cdr loi)))))]))

;; Exercise 1.31
(define (leaf x)
  x)
(define (interior-node s l r)
  (list s l r))
(define (leaf? t)
  (not (list? t)))
(define (lson t)
  (cadr t))
(define (rson t)
  (caddr t))
(define (contents-of t)
  (if (leaf? t)
      t
      (car t)))

;; Exercise1.32
(define (double-tree t)
  (if (leaf? t)
      (leaf (* 2 (contents-of t)))
      (interior-node (contents-of t) (double-tree (lson t)) (double-tree (rson t)))))

;; Exercise1.33
(define (mark-leaves-with-red-depth t)
  (define (mark-leaves-with-red-depth-helper t d)
    (if (leaf? t)
        (leaf d)
        (let ([d (if (equal? 'red (contents-of t)) (+ d 1) d)])
          (interior-node (contents-of t)
                         (mark-leaves-with-red-depth-helper (lson t) d)
                         (mark-leaves-with-red-depth-helper (rson t) d)))))
  (mark-leaves-with-red-depth-helper t 0))

;; Exercise 1.34
(define (path-helper n bst history)
  (if (list? bst)
      (if (null? bst)
          '()
          (if (= n (car bst))
              (reverse history)
              (let ([l (path-helper n (cadr bst) (cons 'left history))]
                    [r (path-helper n (caddr bst) (cons 'right history))])
                (if (null? r)
                    l
                    r))))
      (if (= n bst)
          (reverse history)
          '())))

(define (path n bst)
  (path-helper n bst '()))

;; Exercise 1.35
(define (number-leaves bt)
  (define (number-leaves-helper bt cnt)
    (if (leaf? bt)
        (cons (leaf cnt) (+ cnt 1))
        (let ([lt (number-leaves-helper (lson bt) cnt)])
          (let ([rt (number-leaves-helper (rson bt) (cdr lt))])
            (cons (interior-node (contents-of bt) (car lt) (car rt)) (cdr rt))))))
  (car (number-leaves-helper bt 0)))

(module+ test
  (require rackunit))

;; Exercise 1.36
(define number-elements2
  (lambda (lst)
    (define g
      (lambda (head tail)
        (cons head
              (map (lambda (item)
                     (list (+ (car item) 1) (cadr item)))
                   tail))))
    (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements2 (cdr lst))))))

(module+ test
  (check-equal? (+ 2 2) 4)
  (check-equal? (list-length '()) 0)
  (check-equal? (list-length '(a)) 1)
  (check-equal? (nth-element '(a b c) 0) 'a)
  (check-equal? (nth-element '(a b c) 2) 'c)
  (check-true (occurs-free? 'x 'x))
  (check-false (occurs-free? 'x 'y))
  (check-false (occurs-free? 'x '(lambda (x) (x y))))
  (check-true (occurs-free? 'x '(lambda (y) (x y))))
  (check-true (occurs-free? 'x '((lambda (x) x) (x y))))
  (check-true (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z))))))
  (check-equal? (subst 'a 'b '((b c) (b () d))) '((a c) (a () d)))
  (check-equal? (duple 2 3) '(3 3))
  (check-equal? (duple 4 '(ha ha)) '((ha ha) (ha ha) (ha ha) (ha ha)))
  (check-equal? (duple 0 '(blah)) '())
  (check-equal? (invert '((a 1) (b 2) (1 b) (2 b))) '((1 a) (2 b) (b 1) (b 2)))
  (check-equal? (down '(1 2 3)) '((1) (2) (3)))
  (check-equal? (down '((a) (fine) (idea))) '(((a)) ((fine)) ((idea))))
  (check-equal? (down '(a (more (complicated)) object)) '((a) ((more (complicated))) (object)))
  (check-equal? (swapper 'a 'd '(a b c d)) '(d b c a))
  (check-equal? (swapper 'a 'd '(a d () c d)) '(d a () c a))
  (check-equal? (swapper 'x 'y '((x) y (z (x)))) '((y) x (z (y))))
  (check-equal? (list-set '(a b c d) 2 '(1 2)) '(a b (1 2) d))
  (check-equal? (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3) '(1 5 10))
  (check-equal? (count-occurrences 'x '((f x) y (((x z) x)))) 3)
  (check-equal? (count-occurrences 'x '((f x) y (((x z) () x)))) 3)
  (check-equal? (count-occurrences 'w '((f x) y (((x z) x)))) 0)
  (check-equal? (product '(a b c) '(x y)) '((a x) (a y) (b x) (b y) (c x) (c y)))
  (check-equal? (filter-in number? '(a 2 (1 3) b 7)) '(2 7))
  (check-equal? (filter-in symbol? '(a (b c) 17 foo)) '(a foo))
  (check-equal? (list-index number? '(a 2 (1 3) b 7)) 1)
  (check-equal? (list-index symbol? '(a (b c) 17 foo)) 0)
  (check-equal? (list-index symbol? '(1 2 (a b) 3)) #f)
  (check-false (every? number? '(a b c 3 e)))
  (check-true (every? number? '(1 2 3 5 4)))
  (check-true (exists? number? '(a b c 3 e)))
  (check-false (exists? number? '(a b c d e)))
  (check-equal? (up '((1 2) (3 4))) '(1 2 3 4))
  (check-equal? (up '((x (y)) z)) '(x (y) z))
  (check-equal? (flatten '(a b c)) '(a b c))
  (check-equal? (flatten '((a) () (b ()) () (c))) '(a b c))
  (check-equal? (flatten '((a b) c (((d)) e))) '(a b c d e))
  (check-equal? (flatten '(a b (() (c)))) '(a b c))
  (check-equal? (merge '(1 4) '(1 2 8)) '(1 1 2 4 8))
  (check-equal? (merge '(35 62 81 90 91) '(3 83 85 90)) '(3 35 62 81 83 85 90 90 91))
  (check-equal? (quicksort '(8 2 5 2 3)) '(2 2 3 5 8))
  (check-equal? (sort/predicate < '(8 2 5 2 3)) '(2 2 3 5 8))
  (check-equal? (sort/predicate > '(8 2 5 2 3)) '(8 5 3 2 2))
  (check-equal? (contents-of (leaf 1)) 1)
  (check-equal? (contents-of (interior-node 'baz (leaf 1) (leaf 2))) 'baz)
  (check-equal? (contents-of (lson (double-tree (interior-node 'baz (leaf 1) (leaf 2))))) 2)
  (check-equal? (contents-of (rson (double-tree (interior-node 'baz (leaf 1) (leaf 2))))) 4)
  (check-equal? (mark-leaves-with-red-depth (leaf 3)) (leaf 0))
  (check-equal? (contents-of (mark-leaves-with-red-depth (interior-node 'e (leaf 3) (leaf 4)))) 'e)
  (check-equal? (lson (mark-leaves-with-red-depth (interior-node 'e (leaf 3) (leaf 4)))) 0)
  (check-equal? (rson (rson (rson (mark-leaves-with-red-depth
                                   (interior-node 'red
                                                  (interior-node 'bar
                                                                 (leaf 26)
                                                                 (leaf 12))
                                                  (interior-node 'red
                                                                 (leaf 11)
                                                                 (interior-node 'quux
                                                                                (leaf 117)
                                                                                (leaf 14))))))))
                2)
  (check-equal? (path 17 '(14 (7 () (12 () ()))
                              (26 (20 (17 () ())
                                      ())
                                  (32 () ()))))
                '(right left left))
  (check-equal? (number-leaves
                 (interior-node 'foo
                                (interior-node 'bar
                                               (leaf 26)
                                               (leaf 12))
                                (interior-node 'baz
                                               (leaf 11)
                                               (interior-node 'quux
                                                              (leaf 117)
                                                              (leaf 14)))))
                '(foo
                  (bar 0 1)
                  (baz
                   2
                   (quux 3 4))))
  (check-equal? (number-elements2 '(a b c d)) '((0 a) (1 b) (2 c) (3 d)))
  )
