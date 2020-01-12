#lang scheme
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


;(atom? (quote()))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote()))
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) (quote()))
      ((or
        (eq? o1 (car lat))
        (eq? o2 (car lat)))
       (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote()))
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote()))
      ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (o+ (add1 n) (sub1 m))))))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (o- (sub1 n) (sub1 m))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(define ox
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (ox n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
      ((o< n m) #f)
      ((o> n m) #f)
      (else #t))))

(define o^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (ox n (o^ n (sub1 m)))))))

(define o/
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (o/ (- n m) m))))))

(define olength
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (olength (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote()))
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (o= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

(define one?
  (lambda (n)
    (o= n 1)))

(define rempick2
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? a (car l))
          (rember* a (cdr l)))
         (else 
          (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? old (car l))
          (cons old (cons new (insertR* new old (cdr l)))))
         (else
          (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote()))
      ((atom? (car l))
       (cond
         (
          (eq? old (car l))
          (cons new (cons (car l) (insertL* new old (cdr l))))
          )
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or
         (eq? a (car l))
         (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1)) (atom? (car l2)))
       (cond
         ((eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))
         (else #f)))
      ((or (atom? (car l1)) (atom? (car l2))) #f)
      (else
       (and
        (eqlist? (car l1) (car l2))
        (eqlist? (cdr l1) (cdr l2)))))))

(define eqlist2?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else
       (and
        (equal? (car l1) (car l2))
        (equal? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))
      
(define rember2
  (lambda (s l)
    (cond
      ((null? l) (quote()))
      ((equal? (car l) s) (cdr l))
      (else (cons (car l) (rember2 s (cdr l)))))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      ((eq? (car (cdr aexp)) (quote +)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote ^)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp))))))
      ((eq? (car (cdr aexp)) (quote x)) (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(define numbered2?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered2? (car aexp)) (numbered2? (car (cdr (cdr aexp)))))))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (car (cdr nexp)) (quote +)) (o+ (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote ^)) (o^ (value (car nexp)) (value (car (cdr (cdr nexp))))))
      ((eq? (car (cdr nexp)) (quote x)) (ox (value (car nexp)) (value (car (cdr (cdr nexp)))))))))




;PAGE: 104
(value '(1 + (3 ^ 4)))
;(numbered2? '(3 + (4 x 5)))
;(numbered? '(3 + (4 x 5)))
;(eqlist2? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))
;(eqlist2? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (cola))))
;(equal? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))
;(equal? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (cola))))
;(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (soda))))
;(eqlist? '(beef ((sausage)) (and (soda))) '(beef ((sausage)) (and (cola))))
;(leftmost '((potato) (chips ((with) fish) (chips))))
;(member* (car (quote(chips))) (quote('(potato)'(chips'('(with)fish)'(chips)))))
;(insertL* (car (quote(pecker))) (car (quote(chuck))) (quote(how much '(wood) could '('(a '(wood) chuck)) '('('(chuck))) '(if '(a) '('(wood chuck))) could chuck wood)))
;(subst* (car (quote(orange))) (car (quote(banana))) (quote(quote(banana) (quote(split (quote(quote(quote(quote(banana ice))) (quote(cream (quote(banana)))) (quote(sherbert)) (quote(banana)) (quote(bread)) (quote(banana brandy)))))))))
;(occur* (car (quote(banana))) (quote(quote(banana) (quote(split (quote(quote(quote(quote(banana ice))) (quote(cream (quote(banana)))) (quote(sherbert)) (quote(banana)) (quote(bread)) (quote(banana brandy)))))))))
;(insertR* (car (quote(biscuit))) (car (quote(cup))) (quote((quote(coffee)) cup (quote(quote(tea) cup)) (quote(and (quote(hick)))) cup)))
;(rember* (car (quote(cup))) (quote((quote(coffee)) cup (quote(quote(tea) cup)) (quote(and (quote(hick)))) cup)))
;(rempick2 3 (quote(1 car 3 ball 3 car)))
;(one? 1)
;(one? 2)
;(occur (car (quote(car))) (quote(1 car 2 ball 3 car)))
;(occur 3 (quote(1 car 3 ball 3 car)))
;(occur 4 (quote(1 car 3 ball 3 car)))
;(eqan? 4 (car (quote(car))))
;(eqan? 3 2)
;(eqan? 3 3)
;(eqan? (car (quote(car))) (car (quote(ball))))
;(eqan? (car (quote(car))) (car (quote(car))))
;(all-nums (quote(5 pears 6 prunes 9 dates)))
;(no-nums (quote(5 pears 6 prunes 9 dates)))
;(rempick 3 (quote(hotdogs with host mustard)))
;(pick 4 (quote(lasagna spaghetti ravioli macaroni meatball)))
;(olength (quote(hame and cheese on rye)))
;(o/ 15 4)
;(o^ 5 3)
;(o> 3 3)
;(tup+ (quote(3 7 8 1)) (quote(4 6)))
;(ox 3 12)
;(addtup (quote(1 2 3 4)))
;(o+ 46 12)
;(o- 17 9)
;(lat? (quote(bacon and eggs)))
;(insertR (car (quote(topping))) (car (quote(fudge))) (quote(ice cream with fudge for fudge dessert)))
;(insertL (car (quote(topping))) (car (quote(fudge))) (quote(ice cream with fudge for fudge dessert)))
;(subst (car (quote(topping))) (car (quote(fudge))) (quote(ice cream with fudge for fudge dessert)))
;(subst2 (car (quote(vanilla))) (car (quote(chocolate))) (car (quote(banana))) (quote(banana ice cream with chocolate topping)))
;(multirember (car (quote(cup))) (quote(coffee cup tea cup and hick cup)))
;(multiinsertR (car (quote(topping))) (car (quote(fudge))) (quote(ice cream with fudge for fudge dessert)))
;(multiinsertL (car (quote(topping))) (car (quote(fudge))) (quote(ice cream with fudge for fudge dessert)))