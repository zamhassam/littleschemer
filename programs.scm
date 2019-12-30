#lang scheme
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(atom? (quote()))

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

;(lat? (quote(bacon and eggs)))
;(insertR (car (quote(topping))) (car (quote(fudge))) (quote(ice cream with fudge for fudge dessert)))
;(insertL (car (quote(topping))) (car (quote(fudge))) (quote(ice cream with fudge for fudge dessert)))
;(subst (car (quote(topping))) (car (quote(fudge))) (quote(ice cream with fudge for fudge dessert)))
;(subst2 (car (quote(vanilla))) (car (quote(chocolate))) (car (quote(banana))) (quote(banana ice cream with chocolate topping)))
;(multirember (car (quote(cup))) (quote(coffee cup tea cup and hick cup)))
(multiinsertR (car (quote(topping))) (car (quote(fudge))) (quote(ice cream with fudge for fudge dessert)))
(multiinsertL (car (quote(topping))) (car (quote(fudge))) (quote(ice cream with fudge for fudge dessert)))