#lang scheme


;1
(define (TRANSPORTATION-COST farm)
  (if (null? (t-cost FARMS farm))
      0
      (t-cost FARMS farm)))

(define (t-cost flist farm)
  (if (equal? flist null)
      null
      (if (equal? (car(car flist)) farm)
          (car(cdr(car flist)))
          (t-cost (cdr flist) farm))))

;2
(define (AVAILABLE-CROPS farm) (a-c FARMS farm))

(define (a-c flist farm)
  (cond
    ((null? flist) '())
    ((equal? farm (car(car flist))) (car(cdr(cdr(car flist)))))
    (else (a-c (cdr flist) farm))))

;3
(define (INTERESTED-CROPS customer) (i-c CUSTOMERS customer))

(define (i-c clist customer)
  (cond
    ((null? clist) '())
    ((equal? (car(car clist)) customer) (car(cdr(cdr(car clist)))))
    (else (i-c (cdr clist) customer))))


;4
(define (CONTRACT-FARMS customer) (c-f CUSTOMERS customer))

(define (c-f clist customer)
  (cond
    ((null? clist) '())
    ((equal? (car(car clist)) customer) (car(cdr(car clist))))
    (else (c-f (cdr clist) customer))))


;5
(define (CONTRACT-WITH-FARM farm) (c-w-c '() CUSTOMERS farm))

(define (member? list mem)
  (if (null? list) #f
      (or (eq? (car list) mem) (member? (cdr list) mem))))

(define (c-w-c rlist clist farm)
  (cond
    ((null? clist) rlist)
    ((member? (CONTRACT-FARMS (car(car clist))) farm) (c-w-c (append rlist (list(car(car clist)))) (cdr clist) farm))
    (else (c-w-c rlist (cdr clist) farm))))

;6
(define (INTERESTED-IN-CROP crop) (i-i-c '() CUSTOMERS crop))

(define (i-i-c rlist clist crop)
  (cond
    ((null? clist) rlist)
    ((member? (INTERESTED-CROPS (car(car clist))) crop) (i-i-c (append rlist (list(car(car clist)))) (cdr clist) crop))
    (else (i-i-c rlist (cdr clist) crop))))

;7
(define (MIN-SALE-PRICE crop)
  (cond
    ((null? (msp crop '() CROPS)) 0)
    (else (Min (msp crop '() CROPS)))))

(define (Min alist)
  (cond
    ((null? alist) alist)
    (else(if (null? (cdr alist))
        (car alist)
        (if (< (car alist) (Min (cdr alist)))
            (car alist)
            (Min (cdr alist)))))))


(define (msp crop rlist clist)
  (cond
    ((null? clist) rlist)
    ((equal? (car(car clist)) crop) (msp crop (append rlist (list(car(cdr(cdr(car clist)))))) (cdr clist)))
    (else (msp crop rlist (cdr clist)))))

;8
(define (CROPS-BETWEEN min max) (remove-duplicates (cb min max '() CROPS)))

(define (btw min max x) (and (<= x max) (>= x min)))

(define (cb min max rlist clist)
  (cond
    ((null? clist) rlist)
    ((btw min max (car(cdr(cdr(car clist))))) (cb min max (append rlist (list(car(car clist)))) (cdr clist)))
    (else (cb min max rlist (cdr clist)))))
    
;9
(define (BUY-PRICE customer crop)
  (cond
    ((null? (Min (bp (CONTRACT-FARMS customer) crop '() CROPS))) 0)
    (else (Min (bp (CONTRACT-FARMS customer) crop '() CROPS)))))

(define (bp flist crop rlist crops)
  (cond
    ((null? flist) rlist)
    ((null? (return-price (car flist) crops crop)) (bp (cdr flist) crop rlist crops))
    (else (bp (cdr flist) crop (append rlist (return-price (car flist) crops crop)) crops))))

(define (return-price farm crops crop)
  (cond
    ((null? crops) '())
    ((and (equal? farm (car(cdr(car crops)))) (equal? crop (car(car crops)))) (list(+ (TRANSPORTATION-COST farm) (car(cdr(cdr(car crops)))))))
    (else (return-price farm (cdr crops) crop))))

  
;10
(define (TOTAL-PRICE customer) (sumup (tp (INTERESTED-CROPS customer) customer '())))

(define (tp clist customer rlist)
  (cond
    ((null? clist) rlist)
    (else (tp (cdr clist) customer (append rlist (list(BUY-PRICE customer (car clist))))))))

(define (sumup l1)
  (cond
    ((null? l1) 0)
    ((null? (cdr l1)) (car l1))
    (else (+ (car l1) (sumup (cdr l1))))))


