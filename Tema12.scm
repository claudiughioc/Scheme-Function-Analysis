(include "Reflection.scm")

;; 1) Detectie tip recursivitate
(define nu 'NON-RECURSIVE)
(define stiva 'STACK-RECURSIVE)
(define coada 'TAIL-RECURSIVE)
(define tree 'TREE-RECURSIVE)


(define (maxim L)
  (cond 
    ((null? L) 0)
    (else (let ((maxo (maxim (cdr L))))
            (cond
              ((> (car L)  maxo) (car L))
              (else maxo))))))
;(maxim '(2 3 5))
(define (curry f)
  (lambda (name)
    (lambda (L)
      (f L name))))

(define (count L el)
  (cond
    ((null? L) 0)
    (else 
     (cond
      ((equal? (car L) el) (add1 (count (cdr L) el)))
      (else (count (cdr L) el))))))
;(count '(1 2 3 1) 1)
;(((curry count) 1) '(1 2 3 4 1))
;(map ((curry count) 1) '((1 2 3 1) (1 2 1)))
(define (recu L name)
  (cond
    ((not (list? L)) 0)
    ((equal? (car L) 'lambda) (recu (caddr L) name))
    ((equal? (car L) 'let) (recu (caddr L) name))
    ((equal? (car L) name) 1)
    ((equal? (car L) 'if)
     (let ((item1 (recu (caddr L) name))
           (item2 (recu (cadddr L) name)))
       (max item1 item2)))
    ((or (equal? (car L) 'else) (equal? (car L) 'car) (equal? (car L) 'cdr) (equal? (car L) 'null?)) 
     (let ((rezultat (recu (cdr L) name)))
       (cond
         ((= rezultat 1) 2)
         (else rezultat))))
    ((equal? (car L) 'cond) 
     (let ((List (map ((curry recu) name) (cdr L))))
       (maxim List)))
    ((list? (car L))
     (let ((List (map ((curry recu) name) L)))
       (maxim List)))
    (else 
     (let* ((List (map ((curry recu) name) (cdr L)))
            (stacks (count List 2))
            (tails (count List 1))
            (trees (count List 3)))
       (cond
         ((and (= stacks 0) (= tails 0) (= trees 0)) 0)
         ((or (> stacks 1) (> tails 1) (> trees 0) (> (+ stacks tails) 1)) 3)
         (else 2))))))
(define (recursion-type f)
  (let ((rez (recu (get-lambda f) (get-name f))))
    (cond
      ((= rez 0) nu)
      ((= rez 1) coada)
      ((= rez 2) stiva)
      (else tree))))
   
;(recursion-type no-recursive-1)

 
 

;; 2) Constant-folding

(define (check L el)
  (cond
    ((null? L) 0)
    ((list? (car L)) (check (append (car L) (cdr L)) el))
    ((equal? el (car L)) (add1 (check (cdr L) el)))
    (else (check (cdr L) el))))

(define (exists L A)
  (cond
    ((null? A) 0)
    ((+ (check L (car A)) (exists L (cdr A))))))
;(exists '(1 '(3 5) 2 3 4 3) '(3 5))
(define (curry2 f)
  (lambda (name)
    (lambda (A)
      (lambda (L)
        (f L A name)))))
(define (analyze-cond L A name)
;  (display "In cond: ")
  ;(display L)
  
  (cond
    ((null? L) null)
    (else
     (let* ((branch (car L))
            (condition (car branch))
            (expresion (cadr branch)))
       (cond 
         ((equal? (car branch) 'else);daca e ramura else o intorc cu expresia evaluata
          (cons (list 'else (evaluate expresion A name)) '()))
         ((= 0 (exists condition A)) 
          (let ((value (eval condition)))
            (cond
              ((equal? value #t) (list 'else (evaluate expresion A name)) );daca ramura mea e adevarata tot timpul o fac else
              (else (analyze-cond (cdr L) A name))))) ;altfel o sterg si trec la urmatoarea
         (else (cons (list (evaluate condition A name) (evaluate expresion A name)) (analyze-cond (cdr L) A name))))))))

(define (evaluate L A name ) ;L = lambda expresia, A = lista de argumente; 
  ;(display "Intru cu lista: ")
  ;(display L )
  ;(display "\n")
  (cond
    ((not (list? L)) L)
    ((null? L) null)
    (else
     (let ((nr (exists (list L) A)))
       (cond
         ((equal? (car L) 'lambda)
          (list 'lambda (cadr L) (evaluate (caddr L) (append (cadr L) A) name)))
         ((= nr 0) (eval L))
         ((equal? (car L) 'let)
          (cons 'let (list (map (((curry2 evaluate) name) A) (cadr L)) (evaluate (caddr L) (append A (map car (cadr L))) name)))) ;adaug let la restul listei deja evaluata el cu el
         ((equal? (car L) name)
          (cons name (map (((curry2 evaluate) name) A) (cdr L))));adaug numele functiei la restul listei deja evaluata
         ((equal? (car L) 'if)
          (let ((condition (cadr L))
                (first (caddr L))
                (second (cadddr L)))
            (if (= 0 (exists condition A));daca nu sunt argumente prin conditie
                (let ((value (eval condition)))
                  (cond
                    ((equal? value #t) (evaluate first A name))
                    (else (evaluate second A name))))
                (cons 'if (cons (evaluate condition A name) (list (evaluate first A name) (evaluate second A name)))))))
         ((equal? (car L) 'cond)
          (let ((List (analyze-cond (cdr L) A name)))
            (cond
              ((equal? (car List) 'else) (cadr List));daca prima ramura e tot timpul adevarata intorc expresia ei evaluata
              (else (cons 'cond List)))))
         ((or (equal? (car L) 'car) (equal? (car L) 'cdr) (equal? (car L) 'null?))
          (let ((arg (car L)))
            (if (= 0 (exists (list(cadr L)) A))
                (eval L)
                (list arg (evaluate (cadr L) A name)))))
         ((or (equal? (car L) 'cons) (equal? (car L) 'map) (equal? (car L) 'filter) (equal? (car L) '=) (equal? (car L) '>) (equal? (car L) '<))
          (let ((operator (car L))
                (nr1 (exists (list (cadr L)) A))
                (nr2 (exists (list (caddr L)) A)))
            (cond   
              ((and (> nr1 0) (> nr2 0)) (eval L))
              ((and (= nr1 0) (= nr2 0)) (cons operator (list (eval (cadrL)) (eval (caddr L)))))
              ((and (= nr1 0) (> nr2 0)) (cons operator (list (eval (cadr L)) (evaluate (caddr L) A name))))
              (else (cons operator (list (evaluate (cadr L) A name) (evaluate (caddr L) A name)))))))
         ((or (equal? (car L) '+) (equal? (car L) '-) (equal? (car L) '*) (equal? (car L) '/))
          (let ((nr1 (exists L A)))
            (cond
              ((= nr1 0) (eval L))
              (else (cons (car L) (map (((curry2 evaluate) name) A) (cdr L)))))))
         (else 
          (let ((firstarg (car L)) ;oricarea ar fi el, apel de functie definita la let
                (nr (exists (cdr L) A)))
            (cond
              ((= nr 0) (cons firstarg (eval (cdr L))))
              (else (cons firstarg (map (((curry2 evaluate) name) A) (cdr L))))))))))))


(define (optimize f)
  (let  ((Llist (get-lambda f))
         (name (get-name f)))
    (evaluate Llist (cadr Llist) name)))
;(eval (caddr (get-lambda folding-1-2)))
;(optimize folding-2-2)

  
