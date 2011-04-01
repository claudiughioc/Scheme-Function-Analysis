;; Tema1 - PP
;; Mini - API pentru functii capabile de reflexie.

(define-syntax define-r
  (syntax-rules ()
    ((_ name lambda-ex)
     (define name
       (make-reflection-function 'name 'lambda-ex)))))

(define (get-name f) (f '__GET_NAME__))

(define (get-lambda f) (f '__GET_LAMBDA__))

;;------------------------------------------------------------------------------

;; Functii ajutatoare (nu se vor folosi)

(define (make-reflection-function name lambda-ex)
  (if (not (valid-lambda-ex? lambda-ex))
      (error (string-append "INVALID LAMBDA EXPRESSION FOR FUNCTION "
                            (symbol->string name)))
      (let ((real-function (eval lambda-ex)))
        (lambda args
          (if (and (not (null? args)) (null? (cdr args)))
              (cond
                ((equal? (car args) '__GET_NAME__) name)
                ((equal? (car args) '__GET_LAMBDA__) lambda-ex)
                (else (real-function (car args))))
              (apply real-function args))))))


(define (valid-lambda-ex? ex)
  (and (list? ex)
       (has-length? 3 ex)
       (equal? 'lambda (car ex))
       (list? (cadr ex))))

(define (has-length? n L)
  (cond
    ((zero? n) (null? L))
    ((null? L) #f)
    (else (has-length? (sub1 n) (cdr L)))))
