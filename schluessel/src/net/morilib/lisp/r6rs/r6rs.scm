;;;
;;; Copyright 2009 Yuichiro Moriguchi
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(define (string-for-each proc . strings)
  (let loop ((itr (apply string-group-iterator strings)))
    (cond ((iterator-null? itr) (if #f #f))
          (else
            (apply proc (iterator-car itr))
            (loop (iterator-cdr itr))))))

(define (vector-map proc . vectors)
  (let loop ((itr (apply vector-group-iterator vectors)))
    (if (iterator-null? itr)
        '()
        (cons (apply proc (iterator-car itr))
              (loop (iterator-cdr itr))))))

(define (vector-for-each proc . vectors)
  (let loop ((itr (apply vector-group-iterator vectors)))
    (cond ((iterator-null? itr) (if #f #f))
          (else
            (apply proc (iterator-car itr))
            (loop (iterator-cdr itr))))))

(define (find proc lst)
  (let loop ((itr (list-group-iterator lst)))
    (cond ((iterator-null? itr) #f)
          ((apply proc (iterator-car itr)) (car (iterator-car itr)))
          (else (loop (iterator-cdr itr))))))

(define (for-all proc . lists)
  (let loop ((itr (apply list-group-iterator lists)) (r #t))
    (cond ((iterator-null? itr) r)
          ((apply proc (iterator-car itr)) =>
            (lambda (x) (loop (iterator-cdr itr) x)))
          (else #f))))

(define (exists proc . lists)
  (let loop ((itr (apply list-group-iterator lists)))
    (cond ((iterator-null? itr) #f)
          ((apply proc (iterator-car itr)) => (lambda (x) x))
          (else (loop (iterator-cdr itr))))))

(define (filter proc lst)
  (cond ((null? lst) '())
        ((proc (car lst)) (cons (car lst) (filter proc (cdr lst))))
        (else (filter proc (cdr lst)))))

(define (partition proc lst)
  (values (filter proc lst)
          (filter (lambda (x) (not (proc x))) lst)))

(define (fold-left combine nil . lists)
  (let loop ((nil nil) (itr (apply list-group-iterator lists)))
    (if (iterator-null? itr)
        nil
        (loop (apply combine `(,@(iterator-car itr) ,nil))
              (iterator-cdr itr)))))

(define (fold-right combine nil . lists)
  (let loop ((itr (apply list-group-iterator lists)))
    (if (iterator-null? itr)
        nil
        (apply combine
               `(,@(iterator-car itr) ,(loop (iterator-cdr itr)))))))

(define (remp proc lst)
  (cond ((null? lst) '())
        ((proc (car lst)) (remp proc (cdr lst)))
        (else (cons (car lst) (remp proc (cdr lst))))))
(define (remove obj list)
  (remp (lambda (x) (equal? x obj)) list))
(define (remv obj list)
  (remp (lambda (x) (eqv? x obj)) list))
(define (remq obj list)
  (remp (lambda (x) (eq? x obj)) list))

(define (memp proc lst)
  (cond ((null? lst) #f)
        ((proc (car lst)) lst)
        (else (memp proc (cdr lst)))))

(define (assp proc alst)
  (cond ((null? alst) #f)
        ((proc (caar alst)) (car alst))
        (else (assp proc (cdr alst)))))

(define (cons* . objs)
  (if (null? objs) '()
      (let loop ((lst objs))
        (if (null? (cdr lst))
            (car lst)
            (cons (car lst) (loop (cdr lst)))))))

(define-sharp-quote "#'" 'syntax)
(define-sharp-quote "#`" 'quasisyntax)
(define-sharp-quote "#," 'unsyntax)
(define-sharp-quote "#,@" 'unsyntax-splicing)

;(define-syntax with-syntax
;  (lambda (x)
;    (syntax-case x ()
;      ((_ ((p e0) ...) e1 e2 ...)
;       (syntax (syntax-case (list e0 ...) ()
;                 ((p ...) (let () e1 e2 ...))))))))

(define (make-variable-transformer proc) proc)

;(define (syntax-error fmt . args)
;  (error (get-default-message
;           'err.syntaxcase.usererror
;           (format #f fmt args))))
(define-syntax syntax-error
  (syntax-rules ()
    ((_ fmt args ...)
      (error (get-default-message
               'err.syntaxcase.usererror
               (format #f fmt args ...))))))

(define (generate-temporaries l)
  (cond ((null? l) '())
         ((pair? l) (cons (generate-temporaries (car l))
                          (generate-temporaries (cdr l))))
         ((identifier? l) (gensym))
         (else l)))

