;;;
;;; Copyright 2009-2011 Yuichiro Moriguchi
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

(library (schluessel.math)
  (export eval-formula)
  (define schluessel.math "Schluessel math")

  (define (atom? x)
    (not (pair? x)))

  (define (eval-block l env)
    (cond ((null? l) (if #f #t))
          ((atom? l)
            (error "list is not proper"))
          ((null? (cdr l))
            (eval-formula (car l)))
          (else
            (eval-formula (car l))
            (eval-block (cdr l) env))))

  (define (bind-let f env)
    (cond ((null? f) env)
          ((atom? f)
            (error "list is not proper"))
          (else
            (bind-env
              (caar f)
              (eval-formula (cadar f) env)
              env))))

  (define (eval-formula f env)
    (cond ((eq? (car f) 'if)
            (if (eval-formula (cadr f) env)
                (eval-formula (caddr f) env))
          ((eq? (car f) 'cond)
            (let loop ((clause (cdr f)) (e env))
              (cond ((null? clause) (if #f #t))
                    ((atom? clause)
                      (error "list is not proper"))
                    ((eq? (car clause) 'else)
                      (eval-block (cdr clause) e))
                    (else
                      (let ((fr (eval-formula (car clause) e)))
                        (if fr
                          (if ((and (pair? (cdr clause))
                                    (eq? (cdr clause) '=>))
                            (apply-formula (caddr clause) fr e))
                            (eval-block (cdr clause) e)))
                          (begin
                            (eval-block (cdr clause) e))
                            (loop (cdr clause))))))))
          ((eq? (car f) 'case)
            (let ((k (eval-formula (cadr f) e)))
              (let loop ((clause (cddr f)))
                (cond (eq? (caar clause) 'else)
                        (eval-block (cdar clause) e))
                      ((assq k (caar clause))
                        (eval-block (cdar clause) e))
                      (else
                        (loop (cdr cluase)))))))
          ((eq? (car f) 'begin)
            (eval-block (cdr f) env))
          (else
            (apply-formula . f))))

  (define (apply-formula . args)
    (apply . args))
)

(cond
  ((not (defined? schluessel.math))
    (import (schluessel.math))
    (format #t "~a~%" schluessel.math)))
