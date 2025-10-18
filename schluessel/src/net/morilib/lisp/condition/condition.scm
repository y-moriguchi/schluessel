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

(define-syntax define-condition-type
  (lambda (x)
    (syntax-case x ()
      ((_ condnm super pred (field accessor) ...)
        #`(begin
             (define condnm
               (make-condition-type
                 'condnm super '(field ...)))
             (define (pred pr)
               (condition-has-type? pr condnm))
             #,(let loop ((f1 #'(field ...)) (a1 #'(accessor ...)))
                  (cond ((null? f1) (if #f #f))
                        (else #`(begin
                                  (define (#,(car a1) pr)
                                          (condition-ref pr '#,(car f1)))
                                  #,(loop (cdr f1) (cdr a1))))))))
      ((_ condnm super constr pred (field accessor) ...)
        #`(begin
             (define condnm
               (make-condition-type
                 'condnm super '(field ...)))
             (define (pred pr)
               (condition-has-type? pr condnm))
             (define (constr field ...)
               (make-condition condnm (list 'field field) ...))
             #,(let loop ((f1 #'(field ...)) (a1 #'(accessor ...)))
                  (cond ((null? f1) (if #f #f))
                        (else #`(begin
                                  (define (#,(car a1) pr)
                                          (condition-ref pr '#,(car f1)))
                                  #,(loop (cdr f1) (cdr a1)))))))))))

(define-syntax condition-srfi
  (syntax-rules ()
    ((_ (cnd (field val) ...) ...)
     (make-compound-condition
       (make-condition cnd (list 'field val) ...) ...))))

(define-condition-type &message &condition
  make-message-condition message-condition?
  (message condition-message))

(define-condition-type &warning &condition
  make-warning warning?)

(define-condition-type &serious &condition
  make-serious-condition serious-condition?)

(define-condition-type &error &serious
  make-error error?)

(define-condition-type &violation &serious
  make-violation violation?)

(define-condition-type &assertion &violation
  make-assertion-violation assertion-violation?)

(define-condition-type &irritants &condition
  make-irritants-condition irritants-condition?
  (irritants condition-irritants))

(define-condition-type &who &condition
  make-who-condition who-condition?
  (who condition-who))

(define-condition-type &non-continuable &violation
  make-non-continuable-violation
  non-continuable-violation?)

(define-condition-type &implementation-restriction
  &violation
  make-implementation-restriction-violation
  implementation-restriction-violation?)

(define-condition-type &lexical &violation
  make-lexical-violation lexical-violation?)

(define-condition-type &syntax &violation
  make-syntax-violation syntax-violation?
  (form syntax-violation-form)
  (subform syntax-violation-subform))

(define-condition-type &undefined &violation
  make-undefined-violation undefined-violation?)

;;
