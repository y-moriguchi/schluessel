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
;; --------------------------------
;; Object system
;; --------------------------------
;(library (init$sos)
;  (export
;    define-generic remove-method make initialize define-class)

  (define sos$*slot-args* (make-identity-hash-table))

  (define (sos$concat-symbol2 s1 s2)
    (string->symbol (string-append
      (symbol->string s1) (symbol->string s2))))

  (define (sos$validkeyword k)
    (memq k '(:allocation :init-value :init-keyword ;:init-form
              :getter :setter :accessor :documentation)))

  (define (sos$validalloc k)
    (memq k '(:instance :class :each-subclass :virtual)))

  (define (sos$validate-lst lst)
    (let loop ((l lst))
      (if (null? l) #t
          (let ((a (car l)))
            (cond ((null? a)
                    (error (get-default-message
                             'err.defineclass.malform.slot a)))
                  ((not (symbol? (car a)))
                    (error (get-default-message
                             'err.require.symbol a)))
                  (else 
                    (let loop2 ((m (cdr a)))
                      (cond ((null? m) (loop (cdr l)))
                            ((or (not (pair? m)) (not (pair? (cdr m))))
                              (error (get-default-message
                                       'err.defineclass.malform.slot a)))
                            ((not (sos$validkeyword (car m)))
                              (error (get-default-message
                                       'err.defineclass.slot.unknown
                                       (car m))))
                            ((and
                               (eq? (car m) :allocation)
                               (not (sos$validalloc (cadr m))))
                              (error (get-default-message
                                       'err.defineclass.allocation.unknown
                                       (cadr m))))
                            ((keyword? (car m)) (loop (cdr l)))
                            (else (loop2 (cddr m)))))))))))

  (define (sos$contain-keyword1 a key1)
    (let loop2 ((m (cdr a)))
      (cond ((null? m) #f)
            ((eq? key1 (car m))
              (if (symbol? (cadr m)) (cadr m)
                      (error (get-default-message
                               'err.require.symbol (cadr m)))))
            (else (loop2 (cddr m))))))

  (define (sos$extract-by-keyword1 a key1)
    (if (null? a) (if #f #f))
        (let loop2 ((m (cdr a)))
          (cond ((null? m) (if #f #f))
                ((eq? key1 (car m)) (cadr m))
                (else (loop2 (cddr m))))))

  (define (sos$extract-by-value a key1)
    (if (null? a) (if #f #f))
        (let loop2 ((m (cdr a)))
          (cond ((null? m) (if #f #f))
                ((eq? key1 (car m)) (cadr m))
                (else (loop2 (cddr m))))))

  (define (sos$extract-by-keyword2 lst key1 key2 isdef)
    (let loop ((l lst))
      (if (null? l) '()
          (let ((a (car l)))
            (let loop2 ((m (cdr a)))
              (cond ((null? m)
                      (if isdef
                        (cons (car a) (loop (cdr l)))
                        (loop (cdr l))))
                    ((and (eq? key1 (car m)) (eq? key2 (cadr m)))
                      (cons (car a) (loop (cdr l))))
                    (else (loop2 (cddr m)))))))))

  (define (sos$crassoc lst key1)
    (let loop ((l lst))
      (if (null? l) '()
          (let ((a (car l)))
            (let loop2 ((m (cdr a)))
              (cond ((null? m) (loop (cdr l)))
                    ((eq? key1 (car m))
                      (cons (cons (car a) (cadr m)) (loop (cdr l))))
                    (else (loop2 (cddr m)))))))))

  (define (sos$search-keyword cls k1)
    (let loop0 ((l0 (class-precedence-list cls)))
      (cond ((null? l0)
              (error (get-default-message 'err.initkeyword k1)))
            ((not (identity-hash-table-exists? sos$*slot-args*
                    (class-name (car l0))))
              (loop0 (cdr l0)))
            (else
              (let loop
                  ((l (identity-hash-table-get sos$*slot-args*
                        (class-name (car l0)))))
                (if (null? l) (loop0 (cdr l0))
                    (let ((a (car l)))
                    (let loop2 ((m (cdr a)))
                      (cond ((null? m) (loop (cdr l)))
                            ((and
                                (eq? (car m) :init-keyword)
                                (eq? (cadr m) k1))
                              (car a))
                            (else (loop2 (cddr m))))))))))))

  (define-syntax define-generic
    (syntax-rules ()
      ((_ name :class cls)
       (define name (instantiate-generic cls)))))

  (define-syntax remove-method
    (syntax-rules ()
      ((_ name (cls ...))
       (sos$remove-method-proc name (list cls ...)))))

  (define-method make ((cls <class>) . initargs)
    (let ((res (instantiate cls)))
      (letrec ((init
                 (lambda (l)
                   (cond ((null? l) #t)
                         ((or (not (pair? l)) (not (pair? (cdr l))))
                           (error (get-default-message
                             'err.make.malform initargs)))
                         (else
                           (slot-set! res
                             (sos$search-keyword cls (car l)) (cadr l))
                           (init (cddr l)))))))
        (sos$init res)
        (init initargs)
        (initialize res initargs)
        res)))

  (define-method initialize ((obj <object>) initargs)
    (if #f #f))

  (define-syntax sos$definit
    (syntax-rules ()
      ((_ obj k1 ()) #f)
      ((_ obj k1 ((slnm1 keys1 ...) (slnm2 keys2 ...) ...))
       (begin (display "aaaa ")
         (if (eq? (slot-allocation obj 'slnm1) k1)
             (slot-set!
               obj 'slnm1
               (sos$extract-by-value '(slnm1 keys1 ...) :init-value)))
         (sos$definit obj k1 ((slnm2 keys2 ...) ...))))))

  (define (sos$removesp x)
    (cond ((null? x) '())
           ((symbol? (car x))
             (cons (syntax->datum (car x)) (sos$removesp (cdr x))))
           (else (cons (car x) (sos$removesp (cdr x))))))

  (define-macro (sos$defmth lst k1 args body)
    (let ((sym (sos$contain-keyword1 lst k1)))
      (if sym
          `(define-method
            ,sym ,args ,(sos$removesp body))
          #f)))

  (define-macro (sos$defmth2 lst k1 args body)
    (let ((sym0 (sos$contain-keyword1 lst k1)))
      (if sym0
        (let ((sym (sos$concat-symbol2 'setter$ sym0)))
          `(define-method ,sym ,args ,(sos$removesp body)))
        #f)))

  (define-macro (sos$defset lst k1)
    (let ((sym0 (sos$contain-keyword1 lst k1)))
      (if sym0
        (let ((sym (sos$concat-symbol2 'setter$ sym0)))
          `(set! (setter ,sym0) ,sym))
        #f)))

  (define-macro (sos$undef name k1 typ)
    (if (identity-hash-table-exists? sos$*slot-args* name)
      (let loop ((l0 (identity-hash-table-get sos$*slot-args* name)))
        (if (null? l0) #f
            (let ((a (sos$contain-keyword1 (car l0) k1)))
              (if a `(remove-method ,a ,typ) (loop (cdr l0))))))))

  (define-macro (sos$undef2 name k1 typ)
    (if (identity-hash-table-exists? sos$*slot-args* name)
      (let loop ((l0 (identity-hash-table-get sos$*slot-args* name)))
        (if (null? l0) #f
            (let ((a (sos$contain-keyword1 (car l0) k1)))
              (if a
                  `(remove-method ,(sos$concat-symbol2 'setter$ a) ,typ)
                  (loop (cdr l0))))))))

  (define-syntax sos$defgetter
    (syntax-rules ()
      ((_ name ()) #f)
      ((_ name ((slnm1 keys1 ...) (slnm2 keys2 ...) ...))
       (begin
         (sos$defmth
           (slnm1 keys1 ...) :getter
           ((x name))
           (slot-ref x 'slnm1))
         (sos$defgetter name ((slnm2 keys2 ...) ...))))))

  (define-syntax sos$defsetter
    (syntax-rules ()
      ((_ name ()) #f)
      ((_ name ((slnm1 keys1 ...) (slnm2 keys2 ...) ...))
       (begin
         (sos$defmth
           (slnm1 keys1 ...) :setter
           ((x name) s)
           (slot-set! x 'slnm1 s))
         (sos$defsetter name ((slnm2 keys2 ...) ...))))))

  (define-syntax sos$defaccessor
    (syntax-rules ()
      ((_ name ()) #f)
      ((_ name ((slnm1 keys1 ...) (slnm2 keys2 ...) ...))
       (begin
         (sos$defmth
           (slnm1 keys1 ...) :accessor
           ((x name))
           (slot-ref x 'slnm1))
         (sos$defmth2
           (slnm1 keys1 ...) :accessor
           ((x name) s)
           (slot-set! x 'slnm1 s))
         (sos$defset (slnm1 keys1 ...) :accessor)
         (sos$defaccessor name ((slnm2 keys2 ...) ...))))))

  (define-method sos$init (a) #f)

  (define-syntax define-class
    (syntax-rules ()
      ((_ name (super ...) ((slotnm keys ...) ...))
       (cond ((and #f (defined? name) (class? name)) #f)  ; redefine is not supported
;             (sos$validate-lst '((slotnm keys ...) ...))
;             (sos$undef name :getter (name))
;             (sos$undef name :setter (name <top>))
;             (sos$undef name :accessor (name))
;             (sos$undef2 name :accessor (name <top>))
;             (define name
;               (redefine-class name (list super ...)
;                 (sos$extract-by-keyword2
;                   '((slotnm keys ...) ...) :allocation :instance #t)
;                 (sos$extract-by-keyword2
;                   '((slotnm keys ...) ...) :allocation :class #f)
;                 (sos$extract-by-keyword2
;                   '((slotnm keys ...) ...)
;                   :allocation :each-subclass #f)
;                 (sos$crassoc '((slotnm keys ...) ...) :init-form)))
;             (sos$defgetter name ((slotnm keys ...) ...))
;             (sos$defsetter name ((slotnm keys ...) ...))
;             (sos$defaccessor name ((slotnm keys ...) ...))
;             (define-method sos$init ((obj name))
;               (next-method)
;               (sos$definit obj :instance ((slotnm keys ...) ...)))
;             (identity-hash-table-put!
;               sos$*slot-args* 'name '((slotnm keys ...) ...)))
             (else
               (sos$validate-lst '((slotnm keys ...) ...))
               (define name
                 (instantiate-class (list super ...)
                   (sos$extract-by-keyword2
                     '((slotnm keys ...) ...) :allocation :instance #t)
                   (sos$extract-by-keyword2
                     '((slotnm keys ...) ...) :allocation :class #f)
                   (sos$extract-by-keyword2
                     '((slotnm keys ...) ...)
                     :allocation :each-subclass #f)))
               (sos$definit name :class ((slotnm keys ...) ...))
               (sos$definit name :each-subclass ((slotnm keys ...) ...))
               (define-method sos$init ((obj name))
                 (next-method)
                 (sos$definit obj :instance ((slotnm keys ...) ...)))
               (sos$defgetter name ((slotnm keys ...) ...))
               (sos$defsetter name ((slotnm keys ...) ...))
               (sos$defaccessor name ((slotnm keys ...) ...))
               (identity-hash-table-put!
                 sos$*slot-args* 'name '((slotnm keys ...) ...)))))
      ((_ name (super ...) ((slotnm keys ...) ...) :metaclass meta)
       (cond ((and #f (defined? name) (class? name)) #f)  ; redefine is not supported
;             (sos$validate-lst '((slotnm keys ...) ...))
;             (sos$undef name :getter (name))
;             (sos$undef name :setter (name <top>))
;             (sos$undef name :accessor (name))
;             (sos$undef2 name :accessor (name <top>))
;             (define name
;               (redefine-class name (list super ...)
;                 (sos$extract-by-keyword2
;                   '((slotnm keys ...) ...) :allocation :instance #t)
;                 (sos$extract-by-keyword2
;                   '((slotnm keys ...) ...) :allocation :class #f)
;                 (sos$extract-by-keyword2
;                   '((slotnm keys ...) ...)
;                   :allocation :each-subclass #f)
;                 (sos$crassoc '((slotnm keys ...) ...) :init-form)
;                 (make meta)))
;             (sos$defgetter name ((slotnm keys ...) ...))
;             (sos$defsetter name ((slotnm keys ...) ...))
;             (sos$defaccessor name ((slotnm keys ...) ...))
;             (define-method sos$init ((obj name))
;               (next-method)
;               (sos$definit obj :instance ((slotnm keys ...) ...)))
;             (identity-hash-table-put!
;               sos$*slot-args* 'name '((slotnm keys ...) ...)))
             (else
               (sos$validate-lst '((slotnm keys ...) ...))
               (define name
                 (instantiate-class (list super ...)
                   (sos$extract-by-keyword2
                     '((slotnm keys ...) ...) :allocation :instance #t)
                   (sos$extract-by-keyword2
                     '((slotnm keys ...) ...) :allocation :class #f)
                   (sos$extract-by-keyword2
                     '((slotnm keys ...) ...)
                     :allocation :each-subclass #f)
                   (make meta)))
               (sos$definit name :class ((slotnm keys ...) ...))
               (sos$definit name :each-subclass ((slotnm keys ...) ...))
               (define-method sos$init ((obj name))
                 (next-method)
                 (sos$definit obj :instance ((slotnm keys ...) ...)))
               (sos$defgetter name ((slotnm keys ...) ...))
               (sos$defsetter name ((slotnm keys ...) ...))
               (sos$defaccessor name ((slotnm keys ...) ...))
               (identity-hash-table-put!
                 sos$*slot-args* 'name '((slotnm keys ...) ...)))))))
;  )

;(import (init$sos))

