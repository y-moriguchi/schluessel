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

;;
(library (srfi-1)
  (export
    srfi-1
    xcons cons* make-list list-tabulate list-copy circular-list
    iota proper-list? circular-list? dotted-list? null-list?
    not-pair? list= first second third fourth fifth sixth seventh
    eighth ninth tenth car+cdr take drop take-right drop-right
    take! drop-right! split-at split-at! last last-pair length+
    append! concatenate concatenate! reverse! append-reverse
    append-reverse! zip unzip1 unzip2 unzip3 unzip4 unzip5
    count fold fold-right pair-fold pair-fold-right reduce
    reduce-right unfold unfold-right append-map append-map!
    map! map-in-order pair-for-each filter-map filter
    partition remove filter! partition! remove! find find-tail
    take-while take-while! drop-while span span! break break!
    any every list-index member delete delete! delete-duplicates
    delete-duplicates! assoc alist-cons alist-copy alist-delete
    alist-delete! lset<= lset= lset-adjoin lset-union
    lset-intersection lset-difference lset-xor
    lset-diff+intersection lset-union! lset-intersection!
    lset-difference! lset-xor! lset-diff+intersection!)
  (define srfi-1 "SRFI-1 Library")

  (define (srfi-1$chk-naturalnum o)
    (if (and (integer? o) (exact? o) (not (negative? o)))
        #t
        (error (get-default-message
                 'err.require.int.nonnegative o))))

  (define (srfi-1$chk-num o)
    (if (number? o)
        #t
        (error (get-default-message
                 'err.require.number o))))

  (define (srfi-1$chk-procedure o)
    (if (procedure? o)
        #t
        (error (get-default-message
                 'err.require.procedure o))))

  (define (srfi-1$cas ll p r lsls)
    (call/cc
      (lambda (k)
        (let loop ((ll ll))
          (cond ((null? ll) r)
                ((pair? ll)
                   (cond ((null? (car ll)) (k '()))
                         ((pair? (car ll))
                            (cons (p ll) (loop (cdr ll))))
                         (else (error (get-default-message
                                        'err.list lsls)))))
                (else (error (get-default-message
                               'err.list lsls))))))))

  (define-syntax srfi-1$opt1
    (syntax-rules ()
      ((_ (nm opt (opt1 def1)) body ...)
        (if (proper-list? opt)
            (let ((opt1 (if (null? opt) def1 (car opt)))
                  (optd (if (null? opt) '()  (cdr opt))))
              (if (null? optd) #t
                  (error (get-default-message 'err.argument nm)))
              body ...)
            (error (get-default-message 'err.argument nm))))))

  (define (srfi-1$filter! pred lst sl)
    (letrec ((srch (lambda (m)
                     (cond ((null? m) '())
                           ((pair? m)
                              (if (pred (sl m))
                                  m
                                  (srch (cdr m))))
                           (else (error (get-default-message
                                          'err.list lst)))))))
      (let loop ((l lst) (res '()))
        (cond ((null? l) res)
              ((pair? l)
                 (let ((a (srch (cdr l))))
                   (if (pred (sl l))
                       (begin
                         (set-cdr! l a)
                         (loop a lst))
                       (if (null? a) res
                           (begin
                             (set-car! l (car a))
                             (set-cdr! l (cdr a))
                             (loop l lst))))))
              (else (error (get-default-message 'err.list lst)))))))


;; Constractors
  (define (xcons cd ca) (cons ca cd))

  (define (cons* . lis)
    (cond ((null? lis) '())
          ((pair? lis)
            (cond ((null? (cdr lis)) (car lis))
                  ((pair? (cdr lis))
                     (cons (car lis) (apply cons* (cdr lis))))
                  (else (error (get-default-message
                                 'err.argument 'cons*)))))
          (else (error (get-default-message
                         'err.argument 'cons*)))))

  (define (make-list n . obj)
    (srfi-1$opt1 ('make-list obj (o #f))
      (list-tabulate n (lambda (n) o))))

  (define (list-tabulate n proc)
    (letrec ((Ml (lambda (n res)
                    (cond ((zero? n) res)
                          (else (Ml
                                  (- n 1)
                                  (cons (proc n) res)))))))
      (srfi-1$chk-naturalnum n)
      (srfi-1$chk-procedure proc)
      (Ml n '())))

  (define (list-copy flist)
    (cond ((null? flist) '())
          ((pair? flist)
             (cons (car flist) (list-copy (cdr flist))))
          (else flist)))

  (define (circular-list . lis)
    (let loop ((l lis))
      (cond ((null? l) '())
            ((not (pair? l))
               (error (get-default-message
                        'err.argument
                        'circular-list)))
            ((null? (cdr l))
               (set-cdr! l lis)
               lis)
            (else (loop (cdr l))))))

  (define (iota count . opts)
    (let ((start (if (null? opts) 0 (car opts)))
          (opts2 (if (null? opts) '() (cdr opts))))
      (let ((step (if (null? opts2) 1 (car opts2)))
            (opte (if (null? opts2) '() (cdr opts2))))
        (letrec ((ia (lambda (n v)
                       (cond ((zero? n) '())
                             (else (cons v (ia (- n 1) (+ v step))))))))
          (if (null? opte)
              #t
              (error (get-default-message 'err.argument 'iota)))
          (srfi-1$chk-naturalnum count)
          (srfi-1$chk-num start)
          (srfi-1$chk-num step)
          (ia count start)))))


;; Predicates
  (define-java-subr
    proper-list?   net.morilib.lisp.lib.srfi001.IsProperList)
  (define-java-subr
    circular-list? net.morilib.lisp.lib.srfi001.IsCircularList)
  (define-java-subr
    dotted-list?   net.morilib.lisp.lib.srfi001.IsDottedList)

  (define (null-list? lis)
    (cond ((pair? lis) #f)
          ((null? lis) #t)
          (else (error (get-default-message 'err.list lis)))))

  (define (not-pair? x) (not (pair? x)))

  (define (list= elt= . lsls)
    (srfi-1$chk-procedure elt=)
    (letrec ((l= (lambda (l0 ll)
                   (cond ((null? ll) #t)
                         ((pair? ll)
                            (and (eql? l0 (car ll))
                                 (l= l0 (cdr ll))))
                         (else (error (get-default-message
                                        'err.list lsls))))))
             (eql? (lambda (l0 l1)
                     (cond ((and (null? l0) (null? l1)) #t)
                           ((null? l0) #f)
                           ((null? l1) #f)
                           ((and (pair? l0) (pair? l1))
                              (and (elt= (car l0) (car l1))
                                   (eql? (cdr l0) (cdr l1))))
                           (else (error (get-default-message
                                          'err.list lsls)))))))
      (cond ((null? lsls) #t)
            ((pair? lsls) (l= (car lsls) (cdr lsls)))
            (else (error (get-default-message
                           'err.list lsls))))))
            
  (define (first   l) (car l))
  (define (second  l) (cadr l))
  (define (third   l) (caddr l))
  (define (fourth  l) (cadddr l))
  (define (fifth   l) (car (cddddr l)))
  (define (sixth   l) (cadr (cddddr l)))
  (define (seventh l) (caddr (cddddr l)))
  (define (eighth  l) (cadddr (cddddr l)))
  (define (ninth   l) (car (cddddr (cddddr l))))
  (define (tenth   l) (cadr (cddddr (cddddr l))))
  (define (car+cdr p) (values (car p) (cdr p)))

  (define (take l n)
    (srfi-1$chk-naturalnum n)
    (let loop ((l l) (n n))
      (cond ((zero? n) '())
            (else (cons (car l) (loop (cdr l) (- n 1)))))))

  (define (drop l n)
    (srfi-1$chk-naturalnum n)
    (let loop ((l l) (n n))
      (cond ((zero? n) l)
            (else (loop (cdr l) (- n 1))))))

  (define (take-right fl n)
    (srfi-1$chk-naturalnum n)
    (let loop ((l fl) (drl (drop fl n)))
      (if (pair? drl) (loop (cdr l) (cdr drl)) l)))

  (define (drop-right fl n)
    (srfi-1$chk-naturalnum n)
    (let loop ((l fl) (drl (drop fl n)))
      (if (pair? drl) 
          (cons (car l) (loop (cdr l) (cdr drl)))
          '())))

  (define (take! lst n)
    (srfi-1$chk-naturalnum n)
    (if (zero? n)
        '()
        (let loop ((l lst) (n n))
          (cond ((= n 1)
                   (set-cdr! l '())
                   lst)
                (else (loop (cdr l) (- n 1)))))))

  (define (drop-right! fl n)
    (srfi-1$chk-naturalnum n)
      (let loop ((l fl) (m 0))
        (cond ((pair? l) (loop (cdr l) (+ m 1)))
              (else (take! fl (- m n))))))

  (define (split-at x n)
    (values (take x n) (drop x n)))

  (define (split-at! x n)
    (srfi-1$chk-naturalnum n)
    (if (zero? n)
        (values '() x)
        (let loop ((l x) (n n))
          (cond ((= n 1)
                   (let ((ld (cdr l)))
                     (set-cdr! l '())
                     (values x ld)))
                (else (loop (cdr l) (- n 1)))))))

  (define (last l)
    (cond ((null? (cdr l)) (car l))
          ((pair? (cdr l)) (last (cdr l)))
          (else (car l))))

  (define (last-pair l)
    (cons (last l) '()))


;; Miscellaneous: length, append, concatenate, reverse, zip & count
  (define (length+ l)
    (if (circular-list? l) #f (length l)))

  (define (append! . lsls)
    (letrec ((ap! (lambda (endp ll)
                    (if (pair? ll)
                        (cond ((null? (cdr ll))
                                 (set-cdr! endp (car ll)))
                              ((null? endp)
                                 (error (get-default-message
                                          'err.list lsls)))
                              (else
                                 (set-cdr! endp (car ll))
                                 (ap! (ep endp (car ll)) (cdr ll))))
                        (error (get-default-message 'err.list lsls)))))
             (ep  (lambda (endp ll)
                     (cond ((null? ll) endp)
                           ((pair? ll)
                             (let loop ((l ll))
                               (cond ((null? (cdr l)) l)
                                     ((pair? (cdr l)) (loop (cdr l)))
                                     (else '()))))
                           (else '())))))
      (let loop ((ll lsls))
        (cond ((null? ll) '())
              ((not (pair? ll))
                 (error (get-default-message 'err.list lsls)))
              ((null? (car ll)) (loop (cdr ll)))
              ((null? (cdr ll)) (car ll))
              (else
                (ap! (ep '() (car ll)) (cdr ll))
                (car ll))))))

  (define (concatenate  lsls) (apply append  lsls))
  (define (concatenate! lsls) (apply append! lsls))

;(define (reverse! l)
;  (letrec ((loop2 (lambda (l)
;                    (cond ((pair? (cdr l))
;                             (let* ((l2  (loop2 (cdr l)))
;                                    (l2a (car l2))
;                                    (l1a (car l)))
;                      (set-car! l2 l1a)
;                      (set-car! l  l2a)
;                      l))
;                    (else l))))
;           (loop (lambda (l)
;                   (cond ((pair? (cdr l))
;                            (loop2 l)
;                            (loop  (cdr l)))
;                   (else l)))))
;    (cond ((pair? l)
;             (loop l)
;             l)
;          (else l)))) 
  (define (reverse! lst)
    (let loop ((l lst) (res '()))
      (cond ((null? l) '())
            ((pair? l)
               (let ((cd (cdr l)))
                 (set-cdr! l res)
                 (cond ((null? cd) l)
                       ((pair? cd) (loop cd l))
                       (else
                         (error (get-default-message 'err.list cd))))))
            (else (error (get-default-message 'err.list lst))))))

  (define (append-reverse rev tail)
    (let loop ((l rev) (r tail))
      (cond ((null? l) r)
            ((pair? l)
               (loop (cdr l) (cons (car l) r)))
            (else (error (get-default-message 'err.list rev))))))

  (define (append-reverse! rev tail)
    (append! (reverse! rev) tail))

  (define (zip . lss) (apply map list lss))
  (define (srfi-1$unzip l proc)  (map proc l))

  (define (unzip1 l)  (srfi-1$unzip l car))

  (define (unzip2 l)
    (values (srfi-1$unzip l car)
            (srfi-1$unzip l cadr)))

  (define (unzip3 l)
    (values (srfi-1$unzip l car)
            (srfi-1$unzip l cadr)
            (srfi-1$unzip l caddr)))

  (define (unzip4 l)
    (values (srfi-1$unzip l car)
            (srfi-1$unzip l cadr)
            (srfi-1$unzip l caddr)
            (srfi-1$unzip l cadddr)))

  (define (unzip5 l)
    (values (srfi-1$unzip l car)
            (srfi-1$unzip l cadr)
            (srfi-1$unzip l caddr)
            (srfi-1$unzip l cadddr)
            (srfi-1$unzip l fifth)))

  (define (count pred . lsls)
    (let loop ((ll lsls) (res 0))
      (let ((a (srfi-1$cas ll caar '() lsls)))
        (if (null? a)
            res
            (loop (srfi-1$cas ll cdar '() lsls)
                  (if (apply pred a) (+ res 1) res))))))


;; Fold, unfold & map
  (define fold #f)
  (define fold-right #f)
  (define pair-fold #f)
  (define pair-fold-right #f)

  (letrec ((f (lambda (p proc init lsls)
                (let loop ((ll lsls) (res init))
                  (let ((a (srfi-1$cas ll p (cons res '()) lsls)))
                    (if (null? a)
                        res
                        (loop (srfi-1$cas ll cdar '() lsls)
                              (apply proc a)))))))
           (g (lambda (p proc init lsls)
                (let loop ((ll lsls))
                  (let ((a (srfi-1$cas ll p '() lsls)))
                    (if (null? a)
                        init
                        (let ((b (loop (srfi-1$cas ll cdar '() lsls))))
                          (apply proc (append a (cons b '()))))))))))
    (set! fold
      (lambda (proc init . lsls) (f caar proc init lsls)))
    (set! fold-right
      (lambda (proc init . lsls) (g caar proc init lsls)))
    (set! pair-fold
      (lambda (proc init . lsls) (f car proc init lsls)))
    (set! pair-fold-right
      (lambda (proc init . lsls) (g car proc init lsls))))

  (define (reduce f ridentity lst)
    (cond ((null? lst) ridentity)
          ((pair? lst) (fold f (car lst) (cdr lst)))
          (else (error (get-default-message 'err.list lst)))))

  (define (reduce-right f ridentity lst)
    (cond ((null? lst) ridentity)
          ((not (pair? lst)) (error (get-default-message 'err.list lst)))
          ((null? (cdr lst)) (car lst))
          (else (f (car lst) (reduce-right f ridentity (cdr lst))))))

  (define (unfold p f g seed . opt)
    (srfi-1$opt1 ('unfold opt (tail-gen (lambda (x) '())))
      (let loop ((seed seed))
        (if (p seed)
            (tail-gen seed)
            (cons (f seed) (loop (g seed)))))))

  (define (unfold-right p f g seed . opt)
    (srfi-1$opt1 ('unfold-right opt (tail '()))
      (let lp ((seed seed) (lis tail))
        (if (p seed) lis
            (lp (g seed) (cons (f seed) lis))))))

  (define (append-map . args)
    (apply append (apply map args)))

  (define (append-map! . args)
    (apply append! (apply map args)))

  (define (map! f . lsls)
    (if (pair? lsls) #t
        (get-default-message 'err.argument 'map!))
    (let ((lst1 (car lsls)))
      (if (proper-list? lst1) #t
          (error (get-default-message 'err.list lst1)))
      (if (null? lst1)
          '()
          (let loop ((ll lsls) (le lst1))
            (let ((a (srfi-1$cas ll caar '() lsls)))
              (if (null? a)
                  (if (eq? le lst1)
                      '()
                      (begin
                        (if (null? le) #t (set-cdr! le '()))
                        lst1))
                  (begin
                    (set-car! le (apply f a))
                    (loop (srfi-1$cas ll cdar '() lsls) (cdr le)))))))))

  (define map-in-order map)

  (define (pair-for-each f . lsls)
    (let loop ((ll lsls))
      (let ((a (srfi-1$cas ll car '() lsls)))
        (if (null? a)
            (if #f #f)
            (begin
              (apply f a)
              (loop (srfi-1$cas ll cdar '() lsls)))))))

  (define (filter-map f . lsls)
    (let loop ((ll lsls))
      (let ((a (srfi-1$cas ll caar '() lsls)))
        (if (null? a)
            '()
            (let ((r (apply f a))
                  (b (srfi-1$cas ll cdar '() lsls)))
              (if r (cons r (loop b)) (loop b)))))))


;; Filtering & partitioning
  (define (filter pred lst)
    (let loop ((l lst))
      (cond ((null? l) '())
            ((pair? l)
               (if (pred (car l))
                   (cons (car l) (loop (cdr l)))
                   (loop (cdr l))))
            (else (error (get-default-message 'err.list lst))))))

  (define (partition pred lst)
    (let loop ((l lst))
      (cond ((null? l) (values '() '()))
            ((pair? l)
               (if (pred (car l))
                   (call-with-values (lambda () (loop (cdr l)))
                                     (lambda (a b)
                                       (values (cons (car l) a) b)))
                   (call-with-values (lambda () (loop (cdr l)))
                                     (lambda (a b)
                                       (values a (cons (car l) b))))))
            (else (error (get-default-message 'err.list lst))))))

  (define (remove pred lst)
    (filter (lambda (x) (not (pred x))) lst))

  (define (filter! pred lst)
    (srfi-1$filter! pred lst car))

  (define (partition! pred lst)
    (letrec ((srch (lambda (m)
                     (cond ((null? m) '())
                           ((pair? m)
                              (if (pred (car m))
                                  m
                                  (srch (cdr m))))
                           (else (error (get-default-message
                                          'err.list lst))))))
             (srcn (lambda (m)
                     (cond ((null? m) '())
                           ((pair? m)
                              (if (pred (car m))
                                  (srcn (cdr m))
                                  m))
                           (else (error (get-default-message
                                          'err.list lst)))))))
      (let loop ((l lst) (res '()) (reb '()))
        (cond ((null? l) (values res reb))
              ((pair? l)
                 (let ((d (cdr l)) (a (srch (cdr l))) (b (srcn (cdr l))))
                   (if (pred (car l))
                       (begin
                         (set-cdr! l a)
                         (loop d
                               (if (null? res) l res)
                               (if (null? reb) b reb)))  
                       (begin
                         (set-cdr! l b)
                         (loop d
                               (if (null? res) a res)
                               (if (null? reb) l reb))))))
              (else (error (get-default-message 'err.list lst)))))))

  (define (remove! pred lst)
    (filter! (lambda (x) (not (pred x))) lst))


;; Searching
  (define find #f)
  (define find-tail #f)

  (let ()
    (define (find$ pred lst gt)
      (let ((ht (make-identity-hash-table)))
        (let loop ((l lst))
          (cond ((null? l) #f)
                ((identity-hash-table-exists? ht l)
                   (error (get-default-message
                            'err.list.circulated lst)))
                ((pair? l)
                   (identity-hash-table-put! ht l #t)
                   (if (pred (car l)) (gt l) (loop (cdr l))))
                (else (error (get-default-message 'err.list lst)))))))
    (set! find
      (lambda (pred lst) (find$ pred lst car)))
    (set! find-tail
      (lambda (pred lst) (find$ pred lst (lambda (x) x)))))

  (define (take-while pred lst)
    (let ((ht (make-identity-hash-table)))
      (let loop ((l lst))
        (cond ((null? l) '())
              ((identity-hash-table-exists? ht l) l)
              ((pair? l)
                 (identity-hash-table-put! ht l #t)
                 (if (pred (car l))
                     (cons (car l) (loop (cdr l)))
                     '()))
              (else (error (get-default-message 'err.list lst)))))))

  (define (take-while! pred lst)
    (let ((ht (make-identity-hash-table)))
      (let loop ((l lst))
        (cond ((null? l) '())
              ((identity-hash-table-exists? ht l) l)
              ((pair? l)
                 (identity-hash-table-put! ht l #t)
                 (cond ((not (pred (car l))) '())
                       ((null? (cdr l)) lst)
                       ((identity-hash-table-exists? ht (cdr l)) lst)
                       ((not (pair? (cdr l)))
                          (error (get-default-message 'err.list lst)))
                       ((pred (cadr l)) (loop (cdr l)))
                       (else
                         (set-cdr! l '())
                         lst)))
              (else (error (get-default-message 'err.list lst)))))))

  (define (drop-while pred lst)
    (let ((ht (make-identity-hash-table)))
      (let loop ((l lst))
        (cond ((null? l) '())
              ((identity-hash-table-exists? ht l) '())
              ((pair? l)
                 (identity-hash-table-put! ht l #t)
                 (if (pred (car l))
                     (loop (cdr l))
                     l))
              (else (error (get-default-message 'err.list lst)))))))

  (define (span pred lst)
    (let ((ht (make-identity-hash-table)))
      (let loop ((l lst))
        (cond ((null? l) (values '() '()))
              ((identity-hash-table-exists? ht l) (values l '()))
              ((pair? l)
                 (identity-hash-table-put! ht l #t)
                 (if (pred (car l))
                     (call-with-values
                       (lambda () (loop (cdr l)))
                       (lambda (x y) (values (cons (car l) x) y)))
                     (values '() l)))
              (else (error (get-default-message 'err.list lst)))))))

  (define (span! pred lst)
    (let ((ht (make-identity-hash-table)))
      (let loop ((l lst))
        (cond ((null? l) (values '() '()))
              ((identity-hash-table-exists? ht l) (values l '()))
              ((pair? l)
                 (identity-hash-table-put! ht l #t)
                 (cond ((not (pred (car l))) (values '() l))
                       ((null? (cdr l)) (values lst '()))
                       ((identity-hash-table-exists? ht (cdr l))
                          (values lst '()))
                       ((not (pair? (cdr l)))
                          (error (get-default-message 'err.list lst)))
                       ((pred (cadr l)) (loop (cdr l)))
                       (else
                         (let ((d (cdr l)))
                           (set-cdr! l '())
                           (values lst d)))))
              (else (error (get-default-message 'err.list lst)))))))

  (define (break pred lst)
    (span (lambda (x) (not (pred x))) lst))

  (define (break! pred lst)
    (span! (lambda (x) (not (pred x))) lst))

  (define any #f)
  (define every #f)
  (define list-index #f)
  (let ()
    (define (make-ht-lst lsls)
      (let loop ((l lsls) (res '()))
        (if (null? l)
            res
            (loop (cdr l) (cons (make-identity-hash-table) res)))))
    (define (ht-exists-all? ht a)
      (let loop ((h ht) (l a))
        (if (null? h)
            #t
            (and (identity-hash-table-exists? (car h) (car l))
                 (loop (cdr h) (cdr l))))))
    (define (ht-put-all! ht a)
      (let loop ((h ht) (l a))
        (cond ((null? h) (if #t #t))
              (else
                (identity-hash-table-put! (car h) (car l) #t)
                (loop (cdr h) (cdr l))))))
    (define ($any pred . lsls)
      (if (null? lsls)
          (error (get-default-message 'err.argument 'any)))
      (let ((ht (make-ht-lst lsls)))
        (let loop ((ll lsls))
          (let ((a (srfi-1$cas ll caar '() lsls)))
            (cond ((null? a) #f)
                  ((ht-exists-all? ht ll)
                     (error (get-default-message
                              'err.list.circulated lsls)))
                  (else
                    (ht-put-all! ht ll)
                    (or (apply pred a)
                        (loop (srfi-1$cas ll cdar '() lsls)))))))))
    (define ($every pred . lsls)
      (if (null? lsls)
          (error (get-default-message 'err.argument 'every)))
      (let ((ht (make-ht-lst lsls)))
        (let loop ((ll lsls))
          (let ((a (srfi-1$cas ll caar '() lsls)))
            (cond ((null? a) #t)
                  ((ht-exists-all? ht ll)
                     (error (get-default-message
                              'err.list.circulated lsls)))
                  (else
                    (ht-put-all! ht ll)
                    (and (apply pred a)
                         (loop (srfi-1$cas ll cdar '() lsls)))))))))
    (define ($list-index pred . lsls)
      (if (null? lsls)
          (error (get-default-message 'err.argument 'every)))
      (let ((ht (make-ht-lst lsls)))
        (let loop ((ll lsls) (idx 0))
          (let ((a (srfi-1$cas ll caar '() lsls)))
            (cond ((null? a) #f)
                  ((ht-exists-all? ht ll)
                     (error (get-default-message
                              'err.list.circulated lsls)))
                  (else
                    (ht-put-all! ht ll)
                    (if (apply pred a)
                        idx
                        (loop (srfi-1$cas ll cdar '() lsls)
                              (+ idx 1)))))))))
    (set! any $any)
    (set! every $every)
    (set! list-index $list-index))

  (define (member x lst . opt)
    (srfi-1$opt1 ('member opt (opt= equal?))
      (let loop ((l lst))
        (cond ((null? l) #f)
              ((not (pair? l))
                 (error (get-default-message 'err.list lst)))
              ((opt= x (car l)) l)
              (else (loop (cdr l)))))))


;; Deletion
  (define (delete x lst . opt)
    (srfi-1$opt1 ('delete opt (opt= equal?))
      (let loop ((l lst))
        (cond ((null? l) '())
              ((not (pair? l))
                 (error (get-default-message 'err.list lst)))
              ((opt= x (car l)) (loop (cdr l)))
              (else (cons (car l) (loop (cdr l))))))))

  (define (delete! x lst . opt)
    (srfi-1$opt1 ('delete! opt (opt= equal?))
      (remove! (lambda (y) (opt= x y)) lst)))

;(define (delete-duplicates lst . opt)
;  (srfi-1$opt1 ('delete-duplicates opt (opt= equal?))
;    (letrec ((fd (lambda (y dr)
;                   (cond ((null? dr) #f)
;                         ((pair? dr)
;                           (or (opt= y (car dr)) (fd y (cdr dr))))
;                         (else
;                           (error (get-default-message
;                                    'err.list lst)))))))
;      (let loop ((l lst))
;        (cond ((null? l) '())
;              ((not (pair? l))
;                 (error (get-default-message 'err.list lst)))
;              ((fd (car l) (cdr l)) (loop (cdr l)))
;              (else (cons (car l) (loop (cdr l)))))))))
  (define (delete-duplicates lst . opt)
    (srfi-1$opt1 ('delete-duplicates opt (opt= equal?))
      (let loop ((l lst))
        (cond ((null? l) '())
              ((pair? l)
                (cons (car l) (loop (delete (car l) (cdr l) opt=))))
              (else (error (get-default-message 'err.list lst)))))))

  (define (delete-duplicates! lst . opt)
    (srfi-1$opt1 ('delete-duplicates! opt (opt= equal?))
      (let loop ((l lst))
        (cond ((null? l) lst)
              ((pair? l)
                (let ((dl (cdr l)))
                  (set! dl (delete! (car l) dl opt=))
                  (set-cdr! l dl)
                  (loop dl)))
              (else (error (get-default-message 'err.list lst)))))))


;; Association lists
  (define (assoc key alst . opt)
    (srfi-1$opt1 ('assoc opt (opt= equal?))
      (let loop ((l alst))
        (cond ((null? l) #f)
              ((not (pair? l))
                 (error (get-default-message 'err.list alst)))
              ((opt= key (caar l)) (car l))
              (else (loop (cdr l)))))))

  (define (alist-cons key datum alist) (cons (cons key datum) alist))
  (define (alist-copy a)
    (map (lambda (elt) (cons (car elt) (cdr elt))) a))

  (define (alist-delete key alst . opt)
    (srfi-1$opt1 ('alist-delete opt (opt= equal?))
      (let loop ((l alst))
        (cond ((null? l) '())
              ((not (pair? l))
                 (error (get-default-message 'err.list alst)))
              ((opt= key (caar l)) (loop (cdr l)))
              (else (cons (car l) (loop (cdr l))))))))

  (define (alist-delete! x lst . opt)
    (srfi-1$opt1 ('alist-delete! opt (opt= equal?))
      (filter! (lambda (y) (not (opt= x (car y)))) lst)))


;; Set operations on lists
  (define lset<= #f)
  (define lset= #f)

  (letrec ((ss? (lambda (a b f=)
                  (let loop ((l a))
                    (cond ((null? l) #t)
                          ((pair? l)
                             (and (member (car l) b f=) (loop (cdr l))))
                          (else (error (get-default-message
                                         'err.list a))))))))
    (define ($lset ss? lsls)
      (letrec ((l<= (lambda (a ll)
                      (cond ((null? ll) #t)
                            ((pair? ll)
                               (and (ss? a (car ll))
                                    (l<= (car ll) (cdr ll))))
                            (else (error (get-default-message
                                           'err.list lsls)))))))
        (cond ((null? lsls) #t)
              ((pair? lsls) (l<= (car lsls) (cdr lsls)))
              (else (error (get-default-message 'err.list lsls))))))
    (set! lset<=
      (lambda (f= . lsls)
        ($lset (lambda (a b) (ss? a b f=)) lsls)))
    (set! lset=
      (lambda (f= . lsls)
        ($lset (lambda (a b) (and (ss? a b f=) (ss? b a f=))) lsls))))

  (define lset-adjoin #f)
  (define lset-union #f)
  (define lset-intersection #f)
  (define lset-difference #f)
  (define lset-xor #f)
  (define lset-diff+intersection #f)
  (define lset-union! #f)
  (define lset-intersection! #f)
  (define lset-difference! #f)
  (define lset-xor! #f)
  (define lset-diff+intersection! #f)
  (let ()
    (define ($lset-adjoin lst elts f=)
      (let loop ((l elts) (res lst))
        (cond ((null? l) res)
              ((not (pair? l))
                 (error (get-default-message 'err.list elts)))
              ((member (car l) res f=) (loop (cdr l) res))
              (else (loop (cdr l) (cons (car l) res))))))
    (define (srfi-1$lset-int2 lst dls f=)
      (let loop ((l lst) (res '()))
        (cond ((null? l) res)
              ((not (pair? l))
                 (error (get-default-message 'err.list lst)))
              ((member (car l) dls f=)
                         (loop (cdr l) (cons (car l) res)))
              (else (loop (cdr l) res)))))
    (define (srfi-1$lset-dif2 lst dls f=)
      (let loop ((l lst) (res '()))
        (cond ((null? l) res)
              ((not (pair? l))
                 (error (get-default-message 'err.list lst)))
              ((member (car l) dls f=) (loop (cdr l) res))
              (else (loop (cdr l) (cons (car l) res))))))
    (define (lset-xor2 lst dls f=)
      (let loop ((l lst) (res (srfi-1$lset-dif2 dls lst f=)))
        (cond ((null? l) res)
              ((not (pair? l))
                 (error (get-default-message 'err.list lst)))
              ((member (car l) dls f=) (loop (cdr l) res))
              (else (loop (cdr l) (cons (car l) res))))))
    (define (lset-loop f= prc lsls)
      (cond ((null? lsls) '())
            ((pair? lsls)
               (let loop ((l (cdr lsls)) (res (car lsls)))
                 (cond ((null? l) res)
                       ((pair? l)
                          (loop (cdr l) (prc res (car l) f=)))
                       (else (error (get-default-message
                                      'err.list lsls))))))
            (else (error (get-default-message 'err.list lsls)))))
    (define (lset-diff+int f= lsls)
      (define (lset-dif+int2 lst dls)
        (call-with-values
          (lambda () lst)
          (lambda (a b)
            (values (srfi-1$lset-dif2 a dls f=)
                    (srfi-1$lset-int2 b dls f=)))))
      (cond ((null? lsls) (values '() '()))
            ((pair? lsls)
               (let loop ((l (cdr lsls))
                          (res (values (car lsls) (car lsls))))
                 (cond ((null? l) res)
                       ((pair? l)
                          (loop (cdr l) (lset-dif+int2 res (car l))))
                       (else (error (get-default-message
                                      'err.list lsls))))))
            (else (error (get-default-message 'err.list lsls)))))
    (set! lset-adjoin
      (lambda (f= lst . elts) ($lset-adjoin lst elts f=)))
    (set! lset-union
      (lambda (f= . lsls) (lset-loop f= $lset-adjoin lsls)))
    (set! lset-intersection
      (lambda (f= . lsls) (lset-loop f= srfi-1$lset-int2 lsls)))
    (set! lset-difference
      (lambda (f= . lsls) (lset-loop f= srfi-1$lset-dif2 lsls)))
    (set! lset-xor
      (lambda (f= . lsls) (lset-loop f= lset-xor2 lsls)))
    (set! lset-diff+intersection
      (lambda (f= . lsls) (lset-diff+int f= lsls)))
    (set! lset-diff+intersection!
      (lambda (f= . lsls) (lset-diff+int f= lsls))))

  (let-syntax ((loop-res!
                 (syntax-rules ()
                   ((_ loop res l)
                    (let ((cd (cdr l)))
                      (set-cdr! l res)
                      (loop cd l)))
                   ((_ loop res l m)
                    (let ((cd (cdr l)) (ce (cdr m)))
                      (set-cdr! m res)
                      (set-cdr! l m)
                      (loop cd ce l))))))
    (define ($lset-adjoin lst elts f=)
      (let loop ((l elts) (res lst))
        (cond ((null? l) res)
              ((not (pair? l))
                 (error (get-default-message 'err.list elts)))
              ((member (car l) res f=) (loop (cdr l) res))
              (else (loop-res! loop res l)))))
    (define (srfi-1$lset-int2 lst dls f=)
      (let loop ((l lst) (res '()))
        (cond ((null? l) res)
              ((not (pair? l))
                 (error (get-default-message 'err.list lst)))
              ((member (car l) dls f=)
                         (loop-res! loop res l))
              (else (loop (cdr l) res)))))
    (define (srfi-1$lset-dif2 lst dls f=)
      (let loop ((l lst) (res '()))
        (cond ((null? l) res)
              ((not (pair? l))
                 (error (get-default-message 'err.list lst)))
              ((member (car l) dls f=) (loop (cdr l) res))
              (else (loop-res! loop res l)))))
    (define (lset-xor2 lst dls f=)
      (let loop ((l lst) (m dls) (res '()))
        (cond ((and (null? l) (null? m)) res)
              ((null? l)
                 (cond ((not (pair? m))
                          (error (get-default-message 'err.list lst)))
                       ((member (car m) lst f=) (loop l (cdr m) res))
                       (else
                          (let ((cd (cdr m)))
                            (set-cdr! m res)
                            (loop l cd m)))))
              ((null? m)
                 (cond ((not (pair? l))
                          (error (get-default-message 'err.list lst)))
                       ((member (car l) dls f=) (loop (cdr l) m res))
                       (else
                          (let ((cd (cdr l)))
                            (set-cdr! l res)
                            (loop cd m l)))))
              ((not (pair? l))
                 (error (get-default-message 'err.list lst)))
              ((not (pair? l))
                 (error (get-default-message 'err.list lst)))
              ((member (car l) dls f=) (loop (cdr l) m res))
              ((member (car m) lst f=) (loop l (cdr m) res))
              (else (loop-res! loop res l m)))))
    (define (lset-loop f= prc lsls)
      (cond ((null? lsls) '())
            ((pair? lsls)
               (let loop ((l (cdr lsls)) (res (car lsls)))
                 (cond ((null? l) res)
                       ((pair? l)
                          (loop (cdr l) (prc res (car l) f=)))
                       (else (error (get-default-message
                                      'err.list lsls))))))
            (else (error (get-default-message 'err.list lsls)))))
;  (define (lset-diff+int f= lsls)
;    (define (lset-dif+int2 lst dls)
;      (call-with-values
;        (lambda () lst)
;        (lambda (a b)
;          (values (srfi-1$lset-dif2 a dls f=)
;                  (srfi-1$lset-int2 b dls f=)))))
;    (cond ((null? lsls) (values '() '()))
;          ((pair? lsls)
;             (let loop ((l (cdr lsls))
;                        (res (values (car lsls) (car lsls))))
;               (cond ((null? l) res)
;                     ((pair? l)
;                        (loop (cdr l) (lset-dif+int2 res (car l))))
;                     (else (error (get-default-message
;                                    'err.list lsls))))))
;          (else (error (get-default-message 'err.list lsls)))))
    (set! lset-union!
      (lambda (f= . lsls) (lset-loop f= $lset-adjoin lsls)))
    (set! lset-intersection!
      (lambda (f= . lsls) (lset-loop f= srfi-1$lset-int2 lsls)))
    (set! lset-difference!
      (lambda (f= . lsls) (lset-loop f= srfi-1$lset-dif2 lsls)))
    (set! lset-xor!
      (lambda (f= . lsls) (lset-loop f= lset-xor2 lsls))))
  )

(cond
  ((not (defined? srfi-1))
    (import (srfi-1))))
