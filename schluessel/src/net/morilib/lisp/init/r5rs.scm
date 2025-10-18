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

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (caar x)))
(define (caadr x) (car (cadr x)))
(define (cadar x) (car (cdar x)))
(define (caddr x) (car (cddr x)))
(define (cdaar x) (cdr (caar x)))
(define (cdadr x) (cdr (cadr x)))
(define (cddar x) (cdr (cdar x)))
(define (cdddr x) (cdr (cddr x)))
(define (caaaar x) (car (caaar x)))
(define (caaadr x) (car (caadr x)))
(define (caadar x) (car (cadar x)))
(define (caaddr x) (car (caddr x)))
(define (cadaar x) (car (cdaar x)))
(define (cadadr x) (car (cdadr x)))
(define (caddar x) (car (cddar x)))
(define (cadddr x) (car (cdddr x)))
(define (cdaaar x) (cdr (caaar x)))
(define (cdaadr x) (cdr (caadr x)))
(define (cdadar x) (cdr (cadar x)))
(define (cdaddr x) (cdr (caddr x)))
(define (cddaar x) (cdr (cdaar x)))
(define (cddadr x) (cdr (cdadr x)))
(define (cdddar x) (cdr (cddar x)))
(define (cddddr x) (cdr (cdddr x)))

(define-syntax let*
  (syntax-rules ()
    ((_ () e1 ...)
     (let () e1 ...))
    ((_ ((x1 v1) (x2 v2) ...) e1 ...)
     (let ((x1 v1))
       (let* ((x2 v2) ...) e1 ...)))))

(define (call-with-input-file fname f)
  (let ((prt (open-input-file fname)))
    (let ((res (f prt)))
      (close-input-port prt)
      res)))

(define (call-with-output-file fname f)
  (let ((prt (open-output-file fname)))
    (let ((res (f prt)))
      (close-output-port prt)
      res)))

(define (port? x)
  (or (input-port? x) (output-port? x)))

;(define (equal? x y)
;  (define (vector-equal? x y i)
;    (cond ((= (vector-length x) i) #t)
;          ((not (equal? (vector-ref x i) (vector-ref y i))) #f)
;          (else (vector-equal? x y (+ i 1)))))
;  (cond ((eqv? x y) #t)
;        ((pair? x)
;          (and (pair? y)
;               (equal? (car x) (car y))
;               (equal? (cdr x) (cdr y))))
;        ((string? x) (and (string? y) (string=? x y)))
;        ((vector? x)
;          (and (vector? y)
;               (= (vector-length x) (vector-length y))
;               (vector-equal? x y 0)))
;        (else #f)))

(define (member x lis)
  (cond ((null? lis) #f)
        ((equal? x (car lis)) lis)
        (else (member x (cdr lis)))))

(define (assq x lis)
  (cond ((null? lis) #f)
        ((not (pair? (car lis)))
          (error (get-default-message 'err.require.pair)))
        ((eq? (caar lis) x) (car lis))
        (else (assq x (cdr lis)))))

(define (assv x lis)
  (cond ((null? lis) #f)
        ((not (pair? (car lis)))
          (error (get-default-message 'err.require.pair)))
        ((eqv? (caar lis) x) (car lis))
        (else (assv x (cdr lis)))))

(define (assoc x lis)
  (cond ((null? lis) #f)
        ((not (pair? (car lis)))
          (error (get-default-message 'err.require.pair)))
        ((equal? (caar lis) x) (car lis))
        (else (assoc x (cdr lis)))))

(define dynamic-wind #f)
(let ((wind-list '()))
  (define (dw bef tnk aft)
    (bef)
    (set! wind-list (cons (list bef tnk aft) wind-list))
    (let ((res (tnk)))
      (set! wind-list (cdr wind-list))
      (aft)
      res))
  (define (ex ext lst)
    (let loop ((l (reverse lst)))
      (cond ((null? l) #t)
            (else
              ((ext (car l)))
              (loop (cdr l))))))
  (define (ez ext lst oldwl)
    (let loop ((l oldwl))
      (cond ((null? l) #t)
            ((eq? l lst) #t)
            (else
              ((ext (car l)))
              (loop (cdr l))))))
  (define (f? wx lst)
    (let loop ((l lst))
      (cond ((null? l) #f)
            ((eq? wx l) #t)
            (else (loop (cdr l))))))
  (set! dynamic-wind dw)
  (set! call/cc
    (let ((ccorg call/cc))
      (lambda (pr)
        (letrec ((l1 (lambda (k)
                       (let ((wl wind-list))
                         (lambda (m)
                           (cond ((and (null? wl) (null? wind-list)) #f)
                                 ((or (null? wl) (f? wl wind-list))
                                   (let ((oldwl wind-list))
                                     (set! wind-list wl)
                                     (ez caddr wl oldwl)))
                                 (else
                                   (ex car wl)
                                   (set! wind-list wl)))
                           (k m))))))
          (ccorg (lambda (k) (pr (l1 k)))))))))

(define *load-path* '())
