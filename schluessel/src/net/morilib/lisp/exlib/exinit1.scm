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

(define *use-path* '(net.morilib.lisp.lib))

(define (add-load-path pth . afterp)
  (if (not (string? pth))
    (error (get-default-message 'err.require.string)))
  (if (equal? afterp '(#t))
      (set! *load-path* (append *load-path* (list pth)))
      (set! *load-path* (cons pth *load-path*))))

(define *plugin-path* '())

(define (add-plugin-path pth . afterp)
  (if (not (string? pth))
    (error (get-default-message 'err.require.string)))
  (if (dynamic-defined? '*plugin-path*)
    (if (equal? afterp '(#t))
        (dynamic-set-variable!!
          '*plugin-path*
          (append (dynamic-ref-variable '*plugin-path*) (list pth)))
        (dynamic-set-variable!!
          '*plugin-path*
          (cons pth (dynamic-ref-variable '*plugin-path*))))))

(define-syntax define-java-subr
  (syntax-rules ()
    ((_ v s) (define v (get-java-subr (quote v) (quote s))))))

;; SRFI-0
(define-syntax parse-feature-exist?
  (syntax-rules (and or not)
    ((_ (and feature ...))
     (and (parse-feature-exist? feature) ...))
    ((_ (or feature ...))
     (or  (parse-feature-exist? feature) ...))
    ((_ (not feature))
     (not (parse-feature-exist? feature)))
    ((_ feature) (feature-exist? 'feature))))

(define-syntax parse-apply-feature
  (syntax-rules (and or not)
    ((_ (and feature ...))
     (begin (parse-apply-feature feature) ...))
    ((_ (or feature ...))
     (begin (parse-apply-feature feature) ...))
    ((_ (not feature))
     (if (parse-feature-exist? feature)
         #f
         (parse-apply-feature feature)))
    ((_ feature) (apply-feature 'feature))))

(define-syntax cond-expand
  (syntax-rules (and or not else)
    ((_) (if #f #f))
    ((_ (else body ...))
     (let () body ...))
    ((_ (feature-expr body ...) clause ...)
     (if (parse-feature-exist? feature-expr)
         (let ()
           (parse-apply-feature feature-expr)
           body ...)
         (cond-expand clause ...)))))

(define-feature srfi-0)
(define-feature srfi-1
  (use srfi-1))
(define-feature srfi-2)
(define-feature srfi-4)
(define-feature srfi-6)
(define-feature srfi-8)
(define-feature srfi-9)
(define-feature srfi-10)
(define-feature srfi-11)
(define-feature srfi-13
  (use srfi-13))
(define-feature srfi-16)
(define-feature srfi-17)
(define-feature srfi-18)
(define-feature srfi-19)
(define-feature srfi-22)
(define-feature srfi-23)
(define-feature srfi-25)
(define-feature srfi-26)
(define-feature srfi-28)
(define-feature srfi-30)
(define-feature srfi-34)
(define-feature srfi-38)
(define-feature srfi-39)
(define-feature srfi-41
  (use srfi-41))
;(define-feature srfi-43
;  (use srfi-43))
(define-feature srfi-44)
(define-feature srfi-46)
(define-feature srfi-47)
(define-feature srfi-58)
(define-feature srfi-62)
(define-feature srfi-63)
(define-feature srfi-70)
(define-feature srfi-88)
(define-feature srfi-94
  (use srfi-94))
(define-feature srfi-98)
(define-feature engineer)
(define-feature math)
(define-feature sound)

;; SRFI-2
(define-syntax and-let*
  (syntax-rules ()
    ((_ () e1 ...)
     (let () e1 ...))
    ((_ ((x1 v1) z ...) e1 ...)
     (let ((x1 v1))
       (and-let* (z ...) e1 ...)))
    ((_ ((cl) z ...) e1 ...)
     (and cl (and-let* (z ...) e1 ...)))
    ((_ (((cl ...)) z ...) e1 ...)
     (and (cl ...) (and-let* (z ...) e1 ...)))))

;; SRFI-9
(define-syntax receive
  (syntax-rules ()
    ((receive formals expression body ...)
     (call-with-values (lambda () expression)
                       (lambda formals body ...)))))

;; SRFI-10
(define-sharp-syntax
  "#,,"
  (lambda (x y) (apply (lookup-reader-ctor (car y)) (cdr y)))
  #t)

;; SRFI-11
(use srfi-11)

;; SRFI-16
(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda)
     (lambda args
       (error "CASE-LAMBDA without any clauses.")))
    ((case-lambda 
      (?a1 ?e1 ...) 
      ?clause1 ...)
     (lambda args
       (let ((l (length args)))
         (case-lambda "CLAUSE" args l 
           (?a1 ?e1 ...)
           ?clause1 ...))))
    ((case-lambda "CLAUSE" ?args ?l 
      ((?a1 ...) ?e1 ...) 
      ?clause1 ...)
     (if (= ?l (length '(?a1 ...)))
         (apply (lambda (?a1 ...) ?e1 ...) ?args)
         (case-lambda "CLAUSE" ?args ?l 
           ?clause1 ...)))
    ((case-lambda "CLAUSE" ?args ?l
      ((?a1 . ?ar) ?e1 ...) 
      ?clause1 ...)
     (case-lambda "IMPROPER" ?args ?l 1 (?a1 . ?ar) (?ar ?e1 ...) 
       ?clause1 ...))
    ((case-lambda "CLAUSE" ?args ?l 
      (?a1 ?e1 ...)
      ?clause1 ...)
     (let ((?a1 ?args))
       ?e1 ...))
    ((case-lambda "CLAUSE" ?args ?l)
     (error "Wrong number of arguments to CASE-LAMBDA."))
    ((case-lambda "IMPROPER" ?args ?l ?k ?al ((?a1 . ?ar) ?e1 ...)
      ?clause1 ...)
     (case-lambda "IMPROPER" ?args ?l (+ ?k 1) ?al (?ar ?e1 ...) 
      ?clause1 ...))
    ((case-lambda "IMPROPER" ?args ?l ?k ?al (?ar ?e1 ...) 
      ?clause1 ...)
     (if (>= ?l ?k)
         (apply (lambda ?al ?e1 ...) ?args)
         (case-lambda "CLAUSE" ?args ?l 
           ?clause1 ...)))))

;; SRFI-17
(set! (setter car) set-car!)
(set! (setter cdr) set-cdr!)
(set! (setter caar) (lambda (x v) (set-car! (car x) v)))
(set! (setter cadr) (lambda (x v) (set-car! (cdr x) v)))
(set! (setter cdar) (lambda (x v) (set-cdr! (car x) v)))
(set! (setter cddr) (lambda (x v) (set-cdr! (cdr x) v)))
(set! (setter caaar) (lambda (x v) (set-car! (caar x) v)))
(set! (setter caadr) (lambda (x v) (set-car! (cadr x) v)))
(set! (setter cadar) (lambda (x v) (set-car! (cdar x) v)))
(set! (setter caddr) (lambda (x v) (set-car! (cddr x) v)))
(set! (setter cdaar) (lambda (x v) (set-cdr! (caar x) v)))
(set! (setter cdadr) (lambda (x v) (set-cdr! (cadr x) v)))
(set! (setter cddar) (lambda (x v) (set-cdr! (cdar x) v)))
(set! (setter cdddr) (lambda (x v) (set-cdr! (cddr x) v)))
(set! (setter caaaar) (lambda (x v) (set-car! (caaar x) v)))
(set! (setter caaadr) (lambda (x v) (set-car! (caadr x) v)))
(set! (setter caadar) (lambda (x v) (set-car! (cadar x) v)))
(set! (setter caaddr) (lambda (x v) (set-car! (caddr x) v)))
(set! (setter cadaar) (lambda (x v) (set-car! (cdaar x) v)))
(set! (setter cadadr) (lambda (x v) (set-car! (cdadr x) v)))
(set! (setter caddar) (lambda (x v) (set-car! (cddar x) v)))
(set! (setter cadddr) (lambda (x v) (set-car! (cdddr x) v)))
(set! (setter cdaaar) (lambda (x v) (set-cdr! (caaar x) v)))
(set! (setter cdaadr) (lambda (x v) (set-cdr! (caadr x) v)))
(set! (setter cdadar) (lambda (x v) (set-cdr! (cadar x) v)))
(set! (setter cdaddr) (lambda (x v) (set-cdr! (caddr x) v)))
(set! (setter cddaar) (lambda (x v) (set-cdr! (cdaar x) v)))
(set! (setter cddadr) (lambda (x v) (set-cdr! (cdadr x) v)))
(set! (setter cdddar) (lambda (x v) (set-cdr! (cddar x) v)))
(set! (setter cddddr) (lambda (x v) (set-cdr! (cdddr x) v)))
(set! (setter vector-ref) vector-set!)
(set! (setter string-ref) string-set!)
(set! (setter slot-ref) slot-set!)

(define (getter-with-setter get set)
  (let ((proc (lambda args (apply get args))))
    (set! (setter proc) set)
    proc))

;; SRFI-26
(use srfi-26)

;; SRFI-34
(define-syntax guard
  (syntax-rules (else)
    ((_ (cnd clause ... (else elsecl ...)) body)
      (with-exception-handler
        (lambda (cnd)
          (cond clause ... (else elsecl ...)))
        (lambda () body)))
    ((_ (cnd clause ...) body)
      (with-exception-handler
        (lambda (cnd)
          (cond clause ... (else (raise cnd))))
        (lambda () body)))))

;; SRFI-58
(define-sharp-syntax
  "#([0-9]*A(([0-9]+\\*)*[0-9]+)?|A?([0-9]+\\*)*[0-9]+)(:[0-9a-zA-Z]+)?"
  (lambda (x y) (parse-srfi-58 x y))
  #t)

;; java
(define-syntax |.|
  (lambda (k)
    (syntax-case k ()
      ((_ obj name arg ...)
       (if (or (not (symbol? obj)) (defined? obj))
           (syntax (apply-java obj 'name arg ...))
           (syntax (apply-java-static
                    (symbol->string 'obj) 'name arg ...)))))))

(define-syntax new
  (syntax-rules ()
    ((_ klass arg ...)
     (make-java (symbol->string 'klass) arg ...))))

;; #-syntax
(define (debug-print x)
  (format #t "#?=~A~%" x)
  x)
(define-sharp-quote "#\\?=" 'debug-print)
;(define-sharp-syntax "#\\[[^]]*\\]"
;  (lambda (x)
;    (receive (r0 r1) (rxmatch #/\[(.*)\]/ x)
;      (make-char-set r1))))

(define-sharp-syntax "#`([^`]|\\\\`)*`"
  (expand-now
    (let ((x (gensym)) (r0 (gensym)) (r1 (gensym)))
      `(lambda (,x)
        (receive (,r0 ,r1) (rxmatch #/`(.*)`/ ,x)
          (quasiquote-string ,r1))))))

(define-sharp-syntax "#s8"
  (lambda (x y) (list->s8vector y)) #t)
(define-sharp-syntax "#s16"
  (lambda (x y) (list->s16vector y)) #t)
(define-sharp-syntax "#s32"
  (lambda (x y) (list->s32vector y)) #t)
(define-sharp-syntax "#s64"
  (lambda (x y) (list->s8vector y)) #t)
(define-sharp-syntax "#u8"
  (lambda (x y) (list->u8vector y)) #t)
(define-sharp-syntax "#u16"
  (lambda (x y) (list->u16vector y)) #t)
(define-sharp-syntax "#u32"
  (lambda (x y) (list->u32vector y)) #t)
(define-sharp-syntax "#u64"
  (lambda (x y) (list->u8vector y)) #t)
(define-sharp-syntax "#f32"
  (lambda (x y) (list->f32vector y)) #t)
(define-sharp-syntax "#f64"
  (lambda (x y) (list->f64vector y)) #t)

(define λ lambda)

(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test
         (begin result1 result2 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test)
         (begin result1 result2 ...)))))

(define-syntax push!
  (syntax-rules ()
    ((_ place item)
     (set! place (cons item place)))))

(define-syntax pop!
  (syntax-rules ()
    ((_ place)
     (let ((x (car place)))
       (set! place (cdr place))
       x))))

;; END
