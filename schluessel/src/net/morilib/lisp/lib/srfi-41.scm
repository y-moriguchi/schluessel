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
(library (srfi-41)
  (export
    srfi-41
    stream? stream-null? stream-pair? stream-cons stream-car stream-cdr
    stream-lambda
    define-stream list->stream port->stream stream stream->list
    stream-append stream-concat stream-constant stream-drop
    stream-drop-while stream-filter stream-fold stream-for-each
    stream-from stream-iterate stream-length stream-let stream-map
    stream-match1 stream-match stream-of stream-range stream-ref
    stream-reverse stream-scan stream-take stream-take-while
    stream-unfold stream-unfolds stream-zip)
  (define srfi-41 "SRFI-41 Library")

  ; private
  (define-java-subr stream-kons
    net.morilib.lisp.lib.srfi041.LispStream$StreamKons)
  (define-java-subr stream-kar
    net.morilib.lisp.lib.srfi041.LispStream$StreamKar)
  (define-java-subr stream-kdr
    net.morilib.lisp.lib.srfi041.LispStream$StreamKdr)
  (define-java-subr storim?
    net.morilib.lisp.lib.srfi041.LispStream$IsStorim)
  (define-java-subr stream-nullpo?
    net.morilib.lisp.lib.srfi041.LispStream$IsStreamNullpo)
  (define-java-subr stream-pare?
    net.morilib.lisp.lib.srfi041.LispStream$IsStreamPare)

  (define-syntax stream-wrap
    (syntax-rules ()
      ((_ s)
       (if (promise? s) s (delay s)))))

  (define (stream-force s)
    (if (promise? s) (force s) s))

  (define (stream? s) (storim? (stream-force s)))
  (define (stream-null? s) (stream-nullpo? (stream-force s)))
  (define (stream-pair? s) (stream-pare? (stream-force s)))

  (define-syntax stream-cons
    (syntax-rules ()
      ((_ kar kdr) (stream-kons (delay kar) (delay kdr)))))

  (define (stream-car s) (force (stream-kar (stream-force s))))
  (define (stream-cdr s) (force (stream-kdr (stream-force s))))

  (define-syntax stream-lambda
    (syntax-rules ()
      ((_ vars expr ...)
       (lambda vars (stream-wrap (let () expr ...))))))

  (define-syntax define-stream
    (syntax-rules ()
      ((_ (name . rest) body ...)
       (define name
         (stream-lambda rest body ...)))))

  (define-stream (list->stream lst)
    (cond ((null? lst) stream-null)
          ((pair? lst) (stream-cons
                         (car lst)
                         (list->stream (cdr lst))))
          (else (raise-system-error 'err.list))))

  (define (port->stream . prt)
    (let ((ch (read-char . prt)))
      (cond ((eof-object? ch) stream-null)
            (else (stream-cons ch (port->stream . prt))))))

  (define-syntax stream
    (syntax-rules ()
      ((_) stream-null)
      ((_ a b ...) (stream-cons a (stream b ...)))))

  (define stream->list
    (case-lambda
      ((stream)
        (if (stream-null? stream)
            '()
            (cons (stream-car stream)
                  (stream->list (stream-cdr stream)))))
      ((n stream)
        (if (or (<= n 0) (stream-null? stream))
            '()
            (cons (stream-car stream)
                  (stream->list (- n 1) (stream-cdr stream)))))))

  (define-stream (stream-append% args)
    (define-stream (sappend2 a b)
      (cond ((stream-null? a) b)
            ((stream-pair? a)
              (stream-cons (stream-car a) (sappend2 (stream-cdr a) b)))
            (else
              (raise-system-error 'err.srfi41.require.stream))))
    (cond ((null? args) stream-null)
          ((pair? args)
            (sappend2 (car args) (stream-append%  (cdr args))))
          (else (raise-system-error 'err.list))))
  (define (stream-append . args) (stream-append% args))

  (define-stream (stream-concat args)
    (define-stream (sappend2 a b)
      (cond ((stream-null? a) b)
            ((stream-pair? a)
              (stream-cons (stream-car a) (sappend2 (stream-cdr a) b)))
            (else
              (raise-system-error 'err.srfi41.require.stream))))
    (cond ((stream-null? args) stream-null)
          ((stream-pair? args)
            (sappend2 (stream-car args)
                      (stream-concat (stream-cdr args))))
          (else (raise-system-error 'err.list))))

  (define-stream (stream-constant . args)
    (define (sconst args orgs)
      (cond ((null? args) (sconst orgs orgs))
            ((pair? args)
              (stream-cons (car args) (sconst (cdr args) orgs)))
            (else (raise-system-error 'err.list))))
    (sconst args args))

  (define-stream (stream-drop n strm)
    (if (< n 0) (raise-system-error 'err.require.int.nonnegative n))
    (cond ((stream-null? strm) stream-null)
          ((<= n 0) strm)
          (else (stream-drop (- n 1) (stream-cdr strm)))))

  (define-stream (stream-drop-while pred? strm)
    (cond ((stream-null? strm) stream-null)
          ((pred? (stream-car strm))
            (stream-drop-while pred? (stream-cdr strm)))
          (else strm)))

  (define-stream (stream-filter pred? stream)
    (cond ((stream-null? stream) stream-null)
          ((pred? (stream-car stream))
            (stream-cons (stream-car stream)
                         (stream-filter pred? (stream-cdr stream))))
          (else
            (stream-filter pred? (stream-cdr stream)))))

  (define (stream-fold proc base stream)
    (cond ((stream-null? stream) base)
          ((stream-pair? stream)
            (stream-fold
              proc
              (proc base (stream-car stream))
              (stream-cdr stream)))
          (else (raise-system-error 'err.srfi41.require.stream))))

  ; use by stream-for-each and stream-map
  (define (null-streams? streams)
    (cond ((null? streams) #f)
          ((not (pair? streams)) (raise-system-error 'err.list))
          ((stream-null? (car streams)) #t)
          ((stream-pair? (car streams))
            (null-streams? (cdr streams)))
          (else (raise-system-error 'err.srfi41.require.stream))))

  (define (stream-cars streams)
    (call/cc (lambda (k)
      (let loop ((streams streams))
        (cond ((null? streams) '())
              ((not (pair? streams)) (raise-system-error 'err.list))
              ((stream-null? (car streams)) (k '()))
              ((stream-pair? (car streams))
                (cons (stream-car (car streams))
                      (loop (cdr streams))))
              (else
                (raise-system-error 'err.srfi41.require.stream)))))))

  (define (stream-cdrs streams)
    (call/cc (lambda (k)
      (let loop ((streams streams))
        (cond ((null? streams) '())
              ((not (pair? streams)) (raise-system-error 'err.list))
              ((stream-null? (car streams)) (k '()))
              ((stream-pair? (car streams))
                (cons (stream-cdr (car streams))
                      (loop (cdr streams))))
              (else
                (raise-system-error 'err.srfi41.require.stream)))))))

  (define (stream-for-each proc . streams)
    (cond ((null-streams? streams) (if #f #f))
          (else
            (apply proc (stream-cars streams))
            (apply stream-for-each
              (cons proc (stream-cdrs streams))))))

  (define-stream (stream-from first . args)
    (let ((step
            (cond ((null? args) 1)
                  ((and (pair? args) (null? (cdr args)))
                    (car args))
                  (else (raise-system-error 'err.argument)))))
      (if (not (number? first))
          (raise-system-error 'err.require.number first))
      (if (not (number? step))
          (raise-system-error 'err.require.number step))
      (stream-cons first (stream-from (+ first step) . args))))

  (define-stream (stream-iterate proc base)
    (stream-cons base (stream-iterate proc (proc base))))

  (define (stream-length stream)
    (let loop ((s stream) (n 0))
      (cond ((stream-null? s) n)
            ((stream-pair? s) (loop (stream-cdr s) (+ n 1)))
            (else (raise-system-error 'err.srfi41.require.stream)))))

  (define-syntax stream-let
    (syntax-rules ()
      ((_ tag ((var expr) ...) body ...)
        (let tag ((var expr) ...)
          (stream-wrap (let () body ...))))))
 
  (define-stream (stream-map proc . streams)
    (cond ((null-streams? streams) stream-null)
           (else
             (stream-cons
               (apply proc (stream-cars streams))
               (apply stream-map
                 (cons proc (stream-cdrs streams)))))))

  (define-syntax stream-match1
    (lambda (x)
      (syntax-case x ()
        ((_ strm () fender expr)
         #'(and (stream-null? strm) fender (lambda () expr)))
        ((_ strm (pat0 . patr) fender expr)
         #`(and (stream-pair? strm)
             #,(cond
                ((identifier? #'pat0)
                  #'(let ((pat0 (stream-car strm)))
                      (stream-match1 (stream-cdr strm) patr
                        fender expr)))
                ((pair? #'pat0)
                  #'(stream-match1 (stream-car strm) pat0 #t
                      (let ((x (stream-match1 (stream-cdr strm) patr
                                 fender expr)))
                        (if x (x) #f))))
                (else
                  #'(and (stream-match1 (stream-car strm) pat0 #t #t)
                         (stream-match1 (stream-cdr strm) patr
                           fender expr))))))
        ((_ strm pat fender expr)
         (if (identifier? #'pat)
             #'(let ((pat strm)) (and fender (lambda () expr)))
             #'(and (equal? pat strm) fender (lambda () expr)))))))

  (define-syntax stream-match
    (syntax-rules ()
      ((_ strm (pats expr))
       (let ((x (stream-match1 strm pats #t expr)))
         (if x (x))))
      ((_ strm (pats fender expr))
       (let ((x (stream-match1 strm pats fender expr)))
         (if x (x))))
      ((_ strm (pats expr) clause ...)
       (let ((x (stream-match1 strm pats #t expr)))
         (if x (x) (stream-match strm clause ...))))
      ((_ strm (pats fender expr) clause ...)
       (let ((x (stream-match1 strm pats fender expr)))
         (if x (x) (stream-match strm clause ...))))))

  (define *stream-of-dum1* (gensym))
  (define-syntax stream-of
    (syntax-rules (in is)
      ((_ expr) expr)
      ((_ expr (var in strm-expr) clause ...)
       (let loop ((v strm-expr))
         (if (stream-null? v)
             stream-null
             (let ((var (stream-car v)))
               (let ((r (stream-of expr clause ...)))
                 (if (eq? r *stream-of-dum1*)
                     (loop (stream-cdr v))
                     (stream-cons (stream-of expr clause ...)
                                  (loop (stream-cdr v)))))))))
      ((_ expr (var is v) clause ...)
       (let ((var v))
         (stream-of expr clause ...)))
      ((_ expr (pred? x) clause ...)
       (if (pred? x)
           (stream-of expr clause ...)
           *stream-of-dum1*))))

  (define-stream (stream-range first past . rest)
    (let ((step
            (cond ((null? rest) 1)
                  ((and (pair? rest) (null? (cdr rest))) (car rest))
                  (else (raise-system-error 'err.argument)))))
      (if (not (real? first))
          (raise-system-error 'err.require.number first))
      (if (not (real? past))
          (raise-system-error 'err.require.number past))
      (if (not (real? step))
          (raise-system-error 'err.require.number step))
      (stream-let loop ((x first))
        (if (< x past)
            (stream-cons x (loop (+ x step)))
            stream-null))))

  (define (stream-ref strm n)
    (if (< n 0) (raise-system-error 'err.require.int.nonnegative n))
    (let loop ((s strm) (i 0))
      (cond ((stream-null? s)
             (raise-system-error 'err.srfi41.stream.outofrange))
            ((= i n) (stream-car s))
            (else (loop (stream-cdr s) (+ i 1))))))

  (define-stream (stream-reverse strm)
    (stream-let loop ((s strm) (r stream-null))
      (if (stream-null? s)
          r
          (loop (stream-cdr s) (stream-cons (stream-car s) r)))))

  (define-stream (stream-scan proc base strm)
    (if (stream-null? strm)
        stream-null
        (stream-cons base
                     (stream-scan proc
                       (proc base (stream-car strm))
                       (stream-cdr strm)))))

  (define-stream (stream-take n strm)
    (if (not (integer? n))
        (raise-system-error 'err.require.integer n))
    (if (< n 0) (raise-system-error 'err.require.int.nonnegative n))
    (cond ((stream-null? strm) stream-null)
          ((= n 0) stream-null)
          (else
            (stream-cons (stream-car strm)
                         (stream-take (- n 1) (stream-cdr strm))))))

  (define-stream (stream-take-while pred? strm)
    (cond ((stream-null? strm) stream-null)
          ((pred? (stream-car strm))
            (stream-cons (stream-car strm)
                         (stream-take-while pred? (stream-cdr strm))))
          (else stream-null)))

  (define-stream (stream-unfold map pred? gen base)
    (if (pred? base)
        (stream-cons (map base)
                     (stream-unfold map pred? gen (gen base)))
        stream-null))

  (define (stream-unfolds proc seed)
    (receive (s2 . vals) (proc seed)
      (print vals)
      (if (let loop ((v vals))
            (and (not (null? v)) (or (null? (car v)) (loop (cdr v)))))
          (apply values (vector->list
            (make-vector (length vals) stream-null)))
          (receive vr (stream-unfolds proc s2)
            (apply values
              (let loop ((v0 vals) (vr vr))
                (cond ((null? v0) '())
                      ((car v0) (cons (stream-cons (caar v0) (car vr))
                                      (loop (cdr v0) (cdr vr))))
                      (else (cons (car vr)
                                  (loop (cdr v0) (cdr vr)))))))))))

  (define-stream (stream-zip . args)
    (if (null-streams? args)
        stream-null
        (stream-cons (stream-cars args)
                     (apply stream-zip (stream-cdrs args)))))
)

(cond
  ((not (defined? srfi-41))
    (import (srfi-41))))
;; END
