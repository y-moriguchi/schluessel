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

(define-syntax if3
  (syntax-rules ()
    ((_ c less equal greater)
     (cond ((= c  0) equal)
           ((= c -1) less)
           ((= c  1) greater)
           (else (raise-system-error 'err.srfi67.return.value3))))))

(define-syntax if=?
  (syntax-rules ()
    ((_ c consequent)
     (cond ((= c 0) consequent)
           ((or (= c 1) (= c -1)) (if #f #f))
           (else (raise-system-error 'err.srfi67.return.value3))))
    ((_ c consequent alternate)
     (cond ((= c 0) consequent)
           ((or (= c 1) (= c -1)) alternate)
           (else (raise-system-error 'err.srfi67.return.value3))))))

(define-syntax if<?
  (syntax-rules ()
    ((_ c consequent)
     (cond ((= c -1) consequent)
           ((or (= c 0) (= c 1)) (if #f #f))
           (else (raise-system-error 'err.srfi67.return.value3))))
    ((_ c consequent alternate)
     (cond ((= c -1) consequent)
           ((or (= c 0) (= c 1)) alternate)
           (else (raise-system-error 'err.srfi67.return.value3))))))

(define-syntax if>?
  (syntax-rules ()
    ((_ c consequent)
     (cond ((= c 1) consequent)
           ((or (= c 0) (= c -1)) (if #f #f))
           (else (raise-system-error 'err.srfi67.return.value3))))
    ((_ c consequent alternate)
     (cond ((= c 1) consequent)
           ((or (= c 0) (= c -1)) alternate)
           (else (raise-system-error 'err.srfi67.return.value3))))))

(define-syntax if<=?
  (syntax-rules ()
    ((_ c consequent)
     (cond ((= c 1) (if #f #f))
           ((or (= c 0) (= c -1)) consequent)
           (else (raise-system-error 'err.srfi67.return.value3))))
    ((_ c consequent alternate)
     (cond ((= c 1) alternate)
           ((or (= c 0) (= c -1)) consequent)
           (else (raise-system-error 'err.srfi67.return.value3))))))

(define-syntax if>=?
  (syntax-rules ()
    ((_ c consequent)
     (cond ((= c -1) (if #f #f))
           ((or (= c 0) (= c 1)) consequent)
           (else (raise-system-error 'err.srfi67.return.value3))))
    ((_ c consequent alternate)
     (cond ((= c -1) alternate)
           ((or (= c 0) (= c 1)) consequent)
           (else (raise-system-error 'err.srfi67.return.value3))))))

(define-syntax if-not=?
  (syntax-rules ()
    ((_ c consequent)
     (cond ((= c 0) (if #f #f))
           ((or (= c 1) (= c -1)) consequent)
           (else (raise-system-error 'err.srfi67.return.value3))))
    ((_ c consequent alternate)
     (cond ((= c 0) alternate)
           ((or (= c 1) (= c -1)) consequent)
           (else (raise-system-error 'err.srfi67.return.value3))))))

(define-syntax refine-compare
  (syntax-rules ()
    ((_) 0)
    ((_ c1 c2 ...)
     (let ((c c1))
       (if3 c c (refine-compare c2 ...) c)))))

(define-syntax select-compare
  (syntax-rules (else)
    ((_ x y) 0)
    ((_ x y (else c1 ...)) (refine-compare c1 ...))
    ((_ x y (type? c1 ...) clause ...)
     (cond ((and (type? x) (type? y)) (refine-compare c1 ...))
           ((type? x) -1)
           ((type? y)  1)
           (else (select-compare x y clause ...))))))

(define-syntax cond-compare
  (syntax-rules (else)
    ((_) 0)
    ((_ ((t1 t2) c1 ...) clause ...)
     (let ((r1 t1) (r2 t2))
       (cond ((and r1 r2) (refine-compare c1 ...))
             (r1 -1)
             (r2  1)
             (else (cond-compare clause ...)))))
    ((_ (else c1 ...)) (refine-compare c1 ...))))

(define (default-compare x y)
  (select-compare x y
    (null?    0)
    (pair?    (default-compare (car x) (car y))
              (default-compare (cdr x) (cdr y)))
    (boolean? (boolean-compare x y))
    (char?    (char-compare    x y))
    (string?  (string-compare  x y))
    (symbol?  (symbol-compare  x y))
    (number?  (number-compare  x y))
    (has-vector-interface?
      (let loop ((i 0))
        (cond ((= i (vector-length x))
                (if (= i (vector-length y)) 0 1))
              ((= i (vector-length y)) 1)
              (else
                (let ((c (default-compare
                           (vector-ref x i) (vector-ref y i))))
                  (if (= c 0) (loop (+ i 1)) c))))))
    (else (error "unrecognized types" x y))))

;;
