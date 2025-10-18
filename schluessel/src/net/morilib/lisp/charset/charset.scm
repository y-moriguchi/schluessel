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

(define (char-set-fold kons knil cs)
  (let loop ((kons kons) (knil knil) (l (char-set-cursor cs)))
    (cond ((end-of-char-set? l) knil)
          (else
            (loop kons
                  (kons (char-set-ref cs l) knil)
                  (char-set-cursor-next cs l))))))

(define char-set-unfold
  (case-lambda
    ((p f g seed)
      (char-set-unfold! p f g seed #f))
    ((p f g seed base-cs)
      (char-set-unfold! p f g seed (char-set-copy base-cs)))))

(define (char-set-unfold! p f g seed base-cs)
  (let ((bld (make-char-set-builder base-cs)))
    (let loop ((seed seed))
      (cond ((p seed) (builder->char-set bld))
            (else
              (adjoin-char-set-builder! bld (f seed))
              (loop (g seed)))))))

(define (char-set-for-each proc cs)
  (let loop ((l (char-set-cursor cs)))
    (cond ((end-of-char-set? l) (if #f #f))
          (else
            (proc (char-set-ref cs l))
            (loop (char-set-cursor-next cs l))))))

(define (char-set-map proc cs)
  (let ((bld (make-char-set-builder #f)))
    (let loop ((l (char-set-cursor cs)))
      (cond ((end-of-char-set? l) (builder->char-set bld))
            (else
              (adjoin-char-set-builder! bld (proc (char-set-ref cs l)))
              (loop (char-set-cursor-next cs l)))))))

(define char-set-filter
  (case-lambda
    ((pred cs)
      (char-set-filter! pred cs #f))
    ((pred cs base-cs)
      (char-set-filter! pred cs (char-set-copy base-cs)))))

(define (char-set-filter! pred cs base-cs)
  (let ((bld (make-char-set-builder base-cs)))
    (let loop ((l (char-set-cursor cs)))
      (cond ((end-of-char-set? l) (builder->char-set bld))
            ((pred (char-set-ref cs l))
              (adjoin-char-set-builder! bld (char-set-ref cs l))
              (loop (char-set-cursor-next cs l)))
            (else (loop (char-set-cursor-next cs l)))))))

(define (char-set-count pred cs)
  (let loop ((l (char-set-cursor cs)) (n 0))
    (cond ((end-of-char-set? l) n)
          ((pred (char-set-ref cs l))
            (loop (char-set-cursor-next cs l) (+ n 1)))
          (else (loop (char-set-cursor-next cs l) n)))))

(define (char-set-every pred cs)
  (let loop ((l (char-set-cursor cs)))
    (cond ((end-of-char-set? l) #t)
          ((pred (char-set-ref cs l))
            (loop (char-set-cursor-next cs l)))
          (else #f))))

(define (char-set-any pred cs)
  (let loop ((l (char-set-cursor cs)))
    (cond ((end-of-char-set? l) #f)
          ((pred (char-set-ref cs l)) #t)
          (else (loop (char-set-cursor-next cs l))))))

;; end
