;;;
;;; Copyright 2009-2012 Yuichiro Moriguchi
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

(define (iterator-for-each proc . iterators)
  (let loop ((itr (apply group-iterator iterators)))
    (cond ((iterator-null? itr) (if #f #f))
          (else
            (apply proc (iterator-car itr))
            (loop (iterator-cdr itr))))))

(define (iterator-map proc . iterators)
  (let loop ((itr (apply group-iterator iterators)))
    (cond ((iterator-null? itr) '())
          (else
            (cons (apply proc (iterator-car itr))
                  (loop (iterator-cdr itr)))))))

(define (iterator-fold proc nill . iterators)
  (let loop ((itr (apply group-iterator iterators)))
    (if (iterator-null? itr)
        nill
        (cons (apply proc (iterator-car itr))
              (loop (iterator-cdr itr))))))

;;
