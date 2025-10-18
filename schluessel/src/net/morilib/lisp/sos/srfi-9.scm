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

(define-syntax define-record-type
  (syntax-rules ()
    ((_ type (kons args1 ...) pred (field-tag getter . more) ...)
      (cond ((and (defined? type) (class? type))
              (redefine-class
                type () '(field-tag ...) () () ())
              (define kons
                (lambda args2
                  (let ((ins (instantiate type)))
                    (let loop ((a '(args1 ...)) (b args2))
                      (cond ((and (null? a) (null? b)) ins)
                            ((or  (null? a) (null? b))
                              (error (get-default-message
                                   'err.argument args2)))
                            (else 
                              (slot-set! ins (car a) (car b))
                              (loop (cdr a) (cdr b))))))))
              (define pred
                (lambda (t)
                  (eqv? (class-of t) type)))
              (define-record-field field-tag getter . more)
              ...
              )
            (else
              (define type
                  (instantiate-class () '(field-tag ...) () ()))
              (define kons
                (lambda args2
                  (let ((ins (instantiate type)))
                    (let loop ((a '(args1 ...)) (b args2))
                      (cond ((and (null? a) (null? b)) ins)
                            ((or  (null? a) (null? b))
                              (error (get-default-message
                                   'err.argument args2)))
                            (else 
                              (slot-set! ins (car a) (car b))
                              (loop (cdr a) (cdr b))))))))
              (define pred
                (lambda (t)
                  (eqv? (class-of t) type)))
              (define-record-field field-tag getter . more)
              ...
              )))))

(define-syntax define-record-field
  (syntax-rules ()
    ((_ field-tag getter)
      (define getter
        (lambda (record) (slot-ref record 'field-tag))))
    ((_ field-tag getter setter)
      (begin
        (define getter
          (lambda (record) (slot-ref record 'field-tag)))
        (define setter
          (lambda (record s) (slot-set! record 'field-tag s)))))))
