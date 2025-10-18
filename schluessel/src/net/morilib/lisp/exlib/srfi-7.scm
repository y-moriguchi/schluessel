;;;
;;; Copyright 2009-2012 Yuichiro Moriguchi
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(define (eval-srfi-7 x)
  (if (and (pair? x) (eq? (car x) 'program))
      (eval-program-clause-list (cdr x))
      x))

(define (eval-program-clause-list x)
  `(begin ,@(map eval-program-clause x)))

(define (eval-program-clause x)
  (cond ((not (pair? x)) x)
        ((eq? (car x) 'requires)
          `(begin ,@(eval-requires (cdr x))))
        ((eq? (car x) 'files)
          `(begin ,@(eval-files (cdr x))))
        ((eq? (car x) 'code) `(begin ,@(cdr x)))
        ((eq? (car x) 'feature-cond)
          (eval-feature-cond-clause-list (cdr x)))))

(define (eval-requires x)
  (map (lambda (x) `(apply-features ',x)) x))

(define (eval-files fnlst)
  (map (lambda (x) `(begin ,@(eval-file x))) fnlst))

(define (eval-file fn)
  (call-with-input-file fn
    (lambda (in)
      (let label ((next (read in)))
        (if (eof-object? next)
            '()
            (cons next (label (read in))))))))

(define (eval-feature-cond-clause-list x)
  (cond ((null? x) #f)
        ((eq? (caar x) 'else) (eval-program-clause-list (cdar x)))
        (else
          `(if (apply-features ,(eval-feature-requirement (caar x)))
               ,(eval-program-clause-list (cdar x))
               ,(eval-feature-cond-clause-list (cdr x))))))

(define (eval-feature-requirement x)
  (cond ((pair? x)
          (case (car x)
            ((and)
              `(feature-and ,@(map eval-feature-requirement (cdr x))))
            ((or)
              `(feature-or  ,@(map eval-feature-requirement (cdr x))))
            ((not)
              `(feature-not ,(eval-feature-requirement (cadr x))))
            (else  (error "invalid operator"))))
        ((null? x) (error "syntax error"))
        (else `',x)))

;; END
