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

#|
(define-syntax define-record-type
  (lambda (x)
    (syntax-case x ()
      ((_ name-spec record-clause ...)
       (let ((nm #f) (cn #f) (pr #f))
         (syntax-case name-spec ()
           ((record-name constr-name pred-name)
            (begin
              (set! nm record-name)
              (set! cn constr-name)
              (set! pr pred-name)))
           (record-name
            (begin
              (set! nm record-name)
              (set! cn (string->symbol (string-append
                         "make-" (symbol->string record-name))))
              (set! pr (string->symbol (string-append
                         "make-" (symbol->string pred-name)))))))
         (
|#              

;;
