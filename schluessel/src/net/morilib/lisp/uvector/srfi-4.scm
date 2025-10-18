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

(define-sharp-syntax "#s8"  (lambda (x y) (s8vector  . y)) #t)
(define-sharp-syntax "#s16" (lambda (x y) (s16vector . y)) #t)
(define-sharp-syntax "#s32" (lambda (x y) (s32vector . y)) #t)
(define-sharp-syntax "#s64" (lambda (x y) (s64vector . y)) #t)
(define-sharp-syntax "#u8"  (lambda (x y) (u8vector  . y)) #t)
(define-sharp-syntax "#u16" (lambda (x y) (u16vector . y)) #t)
(define-sharp-syntax "#u32" (lambda (x y) (u32vector . y)) #t)
(define-sharp-syntax "#u64" (lambda (x y) (u64vector . y)) #t)
(define-sharp-syntax "#f32" (lambda (x y) (f32vector . y)) #t)
(define-sharp-syntax "#f64" (lambda (x y) (f64vector . y)) #t)
