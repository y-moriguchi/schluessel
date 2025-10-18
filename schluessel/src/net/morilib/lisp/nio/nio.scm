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

;; SRFI-16
(define (blob-u16-native-ref blob k)
  (blob-u16-ref (endianness native) blob k))
(define (blob-s16-native-ref blob k)
  (blob-s16-ref (endianness native) blob k))
(define (blob-u16-native-set! blob k n)
  (blob-u16-set! (endianness native) blob k n))
(define (blob-s16-native-set! blob k n)
  (blob-s16-set! (endianness native) blob k n))

(define (blob-u32-native-ref blob k)
  (blob-u32-ref (endianness native) blob k))
(define (blob-s32-native-ref blob k)
  (blob-s32-ref (endianness native) blob k))
(define (blob-u32-native-set! blob k n)
  (blob-u32-set! (endianness native) blob k n))
(define (blob-s32-native-set! blob k n)
  (blob-s32-set! (endianness native) blob k n))

(define (blob-u64-native-ref blob k)
  (blob-u64-ref (endianness native) blob k))
(define (blob-s64-native-ref blob k)
  (blob-s64-ref (endianness native) blob k))
(define (blob-u64-native-set! blob k n)
  (blob-u64-set! (endianness native) blob k n))
(define (blob-s64-native-set! blob k n)
  (blob-s64-set! (endianness native) blob k n))

;; Utilities
(define (blob-u16-big-ref blob k)
  (blob-u16-ref (endianness big) blob k))
(define (blob-s16-big-ref blob k)
  (blob-s16-ref (endianness big) blob k))
(define (blob-u16-big-set! blob k n)
  (blob-u16-set! (endianness big) blob k n))
(define (blob-s16-big-set! blob k n)
  (blob-s16-set! (endianness big) blob k n))

(define (blob-u32-big-ref blob k)
  (blob-u32-ref (endianness big) blob k))
(define (blob-s32-big-ref blob k)
  (blob-s32-ref (endianness big) blob k))
(define (blob-u32-big-set! blob k n)
  (blob-u32-set! (endianness big) blob k n))
(define (blob-s32-big-set! blob k n)
  (blob-s32-set! (endianness big) blob k n))

(define (blob-u64-big-ref blob k)
  (blob-u64-ref (endianness big) blob k))
(define (blob-s64-big-ref blob k)
  (blob-s64-ref (endianness big) blob k))
(define (blob-u64-big-set! blob k n)
  (blob-u64-set! (endianness big) blob k n))
(define (blob-s64-big-set! blob k n)
  (blob-s64-set! (endianness big) blob k n))

(define (blob-u16-little-ref blob k)
  (blob-u16-ref (endianness little) blob k))
(define (blob-s16-little-ref blob k)
  (blob-s16-ref (endianness little) blob k))
(define (blob-u16-little-set! blob k n)
  (blob-u16-set! (endianness little) blob k n))
(define (blob-s16-little-set! blob k n)
  (blob-s16-set! (endianness little) blob k n))

(define (blob-u32-little-ref blob k)
  (blob-u32-ref (endianness little) blob k))
(define (blob-s32-little-ref blob k)
  (blob-s32-ref (endianness little) blob k))
(define (blob-u32-little-set! blob k n)
  (blob-u32-set! (endianness little) blob k n))
(define (blob-s32-little-set! blob k n)
  (blob-s32-set! (endianness little) blob k n))

(define (blob-u64-little-ref blob k)
  (blob-u64-ref (endianness little) blob k))
(define (blob-s64-little-ref blob k)
  (blob-s64-ref (endianness little) blob k))
(define (blob-u64-little-set! blob k n)
  (blob-u64-set! (endianness little) blob k n))
(define (blob-s64-little-set! blob k n)
  (blob-s64-set! (endianness little) blob k n))

;; END