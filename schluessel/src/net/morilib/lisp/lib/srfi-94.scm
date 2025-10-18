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

(define-java-subr real-exp net.morilib.lisp.lib.srfi094.RealExp)
(define-java-subr real-ln net.morilib.lisp.lib.srfi094.RealLn)
(define-java-subr real-sin net.morilib.lisp.lib.srfi094.RealSin)
(define-java-subr real-cos net.morilib.lisp.lib.srfi094.RealCos)
(define-java-subr real-tan net.morilib.lisp.lib.srfi094.RealTan)
(define-java-subr real-asin net.morilib.lisp.lib.srfi094.RealAsin)
(define-java-subr real-acos net.morilib.lisp.lib.srfi094.RealAcos)
(define-java-subr real-atan net.morilib.lisp.lib.srfi094.RealAtan)
(define-java-subr real-sqrt net.morilib.lisp.lib.srfi094.RealSqrt)
(define-java-subr real-expt net.morilib.lisp.lib.srfi094.RealExpt)
(define-java-subr real-log net.morilib.lisp.lib.srfi094.RealLog)

(define-java-subr
  integer-sqrt net.morilib.lisp.lib.srfi094.IntegerSqrt)
(define-java-subr
  integer-expt net.morilib.lisp.lib.srfi094.IntegerExpt)
(define-java-subr integer-log net.morilib.lisp.lib.srfi094.IntegerLog)

(define-java-subr quo net.morilib.lisp.lib.srfi094.Quo)
(define-java-subr rem net.morilib.lisp.lib.srfi094.Rem)
(define-java-subr mod net.morilib.lisp.lib.srfi094.Mod)

(define ln log)

(define *srfi-94-safe-mode* #t)

;;