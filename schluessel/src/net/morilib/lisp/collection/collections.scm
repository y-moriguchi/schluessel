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

;; SRFI-44
(bind-flexible-sequence!!
  'net.morilib.lisp.SExpressionDatum 'list 'err.require.list)
(bind-map!!
  'net.morilib.lisp.SExpressionDatum 'alist-map 'err.require.assoc)
(bind-dictionary!!
  'net.morilib.lisp.SExpressionDatum 'alist-map 'err.require.assoc)
(define alist-map? list?)

(bind-sequence!!
  'net.morilib.lisp.LispVector 'vector 'err.require.vector)

(bind-sequence!!
  'net.morilib.lisp.LispString 'string 'err.require.string)
(define-java-subr string-set! net.morilib.lisp.SubrStringSetS)

(bind-set!!
  'net.morilib.lisp.collection.LispHashSet
  'hash-set
  'err.collection.require.hashset)

(bind-set!!
  'net.morilib.lisp.collection.LispTreeSet
  'tree-set
  'err.collection.require.hashset)
(bind-ordered-collection!!
  'net.morilib.lisp.collection.LispTreeSet
  'tree-set
  'err.collection.require.treeset)

;; enums
(bind-set!!
  'net.morilib.lisp.collection.enums.LispEnumSetClass$LispEnumSet
  'enum-set2
  'err.collection.require.enumset)
(bind-map!!
  'net.morilib.lisp.collection.enums.LispEnumMapClass$LispEnumMap
  'enum-map2
  'err.collection.require.enummap)

;; end
