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

;;
(library (srfi-13)
  (export
    srfi-13
    string-null?  string-prefix? string-prefix-ci? string-suffix?
    string-suffix-ci? reverse-list->string substring/shared
    string-ci= string-ci>= string-ci> string-ci<= string->list
    string-ci< string-ci<>
    string-concatenate string-contains
    string-contains-ci string-downcase
    string-drop string-drop-right
    string= string>= string> string-hash string-hash-ci string-join
    string<= string< string<> string-pad
    string-pad-right string-prefix-length
    string-prefix-length-ci string-replace
    string-suffix-length string-suffix-length-ci
    string-take string-take-right string-titlecase string-upcase
    xsubstring string-reverse string-reverse!
    string-copy! string-titlecase! string-upcase! string-downcase!
    string-xcopy! substring-spec-ok? string-every string-any
    string-tabulate string-trim string-trim-right string-trim-both
    string-compare string-compare-ci
    string-index string-index-right
    string-skip string-skip-right
    string-count string-concatenate/shared
    string-append/shared string-concatenate-reverse
    string-concatenate-reverse/shared string-map string-map!
    string-fold string-fold-right string-unfold string-unfold-right
    string-for-each string-for-each-index
    string-filter string-delete string-tokenize
    string-parse-start+end string-parse-final-start+end
    let-string-start+end check-substring-spec
    make-kmp-restart-vector kmp-step string-kmp-partial-search)
  (define srfi-13 "SRFI-13 Library")

  (define-java-subr string-null?
    net.morilib.lisp.lib.srfi013.IsStringNull)
  (define-java-subr string-prefix?
    net.morilib.lisp.lib.srfi013.IsStringPrefix)
  (define-java-subr string-prefix-ci?
    net.morilib.lisp.lib.srfi013.IsStringPrefixCi)
  (define-java-subr string-suffix?
    net.morilib.lisp.lib.srfi013.IsStringSuffix)
  (define-java-subr string-suffix-ci?
    net.morilib.lisp.lib.srfi013.IsStringSuffixCi)
  (define-java-subr reverse-list->string
    net.morilib.lisp.lib.srfi013.ReverseListToString)
  (define-java-subr string->list
    net.morilib.lisp.subr.StringToList)
  (define-java-subr string-ci=
    net.morilib.lisp.lib.srfi013.StringCiEq)
  (define-java-subr string-ci>=
    net.morilib.lisp.lib.srfi013.StringCiGe)
  (define-java-subr string-ci>
    net.morilib.lisp.lib.srfi013.StringCiGt)
  (define-java-subr string-ci<=
    net.morilib.lisp.lib.srfi013.StringCiLe)
  (define-java-subr string-ci<
    net.morilib.lisp.lib.srfi013.StringCiLt)
  (define-java-subr string-ci<>
    net.morilib.lisp.lib.srfi013.StringCiNe)
  (define-java-subr string-concatenate
    net.morilib.lisp.lib.srfi013.StringConcatenate)
  (define-java-subr string-contains
    net.morilib.lisp.lib.srfi013.StringContains)
  (define-java-subr string-contains-ci
    net.morilib.lisp.lib.srfi013.StringContainsCi)
  (define-java-subr string-downcase
    net.morilib.lisp.lib.srfi013.StringDowncase)
  (define-java-subr string-drop
    net.morilib.lisp.lib.srfi013.StringDrop)
  (define-java-subr string-drop-right
    net.morilib.lisp.lib.srfi013.StringDropRight)
  (define-java-subr string=
    net.morilib.lisp.lib.srfi013.StringEq)
  (define-java-subr string>=
    net.morilib.lisp.lib.srfi013.StringGe)
  (define-java-subr string>
    net.morilib.lisp.lib.srfi013.StringGt)
  (define-java-subr string-hash
    net.morilib.lisp.lib.srfi013.StringHash)
  (define-java-subr string-hash-ci
    net.morilib.lisp.lib.srfi013.StringHashCi)
  (define-java-subr string-join
    net.morilib.lisp.lib.srfi013.StringJoin)
  (define-java-subr string<=
    net.morilib.lisp.lib.srfi013.StringLe)
  (define-java-subr string<
    net.morilib.lisp.lib.srfi013.StringLt)
  (define-java-subr string<>
    net.morilib.lisp.lib.srfi013.StringNe)
  (define-java-subr string-pad
    net.morilib.lisp.lib.srfi013.StringPad)
  (define-java-subr string-pad-right
    net.morilib.lisp.lib.srfi013.StringPadRight)
  (define-java-subr string-prefix-length
    net.morilib.lisp.lib.srfi013.StringPrefixLength)
  (define-java-subr string-prefix-length-ci
    net.morilib.lisp.lib.srfi013.StringPrefixLengthCi)
  (define-java-subr string-replace
    net.morilib.lisp.lib.srfi013.StringReplace)
  (define-java-subr string-reverse
    net.morilib.lisp.lib.srfi013.StringReverse)
  (define-java-subr string-suffix-length
    net.morilib.lisp.lib.srfi013.StringSuffixLength)
  (define-java-subr string-suffix-length-ci
    net.morilib.lisp.lib.srfi013.StringSuffixLengthCi)
  (define-java-subr string-take
    net.morilib.lisp.lib.srfi013.StringTake)
  (define-java-subr string-take-right
    net.morilib.lisp.lib.srfi013.StringTakeRight)
  (define-java-subr string-titlecase
    net.morilib.lisp.lib.srfi013.StringTitlecase)
  (define-java-subr string-upcase
    net.morilib.lisp.lib.srfi013.StringUpcase)
  (define-java-subr xsubstring
    net.morilib.lisp.lib.srfi013.Xsubstring)
  (define-java-subr string-tokenize
    net.morilib.lisp.lib.srfi013.StringTokenize)
  (define-java-subr string-copy!
    net.morilib.lisp.LispString$StringCopyS)
  (define-java-subr string-titlecase!
    net.morilib.lisp.LispString$StringTitlecaseS)
  (define-java-subr string-upcase!
    net.morilib.lisp.LispString$StringUpcaseS)
  (define-java-subr string-downcase!
    net.morilib.lisp.LispString$StringDowncaseS)
  (define-java-subr string-reverse!
    net.morilib.lisp.LispString$StringReverseS)
  (define-java-subr string-xcopy!
    net.morilib.lisp.LispString$StringXcopyS)
  (define-java-subr substring-spec-ok?
    net.morilib.lisp.lib.srfi013.IsSubstringSpecOk)

  (define-java-subr char-set?
    net.morilib.lisp.lib.srfi014.IsCharSet)
  (define-java-subr char-set-contains?
    net.morilib.lisp.lib.srfi014.IsCharSetContains)

  (define (checkrange s b e)
    (let ((l (string-length s)))
      (cond ((or (< b 0) (<= l b))
              (raise-system-error 'err.range.invalid b))
            ((or (< e 0) (<  l e))
              (raise-system-error 'err.range.invalid e))
            ((< e b)
              (raise-system-error 'err.range.invalid)))))

  (define (match-ccp? p ch)
    (cond ((char? p) (eqv? p ch))
          ((char-set? p) (char-set-contains? p ch))
          ((procedure? p) (p ch))
          (else
            (raise-system-error 'err.srfi13.require.charcharsetproc))))

  (define string-every
    (letrec
        ((strea (lambda (p s b e)
                  (checkrange s b e)
                  (let loop ((i b))
                    (cond ((>= i e) #t)
                          ((match-ccp? p (string-ref s i))
                            (loop (+ i 1)))
                          (else #f))))))
      (case-lambda
        ((p s)     (strea p s 0 (string-length s)))
        ((p s b)   (strea p s b (string-length s)))
        ((p s b e) (strea p s b e)))))

  (define string-any
    (letrec
        ((strea (lambda (p s b e)
                  (checkrange s b e)
                  (let loop ((i b))
                    (cond ((>= i e) #f)
                          ((match-ccp? p (string-ref s i)) #t)
                          (else (loop (+ i 1))))))))
      (case-lambda
        ((p s)     (strea p s 0 (string-length s)))
        ((p s b)   (strea p s b (string-length s)))
        ((p s b e) (strea p s b e)))))

  (define substring/shared string-copy)

  (define (string-tabulate proc len)
    (let ((b (make-string-builder)))
      (let loop ((i 0))
        (cond ((>= i len) (builder->string b))
              (else (append-char-builder! b (proc i))
                    (loop (+ i 1)))))))

  (define (string-trim2 s p b e)
    (checkrange s b e)
    (let loop ((i b))
      (cond ((>= i e) "")
            ((match-ccp? p (string-ref s i))
              (loop (+ i 1)))
            (else (string-copy s i e)))))

  (define (string-trim-right2 s p b e)
    (checkrange s b e)
    (let loop ((i (- e 1)))
      (cond ((< i b) "")
            ((match-ccp? p (string-ref s i))
              (loop (- i 1)))
            (else (string-copy s b (+ i 1))))))

  (define string-trim
    (case-lambda
      ((s)       (string-trim2
                   s char-set:whitespace 0 (string-length s)))
      ((s p)     (string-trim2 s p 0 (string-length s)))
      ((s p b)   (string-trim2 s p b (string-length s)))
      ((s p b e) (string-trim2 s p b e))))

  (define string-trim-right
    (case-lambda
      ((s)       (string-trim-right2
                   s char-set:whitespace 0 (string-length s)))
      ((s p)     (string-trim-right2 s p 0 (string-length s)))
      ((s p b)   (string-trim-right2 s p b (string-length s)))
      ((s p b e) (string-trim-right2 s p b e))))

  (define string-trim-both
    (case-lambda
      ((s)       (string-trim-right (string-trim s)))
      ((s p)     (string-trim-right (string-trim s p) p))
      ((s p b)   (string-trim-right (string-trim s p b) p))
      ((s p b e) (string-trim-right (string-trim s p b e) p))))

  (define (string-compare2 s1 s2 proc< proc= proc> b1 e1 b2 e2)
    (checkrange s1 b1 e1)
    (checkrange s2 b2 e2)
    (let loop ((i1 b1) (i2 b2))
      (if (or (>= i1 e1) (>= i2 e2))
          (cond ((< (- e1 b1) (- e2 b2)) (proc< i1))
                ((> (- e1 b1) (- e2 b2)) (proc> i1))
                (else (proc= i1)))
          (let ((c1 (string-ref s1 i1))
                (c2 (string-ref s2 i2)))
            (cond ((char<? c1 c2) (proc< i1))
                  ((char>? c1 c2) (proc> i1))
                  (else (loop (+ i1 1) (+ i2 1))))))))

  (define string-compare
    (case-lambda
      ((s1 s2 proc< proc= proc>)
         (string-compare2 s1 s2 proc< proc= proc>
                          0 (string-length s1)
                          0 (string-length s2)))
      ((s1 s2 proc< proc= proc> b1 e1 b2 e2)
         (string-compare2 s1 s2 proc< proc= proc> b1 e1 b2 e2))))

  (define (string-compare-ci2 s1 s2 proc< proc= proc> b1 e1 b2 e2)
    (checkrange s1 b1 e1)
    (checkrange s2 b2 e2)
    (let loop ((i1 b1) (i2 b2))
      (if (or (>= i1 e1) (>= i2 e2))
          (cond ((< (- e1 b1) (- e2 b2)) (proc< i1))
                ((> (- e1 b1) (- e2 b2)) (proc> i1))
                (else (proc= i1)))
          (let ((c1 (char-downcase (char-upcase (string-ref s1 i1))))
                (c2 (char-downcase (char-upcase (string-ref s2 i2)))))
            (cond ((char<? c1 c2) (proc< i1))
                  ((char>? c1 c2) (proc> i1))
                  (else (loop (+ i1 1) (+ i2 1))))))))

  (define string-compare-ci
    (case-lambda
      ((s1 s2 proc< proc= proc>)
         (string-compare-ci2 s1 s2 proc< proc= proc>
                          0 (string-length s1)
                          0 (string-length s2)))
      ((s1 s2 proc< proc= proc> b1 e1 b2 e2)
         (string-compare-ci2 s1 s2 proc< proc= proc> b1 e1 b2 e2))))

  (define (string-index2 s p b e)
    (checkrange s b e)
    (let loop ((i b))
      (cond ((>= i e) #f)
            ((match-ccp? p (string-ref s i)) i)
            (else (loop (+ i 1))))))

  (define string-index
    (case-lambda
      ((s p)     (string-index2 s p 0 (string-length s)))
      ((s p b)   (string-index2 s p b (string-length s)))
      ((s p b e) (string-index2 s p b e))))

  (define (string-index-right2 s p b e)
    (checkrange s b e)
    (let loop ((i (- e 1)))
      (cond ((< i b) #f)
            ((match-ccp? p (string-ref s i)) i)
            (else (loop (- i 1))))))

  (define string-index-right
    (case-lambda
      ((s p)     (string-index-right2 s p 0 (string-length s)))
      ((s p b)   (string-index-right2 s p b (string-length s)))
      ((s p b e) (string-index-right2 s p b e))))

  (define (string-skip2 s p b e)
    (checkrange s b e)
    (let loop ((i b))
      (cond ((>= i e) #f)
            ((match-ccp? p (string-ref s i)) (loop (+ i 1)))
            (else i))))

  (define string-skip
    (case-lambda
      ((s p)     (string-skip2 s p 0 (string-length s)))
      ((s p b)   (string-skip2 s p b (string-length s)))
      ((s p b e) (string-skip2 s p b e))))

  (define (string-skip-right2 s p b e)
    (checkrange s b e)
    (let loop ((i (- e 1)))
      (cond ((< i b) #f)
            ((match-ccp? p (string-ref s i)) (loop (- i 1)))
            (else i))))

  (define string-skip-right
    (case-lambda
      ((s p)     (string-skip-right2 s p 0 (string-length s)))
      ((s p b)   (string-skip-right2 s p b (string-length s)))
      ((s p b e) (string-skip-right2 s p b e))))

  (define (string-count2 s p b e)
    (checkrange s b e)
    (let loop ((i b) (r 0))
      (cond ((>= i e) r)
            ((match-ccp? p (string-ref s i)) (loop (+ i 1) (+ r 1)))
            (else r))))

  (define string-count
    (case-lambda
      ((s p)     (string-count2 s p 0 (string-length s)))
      ((s p b)   (string-count2 s p b (string-length s)))
      ((s p b e) (string-count2 s p b e))))

  (define string-concatenate/shared string-concatenate)
  (define string-append/shared string-append)

  (define string-concatenate-reverse
    (case-lambda
      ((string-list) (string-concatenate (reverse string-list)))
      ((string-list final-string)
        (string-concatenate 
          (reverse (cons final-string string-list))))
      ((string-list final-string end)
        (string-concatenate 
          (reverse (cons (substring/shared final-string 0 end)
                         string-list))))))

  (define string-concatenate-reverse/shared string-concatenate-reverse)

  (define (string-map2 p s b e)
    (checkrange s b e)
    (let ((m (make-string-builder)))
      (let loop ((i b))
        (cond ((>= i e) (builder->string m))
              (else
                (append-char-builder! m (p (string-ref s i)))
                (loop (+ i 1)))))))

  (define string-map
    (case-lambda
      ((p s)     (string-map2 p s 0 (string-length s)))
      ((p s b)   (string-map2 p s b (string-length s)))
      ((p s b e) (string-map2 p s b e))))

  (define (string-map2! p s b e)
    (checkrange s b e)
    (let loop ((i b))
      (cond ((>= i e) (if #f #f))
            (else
              (string-set! s i (p (string-ref s i)))
              (loop (+ i 1))))))

  (define string-map!
    (case-lambda
      ((p s)     (string-map2! p s 0 (string-length s)))
      ((p s b)   (string-map2! p s b (string-length s)))
      ((p s b e) (string-map2! p s b e))))

  (define (string-fold2 kons knil s b e)
    (if (>= b e)
        knil
        (string-fold2
          kons (kons (string-ref s b) knil)
          s (+ b 1) e)))

  (define string-fold
    (case-lambda
      ((kons knil s)
         (string-fold2 kons knil s 0 (string-length s)))
      ((kons knil s b)
         (checkrange s b e)
         (string-fold2 kons knil s b (string-length s)))
      ((kons knil s b e)
         (checkrange s b e)
         (string-fold2 kons knil s b e))))


  (define (string-fold-right2 kons knil s b e)
    (if (>= b e)
        knil
        (string-fold-right2
          kons (kons (string-ref s (- e 1)) knil)
          s b (- e 1))))

  (define string-fold-right
    (case-lambda
      ((kons knil s)
         (string-fold-right2 kons knil s 0 (string-length s)))
      ((kons knil s b)
         (checkrange s b e)
         (string-fold-right2 kons knil s b (string-length s)))
      ((kons knil s b e)
         (checkrange s b e)
         (string-fold-right2 kons knil s b e))))

  (define (string-unfold2 p f g seed base make-final)
    (let ((bf (make-string-builder)))
      (let loop ((seed seed))
        (cond ((p seed)
                 (append-builder! bf (make-final seed))
                 (builder->string bf))
              (else
                 (append-char-builder! bf (f seed))
                 (loop (g seed)))))))

  (define string-unfold
    (case-lambda
      ((p f g seed)
         (string-unfold2 p f g seed "" (lambda (x) "")))
      ((p f g seed base)
         (string-unfold2 p f g seed base (lambda (x) "")))
      ((p f g seed base make-final)
         (string-unfold2 p f g seed base make-final))))

  (define (string-unfold-right2 p f g seed base make-final)
    (let lp ((seed seed) (ans base))
      (if (p seed) 
          (string-append (make-final seed) ans)
          (lp (g seed) (string-append (string (f seed)) ans)))))

  (define string-unfold-right
    (case-lambda
      ((p f g seed)
         (string-unfold-right2 p f g seed "" (lambda (x) "")))
      ((p f g seed base)
         (string-unfold-right2 p f g seed base (lambda (x) "")))
      ((p f g seed base make-final)
         (string-unfold-right2 p f g seed base make-final))))

  (define (string-for-each2 p s b e)
    (checkrange s b e)
    (let loop ((i b))
      (cond ((>= i e) (if #f #f))
            (else
              (p (string-ref s i))
              (loop (+ i 1))))))

  (define string-for-each
    (case-lambda
      ((p s)     (string-for-each2 p s 0 (string-length s)))
      ((p s b)   (string-for-each2 p s b (string-length s)))
      ((p s b e) (string-for-each2 p s b e))))

  (define (string-for-each-index2 p s b e)
    (checkrange s b e)
    (let loop ((i b))
      (cond ((>= i e) (if #f #f))
            (else
              (p i)
              (loop (+ i 1))))))

  (define string-for-each-index
    (case-lambda
      ((p s)     (string-for-each-index2 p s 0 (string-length s)))
      ((p s b)   (string-for-each-index2 p s b (string-length s)))
      ((p s b e) (string-for-each-index2 p s b e))))

  (define (string-filter2 p s b e)
    (checkrange s b e)
    (let ((m (make-string-builder)))
      (let loop ((i b))
        (if (>= i e) (builder->string m)
            (let ((ch (string-ref s i)))
              (if (match-ccp? p ch) (append-char-builder! m ch))
              (loop (+ i 1)))))))

  (define string-filter
    (case-lambda
      ((p s)     (string-filter2 p s 0 (string-length s)))
      ((p s b)   (string-filter2 p s b (string-length s)))
      ((p s b e) (string-filter2 p s b e))))

  (define string-delete
    (case-lambda
      ((p s)     (string-filter2
                   (lambda (x) (not (match-ccp? p x)))
                   s 0 (string-length s)))
      ((p s b)   (string-filter2
                   (lambda (x) (not (match-ccp? p x)))
                   s b (string-length s)))
      ((p s b e) (string-filter2
                   (lambda (x) (not (match-ccp? p x)))
                   s b e))))

  (define (string-parse-start+end p s args)
    (cond ((null? args) (values '() 0 (string-length s)))
          ((null? (cdr args))
            (checkrange s (car args) (string-length s))
            (values '() (car args) (string-length s)))
          (else
            (checkrange s (car args) (cadr args))
            (values (cddr args) (car args) (cadr args)))))

  (define (string-parse-final-start+end p s args)
    (cond ((null? args) (values '() 0 (string-length s)))
          ((null? (cdr args))
            (checkrange s (car args) (string-length s))
            (values '() (car args) (string-length s)))
          ((null? (cddr args))
            (checkrange s (car args) (cadr args))
            (values '() (car args) (cadr args)))
          (else
            (raise-system-error 'err.argument args))))

  (define-syntax let-string-start+end
    (syntax-rules ()
      ((_ (start end) proc-exp s-exp args-exp body ...)
        (receive (dum start end)
            (string-parse-final-start+end proc-exp s-exp args-exp)
          body ...))
      ((_ (start end rest) proc-exp s-exp args-exp body ...)
        (receive (rest start end)
            (string-parse-start+end proc-exp s-exp args-exp)
          body ...))))

  (define (check-substring-spec proc s start end)
    (checkrange s start end))

  (define (make-kmp-restart-vector2 s c= b e)
    (checkrange s b e)
    (letrec
        ((sref (lambda (i) (string-ref s (+ i b)))))
      (let ((slen (- e b)))
        (cond ((= slen 0)
                (raise-system-error 'err.range.invalid 0))
              ((= slen 1) #(-1))
              (else
                (let ((t (make-vector slen)))
                  (vector-set! t 0 -1)
                  (vector-set! t 1 0)
                  (let loop ((i 2) (j 0))
                    (cond ((>= i slen) t)
                          ((c= (sref (- i 1)) (sref j))
                             (vector-set! t i (+ j 1))
                             (loop (+ i 1) (+ j 1)))
                          ((> j 0) (loop i (vector-ref t j)))
                          (else
                             (vector-set! t i 0)
                             (loop (+ 1 i) 0))))))))))

  (define make-kmp-restart-vector
    (case-lambda
      ((s)
        (make-kmp-restart-vector2 s char=? 0 (string-length s)))
      ((s c=)     (make-kmp-restart-vector2 s c= 0 (string-length s)))
      ((s c= b)   (make-kmp-restart-vector2 s c= b (string-length s)))
      ((s c= b e) (make-kmp-restart-vector2 s c= b e))))

  (define (kmp-step pat rv c i c= p-start)
    (let lp ((i i))
      (if (c= c (string-ref pat (+ i p-start)))
          (+ i 1)
          (let ((i (vector-ref rv i)))
            (if (= i -1) 0
                (lp i))))))

  (define (string-kmp-partial-search2
           pat rv s i c= p-start s-start s-end)
    (checkrange s s-start s-end)
    (let ((patlen (vector-length rv)))
      (let lp ((si s-start)
               (vi i))
        (cond ((= vi patlen) (- si))
              ((= si s-end) vi)
              (else (lp (+ si 1)
                        (kmp-step pat rv (string-ref s si)
                                  vi c= p-start)))))))

  (define string-kmp-partial-search
    (case-lambda
      ((pat rv s i)
         (string-kmp-partial-search2
           pat rv s i char=? 0 0 (string-length s)))
      ((pat rv s i c=)
         (string-kmp-partial-search2
           pat rv s i c= 0 0 (string-length s)))
      ((pat rv s i c= p-start)
         (string-kmp-partial-search2
           pat rv s i c= p-start 0 (string-length s)))
      ((pat rv s i c= p-start s-start)
         (string-kmp-partial-search2
           pat rv s i c= p-start s-start (string-length s)))
      ((pat rv s i c= p-start s-start s-end)
         (string-kmp-partial-search2
           pat rv s i c= p-start s-start s-end))))
)

(cond
  ((not (defined? srfi-13))
    (import (srfi-13))
    (format #t "~a~%" srfi-13)))
