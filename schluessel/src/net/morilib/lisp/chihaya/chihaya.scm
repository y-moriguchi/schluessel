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

;identifier?
;bound-identifier=?
;free-identifier=?
;literal-identifier=?
;datum->syntax-object
;syntax-object->datum
;make-capturing-identifier
;begin-for-syntax
;around-syntax
;syntax-error

;free-variable?
;bound-variable?
;wrap-object?, wrap, unwrap -> syntax
;bind-environment!
;find-environment
;bind-interaction-environment!
;make-macro
;append-template
;refer-template

;unsyntax
(define-java-subr wrap-object? net.morilib.lisp.chihaya.IsWrapObject)
(define-java-subr bind-interaction-environment!
  net.morilib.lisp.chihaya.BindInteractionEnvironmentS)
(define-java-subr syntax72 net.morilib.lisp.chihaya.SubrSyntax72)
(define-java-subr syntax->datum net.morilib.lisp.chihaya.SyntaxToDatum72)
(define-java-subr macro-id+ net.morilib.lisp.Symbol$MacroIdAdd)
(define-java-subr make-macro-identifier
  net.morilib.lisp.chihaya.GenerateMacroIdentifier)
(define-java-subr append-template net.morilib.lisp.chihaya.AppendTemplate)
(define-java-subr refer-template net.morilib.lisp.chihaya.ReferTemplate)
(define-java-subr bound-template? net.morilib.lisp.chihaya.IsBoundTemplate)
(define-java-subr no-match-template net.morilib.lisp.chihaya.NoMatchTemplate)
(define-java-subr ->toplevel-symbol net.morilib.lisp.chihaya.ToToplevelSymbol)
(define-java-subr define-syntax net.morilib.lisp.SynDefineMacro72)
(define-java-subr let-syntax net.morilib.lisp.SynLetSyntax72)

(define (errsyn x)
  (or (null? (cddr x)) (error "error")))

(define (bind-variable! s)
  (let ((b1 (make-macro-identifier s)))
    (bind-interaction-environment! s b1)
    b1))

(define-macro-quote syntax
  (lambda (d)
    (macro-id+ 1)
    (syntax2 d)))

(define syntax2
  (lambda (x)
    (syntax72
      (let loop ((x x))
        (cond ((null? x) '())
              ((eq? x '_) '_)
              ((eq? x '...) '...)
              ((free-variable? x) (bind-variable! x))
              ((bound-variable? x) x)
              ((syntax? x) (syntax->datum x))
              ((atom? x) x)
              (else
                (cons (loop (car x)) (loop (cdr x)))))))))

(define-macro-quote quasisyntax
  (lambda (d)
    (macro-id+ 1)
    (quasisyntax2 d)))

(define quasisyntax2
  (lambda (d)
    (syntax72
      (let loop ((x d) (c 0))
        (cond ((null? x) '())
              ((eq? x '_) '_)
              ((eq? x '...) '...)
              ((free-variable? x) (bind-variable! x))
              ((bound-variable? x) x)
              ((syntax? x) (syntax->datum x))
              ((atom? x) x)
              ((eq? (car x) 'syntax)
                (errsyn x)
                (macro-id+ 1)
                (syntax2 (cadr x)))
              ((eq? (car x) 'quasisyntax)
                (errsyn x)
                (macro-id+ 1)
                (quasisyntax2 (cadr x)))
              ((or (eq? (car x) 'unsyntax) (eq? (car x) 'unquote))
                (errsyn x)
                (if (> c 0)
                    (begin
                      (macro-id+ -1)
                      (loop (cadr x) (- c 1)))
                    `(unquote72 ,(quasisyntax3 (cadr x)))))
              ((and (pair? (cdr x))
                    (pair? (cadr x))
                    (or (eq? (caadr x) 'unsyntax-splicing)
                        (eq? (caadr x) 'unquote-splicing)))
                (errsyn (cadr x))
                (cons (loop (car x) c)
                      (if (> c 0)
                          (begin
                            (macro-id+ -1)
                            (loop (cadr x) (- c 1)))
                          `(unquote72 ,(quasisyntax3 (cadadr x))))))
              (else
                (cons (loop (car x) c) (loop (cdr x) c))))))))

(define quasisyntax3
  (lambda (x)
    (cond ((null? x) '())
          ((atom? x) x)
          ((eq? (car x) 'syntax)
            (errsyn x)
            (let ((r (syntax2 (cadr x))))
              r))
          ((eq? (car x) 'quasisyntax)
            (errsyn x)
            (let ((r (quasisyntax2 (cadr x))))
              r))
          (else
            (cons (quasisyntax3 (car x)) (quasisyntax3 (cdr x)))))))

;(define-macro define-syntax
;  (lambda (x y)
;    `(define-macro72 ,x ,(syntax->datum y))))

;(define-macro let-syntax
;  (lambda (vals body)
;    `(let ,(map bind-let-variable vals)
;       (compile ,body))))

;(define-macro letrec-syntax
;  (lambda (vals body)
;    `(letrec
;       ,(map bind-let-variable vals)
;       (compile ,body))))

;(define (bind-let-variable d)
;  (cond ((and (symbol? (car d)) (eq? (caadr d) 'lambda))
;          `(,(car d) (make-macro ,(cadr d))))
;        (else (error "error"))))

(define-macro syntax-case
  (lambda (form literals . clauses)
    #?=(syntax-case72
       form
       literals
       clauses)))

;(define (quote-syncase clauses)
;  (map
;    (lambda (x) `(',(car x) ,(cadr x)))
;    clauses))

;(define syntax-case72
;  (lambda (form literals clauses)
;    (or
;      (let loop ((d clauses))
;        (if (null? d)
;            (error "no pattern")
;            (or (syntax-case1 form literals (car d)) (loop (cdr d)))))
;      (error "error"))))

;(define syntax-case1
;  (lambda (form literals clause)
;    (let ()
;      (and
;        (unify!
;          form literals (car clause) (interaction-environment))
;        (expand-template
;          (cadr clause) 0 #f (interaction-environment))))))

(define syntax-case72
  (lambda (form literals clauses)
    (let loop1 ((d clauses))
       (if (null? d)
           `(error "no pattern")
           `(or ,(syntax-case1 form literals (car d))
                ,(loop1 (cdr d)))))))

(define syntax-case1
  (lambda (form literals clause)
    `(let ()
      (and
        (unify!
          #?=,form ',literals #?=',(car clause) (interaction-environment))
        #?=(expand-template
          ,(cadr clause) 0 #f (interaction-environment))))))

;(define (eval-clauses clauses)
;  (map eval-clauses1 clauses))

;(define (eval-clauses1 x)
;  `(,(car x)
;    ,(let loop ((z (cadr x)))
;       (cond ((null? z) z)
;             ((atom? z) z)
;             ((eq? (car z) 'syntax)
;               (macro-id+ 1)
;               (syntax2 (cadr z)))
;             ((eq? (car z) 'quasisyntax)
;               (macro-id+ 1)
;               (quasisyntax2 (cadr z)))
;             (else (cons (loop (car z)) (loop (cdr z))))))))

; unify
(define (next-ellipses? x)
  (and (pair? (cdr x)) (eq? (cadr x) '...)))

(define (append-query-env env s d)
  (if (or (not (bound-variable? s)) (bound-template? s))
      (append-template (find-environment env s) d)
      d))
;  (append (list (or (find-environment env s) '())) (list d)))

(define (unify! d ltr ptn env)
  (cond ((and (pair? ptn) (next-ellipses? ptn))
          (unify-ellipses! d ltr ptn env))
        ((null? d) (null? ptn))
        ((null? ptn) #f)
        ((memq ptn ltr) (literal-identifier=? d ptn))
        ((symbol? ptn)
          (bind-environment!
           env
           `((,ptn . ,(append-query-env env ptn d)))))
        ((atom? ptn) (eqv? ptn d))
        ((atom? d) #f)
        (else (and (unify! (car d) ltr (car ptn) env)
                   (unify! (cdr d) ltr (cdr ptn) env)))))

(define (unify-ellipses! d ltr ptn env)
  (if (pair? (cddr ptn))
      (call-with-values
        (lambda () (find-longest d ltr ptn))
        (lambda (r d)
          (and r (unify-loop! r ltr (car ptn) env)
                 (unify! d ltr (cddr ptn) env))))
      (unify! (unify-loop! d ltr (car ptn) env)
              ltr
              (cddr ptn)
              env)))

(define (unify-loop! d ltr ptn env)
  (if (null? d)
      (unify-null! ptn env)
      (let loop ((d d))
        (if (pair? d)
            (and (unify! (car d) ltr ptn env) ltr (loop (cdr d))) d))))

(define (unify-null! ptn env)
  (cond ((null? ptn) '())
        ((symbol? ptn)
          (bind-environment! env `((,ptn . ,(no-match-template))))
          '())
        ((atom? ptn) '())
        (else (unify-null! (car ptn) env)
              (unify-null! (cdr ptn) env))))

(define (find-longest x ltr ptn)
  (let loop ((d x) (r '()) (ok? (lambda () (values #f #f))))
    (if (pair? d)
        (car (list
        (loop
          (cdr d)
          (append r (list (car d)))
          (lambda ()
            (if (unify! d (cddr ptn) ltr (make-environment))
                (values r d)
                (ok?))))))
        (or (unify! d (cddr ptn) ltr (make-environment)) (ok?)))))

; expand
(define (expand-template x c cont env)
  (cond ((null? x) '())
        ((bound-template? x env)
          (let ((t (find-environment env x)))
            (or (refer-template t c) (cont #f))))
        ((atom? x) x)
        ((and (pair? (cdr x)) (eq? (->toplevel-symbol (cadr x)) '...))
          (let loop ((c 0))
            (let ((s (call/cc
                       (lambda (k) (expand-template (car x) c k env)))))
              (if s
                 (cons s (loop (+ c 1)))
                 (expand-template (cddr x) 0 #f env)))))
        (else (cons (expand-template (car x) c cont env)
                    (expand-template (cdr x) c cont env)))))

; rules
;(define-macro72 syntax-rules
;  (lambda (x)
;    (syntax-case x '()
;      '((k (i ...) ((keyword . pattern) template) ...)
;          (lambda (y)
;            (syntax-case y '(i ...)
;              '((dummy . pattern) template) ...))))))

;; END
