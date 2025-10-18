/*
 * Copyright 2009 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.morilib.lisp.test.r6rs;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

public class SyntaxCaseTest extends TCSubr {
	
	public void testSyntaxCase1() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(define-syntax myor" +
				"  (lambda (x)" +
				"    (syntax-case x ()" +
				"      [(_) (syntax #f)]" +
				"      [(_ e) (syntax e)]" +
				"      [(_ e1 e2 e3 ...)" +
				"       (syntax (let ([t e1])" +
				"                 (if t t (myor e2 e3 ...))))])))");
		eqi  (l,"(myor #f #f 1)", 1);
		eq   (l,"(myor)", F);
		eqi  (l,"(myor 1 2 3)", 1);
		eq   (l,"(myor #f #f #f)", F);
		
		l.exec ("(define-syntax myor2" +
				"  (lambda (x)" +
				"    (syntax-case x ()" +
				"      [(_) #'#f]" +
				"      [(_ e) #'e]" +
				"      [(_ e1 e2 e3 ...)" +
				"       #'(let ([t e1])" +
				"                 (if t t (myor2 e2 e3 ...)))])))");
		eqi  (l,"(myor2 #f #f 1)", 1);
		eq   (l,"(myor2)", F);
		eqi  (l,"(myor2 1 2 3)", 1);
		eq   (l,"(myor2 #f #f #f)", F);
		
		l.exec ("(define p0 (cons 4 5))");
		l.exec ("(define-syntax p0.car" +
				"  (lambda (x)" +
				"    (syntax-case x ()" +
				"      [(_ . rest) #'((car p0) . rest)]" +
				"      [_  #'(car p0)])))");
		eqi  (l,"p0.car", 4);
		lperr(l,"(set! p0.car 15)");
		
		l.exec ("(define p (cons 4 5))");
		l.exec ("(define-syntax p.car" +
				"  (make-variable-transformer" +
				"    (lambda (x)" +
				"      (syntax-case x (set!)" +
				"        [(set! _ e) #'(set-car! p e)]" +
				"        [(_ . rest) #'((car p) . rest)]" +
				"        [_  #'(car p)]))))");
		l.exec ("(set! p.car 15)");
		eqi  (l,"p.car", 15);
		equal(l,"p", cons(15, 5));
	}
	
	public void testSyntaxCase2() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(define-syntax myor3" +
				"  (lambda (x)" +
				"    (syntax-case x ()" +
				"      [(_) (syntax #f)]" +
				"      [(_ #(e)) (syntax e)]" +
				"      [(_ #(e1 e2 e3 ...))" +
				"       (syntax (let ([t e1])" +
				"                 (if t t (myor3 #(e2 e3 ...)))))])))");
		eqi  (l,"(myor3 #(#f #f 1))", 1);
		eq   (l,"(myor3)", F);
		eqi  (l,"(myor3 #(1 2 3))", 1);
		eq   (l,"(myor3 #(#f #f #f))", F);
	}
	
	public void testIsIdentifier() {
		Scheme l = Scheme.newInstance();
		
		eq   (l,"(identifier? 'x)", T);
		eq   (l,"(identifier? 1)", F);
		eq   (l,"(identifier? #t)", F);
		eq   (l,"(identifier? \"x\")", F);
		eq   (l,"(identifier? #\\a)", F);
		eq   (l,"(identifier? car)", F);
		eq   (l,"(identifier? '())", F);
		eq   (l,"(identifier? '(1 2))", F);
		eq   (l,"(identifier? '#(1 2))", F);
		
		l.exec ("(define-syntax s1" +
				"  (lambda (x)" +
				"    (if (identifier? (cadr x)) #'#t #'#f)))");
		eq   (l,"(s1 x)", T);
		eq   (l,"(s1 1)", F);
	}
	
	public void testSyntaxToDatum() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(define (sym-id=? x y)" +
				"  (eq? (syntax->datum x)" +
				"       (syntax->datum y)))");
		l.exec ("(define-syntax s1" +
				"  (lambda (x)" +
				"    (if (sym-id=? (cadr x) (caddr x))" +
				"        #'#t" +
				"        #'#f)))");
		eq   (l,"(s1 sym sym)", T);
		eq   (l,"(s1 syn sym)", F);
	}
	
	public void testDatumToSyntax() {
		Scheme l = Scheme.newInstance();
		
//		l.exec ("(define-syntax loop" +
//				"  (lambda (x)" +
//				"    (syntax-case x ()" +
//				"      [(k e ...)" +
//				"       (with-syntax" +
//				"           ([break (datum->syntax #'k 'break)])" +
//				"         #'(call-with-current-continuation" +
//				"             (lambda (break)" +
//				"               (let f () e ... (f)))))])))");
		l.exec ("(define-syntax loop" +
				"  (lambda (x)" +
				"    (syntax-case x ()" +
				"      [(k e ...)" +
				"        (let ((break (datum->syntax #'k 'break)))" +
				"          #`(call-with-current-continuation" +
				"              (lambda (#,break)" +
				"                (let f () e ... (f)))))])))");
		equal(l,"(let ((n 3) (ls '()))" +
				"  (loop" +
				"    (if (= n 0) (break ls))" +
				"    (set! ls (cons 'a ls))" +
				"    (set! n (- n 1))))",
				list(sym("a"), sym("a"), sym("a")));
	}
	
	public void testQuasisyntax1() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(define-syntax mycase" +
				"  (lambda (x)" +
				"    (syntax-case x ()" +
				"      [(_ e c1 c2 ...)" +
				"       #`(let ([t e])" +
				"           #,(let f ([c1 #'c1] [cmore #'(c2 ...)])" +
				"               (if (null? cmore)" +
   				"                   (syntax-case c1 (else)" +
				"                     [(else e1 e2 ...)" +
				"                      #'(begin e1 e2 ...)]" +
				"                     [((k ...) e1 e2 ...)" +
				"                      #'(if (memv t '(k ...))" +
				"                            (begin e1 e2 ...))])" +
				"                   (syntax-case c1 ()" +
				"                     [((k ...) e1 e2 ...)" +
				"                      #`(if (memv t '(k ...))" +
				"                            (begin e1 e2 ...)" +
				"                            #,(f (car cmore)" +
				"                                  (cdr cmore)))]))))])))");
		l.exec ("(define (myc x)" +
				"  (mycase x" +
				"    ((1 3 5 7 9) 'odd)" +
				"    ((2 4 6 8 0) 'even)" +
				"    (else 'zzz)))");
		eq    (l,"(myc 1)",  sym("odd"));
		eq    (l,"(myc 2)",  sym("even"));
		eq    (l,"(myc 10)", sym("zzz"));
	}
	
	public void testQuasisyntax2() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(define-syntax mycase" +
				"  (lambda (x)" +
				"    (syntax-case x ()" +
				"      [(_ e c1 c2 ...)" +
				"       #`((lambda (t)" +
				"           #,(let f ([c12 #'c1] [cmore2 #'(c2 ...)])" +
				"               ((lambda (c1 cmore) (if (null? cmore)" +
   				"                   (syntax-case c1 (else)" +
				"                     [(else e1 e2 ...)" +
				"                      #'(begin e1 e2 ...)]" +
				"                     [((k ...) e1 e2 ...)" +
				"                      #'(if (memv t '(k ...))" +
				"                            (begin e1 e2 ...))])" +
				"                   (syntax-case c1 ()" +
				"                     [((k ...) e1 e2 ...)" +
				"                      #`(if (memv t '(k ...))" +
				"                            (begin e1 e2 ...)" +
				"                            #,(f (car cmore)" +
				"                                  (cdr cmore)))]))) c12 cmore2))) e)])))");
		l.exec ("(define (myc x)" +
				"  (mycase x" +
				"    ((1 3 5 7 9) 'odd)" +
				"    ((2 4 6 8 0) 'even)" +
				"    (else 'zzz)))");
		eq    (l,"(myc 1)",  sym("odd"));
		eq    (l,"(myc 2)",  sym("even"));
		eq    (l,"(myc 10)", sym("zzz"));
	}
	
//	public void testWithSyntax1() {
//		Scheme l = Scheme.newInstance();
//		
//		l.exec ("(define-syntax s1" +
//				"  (lambda (x)" +
//				"    (with-syntax (((t ...) (list 1 2 3 4)))" +
//				"      #'(list t ...))))");
//		equal(l,"(s1)", list(1, 2, 3, 4));
//		
//		l.exec ("(define-syntax s2" +
//				"  (lambda (x)" +
//				"    (syntax-case x ()" +
//				"      ((_)" +
//				"        (with-syntax (((t ...) (list 1 2 3 4)))" +
//				"          #'(list t ...))))))");
//		equal(l,"(s2)", list(1, 2, 3, 4));
//	}
	
//	public void testGenerateTemporaries() {
//		Scheme l = Scheme.newInstance();
//		
//		l.exec ("(define-syntax myletrec" +
//				"  (lambda (x)" +
//				"    (syntax-case x ()" +
//				"      ((_ ((i e) ...) b1 b2 ...)" +
//				"       (with-syntax" +
//				"           (((t ...)" +
//				"             (generate-temporaries #'(i ...))))" +
//				"         #'(let ((i (if #f #f)) ...)" +
//				"             (let ((t e) ...)" +
//				"               (set! i t) ..." +
//				"               (let () b1 b2 ...))))))))");
//		eqi  (l,"(myletrec ((x 1) (y 2))" +
//				"  (+ x y))", 3);
//	}
	
//	public void testWithSyntax2() {
//		Scheme l = Scheme.newInstance();
//		
//		l.exec ("(define-syntax mycond" +
//				"  (lambda (x)" +
//				"    (syntax-case x ()" +
//				"      [(_ c1 c2 ...)" +
//				"       (let f ([c1 #'c1] [c2* #'(c2 ...)])" +
//				"         (syntax-case c2* ()" +
//				"           [()" +
//				"            (syntax-case c1 (else =>)" +
//				"              [(else e1 e2 ...) #'(begin e1 e2 ...)]" +
//				"              [(e0) #'e0]" +
//				"              [(e0 => e1)" +
//				"               #'(let ([t e0]) (if t (e1 t)))]" +
//				"              [(e0 e1 e2 ...)" +
//				"               #'(if e0 (begin e1 e2 ...))])]" +
//				"           [(c4 c3 ...)" +
//				"            (with-syntax ([rest (f #'c4 #'(c3 ...))])" +
//				"              (syntax-case c1 (=>)" +
//				"                [(e0) #'(let ([t e0]) (if t t rest))]" +
//				"                [(e0 => e1)" +
//				"                 #'(let ([t e0]) (if t (e1 t) rest))]" +
//				"                [(e0 e1 e2 ...)" +
//				"                 #'(if e0 " +
//				"                       (begin e1 e2 ...)" +
//				"                       rest)]))]))])))");
//		
//		eqi(l, "(mycond (else 1))", 1);
//		
//		l.input("(define (t1 x)" +
//				"  (mycond ((null? x) 0)" +
//				"          ((pair? x) 1)" +
//				"          (else 2)))");
//		eqi(l, "(t1 '())", 0);
//		eqi(l, "(t1 '(a b))", 1);
//		eqi(l, "(t1 1)", 2);
//		
//		l.input("(define (t2 x)" +
//				"  (mycond ((null? x) 0)" +
//				"          ((pair? x) 1)))");
//		eqi(l, "(t2 '())", 0);
//		eqi(l, "(t2 '(a b))", 1);
//		eq (l, "(t2 1)", Undef.UNDEF);
//		
//		lperr(l, "(mycond (else 1) ((null? x) 2))");
//		lperr(l, "(mycond (else 2) (else 2))");
//		
//		l.input("(define (t3 x y)" +
//				"  (mycond ((eq? (car x) 'a) (t3 (cdr x) (car y)))" +
//				"          ((eq? (car x) 'd) (t3 (cdr x) (cdr y)))" +
//				"          (else y)))");
//		eqi  (l, "(t3 '(d a z)   '(1 2 3))", 2);
//		eqi  (l, "(t3 '(d d a z) '(1 2 3))", 3);
//		eqi  (l, "(t3 '(d a a z) '(1 (2 3)))", 2);
//		
//		l.input("(define (t4 x)" +
//				"  (mycond (x)" +
//				"          (else 'false)))");
//		eqi  (l, "(t4 2)", 2);
//		eq   (l, "(t4 #f)", sym("false"));
//		
//		l.input("(define (t5 x)" +
//				"  (mycond ((assv x '((1 . -1) (2 . -3))) => cdr)" +
//				"          (else 'false)))");
//		eqi  (l, "(t5 2)", -3);
//		eq   (l, "(t5 #f)", sym("false"));
//	}
	
}
