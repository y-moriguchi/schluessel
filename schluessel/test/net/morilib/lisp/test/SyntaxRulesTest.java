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
package net.morilib.lisp.test;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.Nil;

public class SyntaxRulesTest extends TCSubr {
	
	public void testRules01() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules (ls) " +
				"  ((_ ls (a b) ...) '(a ... b ...))" +
				"  ((_ a) '(a))" +
				"  ((_ a b c ...) '(c ... a b))))");
		equal(l, "(s1 1)", list(1));
		equal(l, "(s1 1 2 3 4)", list(3, 4, 1, 2));
		equal(l, "(s1 ls (1 2) (3 4))", list(1, 3, 2, 4));
		equal(l, "(s1 1 2)", list(1, 2));
		equal(l, "(s1 (1 2) (3 4))", list(list(1, 2), list(3, 4)));
		equal(l, "(s1 (1 2) 3 (4 5))", list(list(4, 5), list(1, 2), 3));
		equal(l, "(s1 ls)", Nil.NIL);
		equal(l, "(s1 (1 2 3))", list(list(1, 2, 3)));
		lperr(l, "(s1)");
	}
	
	public void testRules02() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ ((a ...) b) ...) '((b a ...) ...))))");
		equal(l, "(s1 ((1 2) 3) ((4 5 6) 7) (() 8))",
				list(list(3, 1, 2), list(7, 4, 5, 6), list(8)));
		equal(l, "(s1 (((1 2) 3) (4 5)))",
				list(list(list(4, 5), list(1, 2), 3)));
		equal(l, "(s1)", Nil.NIL);
		lperr(l, "(s1 (1 2))");
		lperr(l, "(s1 (1) (2 3 4))");
	}
	
	public void testRules03() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules ()" +
				"  ((_ ((a (b c ...) ...) ...)) " +
				"      '(a ... (b ... (c ...) ...) ...))))");
		equal(l, "(s1 ((1 (2 3 4) (5)) (6 (7)) (8)))",
				list(1, 6, 8, list(2, 5, list(3, 4), Nil.NIL),
						list(7, Nil.NIL), Nil.NIL));
		lperr(l, "(s1 ((1 2 3)))");
	}
	
	public void testRules04() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a) a)" +
				"  ((_ a b ...) (list (s1 b ...) a))))");
		equal(l,"(s1 1 2 3 4)",
				list(list(list(4, 3), 2), 1));
	}
	
	public void testRules05() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a) '(1 a))))");
		equal(l,"(let-syntax (" +
				"   (s1 (syntax-rules () " +
				"        ((_ a) '(2 a))))" +
				"   (s2 (syntax-rules () " +
				"        ((_ a) (s1 a)))))" +
				" (s2 0))", list(1, 0));
		equal(l,"(letrec-syntax (" +
				"   (s1 (syntax-rules () " +
				"        ((_ a) '(2 a))))" +
				"   (s2 (syntax-rules () " +
				"        ((_ a) (s1 a)))))" +
				" (s2 0))", list(2, 0));
	}
	
	public void testRules06() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a . b) '(a b))))");
		equal(l,"(s1 1 . 2)", list(1, 2));
	}
	
	public void testRules07() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ #(#(a ...) b) ...) '((b a ...) ...))))");
		equal(l, "(s1 #(#(1 2) 3) #(#(4 5 6) 7) #(#() 8))",
				list(list(3, 1, 2), list(7, 4, 5, 6), list(8)));
		equal(l, "(s1 #(#((1 2) 3) (4 5)))",
				list(list(list(4, 5), list(1, 2), 3)));
		equal(l, "(s1)", Nil.NIL);
		lperr(l, "(s1 #(1 2))");
		lperr(l, "(s1 #(1) #(2 3 4))");
	}
	
	public void testRules90() {
		Scheme l = Scheme.newInstance();
		
		lperr(l,"(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a ...) (a ...) ...)))");
		lperr(l,"(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ (a ...) ...) a)))");
		lperr(l,"(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ (a ...) ...) '(a ...))))");
		lperr(l,"(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ ((a b) ...)) '(a b))))");
		lperr(l,"(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ ((a b) ...)) '(a ... b))))");
		lperr(l,"(define-syntax s1 " +
				" (syntax-rules (ls) " +
				"  ((_ ls ...) '(ls ...))))");
		lperr(l,"(define-syntax s1 " +
				" (syntax-rules ()" +
				"  ((...) '())))");
	}
	
	public void testHygenicLet() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax myor " +
				" (syntax-rules () " +
				"  ((_ a) a)" +
				"  ((_ a b c ...) " +
				"   (let ((t a)) (if t t (myor b c ...))))))");
		equal(l,"(let ((if #f))" +
				" (let ((t 'ok))" +
				"  (myor if t)))", sym("ok"));
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   (let ((mt (let ((mt mt)) (set! mt (+ mt a)) mt))" +
				"         (mu (let ((mu mu)) (set! mu (+ mu a)) mu)))" +
				"    (let ((mv (let ((mv mv))" +
				"               (set! mv (+ mt mu mv))" +
				"               mv)))" +
				"     mv)))))");
		l.input("(define mt 1)");
		l.input("(define mu 1)");
		l.input("(define mv 1)");
		eqi  (l,"(let ((let #f)) (s1 1))", 5);
		eqi  (l,"mt", 1);
		eqi  (l,"mu", 1);
		eqi  (l,"mv", 1);
	}
	
	public void testHygenicLetStar() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax myor " +
				" (syntax-rules () " +
				"  ((_ a) a)" +
				"  ((_ a b c ...) " +
				"   (let* ((t a)) (if t t (myor b c ...))))))");
		equal(l,"(let ((if #f))" +
				" (let ((t 'ok))" +
				"  (myor if t)))", sym("ok"));
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   (let* ((mt (let* ((mt mt))" +
				"               (set! mt (+ mt a)) mt))" +
				"          (mu (let* ((mu mu))" +
				"               (set! mu (+ mu mt)) mu)))" +
				"    (let* ((mv (let* ((mv mv))" +
				"                (set! mv (+ mt mu mv))" +
				"                mv)))" +
				"     mv)))))");
		l.input("(define mt 1)");
		l.input("(define mu 1)");
		l.input("(define mv 1)");
		eqi  (l,"(let* ((let* #f)) (s1 1))", 6);
		eqi  (l,"mt", 1);
		eqi  (l,"mu", 1);
		eqi  (l,"mv", 1);
	}
	
	public void testHygenicLetrec() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax myor " +
				" (syntax-rules () " +
				"  ((_ a) a)" +
				"  ((_ a b c ...) " +
				"   (letrec ((t a)) (if t t (myor b c ...))))))");
		equal(l,"(let ((if #f))" +
				" (let ((t 'ok))" +
				"  (myor if t)))", sym("ok"));
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   (letrec ((mt (let ((mt 1))" +
				"                 (set! mt (+ mt a)) mt))" +
				"            (mu (let ((mu 1))" +
				"                 (set! mu (+ mu a)) mu)))" +
				"    (letrec ((mv (let ((mv 1))" +
				"                  (set! mv (+ mt mu mv))" +
				"                  mv)))" +
				"     mv)))))");
		eqi  (l,"(letrec ((letrec #f)) (s1 1))", 5);
	}
	
	public void testHygenicAnd() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a ...) (and mt a ...))))");
		l.input("(define mt #f)");
		equal(l,"(s1 #t #t #t)", F);
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_) (and " +
				"        (let ((mt mt)) mt) " +
				"        (let ((mt mt)) (not mt))))))");
		l.input("(define mt #t)");
		equal(l,"(let ((and #f)) (s1))", F);
		equal(l,"mt", T);
	}
	
	public void testHygenicBegin() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a) (begin (set! mu a) mt))))");
		l.input("(define mt #f)");
		l.input("(define mu #f)");
		equal(l,"(s1 1)", F);
		eqi  (l,"mu", 1);
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_) (begin (let ((mt mt))" +
				"               (set! mt (+ mt 1)) mt)))))");
		l.input("(define mt 1)");
		eqi  (l,"(let ((begin #f)) (s1))", 2);
		eqi  (l,"mt", 1);
	}
	
	public void testHygenicCase() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   (case a ((0 1 2 3)" +
				"            (let ((mt mt)) (set! mt (+ mt 1)) mt))" +
				"           (else 'ng)))))");
		l.input("(define mt 1)");
		eqi  (l,"(let ((case #f)) (s1 1))", 2);
		eqi  (l,"mt", 1);
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a b) (case a ((b) b)))" +
				"  ((_ a b c ...)" +
				"   (case a ((b) b) (else (s1 a c ...))))))");
		eqi  (l,"(s1 3 1 2 3)", 3);
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_)" +
				"   (case (let ((mt mt)) (set! mt (+ mt 2)) mt)" +
				"    ((1 3)" +
				"     (let ((mv mv))" +
				"      (set! mv (+ mt mu mv))" +
				"      mv))))))");
		l.input("(define mt 1)");
		l.input("(define mu 1)");
		l.input("(define mv 1)");
		eqi (l,"(s1)", 3);
		eqi (l,"mt", 1);
		eqi (l,"mu", 1);
		eqi (l,"mv", 1);
	}
	
	public void testHygenicCond1() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   (cond ((= a 1)" +
				"          (let ((mt mt)) (set! mt (+ mt 1)) mt))" +
				"         (else 'ng)))))");
		l.input("(define mt 1)");
		eqi  (l,"(let ((cond #f)) (s1 1))", 2);
		eqi  (l,"mt", 1);
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a b) (cond ((= a b) b)))" +
				"  ((_ a b c ...)" +
				"   (cond ((= a b) b) (else (s1 a c ...))))))");
		eqi  (l,"(s1 3 1 2 3)", 3);
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_)" +
				"   (cond ((let ((mt mt)) (set! mt (+ mt 1)) (< mt 2))" +
				"          (let ((mu mu)) (set! mu (- mu mt)) mu))" +
				"         ((let ((mt mt)) (set! mt (+ mt 1)) (>= mt 2))" +
				"          (let ((mu mu)) (set! mu (+ mu mt)) mu))))))");
		l.input("(define mt 1)");
		l.input("(define mu 1)");
		eqi (l,"(s1)", 2);
		eqi (l,"mt", 1);
		eqi (l,"mu", 1);
	}
	
	public void testHygenicCond2() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   (cond (a => (lambda (mt) (* mt mt)))" +
				"         (else 'ng)))))");
		l.input("(define mt 1)");
		eqi  (l,"(let ((cond #f)) (s1 3))", 9);
		eqi  (l,"mt", 1);
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ b) (cond (b => not)))" +
				"  ((_ b c ...)" +
				"   (cond (b => not) (else (s1 c ...))))))");
		eqv  (l,"(s1 #f #f #t)", F);
	}
	
	public void testHygenicDelay() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)      " +
				"   (delay (list a " +
				"           (let ((mt 1)) " +
				"            (set! mt (+ mt 3)) mt))))))");
		l.input("(define mt 2)");
		l.input("(define mu #f)");
		l.input("(let ((delay #f)) (set! mu (s1 2)))");
		equal(l,"(force mu)", list(2, 4));
	}
	
	public void testHygenicDo() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a) " +
				"   (do ((mt " +
				"         mt " +
				"         (let ((mt mt)) (set! mt (+ mt 1)) mt)))" +
				"       ((let ((mt (+ mt 1))) (>= mt a))" +
				"        (let ((mt mt)) (set! mt (+ mt 1)) mt))" +
				"     (set! mu mt)))))");
		l.input("(define mt 1)");
		l.input("(define mu #f)");
		eqi  (l,"(let ((do #f)) (s1 10))", 10);
		eqi  (l,"mt", 1);
		eqi  (l,"mu", 8);
	}
	
	public void testHygenicIf() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   (if (let ((mt mt)) (set! mt (+ mt 1)) (= mt a))" +
				"    (let ((mu mu)) (set! mu (+ mu 1)) mu)" +
				"    'ng))))");
		l.input("(define mt 1)");
		l.input("(define mu 1)");
		eqi  (l,"(let ((if #f)) (s1 2))", 2);
		eqi  (l,"mt", 1);
		eqi  (l,"mu", 1);
	}
	
	public void testHygenicLambda() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_) " +
				"   ((lambda (mt) " +
				"     (define mv 0) " +
				"     (define (m+ mt mv) (+ mt mv))" +
				"     (define 1+ (lambda (1+) (+ 1+ 1)))" +
				"     " +
				"     (set! mt (1+ mt))" +
				"     (set! mv (let ((mu mu)) (set! mu (1+ mu)) mu))" +
				"     (let ((mw 0)) (set! mw (m+ mt mv)) mw))" +
				"    mt))))");
		l.input("(define mt 1)");
		l.input("(define mu 1)");
		l.input("(define mv 1)");
		l.input("(define mw 1)");
		eqi  (l,"(let ((lambda #f)) (s1))", 4);
		eqi  (l,"mt", 1);
		eqi  (l,"mu", 1);
		eqi  (l,"mv", 1);
		eqi  (l,"mw", 1);
	}
	
	public void testHygenicOr() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a ...) (or mt a ...))))");
		l.input("(define mt #t)");
		equal(l,"(s1 #f #f #f)", T);
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_) (or " +
				"        (let ((mt mt)) mt) " +
				"        (let ((mt mt)) (not mt))))))");
		l.input("(define mt #f)");
		equal(l,"(let ((or #f)) (s1))", T);
		equal(l,"mt", F);
	}
	
	public void testHygenicQuasiquote() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   `(1 a ,(let ((mt mt)) (set! mt (+ mt a)) mt) 2))))");
		l.input("(define mt 1)");
		equal(l,"(let ((quasiquote #f)) (s1 1))", list(1, 1, 2, 2));
		eqi  (l,"mt", 1);
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   `(1 a ,(let ((mt mt)) (set! mt (+ mt a)) mt)))))");
		l.input("(define mt 1)");
		equal(l,"(let ((quasiquote #f)) (s1 1))", list(1, 1, 2));
		eqi  (l,"mt", 1);
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   `(1 a ,@(let ((mt mt))" +
				"            (set! mt (+ mt a)) (list mt mt)) 2))))");
		l.input("(define mt 1)");
		equal(l,"(let ((quasiquote #f)) (s1 1))", list(1, 1, 2, 2, 2));
		eqi  (l,"mt", 1);
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   `(1 a ,@(let ((mt mt))" +
				"            (set! mt (+ mt a)) (list mt mt))))))");
		l.input("(define mt 1)");
		equal(l,"(let ((quasiquote #f)) (s1 1))", list(1, 1, 2, 2));
		eqi  (l,"mt", 1);
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   `(1 a . ,(let ((mt mt))" +
				"             (set! mt (+ mt a)) (list mt mt))))))");
		l.input("(define mt 1)");
		equal(l,"(let ((quasiquote #f)) (s1 1))", list(1, 1, 2, 2));
		eqi  (l,"mt", 1);
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   `(1 a . ,@(let ((mt mt))" +
				"             (set! mt (+ mt a)) (list mt mt))))))");
		lperr(l,"(s1 1)");
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   `#(1 a ,(let ((mt mt)) (set! mt (+ mt a)) mt) 2))))");
		l.input("(define mt 1)");
		equal(l,"(let ((quasiquote #f)) (s1 1))", vec(1, 1, 2, 2));
		eqi  (l,"mt", 1);
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   `#(1 a ,(let ((mt mt)) (set! mt (+ mt a)) mt)))))");
		l.input("(define mt 1)");
		equal(l,"(let ((quasiquote #f)) (s1 1))", vec(1, 1, 2));
		eqi  (l,"mt", 1);
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   `#(1 a ,@(let ((mt mt))" +
				"            (set! mt (+ mt a)) (list mt mt)) 2))))");
		l.input("(define mt 1)");
		equal(l,"(let ((quasiquote #f)) (s1 1))", vec(1, 1, 2, 2, 2));
		eqi  (l,"mt", 1);
		
		l.input("(define-syntax s1 " +
				" (syntax-rules () " +
				"  ((_ a)" +
				"   `#(1 a ,@(let ((mt mt))" +
				"            (set! mt (+ mt a)) (list mt mt))))))");
		l.input("(define mt 1)");
		equal(l,"(let ((quasiquote #f)) (s1 1))", vec(1, 1, 2, 2));
		eqi  (l,"mt", 1);
	}
	
}
