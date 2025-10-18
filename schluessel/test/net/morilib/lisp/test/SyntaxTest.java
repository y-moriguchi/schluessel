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
import net.morilib.lisp.Undef;

public class SyntaxTest extends TCSubr {
	
	public void testAnd() {
		Scheme l = Scheme.newInstance();
		
		eqi(l, "(and 1 2)", 2);
		eq (l, "(and #f 3)", F);
		eq (l, "(and 3 #f)", F);
		eq (l, "(and)", T);
		
		lperr(l, "(and 1 . 2)");
		
		l.input("(define (t1 x)");
		l.input(" (if (null? x) #t");
		l.input("     (and (number? (car x)) (t1 (cdr x)))))");
		eq (l, "(t1 '(1 2 'a 4 5))", F);
		eq (l, "(t1 '(1 2 3 4 5))", T);
	}
	
	public void testBegin() {
		Scheme l = Scheme.newInstance();
		
		eqi(l, "(begin 1 2 3)", 3);
		
		lperr(l, "(begin 1 . 2)");
		
		l.input("(define (t1 x)" +
				" (if (pair? x)" +
				"     (begin 1 (t1 (cdr x)))" +
				"     x))");
		eqi(l, "(t1 '(1 2 3 4 . 5))", 5);
		eq (l, "(t1 '(1 2 3))", Nil.NIL);
	}
	
	public void testCase() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define (t1 x)" +
				"  (case x ((0 2 4) 0)" +
				"          ((1 3 5) 1)" +
				"          (((1 2)) -1)" +
				"          (else 2)))");
		eqi(l, "(t1 0)", 0);
		eqi(l, "(t1 1)", 1);
		eqi(l, "(t1 'a)", 2);
		eqi(l, "(t1 '(1 2))", 2);
		
		l.input("(define (t2 x)" +
				"  (case x ((0 2 4) 0)" +
				"          ((1 3 5) 1)))");
		eqi(l, "(t2 0)", 0);
		eqi(l, "(t2 1)", 1);
		eq (l, "(t2 'a)", Undef.UNDEF);
		
		lperr(l, "(case)");
		lperr(l, "(case 1 (1 1))");
		lperr(l, "(case 1 (else 1) ((1 2) 2))");
		lperr(l, "(case 1 (else 1) (else 2))");
		
		l.input("(define (t3 x y)" +
				"  (case (car x) ((a 0) (t3 (cdr x) (car y)))" +
				"                ((d 1) (t3 (cdr x) (cdr y)))" +
				"                (else y)))");
		eqi  (l, "(t3 '(d a z)   '(1 2 3))", 2);
		eqi  (l, "(t3 '(d d a z) '(1 2 3))", 3);
		eqi  (l, "(t3 '(d a a z) '(1 (2 3)))", 2);
	}
	
	public void testCond() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define (t1 x)" +
				"  (cond ((null? x) 0)" +
				"        ((pair? x) 1)" +
				"        (else 2)))");
		eqi(l, "(t1 '())", 0);
		eqi(l, "(t1 '(a b))", 1);
		eqi(l, "(t1 1)", 2);
		
		l.input("(define (t2 x)" +
				"  (cond ((null? x) 0)" +
				"        ((pair? x) 1)))");
		eqi(l, "(t2 '())", 0);
		eqi(l, "(t2 '(a b))", 1);
		eq (l, "(t2 1)", Undef.UNDEF);
		
		lperr(l, "(cond (else 1) ((null? x) 2))");
		lperr(l, "(cond (else 2) (else 2))");
		
		l.input("(define (t3 x y)" +
				"  (cond ((eq? (car x) 'a) (t3 (cdr x) (car y)))" +
				"        ((eq? (car x) 'd) (t3 (cdr x) (cdr y)))" +
				"        (else y)))");
		eqi  (l, "(t3 '(d a z)   '(1 2 3))", 2);
		eqi  (l, "(t3 '(d d a z) '(1 2 3))", 3);
		eqi  (l, "(t3 '(d a a z) '(1 (2 3)))", 2);
		
		l.input("(define (t4 x)" +
				"  (cond (x)" +
				"        (else 'false)))");
		eqi  (l, "(t4 2)", 2);
		eq   (l, "(t4 #f)", sym("false"));
		
		l.input("(define (t5 x)" +
				"  (cond ((assv x '((1 . -1) (2 . -3))) => cdr)" +
				"        (else 'false)))");
		eqi  (l, "(t5 2)", -3);
		eq   (l, "(t5 #f)", sym("false"));
	}
	
	public void testDefine() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define s1 1)");
		eqi(l, "s1", 1);
		
		l.input("(define s2 car)");
		eqi(l, "(s2 '(1 2 3))", 1);
		
		l.input("(define t1 (lambda (x) (* x x)))");
		eqi(l, "(t1 3)", 9);
		
		l.input("(define (t2 x) (+ x x))");
		eqi(l, "(t2 3)", 6);
		
		l.input("(define (t3 . x) (apply + x))");
		eqi(l, "(t3 1 2 3 4)", 10);
		
		l.input("(define (z1 x y) (+ x y))");
		lperr(l, "(define)");
		lperr(l, "(define 1 2)");
		lperr(l, "(define (1 x) x)");
		lperr(l, "(z1 1)");
		lperr(l, "(z1 1 2 3)");
	}
	
	public void testDelay() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define s1 (delay (+ x 2)))");
		l.input("(define x 10)");
		eqi(l, "(force s1)", 12);
		
		l.input("(define x 20)");
		eqi(l, "(force s1)", 12);
		
		lperr(l, "(delay)");
	}
	
	public void testDo() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define s1 '())");
		l.input("(define (t1 n)" +
				"  (do ((i n (- i 1)) (a 1 (* a i)))" +
				"      ((zero? i) a)" +
				"      (set! s1 (cons i s1))))");
		eqi(l, "(t1 5)", 120);
		equal(l, "s1", list(1, 2, 3, 4, 5));
		
		l.input("(define (t2 x)" +
				"  (do ((a x (cdr a)))" +
				"      ((car a))))");
		eqi(l, "(t2 '(#f #f 5 6 #f))", 5);
		
		lperr(l, "(do)");
		lperr(l, "(do ((i 5 (- i 1))))");
		lperr(l, "(do (i 5 (- i 1)) ((zero? i)))");
		lperr(l, "(do ((i 5 (- i 1))) ())");
		lperr(l, "(do ((i 5 (- i 1)) . (j 0 (+ j 1))) ((zero? i)))");
		lperr(l, "(do (i 5 (- i 1)) ((zero? i) . i))");
	}
	
	public void testIf() {
		Scheme l = Scheme.newInstance();
		
		eqi(l, "(if 1 2 3)", 2);
		eqi(l, "(if #f 2 3)", 3);
		eqi(l, "(if 1 2)", 2);
		eq (l, "(if #f #f)", Undef.UNDEF);
		
		lperr(l, "(if)");
		lperr(l, "(if 1)");
		lperr(l, "(if 1 2 3 4)");
		lperr(l, "(if 1 . 2)");
		lperr(l, "(if 1 2 . 3)");
		lperr(l, "(if 1 2 3 . 4)");
		
		l.input("(define (t1 x)");
		l.input(" (if (pair? x) (t1 (cdr x))");
		l.input("     x))");
		eq (l, "(t1 '(1 2 3 4 5))", Nil.NIL);
		eq (l, "(t1 '(1 2 3 4 5 . a))", sym("a"));
	}
	
	public void testLambda() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define a 1)");
		eqi  (l, "((lambda (x y) (* x y)) 2 3)", 6);
		equal(l, "(map (lambda (x) (* x x)) '(1 2 3))", list(1, 4, 9));
		eqi  (l, "((lambda () 5))", 5);
		eqi  (l, "((lambda x (apply + x)) 1 2 3 4)", 10);
		eq   (l, "((lambda ()))", Undef.UNDEF);
		eqi  (l, "((lambda (a) a) 10)", 10);
		equal(l, "((lambda x x) 3 4 5 6)", list(3, 4, 5, 6));
		equal(l, "((lambda (x y . z) z) 3 4 5 6)", list(5, 6));
		
		lperr(l, "(lambda)");
		lperr(l, "(lambda (1 2) (+ 1 2))");
		lperr(l, "((lambda (x y) (+ x y)) 1)");
		lperr(l, "((lambda (x y) (+ x y)) 1 2 3)");
	}
	
	public void testLet() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define a 1)");
		eqi  (l, "(let ((x 1) (y 2)) (+ x y))", 3);
		eqi  (l, "(let () 1)", 1);
		eq   (l, "(let ((x 1)))", Undef.UNDEF);
		eqi  (l, "(let ((a 10)) a)", 10);
		
		lperr(l, "(let)");
		lperr(l, "(let ((x 1) . (y 2)) (+ x y))");
		lperr(l, "(let (x 1) (+ x x))");
		
		eqi  (l,"(let ((x 2) (y 3))" +
				"  (let ((x 7)" +
				"       (z (+ x y)))" +
				"       (* z x)))", 35);
		
		// named let
		eqi  (l,"(let loop ((x 5) (y 1))" +
				//"  (display x) (display \",\")" +
				//"  (display y) (newline)" +
				"  (if (zero? x) y" +
				"      (loop (- x 1) (* y x))))", 120);
		equal(l,"(let loop ((x '(1 2 3)) (y '()))" +
				"  (if (null? x) y" +
				"      (loop (cdr x) (cons (car x) y))))",
				list(3, 2, 1));
		lperr(l,"(let loop)");
	}
	
	public void testLetStar() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define a 1)");
		eqi  (l, "(let* ((x 1) (y 2)) (+ x y))", 3);
		eqi  (l, "(let* () 1)", 1);
		eq   (l, "(let* ((x 1)))", Undef.UNDEF);
		eqi  (l, "(let* ((a 10)) a)", 10);
		eqi  (l, "(let* ((a 1) (b (+ 1 a))) b)", 2);
		
		lperr(l, "(let*)");
		lperr(l, "(let* ((x 1) . (y 2)) (+ x y))");
		lperr(l, "(let* (x 1) (+ x x))");
		
		eqi  (l,"(let ((x 2) (y 3))" +
				"  (let* ((x 7)" +
				"        (z (+ x y)))" +
				"        (* z x)))", 70);
	}
	
	public void testLetrec() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define a 1)");
		eqi  (l, "(letrec ((x 1) (y 2)) (+ x y))", 3);
		eqi  (l, "(letrec () 1)", 1);
		eq   (l, "(letrec ((x 1)))", Undef.UNDEF);
		eqi  (l, "(letrec ((a 10)) a)", 10);
		
		lperr(l, "(letrec)");
		lperr(l, "(letrec ((x 1) . (y 2)) (+ x y))");
		lperr(l, "(letrec (x 1) (+ x x))");
		
		eq   (l,"(letrec ((even?          " +
				"          (lambda (n)    " +
				"            (if (zero? n)" +
				"                #t       " +
				"                (odd? (- n 1)))))" + 
				"         (odd?           " +
				"          (lambda (n)    " +
				"            (if (zero? n)" +
				"                #f       " +
				"                (even? (- n 1))))))" +
				"  (even? 88))", T);
	}
	
	public void testOr() {
		Scheme l = Scheme.newInstance();
		
		eqi(l, "(or 1 2)", 1);
		eqi(l, "(or #f 3)", 3);
		eqi(l, "(or 3 #f)", 3);
		eq (l, "(or #f #f)", F);
		eq (l, "(or)", F);
		
		lperr(l, "(or 1 . 2)");
		
		l.input("(define (t1 x)");
		l.input("  (or (null? x) (t1 (cdr x))))");
		eq (l, "(t1 '(a b c 4 e 6))", T);
	}
	
	public void testQuasiquote() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define exp '(1 2))");
		
		equal(l, "`(a b ,@exp ,exp ,@exp)",
				list(sym("a"), sym("b"), 1, 2, list(1, 2), 1, 2));
		equal(l, "`(e `(exp ,exp ,,exp))",
				list(sym("e"), qq(list(
						sym("exp"), uq(sym("exp")), uq(list(1, 2))))));
		equal(l, "`(e `(exp ,exp ,@,exp))",
				list(sym("e"), qq(list(
						sym("exp"), uq(sym("exp")), uqs(list(1, 2))))));
		equal(l, "`(e `(exp ,exp ,,@exp))",
				list(sym("e"), qq(list(
						sym("exp"), uq(sym("exp")), uq(list(1, 2))))));
		equal(l, "`(exp . ,exp)",
				list(sym("exp"), 1, 2));
		equal(l, "`(exp ,(map (lambda (x) (* x x)) exp))",
				list(sym("exp"), list(1, 4)));
		equal(l, "`(exp ,@exp)",
				list(sym("exp"), 1, 2));
		equal(l, "`',exp", qt(list(1, 2)));
		
		lperr(l, ",exp");
		lperr(l, "`(exp ,,@exp)");
		lperr(l, "`(exp . ,@exp)");
	}
	
	public void testQuote() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "''a", qt(sym("a")));
	}
	
	public void testSetS() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define a 1)");
		eqi(l, "a", 1);
		l.input("(set! a 10)");
		eqi(l, "a", 10);
		
		lperr(l, "(set! 10 20)");
	}
	
}
