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

import java.util.logging.Level;
import java.util.logging.Logger;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.util.LispHandler;
import net.morilib.lisp.util.LogEnv;

public class TailRecursionTest extends TCSubr {
	
	private static Logger _log = LogEnv.init("schlush.test.tail");
	
	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		LispHandler.setLoggable2(true);
		LispHandler.setLevel2(Level.FINER);
	}
	
	public void testLen0() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testLen0 start");
		l.exec ("(define len0" +
				"  (letrec" +
				"    ((L  (lambda (l lis)" +
				"           (cond ((null? lis) (Ix 0) (I1 l))" +
				"                 (else (Ix 0)" +
				"                       (L (+ l 1) (cdr lis))))))" +
				"     (Ix (lambda (x) x))" +
				"     (I1 (lambda (x) x)))" +
				"    (lambda (lis)" +
				"      (L 0 lis))))");
		eqi  (l,"(len0 '(1 2 3 4 5))", 5);
		eqi  (l,"(len0 '())", 0);
		_log.info("testLen0 end");
	}
	
	public void testLen1() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testLen1 start");
		l.exec ("(define len1" +
				"  (letrec" +
				"    ((L  (lambda (l lis)" +
				"           (if (null? lis)" +
				"               (I1 l)" +
				"               (L (+ l 1) (cdr lis)))))" +
				"     (I1 (lambda (x) x)))" +
				"    (lambda (lis)" +
				"      (L 0 lis))))");
		eqi  (l,"(len1 '(1 2 3 4 5))", 5);
		eqi  (l,"(len1 '())", 0);
		_log.info("testLen1 end");
	}
	
	public void testLen2() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testLen2 start");
		l.exec ("(define hist '())");
		l.exec ("(define len2" +
				"  (letrec" +
				"    ((L  (lambda (l lis)" +
				"           (cond ((null? lis) l)" +
				"                 (else (set! hist" +
				"                         (cons (car lis) hist))" +
				"                       (L (+ l 1) (cdr lis))))))" +
				"     (Ix (lambda (x) x)))" +
				"    (lambda (lis)" +
				"      (set! hist '())" +
				"      (Ix 0)" +
				"      (L 0 lis))))");
		eqi  (l,"(len2 '(1 2 3 4 5))", 5);
		equal(l,"hist", list(5, 4, 3, 2, 1));
		eqi  (l,"(len2 '())", 0);
		_log.info("testLen2 end");
	}
	
	public void testLen3() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testLen3 start");
		l.exec ("(define hist '())");
		l.exec ("(define len3" +
				"  (letrec" +
				"    ((L  (lambda (l lis)" +
				"           (if (null? lis)" +
				"               l" +
				"               (begin" +
				"                 (set! hist (cons (car lis) hist))" +
				"                 (Ix 0)" +
				"                 (L (+ l 1) (cdr lis))))))" +
				"     (Ix (lambda (x) x)))" +
				"    (lambda (lis)" +
				"      (set! hist '())" +
				"      (L 0 lis))))");
		eqi  (l,"(len3 '(1 2 3 4 5))", 5);
		equal(l,"hist", list(5, 4, 3, 2, 1));
		eqi  (l,"(len3 '())", 0);
		_log.info("testLen3 end");
	}
	
	public void testLen4() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testLen4 start");
		l.exec ("(define hist '())");
		l.exec ("(define len4" +
				"  (letrec" +
				"    ((L  (lambda (l lis)" +
				"           (if (null? lis)" +
				"               l" +
				"               (let ()" +
				"                 (set! hist (cons (car lis) hist))" +
				"                 (Ix 0)" +
				"                 (L (+ l 1) (cdr lis))))))" +
				"     (Ix (lambda (x) x)))" +
				"    (lambda (lis)" +
				"      (set! hist '())" +
				"      (L 0 lis))))");
		eqi  (l,"(len4 '(1 2 3 4 5))", 5);
		equal(l,"hist", list(5, 4, 3, 2, 1));
		eqi  (l,"(len4 '())", 0);
		_log.info("testLen4 end");
	}
	
	public void testLen5() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testLen5 start");
		l.exec ("(define hist '())");
		l.exec ("(define len5" +
				"  (letrec" +
				"    ((L  (lambda (l lis)" +
				"           (if (null? lis)" +
				"               l" +
				"               (letrec ()" +
				"                 (set! hist (cons (car lis) hist))" +
				"                 (Ix 0)" +
				"                 (L (+ l 1) (cdr lis))))))" +
				"     (Ix (lambda (x) x)))" +
				"    (lambda (lis)" +
				"      (set! hist '())" +
				"      (L 0 lis))))");
		eqi  (l,"(len5 '(1 2 3 4 5))", 5);
		equal(l,"hist", list(5, 4, 3, 2, 1));
		eqi  (l,"(len5 '())", 0);
		_log.info("testLen5 end");
	}
	
	public void testDo0() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testDo0 start");
		l.exec ("(define s1 '())");
		l.exec ("(define (1- x)   (- x 1))");
		l.exec ("(define (o* a i) (* a i))");
		l.exec ("(define (re a)   a)");
		l.exec ("(define (Ix x)   x)");
		l.exec ("(define-syntax cn" +
				"  (syntax-rules ()" +
				"    ((_ s i) (set! s (cons i s)))))");
		l.exec ("(define (t1 n)" +
				"  (do ((i n (1- i)) (a 1 (o* a i)))" +
				"      ((zero? i) (Ix 0) (re a))" +
				"    (Ix 0)" +
				"    (cn s1 i)" +
				"    (Ix 0)))");
		eqi(l, "(t1 5)", 120);
		equal(l, "s1", list(1, 2, 3, 4, 5));
		_log.info("testDo0 end");
	}
	
	public void testLetSyntax0() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testLetSyntax0 start");
		l.exec ("(define s1 '())");
		l.exec ("(define (1- x)   (- x 1))");
		l.exec ("(define (Ix x)   x)");
		l.exec ("(define (t1 n)" +
				"  (let-syntax" +
				"    ((cn" +
				"       (syntax-rules ()" +
				"         ((_ s i) (set! s (cons i s)))))" +
				"     (--" +
				"       (syntax-rules ()" +
				"         ((_ x) (1- x)))))" +
				"    (cn s1 n)" +
				"    (Ix 0)" +
				"    (-- n)))");
		eqi(l, "(t1 5)", 4);
		equal(l, "s1", list(5));
		_log.info("testLetSyntax0 end");
	}
	
	public void testLetSyntax1() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testLetSyntax1 start");
		l.exec ("(define s1 '())");
		l.exec ("(define (1- x)   (- x 1))");
		l.exec ("(define (Ix x)   x)");
		l.exec ("(define (t1 n)" +
				"  (letrec-syntax" +
				"    ((cn" +
				"       (syntax-rules ()" +
				"         ((_ s i) (set! s (cons i s)))))" +
				"     (--" +
				"       (syntax-rules ()" +
				"         ((_ x) (1- x)))))" +
				"    (cn s1 n)" +
				"    (Ix 0)" +
				"    (-- n)))");
		eqi(l, "(t1 5)", 4);
		equal(l, "s1", list(5));
		_log.info("testLetSyntax1 end");
	}
	
	public void testEqvAll0() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testEqvAll0 start");
		l.exec ("(define (eqan? x y) (eqv? x y))");
		l.exec ("(define eqv-all0?" +
				"  (lambda (a lis)" +
				"    (or (null? lis)" +
				"        (and (eqan? a (car lis))" +
				"             (eqv-all0? a (cdr lis))))))");
		eq   (l,"(eqv-all0? 1 '(1 1 1 1 1))", T);
		eq   (l,"(eqv-all0? 1 '(1 1 1 1 2))", F);
		eq   (l,"(eqv-all0? 1 '())", T);
		_log.info("testEqvAll0 end");
	}
	
	public void testEqvExist0() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testEqvExist0 start");
		l.exec ("(define (notnull? lis) (not (null? lis)))");
		l.exec ("(define (eqan? x y) (eqv? x y))");
		l.exec ("(define eqv-exist0?" +
				"  (lambda (a lis)" +
				"    (and (notnull? lis)" +
				"         (or (eqan? a (car lis))" +
				"             (eqv-exist0? a (cdr lis))))))");
		eq   (l,"(eqv-exist0? 2 '(1 1 1 1 1))", F);
		eq   (l,"(eqv-exist0? 2 '(1 1 1 1 2))", T);
		eq   (l,"(eqv-exist0? 2 '())", F);
		_log.info("testEqvExist0 end");
	}
	
	public void testNamedLet0() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testNamedLet0 start");
		l.exec ("(define (I1 x) x)");
		l.exec ("(define nl0" +
				"  (lambda (x)" +
				"    (let loop ((l 1) (n x))" +
				"      (if (zero? n)" +
				"          (I1 l)" +
				"          (loop (* l n) (- n 1))))))");
		eqi  (l,"(nl0 5)", 120);
		_log.info("testNamedLet0 end");
	}
	
	public void testNamedLet1() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testNamedLet1 start");
		l.exec ("(define (I1 x) x)");
		l.exec ("(define nl0" +
				"  (lambda (x)" +
				"    (let loop ((l 1) (n x))" +
				"      (let ()" +
				"        (if (zero? n)" +
				"            (I1 l)" +
				"            (loop (* l n) (- n 1)))))))");
		eqi  (l,"(nl0 5)", 120);
		_log.info("testNamedLet1 end");
	}
	
	public void testNamedLet2() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testNamedLet2 start");
		l.exec ("(define (pcn x y)" +
				"  (format #t \"~A~%\" y)" +
				"  (cons x y))");
		l.exec ("(define rember" +
				"  (lambda (a lis)" +
				"    (let loop ((lis lis))" +
				"      (if (null? lis) '()" +
				"          (let ((b (car lis)) (r (cdr lis)))" +
				"            (format #t \"~A ~A ~A \" a b r)" +
				"            (format #t \"~A~%\" (eqv? a b))" +
				"            (if (eqv? a b) (loop r)" +
				"                (pcn b (loop r))))))))");
		equal(l,"(rember 3 '(1 2 3 4 5))", list(1, 2, 4, 5));
		_log.info("testNamedLet2 end");
	}
	
	public void testNamedLet3() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testNamedLet3 start");
		l.exec ("(define (set-cons x y)" +
				"  (if (memv x y) y (cons x y)))");
		l.exec ("(define set-of2" +
				"  (lambda (set1 set2)" +
				"    (let loop1 ((set1 set1))" +
				"      (if (null? set1) '()" +
				"          (let ((x (car set1)))" +
				"            (let loop2 ((set2 set2))" +
				"              (if (null? set2)" +
				"                  (loop1 (cdr set1))" +
				"                  (let ((y (car set2)))" +
				"                    (set-cons" +
				"                      (cons x y)" +
				"                      (loop2 (cdr set2)))))))))))");
		equal(l,"(set-of2 '(1 2) '(3 4))",
				list(cons(1, 3), cons(1, 4), cons(2, 3), cons(2, 4)));
		_log.info("testNamedLet3 end");
	}
	
	public void testNamedLet4() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testNamedLet4 start");
		l.exec ("(define (set-cons x y)" +
				"  (if (memv x y) y (cons x y)))");
		l.exec ("(define (I0 x) x)");
		l.exec ("(define set-of2" +
				"  (lambda (set1 set2)" +
				"    (let loop1 ((set1 set1))" +
				"      (if (null? set1) '()" +
				"          (let ((x (car set1)))" +
				"            (let loop2 ((set2 set2))" +
				"              (if (null? set2)" +
				"                  (I0 (loop1 (cdr set1)))" +
				"                  (let ((y (car set2)))" +
				"                    (set-cons" +
				"                      (cons x y)" +
				"                      (loop2 (cdr set2)))))))))))");
		equal(l,"(set-of2 '(1 2) '(3 4))",
				list(cons(1, 3), cons(1, 4), cons(2, 3), cons(2, 4)));
		_log.info("testNamedLet4 end");
	}
	
	public void testNamedLet5() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testNamedLet5 start");
		l.exec ("(define-syntax set-of" +
				"  (syntax-rules ()" +
				"    ((_ e m ...)" +
				"     (set-of-help e '() m ...))))");
		l.exec ("(define-syntax set-of-help" +
				"  (syntax-rules (in is)" +
				"    ((_ e base)" +
				"     (set-cons e base))" +
				"    ((_ e base (x in s) m ...)" +
				"     (let loop ((set s))" +
				"       (if (null? set)" +
				"           base" +
				"           (let ((x (car set)))" +
				"             (set-of-help e (loop (cdr set)) m ...)))))" +
				"    ((_ e base (x is y) m ...)" +
				"     (let ((x y)) (set-of-help e base m ...)))" +
				"    ((_ e base p m ...)" +
				"     (if p (set-of-help e base m ...) base))))");
		l.exec ("(define (set-cons x y)" +
				"  (if (memv x y) y (cons x y)))");
		equal(l,"(set-of (cons x y)" +
				"  (x in '(1 2 3))" +
				"  (y is (* x x)))",
				list(cons(1, 1), cons(2, 4), cons(3, 9)));
		equal(l,"(set-of (cons x y)" +
				"  (x in '(1 2))" +
				"  (y in '(3 4)))",
				list(cons(1, 3), cons(1, 4), cons(2, 3), cons(2, 4)));
		_log.info("testNamedLet5 end");
	}
	
	public void testNamedLet6() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testNamedLet6 start");
		l.exec ("(define (a x)" +
				"  (let loop ((l 0) (x x))" +
				"    (let ()" +
				"      (let loop2 ()" +
				"        (let ()" +
				"           (if (null? x) l" +
				"               (loop (+ l 1) (cdr x))))))))");
		eqi  (l,"(a '(1 2 3 4 5))", 5);
		_log.info("testNamedLet6 end");
	}
	
	/*public void testNamedLet7() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testNamedLet7 start");
		l.exec ("(define (a x)" +
				"  (let loop ((l 0) (x x))" +
				"    (let ()" +
				"      (let loop2 ()" +
				"        (let ()" +
				"           (if (null? x) l" +
				"               (loop (+ l 1) (cdr x)))))" +
				"      l)))");
		eqi  (l,"(a '(1 2 3 4 5))", 5);
		_log.info("testNamedLet7 end");
	}*/
	
	public void testRember1() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testRember1 start");
		l.exec ("(define (pcn x y)" +
				"  (format #t \"~A~%\" y)" +
				"  (cons x y))");
		l.exec ("(define rember1" +
				"  (lambda (a lis)" +
				"    (if (null? lis) '()" +
				"        (let ((b (car lis)) (r (cdr lis)))" +
				"          (if (eqv? a b) (rember1 a r)" +
				"              (pcn b (rember1 a r)))))))");
		equal(l,"(rember1 3 '(1 2 3 4 5))", list(1, 2, 4, 5));
		_log.info("testRember1 end");
	}
	
	public void testNewForm0() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testNewForm0 start");
		l.exec ("(define (len0 l lis)" +
				"  (if (null? lis) l" +
				"      (len0 (+ l 1) (cdr lis))))");
		eqi  (l,"(len0 0 '(1 2 3 4 5))", 5);
		_log.info("testNewForm0 end");
	}
	
	public void testMutualR0() {
		Scheme l = Scheme.newInstance();
		
		_log.info("testMutualR0 start");
		l.exec ("(define ev?" +
				"  (letrec ((e? (lambda (x)" +
				"                 (if (zero? x) #t (o? (- x 1)))))" +
				"           (o? (lambda (x)" +
				"                 (if (zero? x) #f (e? (- x 1))))))" +
				"    (lambda (x) (e? x))))");
		eq   (l,"(ev? 5)", F);
		eq   (l,"(ev? 4)", T);
		_log.info("testMutualR0 end");
	}
	
}
