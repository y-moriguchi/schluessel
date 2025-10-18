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

public class SubrListTest extends TCSubr {
	
	public void testIsNull() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(null? '())", T);
		eq(l, "(null? 'a)", F);
		lperr(l, "(null?)");
	}
	
	public void testIsPair() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(pair? '(a b c))", T);
		eq(l, "(pair? '())", F);
		eq(l, "(pair? 'a)", F);
		lperr(l, "(pair?)");
	}
	
	public void testIsEq() {
		Scheme l = Scheme.newInstance();
		
		l.set("i1", 12345);
		l.setList("l1", sym("a"), sym("b"));
		l.set("s1", "123");
		l.input("(define s2 (string #\\a #\\b))");
		l.input("(define v1 '#(a b))");
		l.input("(define f1 (lambda (x) x))");
		l.input("(define (f2 x) (lambda () (set x (+ x 1)) x))");
		eq(l, "(eq? 'a 3)", F);
		eq(l, "(eq? #t 't)", F);
		eq(l, "(eq? \"aaa\" 'aaa)", F);
		eq(l, "(eq? 1/2 3/2)", F);
		eq(l, "(eq? 2 2.0)", F);
		eq(l, "(eq? 1/2 #i1/2)", F);
		eq(l, "(eq? i1 i1)", T);
		eq(l, "(eq? #t #t)", T);
		eq(l, "(eq? #f #f)", T);
		eq(l, "(eq? #t #f)", F);
		eq(l, "(eq? (cdr '(1)) '())", T);
		eq(l, "(eq? 'a 'a)", T);
		eq(l, "(eq? 'a 'b)", F);
		eq(l, "(eq? '(a) '(b))", F);
		eq(l, "(eq? l1 l1)", T);
		eq(l, "(eq? \"123\" \"321\")", F);
		eq(l, "(eq? s1 s1)", T);
		eq(l, "(eq? s2 s2)", T);
		eq(l, "(eq? (string #\\a) (string #\\a))", F);
		eq(l, "(eq? '#(a) '#(b))", F);
		eq(l, "(eq? v1 v1)", T);
		eq(l, "(eq? (vector 'a) (vector 'a))", F);
		eq(l, "(eq? eq? eq?)", T);
		eq(l, "(eq? eq? eqv?)", F);
		eq(l, "(eq? f1 f1)", T);
		eq(l, "(eq? (f2 1) (f2 1))", F);
		lperr(l, "(eq? 1)");
	}
	
	public void testIsEqv() {
		Scheme l = Scheme.newInstance();
		
		l.set("i1", 12345);
		l.setList("l1", sym("a"), sym("b"));
		l.set("s1", "123");
		l.input("(define s2 (string #\\a #\\b))");
		l.input("(define v1 '#(a b))");
		l.input("(define f1 (lambda (x) x))");
		l.input("(define (f2 x) (lambda () (set x (+ x 1)) x))");
		eq(l, "(eqv? 'a 3)", F);
		eq(l, "(eqv? #t 't)", F);
		eq(l, "(eqv? \"aaa\" 'aaa)", F);
		eq(l, "(eqv? 1/2 3/2)", F);
		eq(l, "(eqv? 2 2.0)", F);
		eq(l, "(eqv? 1/2 #i1/2)", F);
		eq(l, "(eqv? 3/2 3/2)", T);
		eq(l, "(eqv? 1.2 (+ 1.0 .2))", T);
		eq(l, "(eqv? i1 i1)", T);
		eq(l, "(eqv? #t #t)", T);
		eq(l, "(eqv? #f #f)", T);
		eq(l, "(eqv? #t #f)", F);
		eq(l, "(eqv? (cdr '(1)) '())", T);
		eq(l, "(eqv? 'a 'a)", T);
		eq(l, "(eqv? 'a 'b)", F);
		eq(l, "(eqv? '(a) '(b))", F);
		eq(l, "(eqv? l1 l1)", T);
		eq(l, "(eqv? \"123\" \"321\")", F);
		eq(l, "(eqv? s1 s1)", T);
		eq(l, "(eqv? s2 s2)", T);
		eq(l, "(eqv? (string #\\a) (string #\\a))", T);
		eq(l, "(eqv? '#(a) '#(b))", F);
		eq(l, "(eqv? v1 v1)", T);
		eq(l, "(eqv? (vector 'a) (vector 'a))", F);
		eq(l, "(eqv? eq? eq?)", T);
		eq(l, "(eqv? eq? eqv?)", F);
		eq(l, "(eqv? f1 f1)", T);
		eq(l, "(eqv? (f2 1) (f2 1))", F);
		lperr(l, "(eqv? 1)");
	}
	
	public void testIsEqual() {
		Scheme l = Scheme.newInstance();
		
		l.set("i1", 12345);
		l.setList("l1", sym("a"), sym("b"));
		l.set("s1", "123");
		l.input("(define s2 (string #\\a #\\b))");
		l.input("(define v1 '#(a b))");
		l.input("(define f1 (lambda (x) x))");
		l.input("(define (f2 x) (lambda () (set x (+ x 1)) x))");
		eq(l, "(equal? 'a 3)", F);
		eq(l, "(equal? #t 't)", F);
		eq(l, "(equal? \"aaa\" 'aaa)", F);
		eq(l, "(equal? 1/2 3/2)", F);
		eq(l, "(equal? 2 2.0)", F);
		eq(l, "(equal? 1/2 #i1/2)", F);
		eq(l, "(equal? 3/2 3/2)", T);
		eq(l, "(equal? 1.2 (+ 1.0 .2))", T);
		eq(l, "(equal? i1 i1)", T);
		eq(l, "(equal? #t #t)", T);
		eq(l, "(equal? #f #f)", T);
		eq(l, "(equal? #t #f)", F);
		eq(l, "(equal? (cdr '(1)) '())", T);
		eq(l, "(equal? 'a 'a)", T);
		eq(l, "(equal? 'a 'b)", F);
		eq(l, "(equal? '(a) '(b))", F);
		eq(l, "(equal? '(a b . c) '(a b . c))", T);
		eq(l, "(equal? l1 l1)", T);
		eq(l, "(equal? (list 1 2 3) (list 1 2 3))", T);
		eq(l, "(equal? \"123\" \"321\")", F);
		eq(l, "(equal? s1 s1)", T);
		eq(l, "(equal? s2 s2)", T);
		eq(l, "(equal? (string #\\a) (string #\\a))", T);
		eq(l, "(equal? '#(a) '#(b))", F);
		eq(l, "(equal? '#(a) '#(a))", T);
		eq(l, "(equal? v1 v1)", T);
		eq(l, "(equal? (vector 'a) (vector 'a))", T);
		eq(l, "(equal? eq? eq?)", T);
		eq(l, "(equal? eq? equal?)", F);
		eq(l, "(equal? f1 f1)", T);
		eq(l, "(equal? (f2 1) (f2 1))", F);
		lperr(l, "(equal? 1)");
	}
	
	public void testNot() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(not #t)", F);
		eq(l, "(not #f)", T);
		eq(l, "(not '())", F);
		eq(l, "(not '(a b c))", F);
		lperr(l, "(not)");
	}
	
	public void testCxr() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(car '(a . b))", sym("a"));
		eq(l, "(cdr '(a . b))", sym("b"));
		eq(l, "(car '(a))", sym("a"));
		eq(l, "(cdr '(a))", Nil.NIL);
		lperr(l, "(car 'a)");
		lperr(l, "(cdr 'a)");
		lperr(l, "(car)");
		lperr(l, "(cdr)");
	}
	
	public void testCons() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(cons 'a 'b)", cons(sym("a"), sym("b")));
		equal(l, "(cons 'a '())", list(sym("a")));
		lperr(l, "(cons)");
		lperr(l, "(cons 'a)");
	}
	
	public void testSetCxr() {
		Scheme l = Scheme.newInstance();
		
		l.set("x", cons(sym("a"), sym("b")));
		l.input("(set-car! x 'c)");
		l.input("(set-cdr! x 'd)");
		equal(l, "x", cons(sym("c"), sym("d")));
		
		l.set("y", sym("e"));
		lperr(l, "(set-car! y 'c)");
		lperr(l, "(set-cdr! y 'd)");
		lperr(l, "(set-car! y)");
		lperr(l, "(set-cdr! y)");
	}
	
	public void testApply() {
		Scheme l = Scheme.newInstance();
		
		eqv(l, "(apply + '(1 2 3 4))", newZ(10));
		eqv(l, "(apply + '())", newZ(0));
		equal(l, "(apply list '(1 (2 3) 4))",
				list(1, list(2, 3), 4));
		lperr(l, "(apply +)");
	}
	
	public void testIsSymbol() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(symbol? 'a)", T);
		eq(l, "(symbol? 1)", F);
		eq(l, "(symbol? '())", F);
		lperr(l, "(symbol?)");
	}
	
	public void testList() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(list 1 2)", list(1, 2));
		eq(l, "(list)", Nil.NIL);
		lperr(l, "(list 1 2 . 3)");
	}
	
	public void testIsList() {
		Scheme l = Scheme.newInstance();
		
		l.setList("c1", 1, 2, 3);
		l.setList("c2", 4, 5, 6);
		l.input("(set-cdr! (cddr c1) c2)");
		l.input("(set-cdr! (cddr c2) c1)");
		eq(l, "(list? '(1 2))", T);
		eq(l, "(list? 1)", F);
		eq(l, "(list? '())", T);
		eq(l, "(list? c1)", F);
		eq(l, "(list? '(1 2 . 3))", F);
		lperr(l, "(list?)");
	}
	
	public void testLength() {
		Scheme l = Scheme.newInstance();
		
		eqv(l, "(length '(a b c))", newZ(3));
		eqv(l, "(length '())", newZ(0));
		eqv(l, "(length '(a (b c) d))", newZ(3));
		lperr(l, "(length 1)");
		lperr(l, "(length)");
		lperr(l, "(length '(a . b))");
	}
	
	public void testListRef() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(list-ref '(a b c) 0)", sym("a"));
		eq(l, "(list-ref '(a b c) 2)", sym("c"));
		eq(l, "(list-ref '(a . b) 0)", sym("a"));
		lperr(l, "(list-ref 1 1)");
		lperr(l, "(list-ref '(a b c) 'a)");
		lperr(l, "(list-ref '(a))");
		lperr(l, "(list-ref '(a b) 2)");
		lperr(l, "(list-ref '(a b) -1)");
		lperr(l, "(list-ref '() 0)");
		lperr(l, "(list-ref '(a . b) 1)");
	}
	
	public void testListTail() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(list-tail '(1 2 3) 0)", list(1, 2, 3));
		equal(l, "(list-tail '(1 2 3) 1)", list(2, 3));
		equal(l, "(list-tail '(1 2 3) 2)", list(3));
		eq   (l, "(list-tail '(a b c) 3)", Nil.NIL);
		equal(l, "(list-tail '(1 2 . 3) 1)", cons(2, 3));
		eq   (l, "(list-tail '(a b c . d) 3)", sym("d"));
		eq   (l, "(list-tail '() 0)", Nil.NIL);
		lperr(l, "(list-tail 1 1)");
		lperr(l, "(list-tail '(a b c) 'a)");
		lperr(l, "(list-tail '(a))");
		lperr(l, "(list-tail '(a b) 3)");
		lperr(l, "(list-tail '(a b) -1)");
		lperr(l, "(list-tail '() 1)");
		lperr(l, "(list-tail '(a b c . d) 4)");
	}
	
	public void testAppend() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(append '(1 2) '(3 4))", list(1, 2, 3, 4));
		equal(l, "(append '(1 2) '(3) '(4))", list(1, 2, 3, 4));
		equal(l, "(append '(1 2) '(3 . 4))", listDot(4, 1, 2, 3));
		equal(l, "(append '(1 . 2))", cons(1, 2));
		eq   (l, "(append)", Nil.NIL);
		lperr(l, "(append 1 '(2 3))");
		lperr(l, "(append '(1 2) '(3 . 4) '(5))");
	}
	
	public void testReverse() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(reverse '(1 (2 3) 4 5))", list(5, 4, list(2, 3), 1));
		eq   (l, "(reverse '())", Nil.NIL);
		eqv  (l, "(reverse 1)", newZ(1));
		lperr(l, "(reverse)");
		//lperr(l, "(reverse 1)");
		lperr(l, "(reverse '(a b . c))");
	}
	
	public void testMemq() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(memq 'c '(a b c d))", list(sym("c"), sym("d")));
		eq   (l, "(memq 'z '(a b c d))", F);
		equal(l, "(memq 'c '(a b c . d))", cons(sym("c"), sym("d")));
		//eq   (l, "(memq 3 '(1 2 3 4))", F);
		eq   (l, "(memq '(a b . c) '(1 2 (a b . c) c))", F);
		lperr(l, "(memq 'c)");
		lperr(l, "(memq 'c 1)");
	}
	
	public void testMemv() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(memv 'c '(a b c d))", list(sym("c"), sym("d")));
		eq   (l, "(memv 'z '(a b c d))", F);
		equal(l, "(memv 'c '(a b c . d))", cons(sym("c"), sym("d")));
		equal(l, "(memv 3 '(1 2 3 4))", list(3, 4));
		eq   (l, "(memv '(a b . c) '(1 2 (a b . c) c))", F);
		lperr(l, "(memv 'c)");
		lperr(l, "(memv 'c 1)");
	}
	
	public void testMember() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(member 'c '(a b c d))", list(sym("c"), sym("d")));
		eq   (l, "(member 'z '(a b c d))", F);
		equal(l, "(member 'c '(a b c . d))", cons(sym("c"), sym("d")));
		equal(l, "(member 3 '(1 2 3 4))", list(3, 4));
		equal(l, "(member '(3 4 . 5) '(1 2 (3 4 . 5) c))",
				list(listDot(5, 3, 4), sym("c")));
		lperr(l, "(member 'c)");
		lperr(l, "(member 'c 1)");
	}
	
	public void testAssq() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define x '((a . b) (c . d)))");
		l.input("(define y '((1 . b) (2 . d)))");
		l.input("(define z '(((1 2 . 3) . b) ((4 5 . 6) . d)))");
		equal(l, "(assq 'a x)", cons(sym("a"), sym("b")));
//		eq   (l, "(assq 1 y)", F);
		eq   (l, "(assq '(1 2 . 3) z)", F);
		lperr(l, "(assq 'a '(a b c d))");
		lperr(l, "(assq 'c)");
		lperr(l, "(assq 'c 1)");
	}
	
	public void testAssv() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define x '((a . b) (c . d)))");
		l.input("(define y '((1 . b) (2 . d)))");
		l.input("(define z '(((1 2 . 3) . b) ((4 5 . 6) . d)))");
		equal(l, "(assv 'a x)", cons(sym("a"), sym("b")));
		equal(l, "(assv 1 y)", cons(1, sym("b")));
		eq   (l, "(assv '(1 2 . 3) z)", F);
		lperr(l, "(assv 'a '(a b c d))");
		lperr(l, "(assv 'c)");
		lperr(l, "(assv 'c 1)");
	}
	
	public void testAssoc() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define x '((a . b) (c . d)))");
		l.input("(define y '((1 . b) (2 . d)))");
		l.input("(define z '(((1 2 . 3) . b) ((4 5 . 6) . d)))");
		equal(l, "(assoc 'a x)", cons(sym("a"), sym("b")));
		equal(l, "(assoc 1 y)", cons(1, sym("b")));
		equal(l, "(assoc '(1 2 . 3) z)",
				cons(listDot(3, 1, 2), sym("b")));
		lperr(l, "(assoc 'a '(a b c d))");
		lperr(l, "(assoc 'c)");
		lperr(l, "(assoc 'c 1)");
	}
	
}
