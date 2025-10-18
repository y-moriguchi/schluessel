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
package net.morilib.lisp.test.srfi;

import net.morilib.lisp.Nil;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

public class SRFI01Test extends TCSubr {

	public void testXcons() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(xcons 1 2)", cons(2, 1));
		equal(l,"(xcons '() 2)", list(2));
		lperr(l,"(xcons 1)");
		lperr(l,"(xcons 1 2 3)");
	}

	public void testConsStar() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(cons* 1 2)", cons(1, 2));
		equal(l,"(cons* 1 2 3 4)", listDot(4, 1, 2, 3));
		equal(l,"(cons* 1 2 3 '())", list(1, 2, 3));
		equal(l,"(cons* 1)", newZ(1));
		equal(l,"(cons*)", Nil.NIL);
	}

	public void testMakeList() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(make-list 4 1)", list(1, 1, 1, 1));
		equal(l,"(make-list 0 1)", Nil.NIL);
		equal(l,"(make-list 4)", list(F, F, F, F));
		lperr(l,"(make-list 'a 1)");
		lperr(l,"(make-list 3.0 1)");
		lperr(l,"(make-list -3 1)");
		lperr(l,"(make-list 4 1 2)");
	}

	public void testListTabulate() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(list-tabulate 3 (lambda (x) (* x x)))", list(1, 4, 9));
		equal(l,"(list-tabulate 0 (lambda (x) (* x x)))", Nil.NIL);
		lperr(l,"(list-tabulate 'a *)");
		lperr(l,"(list-tabulate 3.0 *)");
		lperr(l,"(list-tabulate -3 *)");
		lperr(l,"(list-tabulate 4 1)");
		lperr(l,"(list-tabulate 4)");
		lperr(l,"(list-tabulate 4 1 2)");
	}

	public void testListCopy() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		l.exec ("(define x '(1 2 3 4))");
		l.exec ("(define y (list-copy x))");
		equal(l,"y", list(1, 2, 3, 4));
		equal(l,"(eq? x y)", F);
		equal(l,"(list-copy 'a)", sym("a"));
		lperr(l,"(list-copy)");
		lperr(l,"(list-copy x y)");
	}

	public void testCircularList() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		l.exec ("(define x (circular-list 1))");
		l.exec ("(define y (circular-list 1 2))");
		equal(l,"(car y)", newZ(1));
		equal(l,"(cadr y)", newZ(2));
		equal(l,"(caddr y)", newZ(1));
		equal(l,"(eq? (cddr y) y)", T);
		equal(l,"(car x)", newZ(1));
		equal(l,"(cadr x)", newZ(1));
		equal(l,"(eq? (cdr x) x)", T);
		equal(l,"(circular-list)", Nil.NIL);
	}

	public void testIota() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		l.exec ("(define x '(1 2))");
		equal(l,"(iota 4)", list(0, 1, 2, 3));
		equal(l,"(iota 4 1.5)", list(1.5, 2.5, 3.5, 4.5));
		equal(l,"(iota 3 7/2 -1/2)", list(newQ(7, 2), 3, newQ(5, 2)));
		equal(l,"(iota 0)", Nil.NIL);
		equal(l,"(iota 0 1.5)", Nil.NIL);
		equal(l,"(iota 0 1.5 -0.1)", Nil.NIL);
		lperr(l,"(iota 'a)");
		lperr(l,"(iota 3.0)");
		lperr(l,"(iota -3)");
		lperr(l,"(iota 4 'a)");
		lperr(l,"(iota 4 1 'a)");
		lperr(l,"(iota)");
		lperr(l,"(iota 4 1 2 3)");
	}

	public void testIsProperList() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(proper-list? '(1 2 3))", T);
		equal(l,"(proper-list? '())", T);
		equal(l,"(proper-list? '(1 2 3 . 4))", F);
		equal(l,"(proper-list? (circular-list 1 2 3))", F);
		equal(l,"(proper-list? 1)", F);
		lperr(l,"(proper-list?)");
		lperr(l,"(proper-list? '(1 2 3) '(2 3))");
	}

	public void testIsCircularList() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(circular-list? '(1 2 3))", F);
		equal(l,"(circular-list? '())", F);
		equal(l,"(circular-list? '(1 2 3 . 4))", F);
		equal(l,"(circular-list? (circular-list 1 2 3))", T);
		equal(l,"(circular-list? 1)", F);
		lperr(l,"(circular-list?)");
		lperr(l,"(circular-list? '(1 2 3) '(2 3))");
	}

	public void testIsDottedList() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(dotted-list? '(1 2 3))", F);
		equal(l,"(dotted-list? '())", F);
		equal(l,"(dotted-list? '(1 2 3 . 4))", T);
		equal(l,"(dotted-list? (circular-list 1 2 3))", F);
		equal(l,"(dotted-list? 1)", T);
		lperr(l,"(dotted-list?)");
		lperr(l,"(dotted-list? '(1 2 3) '(2 3))");
	}

	public void testIsNullList() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(null-list? '())", T);
		equal(l,"(null-list? '(1 2))", F);
		lperr(l,"(null-list? 1)");
		lperr(l,"(null-list?)");
		lperr(l,"(null-list? '(1 2 3) '(2 3))");
	}

	public void testIsNotPair() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(not-pair? '())", T);
		equal(l,"(not-pair? '(1 2))", F);
		equal(l,"(not-pair? 1)", T);
		lperr(l,"(not-pair?)");
		lperr(l,"(not-pair? '(1 2 3) '(2 3))");
	}

	public void testListEqual() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(list= eqv? '(1 2 3) '(1 2 3) '(1 2 3))", T);
		equal(l,"(list= eqv? '(1 2 3) '(1 2 3) '(1 2))", F);
		equal(l,"(list= eqv? '(1 2 3) '(1 3) '(1 2 3))", F);
		equal(l,"(list= eqv? '(1 2 3) '(1 2 3) '(1 2 4))", F);
		equal(l,"(list= eqv? '(1 2 3) '(1 3 3) '(1 2 3))", F);
		equal(l,"(list= eqv? '(1 2 3) '() '(1 2 3))", F);
		equal(l,"(list= eqv? '(1 2 3) '(1 2 3))", T);
		equal(l,"(list= eqv? '(1 2 3))", T);
		equal(l,"(list= eqv?)", T);
		equal(l,"(list= eqv? '() '() '())", T);
		lperr(l,"(list= 1)");
		lperr(l,"(list= 1 '())");
		lperr(l,"(list= 1 '() '())");
	}

	public void testNth() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(first   '(1 2 3 4 5 6 7 8 9 0 a b c))", newZ(1));
		equal(l,"(second  '(1 2 3 4 5 6 7 8 9 0 a b c))", newZ(2));
		equal(l,"(third   '(1 2 3 4 5 6 7 8 9 0 a b c))", newZ(3));
		equal(l,"(fourth  '(1 2 3 4 5 6 7 8 9 0 a b c))", newZ(4));
		equal(l,"(fifth   '(1 2 3 4 5 6 7 8 9 0 a b c))", newZ(5));
		equal(l,"(sixth   '(1 2 3 4 5 6 7 8 9 0 a b c))", newZ(6));
		equal(l,"(seventh '(1 2 3 4 5 6 7 8 9 0 a b c))", newZ(7));
		equal(l,"(eighth  '(1 2 3 4 5 6 7 8 9 0 a b c))", newZ(8));
		equal(l,"(ninth   '(1 2 3 4 5 6 7 8 9 0 a b c))", newZ(9));
		equal(l,"(tenth   '(1 2 3 4 5 6 7 8 9 0 a b c))", newZ(0));
	}

	public void testCarCdr() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(car+cdr '(1 2))", newZ(1), list(2));
	}

	public void testTakeDrop() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(take '(1 2 3 4 5) 2)", list(1, 2));
		equal(l,"(drop '(1 2 3 4 5) 2)", list(3, 4, 5));
		equal(l,"(take '(1 2 3 . d) 2)", list(1, 2));
		equal(l,"(drop '(1 2 3 . d) 2)", listDot(sym("d"), 3));
		equal(l,"(take '(1 2 3 . d) 3)", list(1, 2, 3));
		equal(l,"(drop '(1 2 3 . d) 3)", sym("d"));
		equal(l,"(take '(1 2 3 4 5) 0)", Nil.NIL);
		equal(l,"(drop '(1 2 3 4 5) 0)", list(1, 2, 3, 4, 5));
		equal(l,"(take '(1 2 3 4 5) 5)", list(1, 2, 3, 4, 5));
		equal(l,"(drop '(1 2 3 4 5) 5)", Nil.NIL);
		equal(l,"(take '() 0)", Nil.NIL);
		equal(l,"(drop '() 0)", Nil.NIL);
		equal(l,"(take 'a 0)", Nil.NIL);
		equal(l,"(drop 'a 0)", sym("a"));
		lperr(l,"(take '(1 2 3 4 5) 6)");
		lperr(l,"(drop '(1 2 3 4 5) 6)");
		lperr(l,"(take '(1 2 3 4 5) -1)");
		lperr(l,"(drop '(1 2 3 4 5) -1)");
		lperr(l,"(take '(1 2 3 4 5) 1.0)");
		lperr(l,"(drop '(1 2 3 4 5) 1.0)");
		lperr(l,"(take '(1 2 3 4 5) 'a)");
		lperr(l,"(drop '(1 2 3 4 5) 'a)");
		lperr(l,"(take '() 1)");
		lperr(l,"(drop '() 1)");
		lperr(l,"(take 'a 1)");
		lperr(l,"(drop 'a 1)");
		l.exec ("(define x (circular-list 1 2))");
		equal(l,"(take x 4)", list(1, 2, 1, 2));
		equal(l,"(eq? (drop x 4) x)", T);

		l.exec ("(define x '(1 2 3 4 5))");
		equax(l,"(take! x 2)", list(1, 2));
		l.exec ("(define x '(1 2 3 . d))");
		equax(l,"(take! x 2)", list(1, 2));
		l.exec ("(define x '(1 2 3 4 5))");
		equal(l,"(take! x 0)", Nil.NIL);
		l.exec ("(define x '(1 2 3 4 5))");
		equax(l,"(take! x 5)", list(1, 2, 3, 4, 5));
		//l.exec ("(define x (circular-list 1 2))");
		//equax(l,"(take! x 4)", list(1, 2, 1, 2));
		lperr(l,"(take! '(1 2 3 4 5) 6)");
		lperr(l,"(take! '(1 2 3 4 5) -1)");
		lperr(l,"(take! '(1 2 3 4 5) 1.0)");
		lperr(l,"(take! '(1 2 3 4 5) 'a)");
		lperr(l,"(take! '() 1)");
		lperr(l,"(take! 'a 1)");
	}

	public void testTakeDropRight() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(take-right '(1 2 3 4 5) 2)", list(4, 5));
		equal(l,"(drop-right '(1 2 3 4 5) 2)", list(1, 2, 3));
		equal(l,"(take-right '(1 2 3 . d) 2)", listDot(sym("d"), 2, 3));
		equal(l,"(drop-right '(1 2 3 . d) 2)", list(1));
		equal(l,"(take-right '(1 2 3 . d) 0)", sym("d"));
		equal(l,"(drop-right '(1 2 3 . d) 0)", list(1, 2, 3));
		equal(l,"(take-right '(1 2 3 4 5) 0)", Nil.NIL);
		equal(l,"(drop-right '(1 2 3 4 5) 0)", list(1, 2, 3, 4, 5));
		equal(l,"(take-right '(1 2 3 4 5) 5)", list(1, 2, 3, 4, 5));
		equal(l,"(drop-right '(1 2 3 4 5) 5)", Nil.NIL);
		equal(l,"(take-right '() 0)", Nil.NIL);
		equal(l,"(drop-right '() 0)", Nil.NIL);
		equal(l,"(take-right 'a 0)", sym("a"));
		equal(l,"(drop-right 'a 0)", Nil.NIL);
		lperr(l,"(take-right '(1 2 3 4 5) 6)");
		lperr(l,"(drop-right '(1 2 3 4 5) 6)");
		lperr(l,"(take-right '(1 2 3 4 5) -1)");
		lperr(l,"(drop-right '(1 2 3 4 5) -1)");
		lperr(l,"(take-right '(1 2 3 4 5) 1.0)");
		lperr(l,"(drop-right '(1 2 3 4 5) 1.0)");
		lperr(l,"(take-right '(1 2 3 4 5) 'a)");
		lperr(l,"(drop-right '(1 2 3 4 5) 'a)");
		lperr(l,"(take-right '() 1)");
		lperr(l,"(drop-right '() 1)");
		lperr(l,"(take-right 'a 1)");
		lperr(l,"(drop-right 'a 1)");

		l.exec ("(define x '(1 2 3 4 5))");
		equax(l,"(drop-right! x 2)", list(1, 2, 3));
		l.exec ("(define x '(1 2 3 . d))");
		equax(l,"(drop-right! x 2)", list(1));
		l.exec ("(define x '(1 2 3 4 5))");
		equal(l,"(drop-right! '(1 2 3 . d) 0)", list(1, 2, 3));
		l.exec ("(define x '(1 2 3 4 5))");
		equal(l,"(drop-right! '(1 2 3 4 5) 0)", list(1, 2, 3, 4, 5));
		equal(l,"(drop-right! '(1 2 3 4 5) 5)", Nil.NIL);
		equal(l,"(drop-right! '() 0)", Nil.NIL);
		equal(l,"(drop-right! 'a 0)", Nil.NIL);
		lperr(l,"(drop-right! '(1 2 3 4 5) 6)");
		lperr(l,"(drop-right! '(1 2 3 4 5) -1)");
		lperr(l,"(drop-right! '(1 2 3 4 5) 1.0)");
		lperr(l,"(drop-right! '(1 2 3 4 5) 'a)");
		lperr(l,"(drop-right! '() 1)");
		lperr(l,"(drop-right! 'a 1)");
	}

	public void testSplitAt() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(split-at '(1 2 3 4 5) 2)",
				list(1, 2), list(3, 4, 5));
		equal(l,"(split-at '(1 2 3 . d) 2)",
				list(1, 2), listDot(sym("d"), 3));
		equal(l,"(split-at '(1 2 3 . d) 3)", list(1, 2, 3), sym("d"));
		equal(l,"(split-at '(1 2 3 4 5) 0)",
				Nil.NIL, list(1, 2, 3, 4, 5));
		equal(l,"(split-at '(1 2 3 4 5) 5)",
				list(1, 2, 3, 4, 5), Nil.NIL);
		equal(l,"(split-at '() 0)", Nil.NIL, Nil.NIL);
		equal(l,"(split-at 'a 0)", Nil.NIL, sym("a"));
		lperr(l,"(split-at '(1 2 3 4 5) 6)");
		lperr(l,"(split-at '(1 2 3 4 5) -1)");
		lperr(l,"(split-at '(1 2 3 4 5) 1.0)");
		lperr(l,"(split-at '(1 2 3 4 5) 'a)");
		lperr(l,"(split-at '() 1)");
		lperr(l,"(split-at 'a 1)");
		l.exec ("(define x (circular-list 1 2))");
		l.exec ("(define a #f)");
		l.exec ("(define b #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (split-at x 4))" +
				"  (lambda (x y) (set! a x) (set! b y)))");
		equal(l,"a", list(1, 2, 1, 2));
		equal(l,"(eq? b x)", T);

		equal(l,"(split-at! '(1 2 3 4 5) 2)",
				list(1, 2), list(3, 4, 5));
		equal(l,"(split-at! '(1 2 3 . d) 2)",
				list(1, 2), listDot(sym("d"), 3));
		equal(l,"(split-at! '(1 2 3 . d) 3)", list(1, 2, 3), sym("d"));
		equal(l,"(split-at! '(1 2 3 4 5) 0)",
				Nil.NIL, list(1, 2, 3, 4, 5));
		equal(l,"(split-at! '(1 2 3 4 5) 5)",
				list(1, 2, 3, 4, 5), Nil.NIL);
		equal(l,"(split-at! '() 0)", Nil.NIL, Nil.NIL);
		equal(l,"(split-at! 'a 0)", Nil.NIL, sym("a"));
		lperr(l,"(split-at! '(1 2 3 4 5) 6)");
		lperr(l,"(split-at! '(1 2 3 4 5) -1)");
		lperr(l,"(split-at! '(1 2 3 4 5) 1.0)");
		lperr(l,"(split-at! '(1 2 3 4 5) 'a)");
		lperr(l,"(split-at! '() 1)");
		lperr(l,"(split-at! 'a 1)");
		//l.exec ("(define x (circular-list 1 2))");
		//l.exec ("(define a #f)");
		//l.exec ("(define b #f)");
		//l.exec ("(call-with-values" +
		//		"  (split-at! x 4)" +
		//		"  (lambda (x y) (set! a x) (set! b x)))");
		//equal(l,"a", list(1, 2, 1, 2));
		//equal(l,"(eq? b x)", T);
	}

	public void testLast() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(last '(1 2 3))", newZ(3));
		equal(l,"(last '(1 2 3 . 4))", newZ(3));
		equal(l,"(last '(1))", newZ(1));
		lperr(l,"(last '())");
		lperr(l,"(last 1)");
		equal(l,"(last-pair '(1 2 3))", list(3));
		equal(l,"(last-pair '(1 2 3 . 4))", list(3));
		equal(l,"(last-pair '(1))", list(1));
		lperr(l,"(last-pair '())");
		lperr(l,"(last-pair 1)");
	}

	public void testLength() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(length+ '(1 2 3 4))", newZ(4));
		equal(l,"(length+ '())", newZ(0));
		equal(l,"(length+ (circular-list 1 2))", F);
		lperr(l,"(length+ '(1 2 3 . 4))");
		lperr(l,"(length+ 1)");
	}

	public void testAppend() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		l.exec ("(define x '(1 2))");
		equax(l,"(append! x '(3 4))", list(1, 2, 3, 4));
		l.exec ("(define x '(1 2))");
		equax(l,"(append! x '(3 4 . 5))", listDot(5, 1, 2, 3, 4));
		l.exec ("(define x '(1 2))");
		equax(l,"(append! x '(3 4) '(5 6))", list(1, 2, 3, 4, 5, 6));
		l.exec ("(define x '(1 2))");
		equax(l,"(append! x '(3 4) 5)", listDot(5, 1, 2, 3, 4));
		l.exec ("(define x '(1 2))");
		equax(l,"(append! x '(3 4) '() 5)", listDot(5, 1, 2, 3, 4));
		l.exec ("(define x '(1 2))");
		equax(l,"(append! x '() '(3 4) '())", list(1, 2, 3, 4));
		l.exec ("(define x '(1 2))");
		equax(l,"(append! x '() '())", list(1, 2));
		l.exec ("(define x '(1 2))");
		equax(l,"(append! '() x '(3 4) '())", list(1, 2, 3, 4));
		l.exec ("(define x '(1 2))");
		equax(l,"(append! x)", list(1, 2));
		equal(l,"(append! 1)", newZ(1));
		equal(l,"(append! '())", Nil.NIL);
		equal(l,"(append!)", Nil.NIL);
		lperr(l,"(append! '(1 2) '(3 4 . 5) '(6 7))");
		lperr(l,"(append! '(1 2) '(3 4 . 5) '())");
		lperr(l,"(append! '(1 2) 3 '(6 7))");
		lperr(l,"(append! '(1 2) 3 '())");
		lperr(l,"(append! 1 2)");
	}

	public void testConcatenate() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(concatenate (list '(1 2) '(3 4)))", list(1, 2, 3, 4));
		equal(l,"(concatenate (list '(1 2) '(3 4 . 5)))",
				listDot(5, 1, 2, 3, 4));
		equal(l,"(concatenate (list '(1 2) '(3 4) '(5 6)))",
				list(1, 2, 3, 4, 5, 6));
		equal(l,"(concatenate (list '(1 2) '(3 4) 5))",
				listDot(5, 1, 2, 3, 4));
		equal(l,"(concatenate (list '(1 2) '(3 4) '() 5))",
				listDot(5, 1, 2, 3, 4));
		equal(l,"(concatenate (list '(1 2) '() '(3 4) '()))",
				list(1, 2, 3, 4));
		equal(l,"(concatenate (list '(1 2) '() '()))", list(1, 2));
		equal(l,"(concatenate (list '() '(1 2) '(3 4) '()))",
				list(1, 2, 3, 4));
		equal(l,"(concatenate (list '(1 2)))", list(1, 2));
		equal(l,"(concatenate (list 1))", newZ(1));
		equal(l,"(concatenate (list '()))", Nil.NIL);
		equal(l,"(concatenate (list))", Nil.NIL);
		lperr(l,"(concatenate (list '(1 2) '(3 4 . 5) '(6 7)))");
		lperr(l,"(concatenate (list '(1 2) '(3 4 . 5) '()))");
		lperr(l,"(concatenate (list '(1 2) 3 '(6 7)))");
		lperr(l,"(concatenate (list '(1 2) 3 '()))");
		lperr(l,"(concatenate (list 1 2))");

		l.exec ("(define x '(1 2))");
		equax(l,"(concatenate! (list x '(3 4)))", list(1, 2, 3, 4));
		l.exec ("(define x '(1 2))");
		equax(l,"(concatenate! (list x '(3 4 . 5)))",
				listDot(5, 1, 2, 3, 4));
		l.exec ("(define x '(1 2))");
		equax(l,"(concatenate! (list x '(3 4) '(5 6)))",
				list(1, 2, 3, 4, 5, 6));
		l.exec ("(define x '(1 2))");
		equax(l,"(concatenate! (list x '(3 4) 5))",
				listDot(5, 1, 2, 3, 4));
		l.exec ("(define x '(1 2))");
		equax(l,"(concatenate! (list x '(3 4) '() 5))",
				listDot(5, 1, 2, 3, 4));
		l.exec ("(define x '(1 2))");
		equax(l,"(concatenate! (list x '() '(3 4) '()))",
				list(1, 2, 3, 4));
		l.exec ("(define x '(1 2))");
		equax(l,"(concatenate! (list x '() '()))", list(1, 2));
		l.exec ("(define x '(1 2))");
		equax(l,"(concatenate! (list '() x '(3 4) '()))",
				list(1, 2, 3, 4));
		l.exec ("(define x '(1 2))");
		equax(l,"(concatenate! (list x))", list(1, 2));
		equal(l,"(concatenate! (list 1))", newZ(1));
		equal(l,"(concatenate! (list '()))", Nil.NIL);
		equal(l,"(concatenate! (list))", Nil.NIL);
		lperr(l,"(concatenate! (list '(1 2) '(3 4 . 5) '(6 7)))");
		lperr(l,"(concatenate! (list '(1 2) '(3 4 . 5) '()))");
		lperr(l,"(concatenate! (list '(1 2) 3 '(6 7)))");
		lperr(l,"(concatenate! (list '(1 2) 3 '()))");
		lperr(l,"(concatenate! (list 1 2))");
	}

	public void testReverse() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(reverse! '(1 2 3))", list(3, 2, 1));
		equal(l,"(reverse! '())", Nil.NIL);
		lperr(l,"(reverse! 1)");
		lperr(l,"(reverse! '(1 2 . 3))");
	}

	public void testAppendReverse() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(append-reverse '(0 1) '(2 3))", list(1, 0, 2, 3));
		equal(l,"(append-reverse '(0 1) '(2 . 3))",
				listDot(3, 1, 0, 2));
		equal(l,"(append-reverse '(0 1) 2)", listDot(2, 1, 0));
		equal(l,"(append-reverse '(0 1) '())", list(1, 0));
		equal(l,"(append-reverse '() '(2 3))", list(2, 3));
		equal(l,"(append-reverse '() '(2 . 3))", cons(2, 3));
		equal(l,"(append-reverse '() 2)", newZ(2));
		equal(l,"(append-reverse '() '())", Nil.NIL);
		lperr(l,"(append-reverse 1 '(2 3))");
		lperr(l,"(append-reverse '(1 2 . 3) '(4 5))");

		equal(l,"(append-reverse! '(0 1) '(2 3))", list(1, 0, 2, 3));
		equal(l,"(append-reverse! '(0 1) '(2 . 3))",
				listDot(3, 1, 0, 2));
		equal(l,"(append-reverse! '(0 1) 2)", listDot(2, 1, 0));
		equal(l,"(append-reverse! '(0 1) '())", list(1, 0));
		equal(l,"(append-reverse! '() '(2 3))", list(2, 3));
		equal(l,"(append-reverse! '() '(2 . 3))", cons(2, 3));
		equal(l,"(append-reverse! '() 2)", newZ(2));
		equal(l,"(append-reverse! '() '())", Nil.NIL);
		lperr(l,"(append-reverse! 1 '(2 3))");
		lperr(l,"(append-reverse! '(1 2 . 3) '(4 5))");
	}

	public void testZip() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(zip '(1 2 3) '(4 5) '(6 7 8 9))",
				list(list(1, 4, 6), list(2, 5, 7)));
		equal(l,"(zip '(1 2 3))", list(list(1), list(2), list(3)));
		equal(l,"(zip '(1 2 3) (circular-list #t #f))",
				list(list(1, T), list(2, F), list(3, T)));
		lperr(l,"(zip)");
		lperr(l,"(zip '(1 2) 3)");
	}

	public void testUnzip() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(unzip1 '((1 2) (3) (4 5 6)))", list(1, 3, 4));
		equal(l,"(unzip1 '())", Nil.NIL);
		lperr(l,"(unzip1 '((1 2) () (4 5 6)))");

		equal(l,"(unzip2 '((1 2) (3 4) (5 6 7 8)))",
				list(1, 3, 5), list(2, 4, 6));
		equal(l,"(unzip2 '())", Nil.NIL, Nil.NIL);
		lperr(l,"(unzip2 '((1 2) (3) (4 5 6)))");

		equal(l,"(unzip3 '((1 2 3 4) (5 6 7) (8 9 10 11 12)))",
				list(1, 5, 8), list(2, 6, 9), list(3, 7, 10));
		equal(l,"(unzip3 '())", Nil.NIL, Nil.NIL, Nil.NIL);
		lperr(l,"(unzip3 '((1 2 3) (4 5) (6 7 8)))");

		equal(l,"(unzip4 '((1 2 3 4 5) (6 7 8 9) (10 11 12 13 14 15)))",
				list(1, 6, 10), list(2, 7, 11), list(3, 8, 12),
				list(4, 9, 13));
		equal(l,"(unzip4 '())", Nil.NIL, Nil.NIL, Nil.NIL, Nil.NIL);
		lperr(l,"(unzip4 '((1 2 3 4) (5 6 7) (8 9 10 11 12)))");

		equal(l,"(unzip5 " +
				" '((1 2 3 4 5 a) (6 7 8 9 10) (11 12 13 14 15 a b)))",
				list(1, 6, 11), list(2, 7, 12), list(3, 8, 13),
				list(4, 9, 14), list(5, 10, 15));
		equal(l,"(unzip5 '())",
				Nil.NIL, Nil.NIL, Nil.NIL, Nil.NIL, Nil.NIL);
		lperr(l,"(unzip5 " +
				" '((1 2 3 4 5) (6 7 8 9) (10 11 12 13 14 15)))");
	}

	public void testCount() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		eqi  (l,"(count even? '(3 1 4 1 5 9 2 5 6))", 3);
		eqi  (l,"(count < '(1 2 4 8) '(2 4 6 8 10 12 14 16))", 3);
		eqi  (l,"(count < '(3 1 4 1) (circular-list 1 10))", 2);
		eqi  (l,"(count = '(1 2 3 4) '())", 0);
		eqi  (l,"(count even? '())", 0);
		lperr(l,"(count 1 '(1 2 3 4))");
		lperr(l,"(count even? '(1 2 3 . 4))");
		lperr(l,"(count even? 1)");
		lperr(l,"(count = '(1 2 3 4 5) '(1 2 3 . 4))");
		lperr(l,"(count = '(1 2 3) 1)");
	}

	public void testFold() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(fold + 0 '(1 2 3 4 5))", newZ(15));
		equal(l,"(fold cons '() '(1 2 3))", list(3, 2, 1));
		equal(l,"(fold cons '(1 2) '(3 4 5))", list(5, 4, 3, 1, 2));
		equal(l,"(fold (lambda (x count)" +
				"        (if (symbol? x) (+ count 1) count))" +
				"      0" +
				"      '(1 2 3 a b 4 c d))", newZ(4));
		equal(l,"(fold (lambda (s max-len)" +
				"        (max max-len (string-length s)))" +
				"      0" +
				"      '(\"aaa\" \"bbbbbb\" \"c\"))", newZ(6));
		equal(l,"(fold cons* '() '(a b c) '(1 2 3 4 5))",
				list(sym("c"), 3, sym("b"), 2, sym("a"), 1));

		equal(l,"(fold + 0 '())", newZ(0));
		equal(l,"(fold cons* '() '(a b c) (circular-list 1 2))",
				list(sym("c"), 1, sym("b"), 2, sym("a"), 1));
		lperr(l,"(fold 1 0 '(1 2 3 4 5))");
		lperr(l,"(fold + 0 '(1 2 3 4 . 5))");
		lperr(l,"(fold + 0 1)");
	}

	public void testFoldRight() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(fold-right + 0 '(1 2 3 4 5))", newZ(15));
		equal(l,"(fold-right cons '() '(1 2 3))", list(1, 2, 3));
		equal(l,"(fold-right (lambda (x l)" +
				"              (if (even? x) (cons x l) l))" +
				"  '() '(1 2 3 4 5 6 7 8))",
				list(2, 4, 6, 8));
		equal(l,"(fold-right cons* '() '(a b c) '(1 2 3 4 5))",
				list(sym("a"), 1, sym("b"), 2, sym("c"), 3));

		equal(l,"(fold-right + 0 '())", newZ(0));
		equal(l,"(fold-right cons* '() '(a b c) (circular-list 1 2))",
				list(sym("a"), 1, sym("b"), 2, sym("c"), 1));
		lperr(l,"(fold-right 1 0 '(1 2 3 4 5))");
		lperr(l,"(fold-right + 0 '(1 2 3 4 . 5))");
		lperr(l,"(fold-right + 0 1)");
	}

	public void testPairFold() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(pair-fold (lambda (pair tail)" +
				"             (set-cdr! pair tail) pair)" +
				"  '() '(1 2 3 4 5))",
				list(5, 4, 3, 2, 1));

		equal(l,"(pair-fold append '() '(1 2) '(4 5 6))",
				list(2, 5, 6, 1, 2, 4, 5, 6));
		equal(l,"(pair-fold append '() '())", Nil.NIL);
		lperr(l,"(pair-fold 1 '() '(1 2 3 4 5))");
		lperr(l,"(pair-fold append '() '(1 2 3 4 . 5))");
		lperr(l,"(pair-fold append '() 1)");
	}

	public void testPairFoldRight() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(pair-fold-right cons '() '(1 2 3))",
				list(list(1, 2, 3), list(2, 3), list(3)));

		equal(l,"(pair-fold-right append '() '(1 2) '(4 5 6))",
				list(1, 2, 4, 5, 6, 2, 5, 6));
		equal(l,"(pair-fold-right append '() '())", Nil.NIL);
		lperr(l,"(pair-fold-right 1 '() '(1 2 3 4 5))");
		lperr(l,"(pair-fold-right append '() '(1 2 3 4 . 5))");
		lperr(l,"(pair-fold-right append '() 1)");
	}

	public void testReduce() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(reduce + 0 '(1 2 3 4 5))", newZ(15));
		equal(l,"(reduce + 0 '(1))", newZ(1));
		equal(l,"(reduce + 0 '())", newZ(0));
		equal(l,"(reduce (lambda (x y)" +
				"          (if (null? y) x (append (list x) y)))" +
				"  '() '(1 2 3 4))", listDot(1, 4, 3, 2));
		lperr(l,"(reduce 1 0 '(1 2 3 4))");
		lperr(l,"(reduce + 0 '(1 2 3 . 4))");
		lperr(l,"(reduce + 0 1)");
	}

	public void testReduceRight() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(reduce-right + 0 '(1 2 3 4 5))", newZ(15));
		equal(l,"(reduce-right + 0 '(1))", newZ(1));
		equal(l,"(reduce-right + 0 '())", newZ(0));
		equal(l,"(reduce-right (lambda (x y)" +
				"          (if (null? y) x (append (list x) y)))" +
				"  '() '(1 2 3 4))", listDot(4, 1, 2, 3));
		lperr(l,"(reduce-right 1 0 '(1 2 3 4))");
		lperr(l,"(reduce-right + 0 '(1 2 3 . 4))");
		lperr(l,"(reduce-right + 0 1)");
	}

	public void testUnfold() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(unfold (lambda (x) (> x 10))" +
				"        (lambda (x) (* x x))" +
				"        (lambda (x) (+ x 1))" +
				"        1)",
				list(1, 4, 9, 16, 25, 36, 49, 64, 81, 100));

		l.exec ("(define lis '(1 2 3))");
		l.exec ("(define l2 (unfold null-list? car cdr lis))");
		equal(l,"l2", list(1, 2, 3));
		equal(l,"(eq? lis l2)", F);
		equal(l,"(unfold null-list? car cdr '())", Nil.NIL);

		l.exec ("(define lis '(1 2 3))");
		l.exec ("(define l2 (unfold not-pair? car cdr lis values))");
		equal(l,"l2", list(1, 2, 3));
		equal(l,"(eq? lis l2)", F);
		l.exec ("(define lis '(1 2 3 . 4))");
		l.exec ("(define l2 (unfold not-pair? car cdr lis values))");
		equal(l,"l2", listDot(4, 1, 2, 3));
		equal(l,"(eq? lis l2)", F);
		equal(l,"(unfold not-pair? car cdr 1 values)", newZ(1));
		equal(l,"(unfold not-pair? car cdr '() values)", Nil.NIL);

		equal(l,"(unfold null-list? car cdr '(1 2 3)" +
				"        (lambda (x) '(4 5 6)))",
				list(1, 2, 3, 4, 5, 6));

		lperr(l,"(unfold 1 car cdr '(1 2 3))");
		lperr(l,"(unfold null? 1 cdr '(1 2 3))");
		lperr(l,"(unfold null? car 1 '(1 2 3))");
		lperr(l,"(unfold null? car cdr '(1 2 3) 1)");
	}

	public void testUnfoldRight() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(unfold-right zero?" +
				"              (lambda (x) (* x x))" +
				"              (lambda (x) (- x 1))" +
				"              10)",
				list(1, 4, 9, 16, 25, 36, 49, 64, 81, 100));
		equal(l,"(unfold-right null-list? car cdr '(1 2 3 4))",
				list(4, 3, 2, 1));
		equal(l,"(unfold-right null-list? car cdr '(1 2 3) '(4 5))",
				list(3, 2, 1, 4, 5));

		equal(l,"(unfold-right null-list? car cdr '())", Nil.NIL);

		lperr(l,"(unfold-right 1 car cdr '(1 2 3))");
		lperr(l,"(unfold-right null? 1 cdr '(1 2 3))");
		lperr(l,"(unfold-right null? car 1 '(1 2 3))");
	}

	public void testMap() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(map cadr '((1 2) (3 4) (5 6)))", list(2, 4, 6));
		equal(l,"(map (lambda (n) (expt n n)) '(1 2 3 4 5))",
				list(1, 4, 27, 256, 3125));
		equal(l,"(map + '(3 1 4 1) (circular-list 1 0))",
				list(4, 1, 5, 1));
	}

	public void testAppendMap() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(append-map list" +
				"  '(1 2 3) '(4 5) '(6 7 8 9))",
				list(1, 4, 6, 2, 5, 7));
		equal(l,"(append-map list" +
				"  '(1 2 3) '(4 5 6) (circular-list 7 8))",
				list(1, 4, 7, 2, 5, 8, 3, 6, 7));
		equal(l,"(append-map list" +
				"  '(1 2 3) '() (circular-list 7 8))", Nil.NIL);
		equal(l,"(append-map list '())", Nil.NIL);
		lperr(l,"(append-map 1 '(1 2 3))");
		lperr(l,"(append-map list)");

		equal(l,"(append-map! (lambda (x) (list x (- x))) '(1 3 8))",
				list(1, -1, 3, -3, 8, -8));
		equal(l,"(append-map! list" +
				"  '(1 2 3) '(4 5) '(6 7 8 9))",
				list(1, 4, 6, 2, 5, 7));
		equal(l,"(append-map! list" +
				"  '(1 2 3) '(4 5 6) (circular-list 7 8))",
				list(1, 4, 7, 2, 5, 8, 3, 6, 7));
		equal(l,"(append-map! list" +
				"  '(1 2 3) '() (circular-list 7 8))", Nil.NIL);
		equal(l,"(append-map! list '())", Nil.NIL);
		lperr(l,"(append-map! 1 '(1 2 3))");
		lperr(l,"(append-map! list)");
	}

	public void testMapS() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		l.exec ("(define x '((1 2) (3 4) (5 6)))");
		equax(l,"(map! cadr x)", list(2, 4, 6));
		l.exec ("(define x '(1 2 3 4 5))");
		equax(l,"(map! (lambda (n) (expt n n)) x)",
				list(1, 4, 27, 256, 3125));
		l.exec ("(define x '(3 1 4 1))");
		equax(l,"(map! + x (circular-list 1 0))", list(4, 1, 5, 1));

		equal(l,"(map! + '() '(1 2 3 4))", Nil.NIL);
		equal(l,"(map! + '(1 2 3 4) '())", Nil.NIL);
		lperr(l,"(map! 1 '(1 2 3) '(4 5))");
		lperr(l,"(map! + 1 '(1 2 3 4))");
		lperr(l,"(map! + '(1 . 2) '(1 2 3 4))");
		lperr(l,"(map! + '(1 2 3 4 5) '(1 2 3 . 4))");
		lperr(l,"(map! + '(1 2) 3)");
	}

	public void testPairForEach() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		l.exec ("(define *x* '())");
		l.exec ("(pair-for-each" +
				"  (lambda (pair) (set! *x* (cons pair *x*)))" +
				"  '(1 2 3))");
		equal(l,"*x*", list(list(3), list(2, 3), list(1, 2, 3)));

		l.exec ("(define *x* '())");
		l.exec ("(pair-for-each" +
				"  (lambda (x y) (set! *x* (cons* x y *x*)))" +
				"  '(1 2 3) '(4 5))");
		equal(l,"*x*",
				list(list(2, 3), list(5), list(1, 2, 3), list(4, 5)));

		l.exec ("(define *x* '())");
		l.exec ("(pair-for-each" +
				"  (lambda (x y) (set! *x* (cons* x y *x*)))" +
				"  '(1 2 3) '())");
		equal(l,"*x*", Nil.NIL);

		lperr(l,"(pair-for-each 1 '(1 2 3) '(4 5))");
		lperr(l,"(pair-for-each list '(1 2 . 3) '(4 5))");
		lperr(l,"(pair-for-each list 1 '(4 5))");
		lperr(l,"(pair-for-each list '(1 2 3) '(4 . 5))");
		lperr(l,"(pair-for-each list '(1 2 3) 4)");
		//lperr(l,"(pair-for-each list)");
	}

	public void testFilterMap() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(filter-map (lambda (x)" +
				"              (and (number? x) (* x x)))" +
				"            '(a 1 b 3 c 7))", list(1, 9, 49));
		equal(l,"(filter-map" +
				"  (lambda (x y)" +
				"    (and (number? x) (number? y) (* x y)))" +
				"  '(a 1 b 2 c 3 d 4) '(a b 1 c d 2 e f 3))",
				list(6));
		equal(l,"(filter-map" +
				"  (lambda (x y)" +
				"    (and (number? x) (number? y) (* x y)))" +
				"  '(a 1 b 2 c 3 d 4) (circular-list 'a 'b 2))",
				list(6));
		equal(l,"(filter-map" +
				"  (lambda (x y)" +
				"    (and (number? x) (number? y) (* x y)))" +
				"  '(a 1 b 2 c 3 d 4) '())",
				Nil.NIL);
		equal(l,"(filter-map" +
				"  (lambda (x y)" +
				"    (and (number? x) (number? y) (* x y)))" +
				"  '(a 1 b 2 c 3 d 4) '(1 a 2 b 3 c))",
				Nil.NIL);

		lperr(l,"(filter-map 1 '(a 1) '(b 2))");
		//lperr(l,"(filter-map +)");
		lperr(l,"(filter-map" +
				"  (lambda (x y)" +
				"    (and (number? x) (number? y) (* x y)))" +
				"  '(a 1 b 2 . 4) '(1 a 2 b 3 c))");
		lperr(l,"(filter-map" +
				"  (lambda (x y)" +
				"    (and (number? x) (number? y) (* x y)))" +
				"  1 '(1 a 2 b 3 c))");
		lperr(l,"(filter-map" +
				"  (lambda (x y)" +
				"    (and (number? x) (number? y) (* x y)))" +
				"  '(a 1 b 2 c 3 d 4) '(1 a 2 b 3 . c))");
		lperr(l,"(filter-map" +
				"  (lambda (x y)" +
				"    (and (number? x) (number? y) (* x y)))" +
				"  '(a 1 b 2 c 3 d 4) 1)");
	}

	public void testFilter() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(filter even? '(0 7 8 8 43 -4))", list(0, 8, 8, -4));
		equal(l,"(filter even? '(7 9 11 13))", Nil.NIL);
		equal(l,"(filter even? '())", Nil.NIL);
		lperr(l,"(filter 1 '(1 2))");
		lperr(l,"(filter even? '(1 2 . 3))");
		lperr(l,"(filter even? 1)");

		l.exec ("(define x '(0 7 8 8 43 -4))");
		equax(l,"(filter! even? x)", list(0, 8, 8, -4));
		l.exec ("(define x '(1 0 7 8 8 43 -4))");
		equax(l,"(filter! even? x)", list(0, 8, 8, -4));
		l.exec ("(define x '(0 8 8 -4))");
		equax(l,"(filter! even? x)", list(0, 8, 8, -4));
		equal(l,"(filter! even? '(7 9 11 13))", Nil.NIL);
		equal(l,"(filter! even? '())", Nil.NIL);
		lperr(l,"(filter! 1 '(1 2))");
		lperr(l,"(filter! even? '(1 2 . 3))");
		lperr(l,"(filter! even? 1)");
	}

	public void testPartition() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(partition even? '(0 7 8 8 43 -4))",
				list(0, 8, 8, -4), list(7, 43));
		equal(l,"(partition even? '(6 8 10 12))",
				list(6, 8, 10, 12), Nil.NIL);
		equal(l,"(partition even? '(7 9 11 13))",
				Nil.NIL, list(7, 9, 11, 13));
		equal(l,"(partition even? '())",
				Nil.NIL, Nil.NIL);
		lperr(l,"(partition 1 '(1 2))");
		lperr(l,"(partition even? '(1 2 . 3))");
		lperr(l,"(partition even? 1)");

		equal(l,"(partition! even? '(0 7 8 8 43 -4))",
				list(0, 8, 8, -4), list(7, 43));
		equal(l,"(partition! even? '(0 8 8 -4 7 43))",
				list(0, 8, 8, -4), list(7, 43));
		equal(l,"(partition! even? '(7 43 0 8 8 -4))",
				list(0, 8, 8, -4), list(7, 43));
		equal(l,"(partition! even? '(6 8 10 12))",
				list(6, 8, 10, 12), Nil.NIL);
		equal(l,"(partition! even? '(7 9 11 13))",
				Nil.NIL, list(7, 9, 11, 13));
		equal(l,"(partition! even? '())",
				Nil.NIL, Nil.NIL);
		lperr(l,"(partition! 1 '(1 2))");
		lperr(l,"(partition! even? '(1 2 . 3))");
		lperr(l,"(partition! even? 1)");
	}

	public void testRemove() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(remove even? '(0 7 8 8 43 -4))", list(7, 43));
		equal(l,"(remove even? '(7 9 11 13))", list(7, 9, 11, 13));
		equal(l,"(remove even? '())", Nil.NIL);
		lperr(l,"(remove 1 '(1 2))");
		lperr(l,"(remove even? '(1 2 . 3))");
		lperr(l,"(remove even? 1)");

		l.exec ("(define x '(0 7 8 8 43 -4))");
		equax(l,"(remove! even? x)", list(7, 43));
		l.exec ("(define x '(1 0 7 8 8 43 -4))");
		equax(l,"(remove! even? x)", list(1, 7, 43));
		equal(l,"(remove! even? '(0 8 8 -4))", Nil.NIL);
		l.exec ("(define x '(7 9 11 13))");
		equax(l,"(remove! even? x)", list(7, 9, 11, 13));
		l.exec ("(define x '(7 9 11 12))");
		equax(l,"(remove! even? x)", list(7, 9, 11));
		l.exec ("(define x '(7))");
		equax(l,"(remove! even? x)", list(7));
		equal(l,"(remove! even? '(8))", Nil.NIL);
		equal(l,"(remove! even? '())", Nil.NIL);
		lperr(l,"(remove! 1 '(1 2))");
		lperr(l,"(remove! even? '(1 2 . 3))");
		lperr(l,"(remove! even? 1)");
	}

	public void testSearching() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(find even? '(1 2 3))", newZ(2));
		equal(l,"(any  even? '(1 2 3))", T);
		equal(l,"(find even? '(1 7 3))", F);
		equal(l,"(any  even? '(1 7 3))", F);
		lperr(l,"(find even? '(1 3 . 2))");
		lperr(l,"(any  even? '(1 3 . 2))");
		//equal(l,"(find even? '(1 2 . x))", newZ(2));
		//equal(l,"(any  even? '(1 2 . x))", T);
		equal(l,"(find even? (circular-list 1 6 3))", newZ(6));
		equal(l,"(any  even? (circular-list 1 6 3))", T);
		lperr(l,"(find even? (circular-list 1 3))");
		lperr(l,"(any  even? (circular-list 1 3))");
	}

	public void testFind() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(find even? '(3 1 4 1 5 9))", newZ(4));
		equal(l,"(find even? '(3 1 1 5 9))", F);
		equal(l,"(find even? '())", F);
		equal(l,"(find even? (circular-list 3 1 4))", newZ(4));
		lperr(l,"(find even? (circular-list 3 1))");
		lperr(l,"(find 1 '(1 2 3))");
	}

	public void testFindTail() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(find-tail even? '(3 1 4 1 5 9))", list(4, 1, 5, 9));
		equal(l,"(find-tail even? '(3 1 1 5 9))", F);
		equal(l,"(find-tail even? '())", F);
		l.exec ("(define x (circular-list 3 1 4))");
		l.exec ("(define y (find-tail even? x))");
		equal(l,"(eq? (cddr x) y)", T);
		lperr(l,"(find-tail even? (circular-list 3 1))");
		lperr(l,"(find-tail 1 '(1 2 3))");
	}

	public void testTakeWhile() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(take-while even? '(2 18 3 10 22 9))", list(2, 18));
		equal(l,"(take-while even? '(2 18 10))", list(2, 18, 10));
		equal(l,"(take-while even? '(3 11 11))", Nil.NIL);
		equal(l,"(take-while even? '())", Nil.NIL);
		equal(l,"(take-while even? (circular-list 2 8 3))", list(2, 8));
		equal(l,"(take-while even? (circular-list 3 7 3))", Nil.NIL);
		l.exec ("(define x (circular-list 2 8 4))");
		l.exec ("(define y (take-while even? x))");
		equal(l,"(eq? x (cdddr y))", T);
		l.exec ("(define x (circular-list 2))");
		l.exec ("(define y (take-while even? x))");
		equal(l,"(eq? x (cdr y))", T);
		lperr(l,"(take-while 1 '(2 18))");
		lperr(l,"(take-while even? '(2 . 18))");
		lperr(l,"(take-while even? 2)");

		l.exec ("(define x '(2 18 3 10 22 9))");
		equax(l,"(take-while! even? x)", list(2, 18));
		l.exec ("(define x '(2 18 10))");
		equax(l,"(take-while! even? x)", list(2, 18, 10));
		equal(l,"(take-while! even? '(3 11 11))", Nil.NIL);
		equal(l,"(take-while! even? '())", Nil.NIL);
		l.exec ("(define x (circular-list 2 8 3))");
		equax(l,"(take-while! even? x)", list(2, 8));
		equal(l,"(take-while! even? (circular-list 3 7 3))", Nil.NIL);
		l.exec ("(define x (circular-list 2 8 4))");
		l.exec ("(define y (take-while! even? x))");
		equal(l,"(eq? x y)", T);
		l.exec ("(define x (circular-list 2))");
		l.exec ("(define y (take-while! even? x))");
		equal(l,"(eq? x y)", T);
		lperr(l,"(take-while! 1 '(2 18))");
		lperr(l,"(take-while! even? '(2 . 18))");
		lperr(l,"(take-while! even? 2)");
	}

	public void testDropWhile() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(drop-while even? '(2 18 3 10 22 9))",
				list(3, 10, 22, 9));
		equal(l,"(drop-while even? '(3 11 11))", list(3, 11, 11));
		equal(l,"(drop-while even? '(2 18 10))", Nil.NIL);
		equal(l,"(drop-while even? '())", Nil.NIL);
		equal(l,"(drop-while even? (circular-list 2 8))", Nil.NIL);
		l.exec ("(define x (circular-list 2 8 3))");
		l.exec ("(define y (drop-while even? x))");
		equal(l,"(eq? (cddr x) y)", T);
		l.exec ("(define x (circular-list 3 9 5))");
		l.exec ("(define y (drop-while even? x))");
		equal(l,"(eq? x y)", T);
		l.exec ("(define x (circular-list 3))");
		l.exec ("(define y (drop-while even? x))");
		equal(l,"(eq? x y)", T);
		lperr(l,"(drop-while 1 '(2 18))");
		lperr(l,"(drop-while even? '(2 . 18))");
		lperr(l,"(drop-while even? 2)");
	}

	public void testSpanBreak() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(span even? '(2 18 3 10 22 9))",
				list(2, 18), list(3, 10, 22, 9));
		equal(l,"(span even? '(2 18 10))", list(2, 18, 10), Nil.NIL);
		equal(l,"(span even? '(3 11 11))", Nil.NIL, list(3, 11, 11));
		equal(l,"(span even? '())", Nil.NIL, Nil.NIL);
		l.exec ("(define x (circular-list 2 8 3))");
		l.exec ("(define y #f)");
		l.exec ("(define z #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (span even? x))" +
				"  (lambda (a b) (set! y a) (set! z b)))");
		equal(l,"y", list(2, 8));
		equal(l,"(eq? (cddr x) z)", T);
		l.exec ("(define x (circular-list 3 7 3))");
		l.exec ("(define y #f)");
		l.exec ("(define z #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (span even? x))" +
				"  (lambda (a b) (set! y a) (set! z b)))");
		equal(l,"y", Nil.NIL);
		equal(l,"(eq? x z)", T);
		l.exec ("(define x (circular-list 2 8 4))");
		l.exec ("(define y #f)");
		l.exec ("(define z #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (span even? x))" +
				"  (lambda (a b) (set! y a) (set! z b)))");
		equal(l,"(eq? x (cdddr y))", T);
		equal(l,"z", Nil.NIL);
		l.exec ("(define x (circular-list 2))");
		l.exec ("(define y #f)");
		l.exec ("(define z #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (span even? x))" +
				"  (lambda (a b) (set! y a) (set! z b)))");
		equal(l,"(eq? x (cdr y))", T);
		equal(l,"z", Nil.NIL);
		lperr(l,"(span 1 '(2 18))");
		lperr(l,"(span even? '(2 . 18))");
		lperr(l,"(span even? 2)");

		equal(l,"(span! even? '(2 18 3 10 22 9))",
				list(2, 18), list(3, 10, 22, 9));
		equal(l,"(span! even? '(2 18 10))", list(2, 18, 10), Nil.NIL);
		equal(l,"(span! even? '(3 11 11))", Nil.NIL, list(3, 11, 11));
		equal(l,"(span! even? '())", Nil.NIL, Nil.NIL);
		l.exec ("(define x (circular-list 2 8 3))");
		l.exec ("(define w (cddr x))");
		l.exec ("(define y #f)");
		l.exec ("(define z #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (span! even? x))" +
				"  (lambda (a b) (set! y a) (set! z b)))");
		equal(l,"y", list(2, 8));
		equal(l,"(eq? w z)", T);
		l.exec ("(define x (circular-list 3 7 3))");
		l.exec ("(define y #f)");
		l.exec ("(define z #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (span! even? x))" +
				"  (lambda (a b) (set! y a) (set! z b)))");
		equal(l,"y", Nil.NIL);
		equal(l,"(eq? x z)", T);
		l.exec ("(define x (circular-list 2 8 4))");
		l.exec ("(define y #f)");
		l.exec ("(define z #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (span! even? x))" +
				"  (lambda (a b) (set! y a) (set! z b)))");
		equal(l,"(eq? x y)", T);
		equal(l,"z", Nil.NIL);
		l.exec ("(define x (circular-list 2))");
		l.exec ("(define y #f)");
		l.exec ("(define z #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (span! even? x))" +
				"  (lambda (a b) (set! y a) (set! z b)))");
		equal(l,"(eq? x y)", T);
		equal(l,"z", Nil.NIL);
		lperr(l,"(span! 1 '(2 18))");
		lperr(l,"(span! even? '(2 . 18))");
		lperr(l,"(span! even? 2)");

		equal(l,"(break odd? '(2 18 3 10 22 9))",
				list(2, 18), list(3, 10, 22, 9));
		equal(l,"(break odd? '(2 18 10))", list(2, 18, 10), Nil.NIL);
		equal(l,"(break odd? '(3 11 11))", Nil.NIL, list(3, 11, 11));
		equal(l,"(break odd? '())", Nil.NIL, Nil.NIL);
		l.exec ("(define x (circular-list 2 8 3))");
		l.exec ("(define y #f)");
		l.exec ("(define z #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (break odd? x))" +
				"  (lambda (a b) (set! y a) (set! z b)))");
		equal(l,"y", list(2, 8));
		equal(l,"(eq? (cddr x) z)", T);
		l.exec ("(define x (circular-list 3 7 3))");
		l.exec ("(define y #f)");
		l.exec ("(define z #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (break odd? x))" +
				"  (lambda (a b) (set! y a) (set! z b)))");
		equal(l,"y", Nil.NIL);
		equal(l,"(eq? x z)", T);
		l.exec ("(define x (circular-list 2 8 4))");
		l.exec ("(define y #f)");
		l.exec ("(define z #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (break odd? x))" +
				"  (lambda (a b) (set! y a) (set! z b)))");
		equal(l,"(eq? x (cdddr y))", T);
		equal(l,"z", Nil.NIL);
		l.exec ("(define x (circular-list 2))");
		l.exec ("(define y #f)");
		l.exec ("(define z #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (break odd? x))" +
				"  (lambda (a b) (set! y a) (set! z b)))");
		equal(l,"(eq? x (cdr y))", T);
		equal(l,"z", Nil.NIL);
		lperr(l,"(break 1 '(2 18))");
		lperr(l,"(break odd? '(2 . 18))");
		lperr(l,"(break odd? 2)");

		equal(l,"(break! odd? '(2 18 3 10 22 9))",
				list(2, 18), list(3, 10, 22, 9));
		equal(l,"(break! odd? '(2 18 10))", list(2, 18, 10), Nil.NIL);
		equal(l,"(break! odd? '(3 11 11))", Nil.NIL, list(3, 11, 11));
		equal(l,"(break! odd? '())", Nil.NIL, Nil.NIL);
		l.exec ("(define x (circular-list 2 8 3))");
		l.exec ("(define w (cddr x))");
		l.exec ("(define y #f)");
		l.exec ("(define z #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (break! odd? x))" +
				"  (lambda (a b) (set! y a) (set! z b)))");
		equal(l,"y", list(2, 8));
		equal(l,"(eq? w z)", T);
		l.exec ("(define x (circular-list 3 7 3))");
		l.exec ("(define y #f)");
		l.exec ("(define z #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (break! odd? x))" +
				"  (lambda (a b) (set! y a) (set! z b)))");
		equal(l,"y", Nil.NIL);
		equal(l,"(eq? x z)", T);
		l.exec ("(define x (circular-list 2 8 4))");
		l.exec ("(define y #f)");
		l.exec ("(define z #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (break! odd? x))" +
				"  (lambda (a b) (set! y a) (set! z b)))");
		equal(l,"(eq? x y)", T);
		equal(l,"z", Nil.NIL);
		l.exec ("(define x (circular-list 2))");
		l.exec ("(define y #f)");
		l.exec ("(define z #f)");
		l.exec ("(call-with-values" +
				"  (lambda () (break! odd? x))" +
				"  (lambda (a b) (set! y a) (set! z b)))");
		equal(l,"(eq? x y)", T);
		equal(l,"z", Nil.NIL);
		lperr(l,"(break! 1 '(2 18))");
		lperr(l,"(break! odd? '(2 . 18))");
		lperr(l,"(break! odd? 2)");
	}

	public void testAny() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(any even? '(3 1 4 1 5 9))", T);
		equal(l,"(any even? '(3 1 1 5 9))", F);
		equal(l,"(any even? '())", F);
		equal(l,"(any even? (circular-list 3 1 4))", T);
		equal(l,"(any integer? '(a 3 b 2.7))", T);
		equal(l,"(any integer? '(a 3.1 b 2.7))", F);
		equal(l,"(any < '(3 1 4 1 5) '(2 7 1 8 2))", T);
		equal(l,"(any < '(1 2 3 4 5) '(0 1 2 3 4))", F);
		equal(l,"(any < '(3 1 4 1 5 9) '(2 7 1 8 2))", T);
		equal(l,"(any < '(1 2 3 4 5 6) '(0 1 2 3 4))", F);
		equal(l,"(any < '(3 4 5 6 7) (circular-list 0 1))", F);
		equal(l,"(any > (circular-list 0 1) '(3 4 5 6 7))", F);
		lperr(l,"(any < '(circular-list 3 4 5) (circular-list 0 1))");
		lperr(l,"(any even? (circular-list 3 1))");
		lperr(l,"(any even?)");
		lperr(l,"(any 1 '(1 2 3))");
	}

	public void testEvery() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(every odd? '(3 1 4 1 5 9))", F);
		equal(l,"(every odd? '(3 1 1 5 9))", T);
		equal(l,"(every odd? '())", T);
		equal(l,"(every odd? (circular-list 3 1 4))", F);
		equal(l,"(every > '(3 1 4 1 5) '(2 7 1 8 2))", F);
		equal(l,"(every > '(1 2 3 4 5) '(0 1 2 3 4))", T);
		equal(l,"(every > '(3 1 4 1 5 9) '(2 7 1 8 2))", F);
		equal(l,"(every > '(1 2 3 4 5 6) '(0 1 2 3 4))", T);
		equal(l,"(every > '(3 4 5 6 7) (circular-list 0 1))", T);
		equal(l,"(every < (circular-list 0 1) '(3 4 5 6 7))", T);
		lperr(l,"(every > '(circular-list 3 4 5) (circular-list 0 1))");
		lperr(l,"(every odd? (circular-list 3 1))");
		lperr(l,"(every even?)");
		lperr(l,"(every 1 '(1 2 3))");
	}

	public void testListIndex() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(list-index even? '(3 1 4 1 5 9))", newZ(2));
		equal(l,"(list-index even? '(3 1 1 5 9))", F);
		equal(l,"(list-index even? '())", F);
		equal(l,"(list-index even? (circular-list 3 1 4))", newZ(2));
		equal(l,"(list-index integer? '(a 3 b 2.7))", newZ(1));
		equal(l,"(list-index integer? '(a 3.1 b 2.7))", F);
		equal(l,"(list-index < '(3 1 4 1 5) '(2 7 1 8 2))", newZ(1));
		equal(l,"(list-index < '(1 2 3 4 5) '(0 1 2 3 4))", F);
		equal(l,"(list-index < '(3 1 4 1 5 9) '(2 7 1 8 2))", newZ(1));
		equal(l,"(list-index < '(1 2 3 4 5 6) '(0 1 2 3 4))", F);
		equal(l,"(list-index < '(3 4 5 6 7) (circular-list 0 1))", F);
		equal(l,"(list-index > (circular-list 0 1) '(3 4 5 6 7))", F);
		lperr(l,"(list-index <" +
				"  '(circular-list 3 4 5) (circular-list 0 1))");
		lperr(l,"(list-index even? (circular-list 3 1))");
		lperr(l,"(list-index even?)");
		lperr(l,"(list-index 1 '(1 2 3))");
	}

	public void testMember() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(member (list 'a) '(b (a) c))",
				list(list(sym("a")), sym("c")));
		equal(l,"(member (list 'a) '(b (a) c) eq?)", F);
		equal(l,"(member 1 '(2 1 3) =)", list(1, 3));
		equal(l,"(member 4 '(2 1 3) =)", F);
		equal(l,"(member 1 '())", F);
		equal(l,"(member 1 '() =)", F);
		lperr(l,"(member 3 '(1 . 2))");
		lperr(l,"(member 1 2)");
	}

	public void testDelete() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(delete (list 'a) '(b (a) c))",
				list(sym("b"), sym("c")));
		equal(l,"(delete (list 'a) '(b (a) c) eq?)",
				list(sym("b"), list(sym("a")), sym("c")));
		equal(l,"(delete 1 '(2 1 3) =)", list(2, 3));
		equal(l,"(delete 4 '(2 1 3) =)", list(2, 1, 3));
		equal(l,"(delete 1 '(1 1 1) =)", Nil.NIL);
		equal(l,"(delete 2 '(1) =)", list(1));
		equal(l,"(delete 1 '(1) =)", Nil.NIL);
		equal(l,"(delete 1 '())", Nil.NIL);
		equal(l,"(delete 1 '() =)", Nil.NIL);
		lperr(l,"(delete 1 '(1 . 2))");
		lperr(l,"(delete 1 2)");

		l.exec ("(define x '(b (a) c))");
		equax(l,"(delete! (list 'a) x)", list(sym("b"), sym("c")));
		l.exec ("(define x '(b (a) c))");
		equax(l,"(delete! (list 'a) x eq?)",
				list(sym("b"), list(sym("a")), sym("c")));
		l.exec ("(define x '(2 1 3))");
		equax(l,"(delete! 1 x =)", list(2, 3));
		l.exec ("(define x '(2 1 3))");
		equax(l,"(delete! 4 x =)", list(2, 1, 3));
		l.exec ("(define x '(2 1 3))");
		equax(l,"(delete! 3 x =)", list(2, 1));
		l.exec ("(define x '(2 1 3))");
		equax(l,"(delete! 2 x =)", list(1, 3));
		equal(l,"(delete! 1 '(1 1 1) =)", Nil.NIL);
		l.exec ("(define x '(1))");
		equax(l,"(delete! 2 x =)", list(1));
		equal(l,"(delete! 1 '(1) =)", Nil.NIL);
		equal(l,"(delete! 1 '())", Nil.NIL);
		equal(l,"(delete! 1 '() =)", Nil.NIL);
		lperr(l,"(delete! 1 '(1 . 2))");
		lperr(l,"(delete! 1 2)");
	}

	public void testDeleteDuplicates() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(delete-duplicates '(a b a c a b c z))",
				list(sym("a"), sym("b"), sym("c"), sym("z")));
		equal(l,"(delete-duplicates" +
				"  '((a . 3) (b . 7) (a . 9) (c . 1))" +
				"  (lambda (x y) (eq? (car x) (car y))))",
				list(cons(sym("a"), 3),
						cons(sym("b"), 7),
						cons(sym("c"), 1)));
		equal(l,"(delete-duplicates '(1 2 3 4 5))",
				list(1, 2, 3, 4, 5));
		equal(l,"(delete-duplicates '(1 1 1 1 1))", list(1));
		equal(l,"(delete-duplicates '(1 1 2 2 2))", list(1, 2));
		equal(l,"(delete-duplicates '(1))", list(1));
		equal(l,"(delete-duplicates '())", Nil.NIL);
		lperr(l,"(delete-duplicates '(1 . 2))");
		lperr(l,"(delete-duplicates 2)");

		l.exec ("(define x '(a b a c a b c z))");
		equax(l,"(delete-duplicates! x)",
				list(sym("a"), sym("b"), sym("c"), sym("z")));
		l.exec ("(define x '((a . 3) (b . 7) (a . 9) (c . 1)))");
		equax(l,"(delete-duplicates!" +
				"  x" +
				"  (lambda (x y) (eq? (car x) (car y))))",
				list(cons(sym("a"), 3),
						cons(sym("b"), 7),
						cons(sym("c"), 1)));
		l.exec ("(define x '(1 2 3 4 5))");
		equax(l,"(delete-duplicates! x)", list(1, 2, 3, 4, 5));
		l.exec ("(define x '(1 1 1 1 1))");
		equal(l,"(delete-duplicates! x)", list(1));
		l.exec ("(define x '(1 1 2 2 2))");
		equal(l,"(delete-duplicates! x)", list(1, 2));
		l.exec ("(define x '(1))");
		equal(l,"(delete-duplicates! x)", list(1));
		equal(l,"(delete-duplicates! '())", Nil.NIL);
		lperr(l,"(delete-duplicates! '(1 . 2))");
		lperr(l,"(delete-duplicates! 2)");
	}

	public void testAssoc() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(assoc (list 'a) '(((a)) ((b)) ((c))))",
				list(list(sym("a"))));
		equal(l,"(assoc (list 'a) '(((a)) ((b)) ((c))) eq?)", F);
		equal(l,"(assoc 5 '((2 3) (5 7) (11 13)) =)", list(5, 7));
		equal(l,"(assoc 4 '((2 3) (5 7) (11 13)) =)", F);
		equal(l,"(assoc 5 '((5 7)) =)", list(5, 7));
		equal(l,"(assoc 4 '((5 7)) =)", F);
		equal(l,"(assoc 5 '() =)", F);
		lperr(l,"(assoc 7 '((2 3) (5 7) . (11 13)))");
		lperr(l,"(assoc 5 5)");
	}

	public void testAlistCons() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(alist-cons 2 3 '((5 . 7) (11 . 13)))",
				list(cons(2, 3), cons(5, 7), cons(11, 13)));
		equal(l,"(alist-cons 2 3 '())", list(cons(2, 3)));
	}

	public void testAlistCopy() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		l.exec ("(define x '((2 . 3) (5 . 7) (11 . 13)))");
		l.exec ("(define y (alist-copy x))");
		equal(l,"y", list(cons(2, 3), cons(5, 7), cons(11, 13)));
		equal(l,"(eq? x y)", F);
		equal(l,"(alist-copy '())", Nil.NIL);
	}

	public void testAlistDelete() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(alist-delete (list 'a) '(((a)) ((b)) ((c))))",
				list(list(list(sym("b"))), list(list(sym("c")))));
		equal(l,"(alist-delete (list 'b) '(((a)) ((b)) ((c))))",
				list(list(list(sym("a"))), list(list(sym("c")))));
		equal(l,"(alist-delete (list 'c) '(((a)) ((b)) ((c))))",
				list(list(list(sym("a"))), list(list(sym("b")))));
		equal(l,"(alist-delete (list 'a) '(((a)) ((b)) ((c))) eq?)",
				list(list(list(sym("a"))),
						list(list(sym("b"))),
						list(list(sym("c")))));
		equal(l,"(alist-delete 5 '((2 3) (5 7) (11 13)) =)",
				list(list(2, 3), list(11, 13)));
		equal(l,"(alist-delete 4 '((2 3) (5 7) (11 13)) =)",
				list(list(2, 3), list(5, 7), list(11, 13)));
		equal(l,"(alist-delete 5 '((5 7)) =)", Nil.NIL);
		equal(l,"(alist-delete 4 '((5 7)) =)", list(list(5, 7)));
		equal(l,"(alist-delete 5 '() =)", Nil.NIL);
		lperr(l,"(alist-delete 5 '((2 3) (5 7) . (11 13)))");
		lperr(l,"(alist-delete 5 5)");

		l.exec ("(define x '(((a)) ((b)) ((c))))");
		equax(l,"(alist-delete! (list 'a) x)",
				list(list(list(sym("b"))), list(list(sym("c")))));
		l.exec ("(define x '(((a)) ((b)) ((c))))");
		equax(l,"(alist-delete! (list 'b) x)",
				list(list(list(sym("a"))), list(list(sym("c")))));
		l.exec ("(define x '(((a)) ((b)) ((c))))");
		equax(l,"(alist-delete! (list 'c) x)",
				list(list(list(sym("a"))), list(list(sym("b")))));
		l.exec ("(define x '(((a)) ((b)) ((c))))");
		equax(l,"(alist-delete! (list 'a) x eq?)",
				list(list(list(sym("a"))),
						list(list(sym("b"))),
						list(list(sym("c")))));
		l.exec ("(define x '((2 3) (5 7) (11 13)))");
		equax(l,"(alist-delete! 5 x =)",
				list(list(2, 3), list(11, 13)));
		l.exec ("(define x '((2 3) (5 7) (11 13)))");
		equax(l,"(alist-delete! 4 x =)",
				list(list(2, 3), list(5, 7), list(11, 13)));
		equal(l,"(alist-delete! 5 '((5 7)) =)", Nil.NIL);
		l.exec ("(define x '((5 7)))");
		equax(l,"(alist-delete! 4 x =)", list(list(5, 7)));
		equal(l,"(alist-delete! 5 '() =)", Nil.NIL);
		lperr(l,"(alist-delete! 5 '((2 3) (5 7) . (11 13)))");
		lperr(l,"(alist-delete! 5 5)");
	}

	public void testLsetLe() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(lset<= eq? '(a) '(a b a) '(a b c c))", T);
		equal(l,"(lset<= eq? '(a d) '(a b a) '(a b c c))", F);
		equal(l,"(lset<= eq? '(a) '(a b d) '(a b c c))", F);
		equal(l,"(lset<= eq? '(a b c c) '(a b a) '(a))", F);
		equal(l,"(lset<= eq?)", T);
		equal(l,"(lset<= eq? '(a))", T);
		lperr(l,"(lset<= 1 '(a) '(a b))");
	}

	public void testLsetEq() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		equal(l,"(lset= eq? '(b e a) '(a e b) '(e e b a))", T);
		equal(l,"(lset= eq? '(a) '(a b a) '(a b c c))", F);
		equal(l,"(lset= eq? '(a d) '(a b a) '(a b c c))", F);
		equal(l,"(lset= eq? '(a) '(a b d) '(a b c c))", F);
		equal(l,"(lset= eq? '(a b c c) '(a b a) '(a))", F);
		equal(l,"(lset= eq?)", T);
		equal(l,"(lset= eq? '(a))", T);
		lperr(l,"(lset= 1 '(a) '(a b))");
	}

	public void testLsetAdjoin() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		l.exec ("(define (setchk? ls1 ls2 e=)" +
				"  (cond ((= (length ls1) (length ls2))" +
				"          (let loop ((l ls1))" +
				"            (if (null? l)" +
				"                #t" +
				"                (and (member (car l) ls2 e=)" +
				"                     (loop (cdr l))))))" +
				"        (else #f)))");
		equal(l,"(setchk?" +
				"  (lset-adjoin eq? '(a b c d c e) 'a 'e 'i 'o 'u)" +
				"  '(u o i a b c d c e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-adjoin eq? '(a b c c d e) 'a 'e 'a)" +
				"  '(a b c c d e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-adjoin eq? '(a b c c d e))" +
				"  '(a b c c d e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-adjoin eq? '() 'a 'e 'a)" +
				"  '(a e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-adjoin eq? '())" +
				"  '() eq?)", T);
	}

	public void testLsetUnion() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		l.exec ("(define (setchk? ls1 ls2 e=)" +
				"  (cond ((= (length ls1) (length ls2))" +
				"          (let loop ((l ls1))" +
				"            (if (null? l)" +
				"                #t" +
				"                (and (member (car l) ls2 e=)" +
				"                     (loop (cdr l))))))" +
				"        (else #f)))");
		equal(l,"(setchk?" +
				"  (lset-union eq? '(a b c d e) '(a e i o u))" +
				"  '(u o i a b c d e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-union eq? '(a a c) '(x a x))" +
				"  '(x a a c) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-union eq? '(a b) '(a d) '(a e))" +
				"  '(a b d e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-union eq? '() '(a e) '())" +
				"  '(a e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-union eq?)" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-union eq? '(a b c))" +
				"  '(a b c) eq?)", T);
		lperr(l,"(lset-union eq? '(a b) '(c . d))");
		lperr(l,"(lset-union eq? '(a b) 'c)");
		lperr(l,"(lset-union 1 '(a b) '(c d))");

		//l.exec ("(define x '(a b c d e))");
		//l.exec ("(lset-union! eq? x '(a e i o u))");
		//equal(l,"(setchk? x '(u o i a b c d e) eq?)", T);
		//l.exec ("(define x '(a a c))");
		//l.exec ("(lset-union! eq? x '(x a x))");
		//equal(l,"(setchk? x '(x a a c) eq?)", T);
		//l.exec ("(define x '(a b))");
		//l.exec ("(lset-union! eq? x '(a d) '(a e))");
		//equal(l,"(setchk? x '(a b d e) eq?)", T);
		//l.exec ("(define x '(a e))");
		//l.exec ("(lset-union! eq? '() x '())");
		//equal(l,"(setchk? x '(a e) eq?)", T);
		//equal(l,"(setchk? (lset-union! eq?) '() eq?)", T);
		//l.exec ("(define x '(a b c))");
		//l.exec ("(lset-union! eq? x)");
		//equal(l,"(setchk? x '(a b c) eq?)", T);
		//lperr(l,"(lset-union! eq? '(a b) '(c . d))");
		//lperr(l,"(lset-union! eq? '(a b) 'c)");
		//lperr(l,"(lset-union! 1 '(a b) '(c d))");
		equal(l,"(setchk?" +
				"  (lset-union! eq? '(a b c d e) '(a e i o u))" +
				"  '(u o i a b c d e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-union! eq? '(a a c) '(x a x))" +
				"  '(x a a c) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-union! eq? '(a b) '(a d) '(a e))" +
				"  '(a b d e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-union! eq? '() '(a e) '())" +
				"  '(a e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-union! eq?)" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-union! eq? '(a b c))" +
				"  '(a b c) eq?)", T);
		lperr(l,"(lset-union! eq? '(a b) '(c . d))");
		lperr(l,"(lset-union! eq? '(a b) 'c)");
		lperr(l,"(lset-union! 1 '(a b) '(c d))");
	}

	public void testLsetIntersection() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		l.exec ("(define (setchk? ls1 ls2 e=)" +
				"  (cond ((= (length ls1) (length ls2))" +
				"          (let loop ((l ls1))" +
				"            (if (null? l)" +
				"                #t" +
				"                (and (member (car l) ls2 e=)" +
				"                     (loop (cdr l))))))" +
				"        (else #f)))");
		equal(l,"(setchk?" +
				"  (lset-intersection eq? '(a b c d e) '(a e i o u))" +
				"  '(a e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-intersection eq? '(a x y a) '(x a x z))" +
				"  '(a x a) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-intersection eq? '(a b) '(a d) '(a e))" +
				"  '(a) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-intersection eq? '(a a b) '(a a a b) '(a b))" +
				"  '(a a b) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-intersection eq? '() '(a e) '())" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-intersection eq?)" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-intersection eq? '(a b c))" +
				"  '(a b c) eq?)", T);
		lperr(l,"(lset-intersection eq? '(a b) '(c . d))");
		lperr(l,"(lset-intersection eq? '(a b) 'c)");
		lperr(l,"(lset-intersection 1 '(a b) '(c d))");

		//l.exec ("(define x '(a b c d e))");
		//l.exec ("(lset-intersection! eq? x '(a e i o u))");
		//equal(l,"(setchk? x '(a e) eq?)", T);
		//l.exec ("(define x '(a x y a))");
		//l.exec ("(lset-intersection! eq? x '(x a x z))");
		//equal(l,"(setchk? x '(a x a) eq?)", T);
		//l.exec ("(define x '(a b))");
		//l.exec ("(lset-intersection! eq? x '(a d) '(a e))");
		//equal(l,"(setchk? x '(a) eq?)", T);
		//l.exec ("(define x '(a a b))");
		//l.exec ("(lset-intersection! eq? x '(a a a b) '(a b))");
		//equal(l,"(setchk? x '(a a b) eq?)", T);
		//l.exec ("(define x '(a e))");
		//l.exec ("(lset-intersection! eq? '() x '())");
		//equal(l,"(setchk? x '() eq?)", T);
		//equal(l,"(setchk? (lset-intersection! eq?) '() eq?)", T);
		//l.exec ("(define x '(a b c))");
		//l.exec ("(lset-intersection! eq? x)");
		//equal(l,"(setchk? x '(a b c) eq?)", T);
		//lperr(l,"(lset-intersection! eq? '(a b) '(c . d))");
		//lperr(l,"(lset-intersection! eq? '(a b) 'c)");
		//lperr(l,"(lset-intersection! 1 '(a b) '(c d))");
		equal(l,"(setchk?" +
				"  (lset-intersection! eq? '(a b c d e) '(a e i o u))" +
				"  '(a e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-intersection! eq? '(a x y a) '(x a x z))" +
				"  '(a x a) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-intersection! eq? '(a b) '(a d) '(a e))" +
				"  '(a) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-intersection! eq? '(a a b) '(a a a b) '(a b))" +
				"  '(a a b) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-intersection! eq? '() '(a e) '())" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-intersection! eq?)" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-intersection! eq? '(a b c))" +
				"  '(a b c) eq?)", T);
		lperr(l,"(lset-intersection! eq? '(a b) '(c . d))");
		lperr(l,"(lset-intersection! eq? '(a b) 'c)");
		lperr(l,"(lset-intersection! 1 '(a b) '(c d))");
	}

	public void testLsetDifference() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		l.exec ("(define (setchk? ls1 ls2 e=)" +
				//"  (format #t \"~a~%\" ls1)" +
				"  (cond ((= (length ls1) (length ls2))" +
				"          (let loop ((l ls1))" +
				"            (if (null? l)" +
				"                #t" +
				"                (and (member (car l) ls2 e=)" +
				"                     (loop (cdr l))))))" +
				"        (else #f)))");
		equal(l,"(setchk?" +
				"  (lset-difference eq? '(a b c d e) '(a e i o u))" +
				"  '(b c d) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-difference eq? '(a x y y a) '(x a x z))" +
				"  '(y y) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-difference eq? '(a b e) '(a d) '(a e))" +
				"  '(b) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-difference eq? '(a a b) '(a a a b) '(a b))" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-difference eq? '(a e) '(a e))" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-difference eq? '(a e) '())" +
				"  '(a e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-difference eq? '() '(a e) '())" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-difference eq?)" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-difference eq? '(a b c))" +
				"  '(a b c) eq?)", T);
		lperr(l,"(lset-difference eq? '(a b) '(c . d))");
		lperr(l,"(lset-difference eq? '(a b) 'c)");
		lperr(l,"(lset-difference 1 '(a b) '(c d))");

		equal(l,"(setchk?" +
				"  (lset-difference! eq? '(a b c d e) '(a e i o u))" +
				"  '(b c d) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-difference! eq? '(a x y y a) '(x a x z))" +
				"  '(y y) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-difference! eq? '(a b e) '(a d) '(a e))" +
				"  '(b) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-difference! eq? '(a a b) '(a a a b) '(a b))" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-difference! eq? '(a e) '(a e))" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-difference! eq? '(a e) '())" +
				"  '(a e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-difference! eq? '() '(a e) '())" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-difference! eq?)" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-difference! eq? '(a b c))" +
				"  '(a b c) eq?)", T);
		lperr(l,"(lset-difference! eq? '(a b) '(c . d))");
		lperr(l,"(lset-difference! eq? '(a b) 'c)");
		lperr(l,"(lset-difference! 1 '(a b) '(c d))");
	}

	public void testLsetXor() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		l.exec ("(define (setchk? ls1 ls2 e=)" +
				//"  (format #t \"~a~%\" ls1)" +
				"  (cond ((= (length ls1) (length ls2))" +
				"          (let loop ((l ls1))" +
				"            (if (null? l)" +
				"                #t" +
				"                (and (member (car l) ls2 e=)" +
				"                     (loop (cdr l))))))" +
				"        (else #f)))");
		equal(l,"(setchk?" +
				"  (lset-xor eq? '(a b c d e) '(a e i o u))" +
				"  '(d c b i o u) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor eq? '(a x y y a) '(x a x z))" +
				"  '(y y z) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor eq? '(a b e) '(a d) '(a e))" +
				"  '(a b d) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor eq? '(a a b) '(a a a b) '(a b))" +
				"  '(a b) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor eq? '(a e) '(a e))" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor eq? '(a e) '(b d))" +
				"  '(a e b d) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor eq? '(a e) '())" +
				"  '(a e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor eq? '() '(a e) '())" +
				"  '(a e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor eq?)" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor eq? '(a b c))" +
				"  '(a b c) eq?)", T);
		lperr(l,"(lset-xor eq? '(a b) '(c . d))");
		lperr(l,"(lset-xor eq? '(a b) 'c)");
		lperr(l,"(lset-xor 1 '(a b) '(c d))");

		equal(l,"(setchk?" +
				"  (lset-xor! eq? '(a b c d e) '(a e i o u))" +
				"  '(d c b i o u) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor! eq? '(a x y y a) '(x a x z))" +
				"  '(y y z) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor! eq? '(a b e) '(a d) '(a e))" +
				"  '(a b d) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor! eq? '(a a b) '(a a a b) '(a b))" +
				"  '(a b) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor! eq? '(a e) '(a e))" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor! eq? '(a e) '(b d))" +
				"  '(a e b d) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor! eq? '(a e) '())" +
				"  '(a e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor! eq? '() '(a e) '())" +
				"  '(a e) eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor! eq?)" +
				"  '() eq?)", T);
		equal(l,"(setchk?" +
				"  (lset-xor! eq? '(a b c))" +
				"  '(a b c) eq?)", T);
		lperr(l,"(lset-xor! eq? '(a b) '(c . d))");
		lperr(l,"(lset-xor! eq? '(a b) 'c)");
		lperr(l,"(lset-xor! 1 '(a b) '(c d))");
	}

	public void testLsetDiffIntersection() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-1)");
		l.exec ("(define (setchk? ls1 ls2 e=)" +
		//		"  (format #t \"~a~%\" ls1)" +
				"  (cond ((= (length ls1) (length ls2))" +
				"          (let loop ((l ls1))" +
				"            (if (null? l)" +
				"                #t" +
				"                (and (member (car l) ls2 e=)" +
				"                     (loop (cdr l))))))" +
				"        (else #f)))");

		l.exec ("(define a #f)");
		l.exec ("(define b #f)");
		l.exec ("(define (ld0)" +
				"  (call-with-values" +
				"    (lambda () (lset-diff+intersection eq?))" +
				"    (lambda (z w) (set! a z) (set! b w))))");
		l.exec ("(define (ld1 x)" +
				"  (call-with-values" +
				"    (lambda () (lset-diff+intersection eq? x))" +
				"    (lambda (z w) (set! a z) (set! b w))))");
		l.exec ("(define (ld x y)" +
				"  (call-with-values" +
				"    (lambda () (lset-diff+intersection eq? x y))" +
				"    (lambda (z w) (set! a z) (set! b w))))");
		l.exec ("(define (ld3 x y q)" +
				"  (call-with-values" +
				"    (lambda () (lset-diff+intersection eq? x y q))" +
				"    (lambda (z w) (set! a z) (set! b w))))");
		l.exec ("(ld '(a b c d e) '(a e i o u))");
		equal(l,"(setchk? a '(b c d) eq?)", T);
		equal(l,"(setchk? b '(a e) eq?)", T);
		l.exec ("(ld '(a x y y a) '(x a x z))");
		equal(l,"(setchk? a '(y y) eq?)", T);
		equal(l,"(setchk? b '(a x a) eq?)", T);
		l.exec ("(ld3 '(a b e) '(a d) '(a e))");
		equal(l,"(setchk? a '(b) eq?)", T);
		equal(l,"(setchk? b '(a) eq?)", T);
		l.exec ("(ld3 '(a a b) '(a a a b) '(a b))");
		equal(l,"(setchk? a '() eq?)", T);
		equal(l,"(setchk? b '(a a b) eq?)", T);
		l.exec ("(ld '(a e) '(a e))");
		equal(l,"(setchk? a '() eq?)", T);
		equal(l,"(setchk? b '(a e) eq?)", T);
		l.exec ("(ld '(a e) '())");
		equal(l,"(setchk? a '(a e) eq?)", T);
		equal(l,"(setchk? b '() eq?)", T);
		l.exec ("(ld3 '() '(a e) '())");
		equal(l,"(setchk? a '() eq?)", T);
		equal(l,"(setchk? b '() eq?)", T);
		l.exec ("(ld0)");
		equal(l,"(setchk? a '() eq?)", T);
		equal(l,"(setchk? b '() eq?)", T);
		l.exec ("(ld1 '(a b c))");
		equal(l,"(setchk? a '(a b c) eq?)", T);
		equal(l,"(setchk? b '(a b c) eq?)", T);
		lperr(l,"(lset-diff+intersection eq? '(a b) '(c . d))");
		lperr(l,"(lset-diff+intersection eq? '(a b) 'c)");
		lperr(l,"(lset-diff+intersection 1 '(a b) '(c d))");

		l.exec ("(define a #f)");
		l.exec ("(define b #f)");
		l.exec ("(define (ld0)" +
				"  (call-with-values" +
				"    (lambda () (lset-diff+intersection! eq?))" +
				"    (lambda (z w) (set! a z) (set! b w))))");
		l.exec ("(define (ld1 x)" +
				"  (call-with-values" +
				"    (lambda () (lset-diff+intersection! eq? x))" +
				"    (lambda (z w) (set! a z) (set! b w))))");
		l.exec ("(define (ld x y)" +
				"  (call-with-values" +
				"    (lambda () (lset-diff+intersection! eq? x y))" +
				"    (lambda (z w) (set! a z) (set! b w))))");
		l.exec ("(define (ld3 x y q)" +
				"  (call-with-values" +
				"    (lambda () (lset-diff+intersection! eq? x y q))" +
				"    (lambda (z w) (set! a z) (set! b w))))");
		l.exec ("(ld '(a b c d e) '(a e i o u))");
		equal(l,"(setchk? a '(b c d) eq?)", T);
		equal(l,"(setchk? b '(a e) eq?)", T);
		l.exec ("(ld '(a x y y a) '(x a x z))");
		equal(l,"(setchk? a '(y y) eq?)", T);
		equal(l,"(setchk? b '(a x a) eq?)", T);
		l.exec ("(ld3 '(a b e) '(a d) '(a e))");
		equal(l,"(setchk? a '(b) eq?)", T);
		equal(l,"(setchk? b '(a) eq?)", T);
		l.exec ("(ld3 '(a a b) '(a a a b) '(a b))");
		equal(l,"(setchk? a '() eq?)", T);
		equal(l,"(setchk? b '(a a b) eq?)", T);
		l.exec ("(ld '(a e) '(a e))");
		equal(l,"(setchk? a '() eq?)", T);
		equal(l,"(setchk? b '(a e) eq?)", T);
		l.exec ("(ld '(a e) '())");
		equal(l,"(setchk? a '(a e) eq?)", T);
		equal(l,"(setchk? b '() eq?)", T);
		l.exec ("(ld3 '() '(a e) '())");
		equal(l,"(setchk? a '() eq?)", T);
		equal(l,"(setchk? b '() eq?)", T);
		l.exec ("(ld0)");
		equal(l,"(setchk? a '() eq?)", T);
		equal(l,"(setchk? b '() eq?)", T);
		l.exec ("(ld1 '(a b c))");
		equal(l,"(setchk? a '(a b c) eq?)", T);
		equal(l,"(setchk? b '(a b c) eq?)", T);
		lperr(l,"(lset-diff+intersection! eq? '(a b) '(c . d))");
		lperr(l,"(lset-diff+intersection! eq? '(a b) 'c)");
		lperr(l,"(lset-diff+intersection! 1 '(a b) '(c d))");
	}

}
