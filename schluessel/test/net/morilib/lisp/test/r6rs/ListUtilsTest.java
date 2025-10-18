/*
 * Copyright 2009-2010 Yuichiro Moriguchi
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

import net.morilib.lisp.Nil;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/23
 */
public class ListUtilsTest extends TCSubr {

	public void testFind() {
		Scheme l = Scheme.newInstance();

		equal(l,"(find even? '(3 1 4 1 5 9))", newZ(4));
		equal(l,"(find even? '(3 1 1 5 9))", F);
		equal(l,"(find even? '())", F);
		equal(l,"(find even? '#0=(3 1 4 . #0#))", newZ(4));
		lperr(l,"(find even? '#0=(3 1 . #0#))");
		lperr(l,"(find 1 '(1 2 3))");
	}

	public void testForAll() {
		Scheme l = Scheme.newInstance();

		equal(l,"(for-all odd? '(3 1 4 1 5 9))", F);
		equal(l,"(for-all even? '(3 1 4 1 5 9 . 2))", F);
		equal(l,"(for-all odd? '(3 1 1 5 9))", T);
		equal(l,"(for-all odd? '())", T);
		equal(l,"(for-all odd? '#0=(3 1 4 . #0#))", F);
		equal(l,"(for-all > '(3 1 4 1 5) '(2 7 1 8 2))", F);
		equal(l,"(for-all > '(1 2 3 4 5) '(0 1 2 3 4))", T);
		equal(l,"(for-all > '(3 1 4 1 5 9) '(2 7 1 8 2))", F);
		equal(l,"(for-all even?)", T);
		lperr(l,"(for-all > '#0=(3 4 5 . #0#) '#1=(0 1 . #1#))");
		lperr(l,"(for-all odd? '#0=(3 1 . #0#))");
		lperr(l,"(for-all 1 '(1 2 3))");
		lperr(l,"(for-all even? '(2 4 14 . 9))");

//		lperr(l,"(for-all > '(1 2 3 4 5 6) '(0 1 2 3 4))");
		equal(l,"(for-all > '(3 4 5 6 7) '#0=(0 1 . #0#))", T);
		equal(l,"(for-all < '#0=(0 1 . #0#) '(3 4 5 6 7))", T);
	}

	public void testExists() {
		Scheme l = Scheme.newInstance();

		equal(l,"(exists even? '(3 1 4 1 5 9))", T);
		equal(l,"(exists even? '(3 1 1 5 9))", F);
		equal(l,"(exists even? '())", F);
		equal(l,"(exists even? '#0=(3 1 4 . #0#))", T);
		equal(l,"(exists integer? '(a 3 b 2.7))", T);
		equal(l,"(exists integer? '(a 3.1 b 2.7))", F);
		equal(l,"(exists < '(3 1 4 1 5) '(2 7 1 8 2))", T);
		equal(l,"(exists < '(1 2 3 4 5) '(0 1 2 3 4))", F);
		equal(l,"(exists < '(3 1 4 1 5 9) '(2 7 1 8 2))", T);
		equal(l,"(exists even?)", F);
		lperr(l,"(exists < '#0=(3 4 5 . #0#) '#1=(0 1 . #1#))");
		lperr(l,"(exists even? '#0=(3 1 . #0#))");
		lperr(l,"(exists 1 '(1 2 3))");
		lperr(l,"(exists even? '(3 1 1 5 9 . 2))");

//		lperr(l,"(exists < '(1 2 3 4 5 6) '(0 1 2 3 4))");
		equal(l,"(exists < '(3 4 5 6 7) '#0=(0 1 . #0#))", F);
		equal(l,"(exists > '#0=(0 1 . #0#) '(3 4 5 6 7))", F);
	}

	public void testFilter() {
		Scheme l = Scheme.newInstance();

		equal(l,"(filter even? '(0 7 8 8 43 -4))", list(0, 8, 8, -4));
		equal(l,"(filter even? '(7 9 11 13))", Nil.NIL);
		equal(l,"(filter even? '())", Nil.NIL);
		lperr(l,"(filter 1 '(1 2))");
		lperr(l,"(filter even? '(1 2 . 3))");
		lperr(l,"(filter even? 1)");
	}

	public void testPartition() {
		Scheme l = Scheme.newInstance();

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
	}

	public void testFoldLeft() {
		Scheme l = Scheme.newInstance();

		equal(l,"(fold-left + 0 '(1 2 3 4 5))", newZ(15));
		equal(l,"(fold-left cons '() '(1 2 3))", list(3, 2, 1));
		equal(l,"(fold-left cons '(1 2) '(3 4 5))", list(5, 4, 3, 1, 2));
		equal(l,"(fold-left (lambda (x count)" +
				"        (if (symbol? x) (+ count 1) count))" +
				"      0" +
				"      '(1 2 3 a b 4 c d))", newZ(4));
		equal(l,"(fold-left (lambda (s max-len)" +
				"        (max max-len (string-length s)))" +
				"      0" +
				"      '(\"aaa\" \"bbbbbb\" \"c\"))", newZ(6));
		equal(l,"(fold-left cons* '() '(a b c) '(1 2 3))",
				list(sym("c"), 3, sym("b"), 2, sym("a"), 1));

		equal(l,"(fold-left + 0 '())", newZ(0));
		lperr(l,"(fold-left 1 0 '(1 2 3 4 5))");
		lperr(l,"(fold-left + 0 '(1 2 3 4 . 5))");
		lperr(l,"(fold-left + 0 1)");
//		lperr(l,"(fold-left cons* '() '(a b c) '#0=(1 2 . #0#))");
	}

	public void testFoldRight() {
		Scheme l = Scheme.newInstance();

		equal(l,"(fold-right + 0 '(1 2 3 4 5))", newZ(15));
		equal(l,"(fold-right cons '() '(1 2 3))", list(1, 2, 3));
		equal(l,"(fold-right (lambda (x l)" +
				"              (if (even? x) (cons x l) l))" +
				"  '() '(1 2 3 4 5 6 7 8))",
				list(2, 4, 6, 8));
		equal(l,"(fold-right cons* '() '(a b c) '(1 2 3))",
				list(sym("a"), 1, sym("b"), 2, sym("c"), 3));

		equal(l,"(fold-right + 0 '())", newZ(0));
		lperr(l,"(fold-right 1 0 '(1 2 3 4 5))");
		lperr(l,"(fold-right + 0 '(1 2 3 4 . 5))");
		lperr(l,"(fold-right + 0 1)");
//		lperr(l,"(fold-right cons* '() '(a b c) '#0=(1 2 . #0#))");
	}

	public void testRemq() {
		Scheme l = Scheme.newInstance();

		equal(l, "(remq 'c '(a b c d c))", list(sym("a"), sym("b"), sym("d")));
		equal(l, "(remq 'z '(a b d))", list(sym("a"), sym("b"), sym("d")));
//		equal(l, "(remq 'b '(b c b . d))", cons(sym("c"), sym("d")));
//		eq   (l, "(remq '(a b . c) '(1 2 (a b . c) c))", F);
		lperr(l, "(remq 'c)");
	}

	public void testRemv() {
		Scheme l = Scheme.newInstance();

		equal(l, "(remv 2 '(1 2 3 4))", list(1, 3, 4));
		equal(l, "(remv 'z '(1 2 3 4))", list(1, 2, 3, 4));
//		equal(l, "(remv 3 '(1 2 3 . 4))", listDot(4, 1, 2));
		equal(l, "(remv 3 '(1 2 3 4))", list(1, 2, 4));
//		eq   (l, "(remv '(a b . c) '(1 2 (a b . c) c))", F);
		lperr(l, "(remv 'c)");
		lperr(l, "(remv 'c 1)");
	}

	public void testMemp() {
		Scheme l = Scheme.newInstance();

		equal(l, "(memp (lambda (x) (eq? 'c x)) '(a b c d))", list(sym("c"), sym("d")));
		eq   (l, "(memp (lambda (x) (eq? 'z x)) '(a b c d))", F);
		equal(l, "(memp (lambda (x) (eq? 'c x)) '(a b c . d))", cons(sym("c"), sym("d")));
		lperr(l, "(memp (lambda (x) (eq? 'c x)))");
		lperr(l, "(memp (lambda (x) (eq? 'c x)) 1)");
	}

	public void testAssq() {
		Scheme l = Scheme.newInstance();

		l.input("(define x '((a . b) (c . d)))");
		l.input("(define y '((1 . b) (2 . d)))");
		l.input("(define z '(((1 2 . 3) . b) ((4 5 . 6) . d)))");
		equal(l, "(assp (lambda (x) (eq? 'a x)) x)", cons(sym("a"), sym("b")));
		equal(l, "(assp (lambda (x) (eq? 'd x)) x)", F);
		lperr(l, "(assp (lambda (x) (eq? 'a x)) '(a b c d))");
		lperr(l, "(assp (lambda (x) (eq? 'c x)))");
		lperr(l, "(assp (lambda (x) (eq? 'c x)) 1)");
	}

	public void testConsStar() {
		Scheme l = Scheme.newInstance();

		equal(l,"(cons* 1 2)", cons(1, 2));
		equal(l,"(cons* 1 2 3 4)", listDot(4, 1, 2, 3));
		equal(l,"(cons* 1 2 3 '())", list(1, 2, 3));
		equal(l,"(cons* 1)", newZ(1));
		equal(l,"(cons*)", Nil.NIL);
	}

}
