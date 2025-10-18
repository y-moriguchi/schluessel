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

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/15
 */
public class R6RSBaseLibraryTest extends TCSubr {

	public void testDivMod() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(div  123  10)", 12);
		eqi  (l,"(mod  123  10)", 3);
		eqi  (l,"(div  123 -10)", -12);
		eqi  (l,"(mod  123 -10)", 3);
		eqi  (l,"(div -123  10)", -13);
		eqi  (l,"(mod -123  10)", 7);
		eqi  (l,"(div -123 -10)", 13);
		eqi  (l,"(mod -123 -10)", 7);
		eqi  (l,"(div-and-mod  123  10)", 12, 3);
		eqi  (l,"(div-and-mod  123 -10)", -12, 3);
		eqi  (l,"(div-and-mod -123  10)", -13, 7);
		eqi  (l,"(div-and-mod -123 -10)", 13, 7);

		eqi  (l,"(div0  123  10)", 12);
		eqi  (l,"(mod0  123  10)", 3);
		eqi  (l,"(div0  123 -10)", -12);
		eqi  (l,"(mod0  123 -10)", 3);
		eqi  (l,"(div0 -123  10)", -12);
		eqi  (l,"(mod0 -123  10)", -3);
		eqi  (l,"(div0 -123 -10)", 12);
		eqi  (l,"(mod0 -123 -10)", -3);
		eqi  (l,"(div0-and-mod0  123  10)", 12, 3);
		eqi  (l,"(div0-and-mod0  123 -10)", -12, 3);
		eqi  (l,"(div0-and-mod0 -123  10)", -12, -3);
		eqi  (l,"(div0-and-mod0 -123 -10)", 12, -3);
	}

	public void testRealValued() {
		Scheme l = Scheme.newInstance();

		eq   (l,"(real-valued? +nan.0)", T);
//		eq   (l,"(real-valued? +nan.0+0i)", T);
		eq   (l,"(real-valued? -inf.0)", T);
		eq   (l,"(real-valued? 3)", T);
		eq   (l,"(real-valued? -2.5+0.0i)", T);
		eq   (l,"(real-valued? -2.5+0i)", T);
		eq   (l,"(real-valued? -2.5)", T);
		eq   (l,"(real-valued? #e1e10)", T);

		eq   (l,"(rational-valued? +nan.0)", F);
		eq   (l,"(rational-valued? -inf.0)", F);
		eq   (l,"(rational-valued? 6/10)", T);
		eq   (l,"(rational-valued? 6/10+0.0i)", T);
		eq   (l,"(rational-valued? 6/10+0i)", T);
		eq   (l,"(rational-valued? 6/3)", T);

		eq   (l,"(integer-valued? 3+0i)", T);
		eq   (l,"(integer-valued? 3+0.0i)", T);
		eq   (l,"(integer-valued? 3.0)", T);
		eq   (l,"(integer-valued? 3.0+0.0i)", T);
		eq   (l,"(integer-valued? 8/4)", T);
	}

	public void testExactInexact() {
		Scheme l = Scheme.newInstance();

		eq   (l,"(exact? 5)", T);
		eq   (l,"(inexact? +inf.0)", T);
	}

	public void testRealPredicate() {
		Scheme l = Scheme.newInstance();

		eq   (l,"(zero? +0.0)", T);
		eq   (l,"(zero? -0.0)", T);
		eq   (l,"(zero? +nan.0)", F);
		eq   (l,"(positive? +inf.0)", T);
		eq   (l,"(negative? -inf.0)", T);
		eq   (l,"(positive? +nan.0)", F);
		eq   (l,"(negative? +nan.0)", F);
		eq   (l,"(finite? +inf.0)", F);
		eq   (l,"(finite? 5)", T);
		eq   (l,"(finite? 5.0)", T);
		eq   (l,"(infinite? 5.0)", F);
		eq   (l,"(infinite? +inf.0)", T);
	}

	public void testExactIntegerSqrt() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(exact-integer-sqrt 0)", 0, 0);
		eqi  (l,"(exact-integer-sqrt 1)", 1, 0);
		eqi  (l,"(exact-integer-sqrt 2)", 1, 1);
		eqi  (l,"(exact-integer-sqrt 4)", 2, 0);
		eqi  (l,"(exact-integer-sqrt 5)", 2, 1);
		eqi  (l,"(exact-integer-sqrt 765)", 27, 36);
		eqi  (l,"(exact-integer-sqrt 841)", 29, 0);
		eqi  (l,"(exact-integer-sqrt 876)", 29, 35);
		lperr(l,"(exact-integer-sqrt -1)");
		lperr(l,"(exact-integer-sqrt +inf.0)");
		lperr(l,"(exact-integer-sqrt 1+1i)");
	}

	public void testIsBooleanEq() {
		Scheme l = Scheme.newInstance();

		eq   (l,"(boolean=?)", T);
		eq   (l,"(boolean=? #t)", T);
		eq   (l,"(boolean=? #f)", T);
		eq   (l,"(boolean=? #t #t)", T);
		eq   (l,"(boolean=? #f #f)", T);
		eq   (l,"(boolean=? #t #t #t #t)", T);
		eq   (l,"(boolean=? #f #f #f #f)", T);
		eq   (l,"(boolean=? 72 83 91 #t)", T);
		eq   (l,"(boolean=? #t #f)", F);
		eq   (l,"(boolean=? #f #t)", F);
		eq   (l,"(boolean=? #t 72 #t #f)", F);
		eq   (l,"(boolean=? #f #f #f 72)", F);
	}

	public void testIsSymbolEq() {
		Scheme l = Scheme.newInstance();

		eq   (l,"(symbol=?)", T);
		eq   (l,"(symbol=? 'a)", T);
		eq   (l,"(symbol=? 'a 'a)", T);
		eq   (l,"(symbol=? 'a 'a 'a)", T);
		eq   (l,"(symbol=? 'a 'a 'b)", F);
		eq   (l,"(symbol=? 'b 'a 'a)", F);
	}

	public void testStringForEach() {
		Scheme l = Scheme.newInstance();

		l.input("(define r '())");
		l.input("(string-for-each" +
				"  (lambda (a b c)" +
				"    (set! r (cons (string a b c) r)))" +
				"  \"abcd\" \"efgh\" \"ijkl\")");
		equal(l,"r", list("dhl", "cgk", "bfj", "aei"));

		l.input("(define r '())");
		l.input("(string-for-each" +
				"  (lambda (a b c)" +
				"    (set! r (cons (string a b c) r)))" +
				"  \"abcd\" \"efg\" \"ij\")");
		equal(l,"r", list("bfj", "aei"));
	}

	public void testVectorMap() {
		Scheme l = Scheme.newInstance();

		equal(l,"(vector-map" +
				"  (lambda (a b c) (string a b c))" +
				"  (list->vector (string->list \"abcd\"))" +
				"  (list->vector (string->list \"efgh\"))" +
				"  (list->vector (string->list \"ijkl\")))",
				list("aei", "bfj", "cgk", "dhl"));
		equal(l,"(vector-map" +
				"  (lambda (a b c) (string a b c))" +
				"  (list->vector (string->list \"abcd\"))" +
				"  (list->vector (string->list \"efg\"))" +
				"  (list->vector (string->list \"ij\")))",
				list("aei", "bfj"));
	}

	public void testVectorForEach() {
		Scheme l = Scheme.newInstance();

		l.input("(define r '())");
		l.input("(vector-for-each" +
				"  (lambda (a b c)" +
				"    (set! r (cons (string a b c) r)))" +
				"  (list->vector (string->list \"abcd\"))" +
				"  (list->vector (string->list \"efgh\"))" +
				"  (list->vector (string->list \"ijkl\")))");
		equal(l,"r", list("dhl", "cgk", "bfj", "aei"));

		l.input("(define r '())");
		l.input("(vector-for-each" +
				"  (lambda (a b c)" +
				"    (set! r (cons (string a b c) r)))" +
				"  (list->vector (string->list \"abcd\"))" +
				"  (list->vector (string->list \"efg\"))" +
				"  (list->vector (string->list \"ij\")))");
		equal(l,"r", list("bfj", "aei"));
	}

}
