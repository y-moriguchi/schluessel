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
package net.morilib.lisp.test.srfi;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/24
 */
public class SRFI67Test extends TCSubr {

	public void testBooleanCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(boolean-compare #t #f)", 1);
		eqi  (l,"(boolean-compare #f #t)", -1);
		eqi  (l,"(boolean-compare #t #t)", 0);
		eqi  (l,"(boolean-compare #f #f)", 0);
		lperr(l,"(boolean-compare 1 #f)");
	}

	public void testCharCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(char-compare #\\a #\\b)", -1);
		eqi  (l,"(char-compare #\\b #\\c)", -1);
		eqi  (l,"(char-compare #\\b #\\a)", 1);
		eqi  (l,"(char-compare #\\a #\\a)", 0);
		eqi  (l,"(char-compare #\\a #\\c)", -1);
		eqi  (l,"(char-compare #\\B #\\a)", -1);
		lperr(l,"(boolean-compare 1 #\\a)");
	}

	public void testCharCompareCi() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(char-compare-ci #\\a #\\b)", -1);
		eqi  (l,"(char-compare-ci #\\b #\\c)", -1);
		eqi  (l,"(char-compare-ci #\\b #\\a)", 1);
		eqi  (l,"(char-compare-ci #\\a #\\a)", 0);
		eqi  (l,"(char-compare-ci #\\a #\\c)", -1);
		eqi  (l,"(char-compare-ci #\\B #\\a)", 1);
		lperr(l,"(boolean-compare 1 #\\a)");
	}

	public void testStringCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(string-compare \"abcd\" \"bcd\")", -1);
		eqi  (l,"(string-compare \"bcd\" \"cde\")", -1);
		eqi  (l,"(string-compare \"abcd\" \"abc\")", 1);
		eqi  (l,"(string-compare \"bcd\" \"abcd\")", 1);
		eqi  (l,"(string-compare \"abcd\" \"cde\")", -1);
		eqi  (l,"(string-compare \"abcd\" \"abcd\")", 0);
		eqi  (l,"(string-compare \"Bcd\" \"abcd\")", -1);
		lperr(l,"(string-compare 1 \"abcd\")");
	}

	public void testStringCompareCi() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(string-compare-ci \"abcd\" \"bcd\")", -1);
		eqi  (l,"(string-compare-ci \"bcd\" \"cde\")", -1);
		eqi  (l,"(string-compare-ci \"abcd\" \"abc\")", 1);
		eqi  (l,"(string-compare-ci \"bcd\" \"abcd\")", 1);
		eqi  (l,"(string-compare-ci \"abcd\" \"cde\")", -1);
		eqi  (l,"(string-compare-ci \"abcd\" \"abcd\")", 0);
		eqi  (l,"(string-compare-ci \"Bcd\" \"abcd\")", 1);
		lperr(l,"(string-compare-ci 1 \"abcd\")");
	}

	public void testSymbolCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(symbol-compare 'abcd 'bcd)", -1);
		eqi  (l,"(symbol-compare 'bcd 'cde)", -1);
		eqi  (l,"(symbol-compare 'abcd 'abc)", 1);
		eqi  (l,"(symbol-compare 'bcd 'abcd)", 1);
		eqi  (l,"(symbol-compare 'abcd 'cde)", -1);
		eqi  (l,"(symbol-compare 'abcd 'abcd)", 0);
		eqi  (l,"(symbol-compare 'Bcd 'abcd)", -1);
		lperr(l,"(symbol-compare 1 'abcd)");
	}

	public void testIntegerCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(integer-compare 1 2)", -1);
		eqi  (l,"(integer-compare 2 3)", -1);
		eqi  (l,"(integer-compare 1 1)", 0);
		eqi  (l,"(integer-compare 1 3)", -1);
		eqi  (l,"(integer-compare 2 1)", 1);
		lperr(l,"(integer-compare 1/2 1)");
	}

	public void testRationlCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(rational-compare 1/3 2/3)", -1);
		eqi  (l,"(rational-compare 2/3 1)", -1);
		eqi  (l,"(rational-compare 1/3 1/3)", 0);
		eqi  (l,"(rational-compare 1/3 1)", -1);
		eqi  (l,"(rational-compare 2/3 1/3)", 1);
		lperr(l,"(rational-compare 0.5 1)");
	}

	public void testRealCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(real-compare 0.3 0.6)", -1);
		eqi  (l,"(real-compare 0.6 1)", -1);
		eqi  (l,"(real-compare 0.3 0.3)", 0);
		eqi  (l,"(real-compare 0.3 1)", -1);
		eqi  (l,"(real-compare 0.6 0.3)", 1);
		lperr(l,"(real-compare 1+i 1)");
	}

	public void testComplexCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(complex-compare 1+i 2-i)", -1);
		eqi  (l,"(complex-compare 2-i 2+i)", -1);
		eqi  (l,"(complex-compare 1+i 1+i)", 0);
		eqi  (l,"(complex-compare 1+i 2+i)", -1);
		eqi  (l,"(complex-compare 2-i 1+i)", 1);
		lperr(l,"(complex-compare 1+i 'a)");
	}

	public void testNumberCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(number-compare 1+i 2-i)", -1);
		eqi  (l,"(number-compare 2-i 2+i)", -1);
		eqi  (l,"(number-compare 1+i 1+i)", 0);
		eqi  (l,"(number-compare 1+i 2+i)", -1);
		eqi  (l,"(number-compare 2-i 1+i)", 1);
		lperr(l,"(number-compare 1+i 'a)");
	}

	public void testVectorCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(vector-compare real-compare #(2 1) #(1 2 3))", -1);
		eqi  (l,"(vector-compare real-compare #(1 2 3) #(2 2 3))", -1);
		eqi  (l,"(vector-compare real-compare #(2 1) #(3))", 1);
		eqi  (l,"(vector-compare real-compare #(1 2 3) #(2 1))", 1);
		eqi  (l,"(vector-compare real-compare #(2 1) #(2 2 3))", -1);
		eqi  (l,"(vector-compare #(2 1) #(1 2 3))", -1);
		eqi  (l,"(vector-compare real-compare #(2 1) #(1 2 3)" +
				" vector-length vector-ref)", -1);
		eqi  (l,"(vector-compare real-compare #(1 2 3) #(2 2 3)" +
				" vector-length vector-ref)", -1);
		eqi  (l,"(vector-compare real-compare #(2 1) #(3)" +
				" vector-length vector-ref)", 1);
		eqi  (l,"(vector-compare real-compare #(1 2 3) #(2 1)" +
				" vector-length vector-ref)", 1);
		eqi  (l,"(vector-compare real-compare #(2 1) #(2 2 3)" +
				" vector-length vector-ref)", -1);
		eqi  (l,"(vector-compare #(2 1) #(1 2 3))", -1);
		lperr(l,"(vector-compare #(2 1) 'a)");
		lperr(l,"(vector-compare real-compare #(2 1))");
		lperr(l,"(vector-compare real-compare #(2 1) #(2) vector-compare)");
	}

	public void testVectorCompareAsList() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(vector-compare-as-list real-compare #(2 1) #(1 2 3))", 1);
		eqi  (l,"(vector-compare-as-list real-compare #(1 2 3) #(2 2 3))", -1);
		eqi  (l,"(vector-compare-as-list real-compare #(2 2 3) #(3))", -1);
		eqi  (l,"(vector-compare-as-list real-compare #(2 1) #(3))", -1);
		eqi  (l,"(vector-compare-as-list real-compare #(1 2 3) #(2 1))", -1);
		eqi  (l,"(vector-compare-as-list real-compare #(2 1) #(2 2 3))", -1);
		eqi  (l,"(vector-compare-as-list real-compare #(1 2 3) #(3))", -1);
		eqi  (l,"(vector-compare-as-list #(2 1) #(1 2 3))", 1);
		eqi  (l,"(vector-compare-as-list real-compare #(2 1) #(1 2 3)" +
				" vector-length vector-ref)", 1);
		eqi  (l,"(vector-compare-as-list real-compare #(1 2 3) #(2 2 3)" +
				" vector-length vector-ref)", -1);
		eqi  (l,"(vector-compare-as-list real-compare #(2 2 3) #(3)" +
				" vector-length vector-ref)", -1);
		eqi  (l,"(vector-compare-as-list real-compare #(2 1) #(3)" +
				" vector-length vector-ref)", -1);
		eqi  (l,"(vector-compare-as-list real-compare #(1 2 3) #(2 1)" +
				" vector-length vector-ref)", -1);
		eqi  (l,"(vector-compare-as-list real-compare #(2 1) #(2 2 3)" +
				" vector-length vector-ref)", -1);
		eqi  (l,"(vector-compare-as-list real-compare #(1 2 3) #(3)" +
				" vector-length vector-ref)", -1);
		eqi  (l,"(vector-compare-as-list #(2 1) #(1 2 3)" +
				" vector-length vector-ref)", 1);
		lperr(l,"(vector-compare-as-list #(2 1) 'a)");
		lperr(l,"(vector-compare-as-list real-compare #(2 1))");
		lperr(l,"(vector-compare-as-list real-compare #(2 1) #(2) vector-compare)");
	}

	public void testListCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(list-compare real-compare '(2 1) '(1 2 3))", 1);
		eqi  (l,"(list-compare real-compare '(1 2 3) '(2 2 3))", -1);
		eqi  (l,"(list-compare real-compare '(2 2 3) '(3))", -1);
		eqi  (l,"(list-compare real-compare '(2 1) '(3))", -1);
		eqi  (l,"(list-compare real-compare '(1 2 3) '(2 1))", -1);
		eqi  (l,"(list-compare real-compare '(2 1) '(2 2 3))", -1);
		eqi  (l,"(list-compare real-compare '(1 2 3) '(3))", -1);
		eqi  (l,"(list-compare '(2 1) '(1 2 3))", 1);
		eqi  (l,"(list-compare real-compare '(2 1) '(1 2 3)" +
				" null? car cdr)", 1);
		eqi  (l,"(list-compare real-compare '(1 2 3) '(2 2 3)" +
				" null? car cdr)", -1);
		eqi  (l,"(list-compare real-compare '(2 2 3) '(3)" +
				" null? car cdr)", -1);
		eqi  (l,"(list-compare real-compare '(2 1) '(3)" +
				" null? car cdr)", -1);
		eqi  (l,"(list-compare real-compare '(1 2 3) '(2 1)" +
				" null? car cdr)", -1);
		eqi  (l,"(list-compare real-compare '(2 1) '(2 2 3)" +
				" null? car cdr)", -1);
		eqi  (l,"(list-compare real-compare '(1 2 3) '(3)" +
				" null? car cdr)", -1);
		eqi  (l,"(list-compare '(2 1) '(1 2 3)" +
				" null? car cdr)", 1);
		lperr(l,"(list-compare '(2 1) 'a)");
		lperr(l,"(list-compare '(2 1) '(2 . 3))");
		lperr(l,"(list-compare real-compare #(2 1))");
		lperr(l,"(list-compare '(2 1) '(2 . 3) null? car)");
	}

	public void testListCompareAsVector() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(list-compare-as-vector real-compare '(2 1) '(1 2 3))", -1);
		eqi  (l,"(list-compare-as-vector real-compare '(1 2 3) '(2 2 3))", -1);
		eqi  (l,"(list-compare-as-vector real-compare '(2 1) '(3))", 1);
		eqi  (l,"(list-compare-as-vector real-compare '(1 2 3) '(2 1))", 1);
		eqi  (l,"(list-compare-as-vector real-compare '(2 1) '(2 2 3))", -1);
		eqi  (l,"(list-compare-as-vector '(2 1) '(1 2 3))", -1);
		eqi  (l,"(list-compare-as-vector real-compare '(2 1) '(1 2 3)" +
				" null? car cdr)", -1);
		eqi  (l,"(list-compare-as-vector real-compare '(1 2 3) '(2 2 3)" +
				" null? car cdr)", -1);
		eqi  (l,"(list-compare-as-vector real-compare '(2 1) '(3)" +
				" null? car cdr)", 1);
		eqi  (l,"(list-compare-as-vector real-compare '(1 2 3) '(2 1)" +
				" null? car cdr)", 1);
		eqi  (l,"(list-compare-as-vector real-compare '(2 1) '(2 2 3)" +
				" null? car cdr)", -1);
		eqi  (l,"(list-compare-as-vector '(2 1) '(1 2 3)" +
				" null? car cdr)", -1);
		lperr(l,"(list-compare-as-vector '(2 1) 'a)");
		lperr(l,"(list-compare-as-vector '(2 1) '(2 . 3))");
		lperr(l,"(list-compare-as-vector real-compare #(2 1))");
		lperr(l,"(list-compare-as-vector '(2 1) '(2 . 3) null? car)");
	}

	public void testPairCompareCar() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define cmp (pair-compare-car real-compare))");
		eqi  (l,"(cmp '(1 . 2) '(2 . 1))", -1);
		lperr(l,"(cmp 2 '(2 . 3))");
		lperr(l,"(cmp '(2 . 3) 2)");
	}

	public void testPairCompareCdr() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define cmp (pair-compare-cdr real-compare))");
		eqi  (l,"(cmp '(1 . 2) '(2 . 1))", 1);
		lperr(l,"(cmp 2 '(2 . 3))");
		lperr(l,"(cmp '(2 . 3) 2)");
	}

	public void testPairCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(pair-compare real-compare char-compare" +
				" '(1 . #\\b) '(2 . #\\a))", -1);
		eqi  (l,"(pair-compare real-compare char-compare" +
				" '(1 . #\\b) '(1 . #\\a))", 1);
		eqi  (l,"(pair-compare real-compare char-compare" +
				" '(1 . #\\a) '(1 . #\\a))", 0);
		eqi  (l,"(pair-compare real-compare '(1 2 3) '(2 3))", -1);
		eqi  (l,"(pair-compare real-compare '(1 2 3) '(1 2 3))", 0);
		eqi  (l,"(pair-compare real-compare '(1 2 . 4) '(1 2 . 3))", 1);
		eqi  (l,"(pair-compare real-compare '(1 2 . 3) '(1 2 . 3))", 0);
		eqi  (l,"(pair-compare real-compare '() '(1))", -1);
		eqi  (l,"(pair-compare real-compare '() 1)", -1);
		eqi  (l,"(pair-compare real-compare '(1) 2)", -1);
		eqi  (l,"(pair-compare real-compare '(1 2 3) '(1 2 . 3))", -1);
		eqi  (l,"(pair-compare real-compare '() '())", 0);
		eqi  (l,"(pair-compare real-compare 2 3)", -1);
		eqi  (l,"(pair-compare real-compare 2 2)", 0);
		eqi  (l,"(pair-compare '(1 2 3) '(2 3))", -1);
		lperr(l,"(pair-compare real-compare '(1))");
	}

	public void testDefaultCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(default-compare '() '())", 0);
		eqi  (l,"(default-compare '() '(1))", -1);
		eqi  (l,"(default-compare '(1) '())", 1);
		eqi  (l,"(default-compare '(1) '(2 1))", -1);
		eqi  (l,"(default-compare '(2 . 3) '(2 . 1))", 1);
		eqi  (l,"(default-compare '(2 . 3) '(2 . 3))", 0);
		eqi  (l,"(default-compare '(1) #f)", -1);
		eqi  (l,"(default-compare #f '(1))", 1);
		eqi  (l,"(default-compare #f #f)", 0);
		eqi  (l,"(default-compare #f #\\a)", -1);
		eqi  (l,"(default-compare #\\a #f)", 1);
		eqi  (l,"(default-compare #\\a #\\a)", 0);
		eqi  (l,"(default-compare #\\a \"a\")", -1);
		eqi  (l,"(default-compare \"a\" #\\a)", 1);
		eqi  (l,"(default-compare \"a\" \"a\")", 0);
		eqi  (l,"(default-compare \"a\" 'a)", -1);
		eqi  (l,"(default-compare 'a \"a\")", 1);
		eqi  (l,"(default-compare 'a 'a)", 0);
		eqi  (l,"(default-compare 'a 1)", -1);
		eqi  (l,"(default-compare 1 'a)", 1);
		eqi  (l,"(default-compare 1 1)", 0);
		eqi  (l,"(default-compare 1 #(1 2))", -1);
		eqi  (l,"(default-compare #(1 2) 1)", 1);
		eqi  (l,"(default-compare #(1 2) #(1 2))", 0);
	}

	public void testRefineCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(refine-compare 0 1)", 1);
		eqi  (l,"(refine-compare 1 -1 1)", 1);
		eqi  (l,"(refine-compare -1 1 1)", -1);
		eqi  (l,"(refine-compare 0 0 -1 1)", -1);
		eqi  (l,"(refine-compare 0 0 1 -1)", 1);
		eqi  (l,"(refine-compare 0 0 0 0)", 0);
		eqi  (l,"(refine-compare)", 0);
		lperr(l,"(refine-compare 2)");
	}

	public void testSelectCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(select-compare 1 1 (integer? 0 0))", 0);
		eqi  (l,"(select-compare 1 #f (integer? 0))", -1);
		eqi  (l,"(select-compare #f 1 (integer? 0))", 1);
		eqi  (l,"(select-compare #f #f (integer? 0))", 0);
		eqi  (l,"(select-compare #f #f (integer? 0) (else 0 1))", 1);
		eqi  (l,"(select-compare 1 1 (char? 1) (integer? 0))", 0);
	}

	public void testCondCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(cond-compare ((#t #f) 0 1))", -1);
		eqi  (l,"(cond-compare ((#f #t) 0 -1))", 1);
		eqi  (l,"(cond-compare ((#t #t) 0 1))", 1);
		eqi  (l,"(cond-compare ((#f #f) 0 -1) (else 1))", 1);
		eqi  (l,"(cond-compare ((#f #f) 0 -1) (else 0 1))", 1);
		eqi  (l,"(cond-compare ((#f #f) 0) ((#f #f) -1))", 0);
		eqi  (l,"(cond-compare ((#f #f) 0) ((#t #t) -1))", -1);
	}

	public void testIf3() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(if3 -1 10 20 30)", 10);
		eqi  (l,"(if3  0 10 20 30)", 20);
		eqi  (l,"(if3  1 10 20 30)", 30);
		lperr(l,"(if3  2 19 29 39)");
	}

	public void testIsIfRel() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(if=?      -1 10 20)", 20);
		eqi  (l,"(if=?       0 10 20)", 10);
		eqi  (l,"(if=?       1 10 20)", 20);
		eqi  (l,"(if<?      -1 10 20)", 10);
		eqi  (l,"(if<?       0 10 20)", 20);
		eqi  (l,"(if<?       1 10 20)", 20);
		eqi  (l,"(if>?      -1 10 20)", 20);
		eqi  (l,"(if>?       0 10 20)", 20);
		eqi  (l,"(if>?       1 10 20)", 10);
		eqi  (l,"(if<=?     -1 10 20)", 10);
		eqi  (l,"(if<=?      0 10 20)", 10);
		eqi  (l,"(if<=?      1 10 20)", 20);
		eqi  (l,"(if>=?     -1 10 20)", 20);
		eqi  (l,"(if>=?      0 10 20)", 10);
		eqi  (l,"(if>=?      1 10 20)", 10);
		eqi  (l,"(if-not=?  -1 10 20)", 10);
		eqi  (l,"(if-not=?   0 10 20)", 20);
		eqi  (l,"(if-not=?   1 10 20)", 10);
		eqi  (l,"(if=?       0 10)", 10);
		eqi  (l,"(if<?      -1 10)", 10);
		eqi  (l,"(if>?       1 10)", 10);
		eqi  (l,"(if<=?     -1 10)", 10);
		eqi  (l,"(if>=?      1 10)", 10);
		eqi  (l,"(if-not=?   1 10)", 10);
	}

	public void testIsRel() {
		Scheme l = Scheme.newInstance();

		equal(l,"(=?     real-compare 1 2)", F);
		equal(l,"(=?     real-compare 1 1)", T);
		equal(l,"(=?     real-compare 1 0)", F);
		equal(l,"(<?     real-compare 1 2)", T);
		equal(l,"(<?     real-compare 1 1)", F);
		equal(l,"(<?     real-compare 1 0)", F);
		equal(l,"(>?     real-compare 1 2)", F);
		equal(l,"(>?     real-compare 1 1)", F);
		equal(l,"(>?     real-compare 1 0)", T);
		equal(l,"(<=?    real-compare 1 2)", T);
		equal(l,"(<=?    real-compare 1 1)", T);
		equal(l,"(<=?    real-compare 1 0)", F);
		equal(l,"(>=?    real-compare 1 2)", F);
		equal(l,"(>=?    real-compare 1 1)", T);
		equal(l,"(>=?    real-compare 1 0)", T);
		equal(l,"(not=?  real-compare 1 2)", T);
		equal(l,"(not=?  real-compare 1 1)", F);
		equal(l,"(not=?  real-compare 1 0)", T);

		equal(l,"(=?     1 2)", F);
		equal(l,"(=?     1 1)", T);
		equal(l,"(=?     1 0)", F);
		equal(l,"(<?     1 2)", T);
		equal(l,"(<?     1 1)", F);
		equal(l,"(<?     1 0)", F);
		equal(l,"(>?     1 2)", F);
		equal(l,"(>?     1 1)", F);
		equal(l,"(>?     1 0)", T);
		equal(l,"(<=?    1 2)", T);
		equal(l,"(<=?    1 1)", T);
		equal(l,"(<=?    1 0)", F);
		equal(l,"(>=?    1 2)", F);
		equal(l,"(>=?    1 1)", T);
		equal(l,"(>=?    1 0)", T);
		equal(l,"(not=?  1 2)", T);
		equal(l,"(not=?  1 1)", F);
		equal(l,"(not=?  1 0)", T);

		l.exec ("(define cmp=?  (=?     real-compare))");
		l.exec ("(define cmp<?  (<?     real-compare))");
		l.exec ("(define cmp>?  (>?     real-compare))");
		l.exec ("(define cmp<=? (<=?    real-compare))");
		l.exec ("(define cmp>=? (>=?    real-compare))");
		l.exec ("(define cmp/=? (not=?  real-compare))");
		equal(l,"(cmp=?   1 2)", F);
		equal(l,"(cmp=?   1 1)", T);
		equal(l,"(cmp=?   1 0)", F);
		equal(l,"(cmp<?   1 2)", T);
		equal(l,"(cmp<?   1 1)", F);
		equal(l,"(cmp<?   1 0)", F);
		equal(l,"(cmp>?   1 2)", F);
		equal(l,"(cmp>?   1 1)", F);
		equal(l,"(cmp>?   1 0)", T);
		equal(l,"(cmp<=?  1 2)", T);
		equal(l,"(cmp<=?  1 1)", T);
		equal(l,"(cmp<=?  1 0)", F);
		equal(l,"(cmp>=?  1 2)", F);
		equal(l,"(cmp>=?  1 1)", T);
		equal(l,"(cmp>=?  1 0)", T);
		equal(l,"(cmp/=?  1 2)", T);
		equal(l,"(cmp/=?  1 1)", F);
		equal(l,"(cmp/=?  1 0)", T);

		l.exec ("(define cmp=  (=?))");
		l.exec ("(define cmp<  (<?))");
		l.exec ("(define cmp>  (>?))");
		l.exec ("(define cmp<= (<=?))");
		l.exec ("(define cmp>= (>=?))");
		l.exec ("(define cmp/= (not=?))");
		equal(l,"(cmp=?   1 2)", F);
		equal(l,"(cmp=?   1 1)", T);
		equal(l,"(cmp=?   1 0)", F);
		equal(l,"(cmp<?   1 2)", T);
		equal(l,"(cmp<?   1 1)", F);
		equal(l,"(cmp<?   1 0)", F);
		equal(l,"(cmp>?   1 2)", F);
		equal(l,"(cmp>?   1 1)", F);
		equal(l,"(cmp>?   1 0)", T);
		equal(l,"(cmp<=?  1 2)", T);
		equal(l,"(cmp<=?  1 1)", T);
		equal(l,"(cmp<=?  1 0)", F);
		equal(l,"(cmp>=?  1 2)", F);
		equal(l,"(cmp>=?  1 1)", T);
		equal(l,"(cmp>=?  1 0)", T);
		equal(l,"(cmp/=?  1 2)", T);
		equal(l,"(cmp/=?  1 1)", F);
		equal(l,"(cmp/=?  1 0)", T);

		lperr(l,"(=? real-compare 1)");
		lperr(l,"(<? real-compare 1)");
		lperr(l,"(>? real-compare 1)");
		lperr(l,"(<=? real-compare 1)");
		lperr(l,"(>=? real-compare 1)");
		lperr(l,"(not=? real-compare 1)");
		lperr(l,"(=? 1)");
		lperr(l,"(<? 1)");
		lperr(l,"(>? 1)");
		lperr(l,"(<=? 1)");
		lperr(l,"(>=? 1)");
		lperr(l,"(not=? 1)");
		lperr(l,"(=? 1 1 1)");
		lperr(l,"(<? 1 1 1)");
		lperr(l,"(>? 1 1 1)");
		lperr(l,"(<=? 1 1 1)");
		lperr(l,"(>=? 1 1 1)");
		lperr(l,"(not=? 1 1 1)");
	}

	public static void testIsRelRel() {
		Scheme l = Scheme.newInstance();

		equal(l,"(</<?   real-compare 1 2 3)", T);
		equal(l,"(</<?   real-compare 2 2 3)", F);
		equal(l,"(</<?   real-compare 3 2 3)", F);
		equal(l,"(</<?   real-compare 1 3 2)", F);
		equal(l,"(</<?   real-compare 1 2 2)", F);
		equal(l,"(</<?   real-compare 2 2 2)", F);
		equal(l,"(</<?   real-compare 3 2 1)", F);
		equal(l,"(</<?   real-compare 3 2 2)", F);
		equal(l,"(</<?   real-compare 2 2 1)", F);
		equal(l,"(<=/<?  real-compare 1 2 3)", T);
		equal(l,"(<=/<?  real-compare 2 2 3)", T);
		equal(l,"(<=/<?  real-compare 3 2 3)", F);
		equal(l,"(<=/<?  real-compare 1 3 2)", F);
		equal(l,"(<=/<?  real-compare 1 2 2)", F);
		equal(l,"(<=/<?  real-compare 2 2 2)", F);
		equal(l,"(<=/<?  real-compare 3 2 1)", F);
		equal(l,"(<=/<?  real-compare 3 2 2)", F);
		equal(l,"(<=/<?  real-compare 2 2 1)", F);
		equal(l,"(</<=?  real-compare 1 2 3)", T);
		equal(l,"(</<=?  real-compare 2 2 3)", F);
		equal(l,"(</<=?  real-compare 3 2 3)", F);
		equal(l,"(</<=?  real-compare 1 3 2)", F);
		equal(l,"(</<=?  real-compare 1 2 2)", T);
		equal(l,"(</<=?  real-compare 2 2 2)", F);
		equal(l,"(</<=?  real-compare 3 2 1)", F);
		equal(l,"(</<=?  real-compare 3 2 2)", F);
		equal(l,"(</<=?  real-compare 2 2 1)", F);
		equal(l,"(<=/<=? real-compare 1 2 3)", T);
		equal(l,"(<=/<=? real-compare 2 2 3)", T);
		equal(l,"(<=/<=? real-compare 3 2 3)", F);
		equal(l,"(<=/<=? real-compare 1 3 2)", F);
		equal(l,"(<=/<=? real-compare 1 2 2)", T);
		equal(l,"(<=/<=? real-compare 2 2 2)", T);
		equal(l,"(<=/<=? real-compare 3 2 1)", F);
		equal(l,"(<=/<=? real-compare 3 2 2)", F);
		equal(l,"(<=/<=? real-compare 2 2 1)", F);

		equal(l,"(>/>?   real-compare 1 2 3)", F);
		equal(l,"(>/>?   real-compare 2 2 3)", F);
		equal(l,"(>/>?   real-compare 3 2 3)", F);
		equal(l,"(>/>?   real-compare 1 3 2)", F);
		equal(l,"(>/>?   real-compare 1 2 2)", F);
		equal(l,"(>/>?   real-compare 2 2 2)", F);
		equal(l,"(>/>?   real-compare 3 2 1)", T);
		equal(l,"(>/>?   real-compare 3 2 2)", F);
		equal(l,"(>/>?   real-compare 2 2 1)", F);
		equal(l,"(>=/>?  real-compare 1 2 3)", F);
		equal(l,"(>=/>?  real-compare 2 2 3)", F);
		equal(l,"(>=/>?  real-compare 3 2 3)", F);
		equal(l,"(>=/>?  real-compare 1 3 2)", F);
		equal(l,"(>=/>?  real-compare 1 2 2)", F);
		equal(l,"(>=/>?  real-compare 2 2 2)", F);
		equal(l,"(>=/>?  real-compare 3 2 1)", T);
		equal(l,"(>=/>?  real-compare 3 2 2)", F);
		equal(l,"(>=/>?  real-compare 2 2 1)", T);
		equal(l,"(>/>=?  real-compare 1 2 3)", F);
		equal(l,"(>/>=?  real-compare 2 2 3)", F);
		equal(l,"(>/>=?  real-compare 3 2 3)", F);
		equal(l,"(>/>=?  real-compare 1 3 2)", F);
		equal(l,"(>/>=?  real-compare 1 2 2)", F);
		equal(l,"(>/>=?  real-compare 2 2 2)", F);
		equal(l,"(>/>=?  real-compare 3 2 1)", T);
		equal(l,"(>/>=?  real-compare 3 2 2)", T);
		equal(l,"(>/>=?  real-compare 2 2 1)", F);
		equal(l,"(>=/>=? real-compare 1 2 3)", F);
		equal(l,"(>=/>=? real-compare 2 2 3)", F);
		equal(l,"(>=/>=? real-compare 3 2 3)", F);
		equal(l,"(>=/>=? real-compare 1 3 2)", F);
		equal(l,"(>=/>=? real-compare 1 2 2)", F);
		equal(l,"(>=/>=? real-compare 2 2 2)", T);
		equal(l,"(>=/>=? real-compare 3 2 1)", T);
		equal(l,"(>=/>=? real-compare 3 2 2)", T);
		equal(l,"(>=/>=? real-compare 2 2 1)", T);

		equal(l,"(</<?    1 2 3)", T);
		equal(l,"(</<?    2 2 3)", F);
		equal(l,"(</<?    3 2 3)", F);
		equal(l,"(</<?    1 3 2)", F);
		equal(l,"(</<?    1 2 2)", F);
		equal(l,"(</<?    2 2 2)", F);
		equal(l,"(</<?    3 2 1)", F);
		equal(l,"(</<?    3 2 2)", F);
		equal(l,"(</<?    2 2 1)", F);
		equal(l,"(<=/<?   1 2 3)", T);
		equal(l,"(<=/<?   2 2 3)", T);
		equal(l,"(<=/<?   3 2 3)", F);
		equal(l,"(<=/<?   1 3 2)", F);
		equal(l,"(<=/<?   1 2 2)", F);
		equal(l,"(<=/<?   2 2 2)", F);
		equal(l,"(<=/<?   3 2 1)", F);
		equal(l,"(<=/<?   3 2 2)", F);
		equal(l,"(<=/<?   2 2 1)", F);
		equal(l,"(</<=?   1 2 3)", T);
		equal(l,"(</<=?   2 2 3)", F);
		equal(l,"(</<=?   3 2 3)", F);
		equal(l,"(</<=?   1 3 2)", F);
		equal(l,"(</<=?   1 2 2)", T);
		equal(l,"(</<=?   2 2 2)", F);
		equal(l,"(</<=?   3 2 1)", F);
		equal(l,"(</<=?   3 2 2)", F);
		equal(l,"(</<=?   2 2 1)", F);
		equal(l,"(<=/<=?  1 2 3)", T);
		equal(l,"(<=/<=?  2 2 3)", T);
		equal(l,"(<=/<=?  3 2 3)", F);
		equal(l,"(<=/<=?  1 3 2)", F);
		equal(l,"(<=/<=?  1 2 2)", T);
		equal(l,"(<=/<=?  2 2 2)", T);
		equal(l,"(<=/<=?  3 2 1)", F);
		equal(l,"(<=/<=?  3 2 2)", F);
		equal(l,"(<=/<=?  2 2 1)", F);

		equal(l,"(>/>?    1 2 3)", F);
		equal(l,"(>/>?    2 2 3)", F);
		equal(l,"(>/>?    3 2 3)", F);
		equal(l,"(>/>?    1 3 2)", F);
		equal(l,"(>/>?    1 2 2)", F);
		equal(l,"(>/>?    2 2 2)", F);
		equal(l,"(>/>?    3 2 1)", T);
		equal(l,"(>/>?    3 2 2)", F);
		equal(l,"(>/>?    2 2 1)", F);
		equal(l,"(>=/>?   1 2 3)", F);
		equal(l,"(>=/>?   2 2 3)", F);
		equal(l,"(>=/>?   3 2 3)", F);
		equal(l,"(>=/>?   1 3 2)", F);
		equal(l,"(>=/>?   1 2 2)", F);
		equal(l,"(>=/>?   2 2 2)", F);
		equal(l,"(>=/>?   3 2 1)", T);
		equal(l,"(>=/>?   3 2 2)", F);
		equal(l,"(>=/>?   2 2 1)", T);
		equal(l,"(>/>=?   1 2 3)", F);
		equal(l,"(>/>=?   2 2 3)", F);
		equal(l,"(>/>=?   3 2 3)", F);
		equal(l,"(>/>=?   1 3 2)", F);
		equal(l,"(>/>=?   1 2 2)", F);
		equal(l,"(>/>=?   2 2 2)", F);
		equal(l,"(>/>=?   3 2 1)", T);
		equal(l,"(>/>=?   3 2 2)", T);
		equal(l,"(>/>=?   2 2 1)", F);
		equal(l,"(>=/>=?  1 2 3)", F);
		equal(l,"(>=/>=?  2 2 3)", F);
		equal(l,"(>=/>=?  3 2 3)", F);
		equal(l,"(>=/>=?  1 3 2)", F);
		equal(l,"(>=/>=?  1 2 2)", F);
		equal(l,"(>=/>=?  2 2 2)", T);
		equal(l,"(>=/>=?  3 2 1)", T);
		equal(l,"(>=/>=?  3 2 2)", T);
		equal(l,"(>=/>=?  2 2 1)", T);

		l.exec ("(define cmp</<?   (</<?   real-compare))");
		l.exec ("(define cmp</<=?  (</<=?  real-compare))");
		l.exec ("(define cmp<=/<?  (<=/<?  real-compare))");
		l.exec ("(define cmp<=/<=? (<=/<=? real-compare))");
		l.exec ("(define cmp>/>?   (>/>?   real-compare))");
		l.exec ("(define cmp>/>=?  (>/>=?  real-compare))");
		l.exec ("(define cmp>=/>?  (>=/>?  real-compare))");
		l.exec ("(define cmp>=/>=? (>=/>=? real-compare))");
		equal(l,"(cmp</<?    1 2 3)", T);
		equal(l,"(cmp</<?    2 2 3)", F);
		equal(l,"(cmp</<?    3 2 3)", F);
		equal(l,"(cmp</<?    1 3 2)", F);
		equal(l,"(cmp</<?    1 2 2)", F);
		equal(l,"(cmp</<?    2 2 2)", F);
		equal(l,"(cmp</<?    3 2 1)", F);
		equal(l,"(cmp</<?    3 2 2)", F);
		equal(l,"(cmp</<?    2 2 1)", F);
		equal(l,"(cmp<=/<?   1 2 3)", T);
		equal(l,"(cmp<=/<?   2 2 3)", T);
		equal(l,"(cmp<=/<?   3 2 3)", F);
		equal(l,"(cmp<=/<?   1 3 2)", F);
		equal(l,"(cmp<=/<?   1 2 2)", F);
		equal(l,"(cmp<=/<?   2 2 2)", F);
		equal(l,"(cmp<=/<?   3 2 1)", F);
		equal(l,"(cmp<=/<?   3 2 2)", F);
		equal(l,"(cmp<=/<?   2 2 1)", F);
		equal(l,"(cmp</<=?   1 2 3)", T);
		equal(l,"(cmp</<=?   2 2 3)", F);
		equal(l,"(cmp</<=?   3 2 3)", F);
		equal(l,"(cmp</<=?   1 3 2)", F);
		equal(l,"(cmp</<=?   1 2 2)", T);
		equal(l,"(cmp</<=?   2 2 2)", F);
		equal(l,"(cmp</<=?   3 2 1)", F);
		equal(l,"(cmp</<=?   3 2 2)", F);
		equal(l,"(cmp</<=?   2 2 1)", F);
		equal(l,"(cmp<=/<=?  1 2 3)", T);
		equal(l,"(cmp<=/<=?  2 2 3)", T);
		equal(l,"(cmp<=/<=?  3 2 3)", F);
		equal(l,"(cmp<=/<=?  1 3 2)", F);
		equal(l,"(cmp<=/<=?  1 2 2)", T);
		equal(l,"(cmp<=/<=?  2 2 2)", T);
		equal(l,"(cmp<=/<=?  3 2 1)", F);
		equal(l,"(cmp<=/<=?  3 2 2)", F);
		equal(l,"(cmp<=/<=?  2 2 1)", F);

		l.exec ("(define cmp</<   (</<?))");
		l.exec ("(define cmp</<=  (</<=?))");
		l.exec ("(define cmp<=/<  (<=/<?))");
		l.exec ("(define cmp<=/<= (<=/<=?))");
		l.exec ("(define cmp>/>   (>/>?))");
		l.exec ("(define cmp>/>=  (>/>=?))");
		l.exec ("(define cmp>=/>  (>=/>?))");
		l.exec ("(define cmp>=/>= (>=/>=?))");
		equal(l,"(cmp</<?    1 2 3)", T);
		equal(l,"(cmp</<?    2 2 3)", F);
		equal(l,"(cmp</<?    3 2 3)", F);
		equal(l,"(cmp</<?    1 3 2)", F);
		equal(l,"(cmp</<?    1 2 2)", F);
		equal(l,"(cmp</<?    2 2 2)", F);
		equal(l,"(cmp</<?    3 2 1)", F);
		equal(l,"(cmp</<?    3 2 2)", F);
		equal(l,"(cmp</<?    2 2 1)", F);
		equal(l,"(cmp<=/<?   1 2 3)", T);
		equal(l,"(cmp<=/<?   2 2 3)", T);
		equal(l,"(cmp<=/<?   3 2 3)", F);
		equal(l,"(cmp<=/<?   1 3 2)", F);
		equal(l,"(cmp<=/<?   1 2 2)", F);
		equal(l,"(cmp<=/<?   2 2 2)", F);
		equal(l,"(cmp<=/<?   3 2 1)", F);
		equal(l,"(cmp<=/<?   3 2 2)", F);
		equal(l,"(cmp<=/<?   2 2 1)", F);
		equal(l,"(cmp</<=?   1 2 3)", T);
		equal(l,"(cmp</<=?   2 2 3)", F);
		equal(l,"(cmp</<=?   3 2 3)", F);
		equal(l,"(cmp</<=?   1 3 2)", F);
		equal(l,"(cmp</<=?   1 2 2)", T);
		equal(l,"(cmp</<=?   2 2 2)", F);
		equal(l,"(cmp</<=?   3 2 1)", F);
		equal(l,"(cmp</<=?   3 2 2)", F);
		equal(l,"(cmp</<=?   2 2 1)", F);
		equal(l,"(cmp<=/<=?  1 2 3)", T);
		equal(l,"(cmp<=/<=?  2 2 3)", T);
		equal(l,"(cmp<=/<=?  3 2 3)", F);
		equal(l,"(cmp<=/<=?  1 3 2)", F);
		equal(l,"(cmp<=/<=?  1 2 2)", T);
		equal(l,"(cmp<=/<=?  2 2 2)", T);
		equal(l,"(cmp<=/<=?  3 2 1)", F);
		equal(l,"(cmp<=/<=?  3 2 2)", F);
		equal(l,"(cmp<=/<=?  2 2 1)", F);

		lperr(l,"(</<? real-compare 1 2)");
		lperr(l,"(</<=? real-compare 1 2)");
		lperr(l,"(<=/<? real-compare 1 2)");
		lperr(l,"(<=/<=? real-compare 1 2)");
		lperr(l,"(>/>? real-compare 1 2)");
		lperr(l,"(>/>=? real-compare 1 2)");
		lperr(l,"(>=/>? real-compare 1 2)");
		lperr(l,"(>=/>=? real-compare 1 2)");
		lperr(l,"(</<? 1 2)");
		lperr(l,"(</<=? 1 2)");
		lperr(l,"(<=/<? 1 2)");
		lperr(l,"(<=/<=? 1 2)");
		lperr(l,"(>/>? 1 2)");
		lperr(l,"(>/>=? 1 2)");
		lperr(l,"(>=/>? 1 2)");
		lperr(l,"(>=/>=? 1 2)");
		lperr(l,"(</<? real-compare 1 2 1 1)");
		lperr(l,"(</<=? real-compare 1 2 1 1)");
		lperr(l,"(<=/<? real-compare 1 2 1 1)");
		lperr(l,"(<=/<=? real-compare 1 2 1 1)");
		lperr(l,"(>/>? real-compare 1 2 1 1)");
		lperr(l,"(>/>=? real-compare 1 2 1 1)");
		lperr(l,"(>=/>? real-compare 1 2 1 1)");
		lperr(l,"(>=/>=? real-compare 1 2 1 1)");
		lperr(l,"(</<? 1 2 1 1)");
		lperr(l,"(</<=? 1 2 1 1)");
		lperr(l,"(<=/<? 1 2 1 1)");
		lperr(l,"(<=/<=? 1 2 1 1)");
		lperr(l,"(>/>? 1 2 1 1)");
		lperr(l,"(>/>=? 1 2 1 1)");
		lperr(l,"(>=/>? 1 2 1 1)");
		lperr(l,"(>=/>=? 1 2 1 1)");
	}

	public void testIsChainRel() {
		Scheme l = Scheme.newInstance();

		equal(l,"(chain=?   real-compare 1 2 3)", F);
		equal(l,"(chain=?   real-compare 2 2 3)", F);
		equal(l,"(chain=?   real-compare 3 2 3)", F);
		equal(l,"(chain=?   real-compare 1 3 2)", F);
		equal(l,"(chain=?   real-compare 1 2 2)", F);
		equal(l,"(chain=?   real-compare 2 2 2)", T);
		equal(l,"(chain=?   real-compare 3 2 1)", F);
		equal(l,"(chain=?   real-compare 3 2 2)", F);
		equal(l,"(chain=?   real-compare 2 2 1)", F);
		equal(l,"(chain<?   real-compare 1 2 3)", T);
		equal(l,"(chain<?   real-compare 2 2 3)", F);
		equal(l,"(chain<?   real-compare 3 2 3)", F);
		equal(l,"(chain<?   real-compare 1 3 2)", F);
		equal(l,"(chain<?   real-compare 1 2 2)", F);
		equal(l,"(chain<?   real-compare 2 2 2)", F);
		equal(l,"(chain<?   real-compare 3 2 1)", F);
		equal(l,"(chain<?   real-compare 3 2 2)", F);
		equal(l,"(chain<?   real-compare 2 2 1)", F);
		equal(l,"(chain>?   real-compare 1 2 3)", F);
		equal(l,"(chain>?   real-compare 2 2 3)", F);
		equal(l,"(chain>?   real-compare 3 2 3)", F);
		equal(l,"(chain>?   real-compare 1 3 2)", F);
		equal(l,"(chain>?   real-compare 1 2 2)", F);
		equal(l,"(chain>?   real-compare 2 2 2)", F);
		equal(l,"(chain>?   real-compare 3 2 1)", T);
		equal(l,"(chain>?   real-compare 3 2 2)", F);
		equal(l,"(chain>?   real-compare 2 2 1)", F);
		equal(l,"(chain<=?  real-compare 1 2 3)", T);
		equal(l,"(chain<=?  real-compare 2 2 3)", T);
		equal(l,"(chain<=?  real-compare 3 2 3)", F);
		equal(l,"(chain<=?  real-compare 1 3 2)", F);
		equal(l,"(chain<=?  real-compare 1 2 2)", T);
		equal(l,"(chain<=?  real-compare 2 2 2)", T);
		equal(l,"(chain<=?  real-compare 3 2 1)", F);
		equal(l,"(chain<=?  real-compare 3 2 2)", F);
		equal(l,"(chain<=?  real-compare 2 2 1)", F);
		equal(l,"(chain>=?  real-compare 1 2 3)", F);
		equal(l,"(chain>=?  real-compare 2 2 3)", F);
		equal(l,"(chain>=?  real-compare 3 2 3)", F);
		equal(l,"(chain>=?  real-compare 1 3 2)", F);
		equal(l,"(chain>=?  real-compare 1 2 2)", F);
		equal(l,"(chain>=?  real-compare 2 2 2)", T);
		equal(l,"(chain>=?  real-compare 3 2 1)", T);
		equal(l,"(chain>=?  real-compare 3 2 2)", T);
		equal(l,"(chain>=?  real-compare 2 2 1)", T);
		equal(l,"(chain=?   real-compare 1)", T);
		equal(l,"(chain<?   real-compare 1)", T);
		equal(l,"(chain>?   real-compare 1)", T);
		equal(l,"(chain<=?  real-compare 1)", T);
		equal(l,"(chain>=?  real-compare 1)", T);
		equal(l,"(chain=?   real-compare)", T);
		equal(l,"(chain<?   real-compare)", T);
		equal(l,"(chain>?   real-compare)", T);
		equal(l,"(chain<=?  real-compare)", T);
		equal(l,"(chain>=?  real-compare)", T);
	}

	public void testIsPairwiseNot() {
		Scheme l = Scheme.newInstance();

		equal(l,"(pairwise-not? real-compare 1 2 3 4 5)", T);
		equal(l,"(pairwise-not? real-compare 1 2 3 4 4)", F);
		equal(l,"(pairwise-not? real-compare 1)", T);
		equal(l,"(pairwise-not? real-compare)", T);
	}

	public void testMaxCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(max-compare real-compare 1 2 6 4 5)", 6);
		eqi  (l,"(max-compare real-compare 1)", 1);
		lperr(l,"(max-compare real-compare)");
	}

	public void testMinCompare() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(min-compare real-compare 1 2 6 4 5)", 1);
		eqi  (l,"(min-compare real-compare 1)", 1);
		lperr(l,"(min-compare real-compare)");
	}

	public void testKthLargest() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(kth-largest real-compare 0  1 2 6 4 5)", 1);
		eqi  (l,"(kth-largest real-compare 5  1 2 6 4 5)", 1);
		eqi  (l,"(kth-largest real-compare 1  1 2 6 4 5)", 2);
		eqi  (l,"(kth-largest real-compare 2  1 2 6 4 5)", 4);
		eqi  (l,"(kth-largest real-compare -1 1 2 6 4 5)", 6);
		eqi  (l,"(kth-largest real-compare 0  1)", 1);
		eqi  (l,"(kth-largest real-compare -1 1)", 1);
		lperr(l,"(kth-largest real-compare 0)");
	}

	public void testCompareByRel() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(compare-by<  <  1 2)", -1);
		eqi  (l,"(compare-by<  <  2 2)", 0);
		eqi  (l,"(compare-by<  <  2 1)", 1);
		eqi  (l,"(compare-by>  >  1 2)", -1);
		eqi  (l,"(compare-by>  >  2 2)", 0);
		eqi  (l,"(compare-by>  >  2 1)", 1);
		eqi  (l,"(compare-by<= <= 1 2)", -1);
		eqi  (l,"(compare-by<= <= 2 2)", 0);
		eqi  (l,"(compare-by<= <= 2 1)", 1);
		eqi  (l,"(compare-by>= >= 1 2)", -1);
		eqi  (l,"(compare-by>= >= 2 2)", 0);
		eqi  (l,"(compare-by>= >= 2 1)", 1);

		l.exec ("(define cmp-by<  (compare-by<  <))");
		l.exec ("(define cmp-by>  (compare-by>  >))");
		l.exec ("(define cmp-by<= (compare-by<= <=))");
		l.exec ("(define cmp-by>= (compare-by>= >=))");
		eqi  (l,"(cmp-by<  1 2)", -1);
		eqi  (l,"(cmp-by<  2 2)", 0);
		eqi  (l,"(cmp-by<  2 1)", 1);
		eqi  (l,"(cmp-by>  1 2)", -1);
		eqi  (l,"(cmp-by>  2 2)", 0);
		eqi  (l,"(cmp-by>  2 1)", 1);
		eqi  (l,"(cmp-by<= 1 2)", -1);
		eqi  (l,"(cmp-by<= 2 2)", 0);
		eqi  (l,"(cmp-by<= 2 1)", 1);
		eqi  (l,"(cmp-by>= 1 2)", -1);
		eqi  (l,"(cmp-by>= 2 2)", 0);
		eqi  (l,"(cmp-by>= 2 1)", 1);

		lperr(l,"(compare-by<  < 1)");
		lperr(l,"(compare-by<  1 2)");
		lperr(l,"(compare-by<)");
		lperr(l,"(compare-by<  1 2 3)");
	}

	public void testCompareByEqRel() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(compare-by=/< = <  1 2)", -1);
		eqi  (l,"(compare-by=/< = <  2 2)", 0);
		eqi  (l,"(compare-by=/< = <  2 1)", 1);
		eqi  (l,"(compare-by=/> = >  1 2)", -1);
		eqi  (l,"(compare-by=/> = >  2 2)", 0);
		eqi  (l,"(compare-by=/> = >  2 1)", 1);

		l.exec ("(define cmp-by<  (compare-by=/<  = <))");
		l.exec ("(define cmp-by>  (compare-by=/>  = >))");
		eqi  (l,"(cmp-by<  1 2)", -1);
		eqi  (l,"(cmp-by<  2 2)", 0);
		eqi  (l,"(cmp-by<  2 1)", 1);
		eqi  (l,"(cmp-by>  1 2)", -1);
		eqi  (l,"(cmp-by>  2 2)", 0);
		eqi  (l,"(cmp-by>  2 1)", 1);

		lperr(l,"(compare-by=/< = < 1)");
		lperr(l,"(compare-by=/< = 1 2)");
		lperr(l,"(compare-by=/< =)");
		lperr(l,"(compare-by=/< 1 2)");
		lperr(l,"(compare-by=/< = 1 2 3)");
	}

	public void testDebugCompare() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define cmp (debug-compare real-compare))");
		l.exec ("(cmp 1 2)");
		l.exec ("(cmp 2 3)");
		l.exec ("(cmp 3 4)");
		l.exec ("(cmp 1 2)");
		l.exec ("(cmp 2 3)");
		l.exec ("(cmp 3 4)");

		lperr(l,"((debug-compare (lambda (x y) 1)) 1 2)");
		lperr(l,"((debug-compare (lambda (x y) (if (= x y) 0 1))) 1 2)");
		l.exec ("(define cmpx (debug-compare" +
				" (lambda (x y) (if (= x y) 0 (if (= y 2) 1 -1)))))");
		l.exec ("(cmpx 1 2)");
		l.exec ("(cmpx 1 1)");
		lperr(l,"(cmpx 2 3)");
	}

}
