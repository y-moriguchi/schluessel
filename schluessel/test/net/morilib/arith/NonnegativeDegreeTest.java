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
package net.morilib.arith;

import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/09/23
 */
public class NonnegativeDegreeTest extends TC {

	NonnegativeDegree deg(String s) {
		return NonnegativeDegree.parse(s);
	}

	NonnegativeDegree deg(int d, int m, int s, int us) {
		return new NonnegativeDegree(d, m, s, us);
	}

	NonnegativeDegree deg(int x) {
		return new NonnegativeDegree(x);
	}

	NonnegativeDegree deg(double x) {
		return new NonnegativeDegree(x);
	}

	public void testNonnegativeDegree4() {
		deg(2047, 59, 59, 99);
		try {
			deg(2048, 59, 59, 99);  fail();
		} catch(IllegalArgumentException e) {}
		try {
			deg(2047, 60, 59, 99);  fail();
		} catch(IllegalArgumentException e) {}
		try {
			deg(2047, 59, 60, 99);  fail();
		} catch(IllegalArgumentException e) {}
		try {
			deg(2047, 59, 59, 100);  fail();
		} catch(IllegalArgumentException e) {}
		try {
			deg(-1, 59, 59, 99);  fail();
		} catch(IllegalArgumentException e) {}
		try {
			deg(0, -1, 59, 99);  fail();
		} catch(IllegalArgumentException e) {}
		try {
			deg(0, 0, -1, 99);  fail();
		} catch(IllegalArgumentException e) {}
		try {
			deg(0, 0, 0, -1);  fail();
		} catch(IllegalArgumentException e) {}
	}

	public void testNonnegativeDegreeD() {
		eq(deg(5400.0), deg(1, 30, 0, 0));
		eq(deg(4500.0), deg(1, 15, 0, 0));
		eq(deg(4050.0), deg(1, 7, 30, 0));
		eq(deg(3825.0), deg(1, 3, 45, 0));
		eq(deg(3712.5), deg(1, 1, 52, 50));
	}

	public void testParse() {
		eq(deg("2047°59′59.99″"), deg(2047, 59, 59, 99));
		eq(deg("0°0′0.0″"),       deg(   0,  0,  0,  0));
		eq(deg("2047°59′59.9″"),  deg(2047, 59, 59, 90));
		eq(deg("2047°59′59″"),    deg(2047, 59, 59,  0));
		eq(deg("2047°59′"),        deg(2047, 59,  0,  0));
		eq(deg("2047°59.99″"),     deg(2047,  0, 59, 99));
		eq(deg("2047°"),            deg(2047, 0, 0, 0));
		eq(deg("59′59.99″"),       deg(   0, 59, 59, 99));
		eq(deg("59′"),              deg(   0, 59,  0,  0));
		eq(deg("59.99″"),           deg(   0,  0, 59, 99));
		eq(deg("59′59.999″"),      deg(   0, 59, 59, 99));

		try {
			deg("2048°59′59.99″");  fail();
		} catch(NumberFormatException e) {}
		try {
			deg("2047°60′59.99″");  fail();
		} catch(NumberFormatException e) {}
		try {
			deg("2047°59′60.99″");  fail();
		} catch(NumberFormatException e) {}
		try {
			deg("");  fail();
		} catch(NumberFormatException e) {}
	}

	void eqadd(NonnegativeDegree x, NonnegativeDegree y,
			NonnegativeDegree z, int c) {
		NonnegativeDegree r;
		int[] rc = new int[1];

		r = x.add(y, rc);
		eq(r, z);
		eq(rc[0], c);
	}

	public void testAdd() {
		eqadd(deg(   1,  2,  3,  4), deg(10, 20, 30, 40), deg(  11, 22, 33, 44), 0);
		eqadd(deg(   1,  2,  3, 60), deg(10, 20, 30, 40), deg(  11, 22, 34,  0), 0);
		eqadd(deg(   1,  2, 30,  4), deg(10, 20, 30, 40), deg(  11, 23,  0, 44), 0);
		eqadd(deg(   1, 40,  3,  4), deg(10, 20, 30, 40), deg(  12,  0, 33, 44), 0);
		eqadd(deg(2038,  2,  3,  4), deg(10, 20, 30, 40), deg(   0, 22, 33, 44), 1);
		eqadd(deg(2037, 39, 29, 60), deg(10, 20, 30, 40), deg(   0,  0,  0,  0), 1);
	}

	void eqsub(NonnegativeDegree x, NonnegativeDegree y,
			NonnegativeDegree z) {
		NonnegativeDegree r;

		r = x.subtract(y);
		eq(r, z);
	}

	public void testSubtract() {
		eqsub(deg(  10, 20, 30, 40), deg( 1,  2,  3,  4), deg(   9, 18, 27, 36));
		eqsub(deg(  10, 20, 30, 40), deg( 1,  2,  3, 41), deg(   9, 18, 26, 99));
		eqsub(deg(  10, 20, 30, 40), deg( 1,  2, 31,  4), deg(   9, 17, 59, 36));
		eqsub(deg(  10, 20, 30, 40), deg( 1, 21,  3,  4), deg(   8, 59, 27, 36));
		eqsub(deg(  10, 20, 30, 40), deg( 1, 20, 30, 41), deg(   8, 59, 59, 99));
		eqsub(deg(  10, 20, 30, 40), deg(10, 20, 30, 40), deg(   0,  0,  0,  0));
		try {
			deg(10, 20, 30, 40).subtract(deg(10, 20, 30, 41));  fail();
		} catch(ArithmeticException e) {}
	}

	void eqmul(NonnegativeDegree x, NonnegativeDegree y,
			NonnegativeDegree z, int c) {
		NonnegativeDegree r;
		int[] rc = new int[1];

		r = x.multiply(y, rc);
		eq(r, z);
		eq(rc[0], c);
	}

	public void testMultiply() {
		eqmul(deg(   0,  0,  1, 99), deg( 0,  0,  1, 99), deg(   0,  0,  3, 96), 0);
		eqmul(deg(368640000), deg(200), deg(0), 1);
		eqmul(deg(368639999), deg(200), deg(737279998), 0);
	}

	void eqdiv(NonnegativeDegree x, NonnegativeDegree y,
			NonnegativeDegree z, int c) {
		NonnegativeDegree r;
		int[] rc = new int[1];

		r = x.divide(y, rc);
		eq(r, z);
		eq(rc[0], c);
	}

	public void testDivide() {
		eqdiv(deg(   0,  0,  1, 99), deg( 0,  0,  1, 99), deg(   0,  0,  1,  0), 0);
		eqdiv(deg(   0,  0,  1, 99), deg( 0,  0,  0, 01), deg(   0,  3, 19,  0), 0);
		eqdiv(deg(7372800), deg(1), deg(0), 1);
		try {
			deg(1).divide(deg(0), null);  fail();
		} catch(ArithmeticException e) {}
	}

	void eqrem(NonnegativeDegree x, NonnegativeDegree y,
			NonnegativeDegree z) {
		NonnegativeDegree r;

		r = x.remainder(y);
		eq(r, z);
	}

	public void testRemainder() {
		eqrem(deg(   0,  0,  1, 99), deg( 0,  0,  1, 99), deg(   0,  0,  0, 00));
		eqrem(deg(   0,  0,  1, 99), deg( 0,  0,  1, 00), deg(   0,  0,  0, 99));
		try {
			deg(1).remainder(deg(0));  fail();
		} catch(ArithmeticException e) {}
	}

	public void testInvert() {
		eq(deg(0, 0,  0, 1).invert(), deg(0, 1, 40, 0));
		eq(deg(0, 0,  1, 0).invert(), deg(0, 0,  1, 0));
		eq(deg(0, 1, 40, 0).invert(), deg(0, 0,  0, 1));
		eq(deg(0, 1, 40, 1).invert(), deg(0, 0,  0, 0));
		try {
			deg(0).invert();  fail();
		} catch(ArithmeticException e) {}
	}

	public void testSignum() {
		eq(deg(0, 0, 0, 0).signum(), 0);
		eq(deg(0, 0, 0, 1).signum(), 1);
	}

	public void testIntValue() {
		eq(deg(0, 0, 1,  0).intValue(), 1);
		eq(deg(0, 0, 1, 99).intValue(), 1);
		eq(deg(0, 0, 2,  0).intValue(), 2);
	}

	public void testLongValue() {
		eq(deg(0, 0, 1,  0).longValue(), 1);
		eq(deg(0, 0, 1, 99).longValue(), 1);
		eq(deg(0, 0, 2,  0).longValue(), 2);
	}

	public void testDoubleValue() {
		eq(deg(0, 0, 1,  0).doubleValue(), 1);
		eq(deg(0, 0, 1, 25).doubleValue(), 1.25);
		eq(deg(0, 0, 2,  0).doubleValue(), 2);
		eq(deg(0, 1, 0, 25).doubleValue(), 60.25);
		eq(deg(1, 0, 0, 25).doubleValue(), 3600.25);
	}

	public void compareTo() {
		eq(deg(0, 0, 1, 0).compareTo(deg(0, 0, 0, 59)),  1);
		eq(deg(0, 0, 1, 0).compareTo(deg(0, 0, 1, 01)), -1);
		eq(deg(0, 0, 1, 0).compareTo(deg(0, 0, 1, 00)),  0);
	}

	public void testHashCode() {
		NonnegativeDegree a, b, c, d;

		a = deg(0, 0, 1, 0);
		b = deg(0, 0, 1, 0);
		c = deg(0, 0, 1, 1);
		d = deg(0, 0, 1, 0);
		ok(a.hashCode() == b.hashCode());
		ng(a.hashCode() == c.hashCode());
		ok(a.hashCode() == a.hashCode());
		ok(b.hashCode() == a.hashCode());
		ok(b.hashCode() == d.hashCode());
		ok(a.hashCode() == d.hashCode());
	}

	public void testEquals() {
		NonnegativeDegree a, b, c, d;

		a = deg(0, 0, 1, 0);
		b = deg(0, 0, 1, 0);
		c = deg(0, 0, 1, 1);
		d = deg(0, 0, 1, 0);
		ok(a.equals(b));
		ng(a.equals(c));
		ok(a.equals(a));
		ok(b.equals(a));
		ok(b.equals(d));  ok(a.equals(b));
		ng(a.equals(Integer.valueOf(100)));
		ng(a.equals(null));
	}

}
