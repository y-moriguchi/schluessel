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
 * @author MORIGUCHI, Yuichiro 2012/09/22
 */
public class DegreeTest extends TC {

	Degree deg(String s) {
		return Degree.parse(s);
	}

	Degree deg(int d, int m, int s, int us) {
		return new Degree(d, m, s, us);
	}

	Degree deg(int x) {
		return new Degree(x);
	}

	public void testParse() {
		eq(deg("+1°"), deg( 1, 0, 0, 0));
		eq(deg("-1°"), deg(-1, 0, 0, 0));
		eq(deg("1°"),  deg( 1, 0, 0, 0));
		try {
			deg("");  fail();
		} catch(NumberFormatException e) {}
	}

	void eqadd(Degree x, Degree y, Degree z, int c) {
		Degree r;
		int[] rc = new int[1];

		r = x.add(y, rc);
		eq(r, z);
		eq(rc[0], c);
	}

	public void testAdd() {
		eqadd(deg( 2037, 39, 29, 60), deg( 10, 20, 30, 40), deg(    0,  0,  0,  0),  1);
		eqadd(deg(-2037, 39, 29, 60), deg(-10, 20, 30, 40), deg(    0,  0,  0,  0), -1);
		eqadd(deg( 2037, 39, 29, 60), deg(-10, 20, 20, 40), deg( 2027, 19,  9, 20),  0);
		eqadd(deg(-2037, 39, 29, 60), deg( 10, 20, 20, 40), deg(-2027, 19,  9, 20),  0);
	}

	void eqsub(Degree x, Degree y, Degree z, int c) {
		Degree r;
		int[] rc = new int[1];

		r = x.subtract(y, rc);
		eq(r, z);
		eq(rc[0], c);
	}

	public void testSubtract() {
		eqsub(deg( 2037, 39, 29, 60), deg(-10, 20, 30, 40), deg(    0,  0,  0,  0),  1);
		eqsub(deg(-2037, 39, 29, 60), deg( 10, 20, 30, 40), deg(    0,  0,  0,  0), -1);
		eqsub(deg( 2037, 39, 29, 60), deg( 10, 20, 20, 40), deg( 2027, 19,  9, 20),  0);
		eqsub(deg(-2037, 39, 29, 60), deg(-10, 20, 20, 40), deg(-2027, 19,  9, 20),  0);
	}

	public void testNegate() {
		eq(deg( 37, 39, 29, 60).negate(), deg(-37, 39, 29, 60));
		eq(deg(-37, 39, 29, 60).negate(), deg( 37, 39, 29, 60));
		eq(deg(  0,  0,  0,  0).negate(), deg(  0,  0,  0,  0));
	}

	void eqmul(Degree x, Degree y, Degree z, int c) {
		Degree r;
		int[] rc = new int[1];

		r = x.multiply(y, rc);
		eq(r, z);
		eq(rc[0], c);
	}

	public void testMultiply() {
		eqmul(deg(   0,  0,  1, 99), deg( 0,  0,  1, 99), deg(   0,  0,  3, 96), 0);
		eqmul(deg(   0,  0, -1, 99), deg( 0,  0,  1, 99), deg(   0,  0, -3, 96), 0);
		eqmul(deg(   0,  0,  1, 99), deg( 0,  0, -1, 99), deg(   0,  0, -3, 96), 0);
		eqmul(deg(   0,  0, -1, 99), deg( 0,  0, -1, 99), deg(   0,  0,  3, 96), 0);
	}

	void eqdiv(Degree x, Degree y, Degree z, int c) {
		Degree r;
		int[] rc = new int[1];

		r = x.divide(y, rc);
		eq(r, z);
		eq(rc[0], c);
	}

	public void testDivide() {
		eqdiv(deg(   0,  0,  1, 99), deg( 0,  0,  1, 99), deg(   0,  0,  1, 00), 0);
		eqdiv(deg(   0,  0, -1, 99), deg( 0,  0,  1, 99), deg(   0,  0, -1, 00), 0);
		eqdiv(deg(   0,  0,  1, 99), deg( 0,  0, -1, 99), deg(   0,  0, -1, 00), 0);
		eqdiv(deg(   0,  0, -1, 99), deg( 0,  0, -1, 99), deg(   0,  0,  1, 00), 0);
	}

	void eqrem(Degree x, Degree y, Degree z) {
		Degree r;

		r = x.remainder(y);
		eq(r, z);
	}

	public void testRemainder() {
		eqrem(deg(   0,  0,  1, 99), deg( 0,  0,  1, 00), deg(0,  0,  0,  99));
		eqrem(deg(   0,  0, -1, 99), deg( 0,  0,  1, 00), deg(0,  0,  0, -99));
		eqrem(deg(   0,  0,  1, 99), deg( 0,  0, -1, 00), deg(0,  0,  0,  99));
		eqrem(deg(   0,  0, -1, 99), deg( 0,  0, -1, 00), deg(0,  0,  0, -99));
	}

	public void testInvert() {
		eq(deg(0, 0, 0,  01).invert(), deg(0,  1, 40, 00));
		eq(deg(0, 0, 0, -01).invert(), deg(0, -1, 40, 00));
	}

	public void testSignum() {
		eq(deg(-1).signum(), -1);
		eq(deg( 0).signum(),  0);
		eq(deg( 1).signum(),  1);
	}

	public void testIntValue() {
		eq(deg( 100).intValue(),  1);
		eq(deg(-100).intValue(), -1);
	}

	public void testLongValue() {
		eq(deg( 100).longValue(),  1l);
		eq(deg(-100).longValue(), -1l);
	}

	public void testDoubleValue() {
		eq(deg( 125).doubleValue(),  1.25);
		eq(deg(-125).doubleValue(), -1.25);
	}

	public void testCompareTo() {
		eq(deg( 100).compareTo(deg( 100)),  0);
		eq(deg( 100).compareTo(deg(-100)),  1);
		eq(deg(-100).compareTo(deg( 100)), -1);
		eq(deg(-100).compareTo(deg(-100)),  0);
		eq(deg( 101).compareTo(deg( 100)),  1);
		eq(new Degree(Signum2.POSITIVE, NonnegativeDegree.ZERO).compareTo(
				new Degree(Signum2.NEGATIVE, NonnegativeDegree.ZERO)), 1);
	}

	public void testHashCode() {
		Degree a, b, c, d, e, f;

		a = deg(0, 0, -1, 0);
		b = deg(0, 0, -1, 0);
		c = deg(0, 0, -1, 1);
		d = deg(0, 0, -1, 0);
		e = new Degree(Signum2.POSITIVE, NonnegativeDegree.ZERO);
		f = new Degree(Signum2.NEGATIVE, NonnegativeDegree.ZERO);
		ok(a.hashCode() == b.hashCode());
		ng(a.hashCode() == c.hashCode());
		ok(a.hashCode() == a.hashCode());
		ok(b.hashCode() == a.hashCode());
		ok(b.hashCode() == d.hashCode());
		ok(a.hashCode() == d.hashCode());
		ok(e.hashCode() == f.hashCode());
	}

	public void testEquals() {
		Degree a, b, c, d, e, f;

		a = deg(0, 0, -1, 0);
		b = deg(0, 0, -1, 0);
		c = deg(0, 0, -1, 1);
		d = deg(0, 0, -1, 0);
		e = new Degree(Signum2.POSITIVE, NonnegativeDegree.ZERO);
		f = new Degree(Signum2.NEGATIVE, NonnegativeDegree.ZERO);
		ok(a.equals(b));
		ng(a.equals(c));
		ok(a.equals(a));
		ok(b.equals(a));
		ok(b.equals(d));  ok(a.equals(b));
		ng(a.equals(Integer.valueOf(100)));
		ng(a.equals(null));
		ok(e.equals(f));
	}

}
