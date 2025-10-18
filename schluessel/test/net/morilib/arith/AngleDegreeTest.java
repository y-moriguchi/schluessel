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
 * @author MORIGUCHI, Yuichiro 2012/09/30
 */
public class AngleDegreeTest extends TC {

	AngleDegree deg(String s) {
		return AngleDegree.parse(s);
	}

	AngleDegree deg(int d, int m, int s, int us) {
		return new AngleDegree(d, m, s, us);
	}

	AngleDegree deg(int x) {
		return new AngleDegree(x);
	}

	public void testParse() {
		eq(deg("+1°"), deg(  1, 0, 0, 0));
		eq(deg("-1°"), deg(359, 0, 0, 0));
		eq(deg("1°"),  deg(  1, 0, 0, 0));
		try {
			deg("");  fail();
		} catch(NumberFormatException e) {}
	}

	void eqadd(AngleDegree x, AngleDegree y, AngleDegree z) {
		eq(x.add(y), z);
	}

	public void testAdd() {
		eqadd(deg(   1, 39, 29, 60), deg( 10, 20, 30, 40), deg(   12,  0,  0,  0));
		eqadd(deg( 349, 39, 29, 60), deg( 10, 20, 30, 40), deg(    0,  0,  0,  0));
	}

	void eqsub(AngleDegree x, AngleDegree y, AngleDegree z) {
		eq(x.subtract(y), z);
	}

	public void testSubtract() {
		eqsub(deg( 349, 39, 39, 60), deg( 10, 20, 30, 40), deg(  339, 19,  9, 20));
		eqsub(deg(   5, 20, 30, 40), deg( 10, 20, 30, 40), deg(  355,  0,  0,  0));
	}

	public void testMultiplyI() {
		eq(deg(10, 0, 0, 0).multiply(3), deg(30, 0, 0, 0));
		eq(deg(10, 0, 0, 0).multiply(-3), deg(330, 0, 0, 0));
		eq(deg( 1, 1, 1, 1).multiply(3), deg(3, 3, 3, 3));
		eq(deg( 1, 1, 1, 1).multiply(-3), deg(356, 56, 56, 97));
		eq(deg( 1, 0, 0, 0).multiply(30000000), deg(120, 0, 0, 0));
	}

	public void testMultiplyD() {
		eq(deg(10, 0, 0, 0).multiply(3.0), deg(30, 0, 0, 0));
		eq(deg(10, 0, 0, 0).multiply(-3.0), deg(330, 0, 0, 0));
		eq(deg( 1, 1, 1, 1).multiply(3.0), deg(3, 3, 3, 3));
//		eq(deg( 1, 1, 1, 1).multiply(-3.0), deg(356, 56, 56, 96));
		eq(deg( 1, 0, 0, 0).multiply(30000000.0), deg(120, 0, 0, 0));
		eq(deg(10, 0, 0, 0).multiply(1.5), deg(15, 0, 0, 0));
		eq(deg(10, 0, 0, 0).multiply(-1.5), deg(345, 0, 0, 0));
	}

	public void testDivideI() {
		eq(deg(30, 0, 0, 0).divide(3), deg(10, 0, 0, 0));
		eq(deg(30, 0, 0, 0).divide(-3), deg(350, 0, 0, 0));
		eq(deg( 0, 0, 1, 0).divide(100), deg(0, 0, 0, 1));
		eq(deg( 0, 0, 1, 0).divide(101), deg(0, 0, 0, 0));
	}

	public void testDivideD() {
		eq(deg(30, 0, 0, 0).divide(3.0), deg(10, 0, 0, 0));
		eq(deg(30, 0, 0, 0).divide(-3.0), deg(350, 0, 0, 0));
		eq(deg( 0, 0, 1, 0).divide(100.0), deg(0, 0, 0, 1));
		eq(deg( 0, 0, 1, 0).divide(101.0), deg(0, 0, 0, 0));
		eq(deg( 0, 0, 1, 0).divide(0.1), deg(0, 0, 10, 0));
	}

	public void testHashCode() {
		AngleDegree a, b, c, d;

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
		AngleDegree a, b, c, d;

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
