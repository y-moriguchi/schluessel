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
package net.morilib.geography;

import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/09/30
 */
public class LatitudeTest extends TC {

	Latitude deg(String s) {
		return Latitude.parse(s);
	}

	Latitude deg(int d, int m, int s, int us) {
		return new Latitude(d, m, s, us);
	}

	Latitude deg(int x) {
		return new Latitude(x);
	}

	public void testParse() {
		eq(deg("1°N"), deg(  1, 0, 0, 0));
		eq(deg("1°S"), deg( -1, 0, 0, 0));
		try {
			deg("1°");  fail();
		} catch(NumberFormatException e) {}
	}

	void eqadd(Latitude x, Latitude y, Latitude z) {
		eq(x.add(y), z);
	}

	public void testAdd() {
		eqadd(deg(   1, 39, 29, 60), deg( 10, 20, 30, 40), deg(   12,  0,  0,  0));
		eqadd(deg( -10, 20, 30, 40), deg( 10, 20, 30, 40), deg(    0,  0,  0,  0));
		eqadd(deg( -90,  0,  0,  0), deg( 90,  0,  0,  0), deg(    0,  0,  0,  0));
		eqadd(deg(  90,  0,  0,  0), deg( 90,  0,  0,  0), deg(    0,  0,  0,  0));
	}

	void eqsub(Latitude x, Latitude y, Latitude z) {
		eq(x.subtract(y), z);
	}

	public void testSubtract() {
		eqsub(deg(  49, 39, 39, 60), deg( 10, 20, 30, 40), deg(   39, 19,  9, 20));
		eqsub(deg(  10, 20, 30, 40), deg(  5, 20, 30, 40), deg(    5,  0,  0,  0));
		eqsub(deg(  -5, 20, 30, 40), deg(-10, 20, 30, 40), deg(    5,  0,  0,  0));
		eqsub(deg( -90,  0,  0,  0), deg( 90,  0,  0,  0), deg(    0,  0,  0,  0));
		eqsub(deg(  90,  0,  0,  0), deg( 90,  0,  0,  0), deg(    0,  0,  0,  0));
	}

	public void testMultiplyI() {
		eq(deg(10, 0, 0, 0).multiply(2), deg(20, 0, 0, 0));
		eq(deg(10, 0, 0, 0).multiply(-2), deg(-20, 0, 0, 0));
		eq(deg( 1, 1, 1, 1).multiply(2), deg(2, 2, 2, 2));
		eq(deg( 1, 1, 1, 1).multiply(-2), deg(-2, 2, 2, 2));
		eq(deg( 1, 0, 0, 0).multiply(30000000), deg(-60, 0, 0, 0));
	}

	public void testMultiplyD() {
		eq(deg(10, 0, 0, 0).multiply(2.0), deg(20, 0, 0, 0));
		eq(deg(10, 0, 0, 0).multiply(-2.0), deg(-20, 0, 0, 0));
		eq(deg(-10, 0, 0, 0).multiply(2.0), deg(-20, 0, 0, 0));
		eq(deg(-10, 0, 0, 0).multiply(-2.0), deg(20, 0, 0, 0));
		eq(deg( 1, 1, 1, 1).multiply(2.0), deg(2, 2, 2, 2));
//		eq(deg( 1, 1, 1, 1).multiply(-2.0), deg(-2, 2, 2, 2));
		eq(deg( 1, 0, 0, 0).multiply(30000000.0), deg(-60, 0, 0, 0));
		eq(deg(10, 0, 0, 0).multiply(1.5), deg(15, 0, 0, 0));
		eq(deg(10, 0, 0, 0).multiply(-1.5), deg(-15, 0, 0, 0));
	}

	public void testDivideI() {
		eq(deg(30, 0, 0, 0).divide(2), deg(15, 0, 0, 0));
		eq(deg(30, 0, 0, 0).divide(-2), deg(-15, 0, 0, 0));
		eq(deg(-30, 0, 0, 0).divide(2), deg(-15, 0, 0, 0));
		eq(deg(-30, 0, 0, 0).divide(-2), deg(15, 0, 0, 0));
		eq(deg( 0, 0, 1, 0).divide(100), deg(0, 0, 0, 1));
		eq(deg( 0, 0, 1, 0).divide(101), deg(0, 0, 0, 0));
	}

	public void testDivideD() {
		eq(deg(30, 0, 0, 0).divide(2.0), deg(15, 0, 0, 0));
		eq(deg(30, 0, 0, 0).divide(-2.0), deg(-15, 0, 0, 0));
		eq(deg(-30, 0, 0, 0).divide(2.0), deg(-15, 0, 0, 0));
		eq(deg(-30, 0, 0, 0).divide(-2.0), deg(15, 0, 0, 0));
		eq(deg( 0, 0, 1, 0).divide(100.0), deg(0, 0, 0, 1));
		eq(deg( 0, 0, 1, 0).divide(101.0), deg(0, 0, 0, 0));
		eq(deg( 0, 0, 1, 0).divide(0.1), deg(0, 0, 10, 0));
	}

	public void testHashCode() {
		Latitude a, b, c, d;

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
		Latitude a, b, c, d;

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
