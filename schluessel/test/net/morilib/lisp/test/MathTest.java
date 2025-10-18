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

import net.morilib.lisp.LispComplex;
import net.morilib.lisp.Scheme;

public class MathTest extends TCSubr {

	public void testExpt() {
		Scheme l = Scheme.newInstance();

		eqv  (l, "(expt 2 8)", newZ(256));
		eqv  (l, "(expt 2 -8)", newQ(1, 256));
		eqv  (l, "(expt 1/2 8)", newQ(1, 256));
		eqv  (l, "(expt 2 8.0)", newR(256.0));
		eqv  (l, "(expt 4 0.5)", newR(2.0));
		eqv  (l, "(real-part (expt +i 2))", newR(-1.0));

		eqv  (l, "(expt 0 0)", newZ(1));
		eqv  (l, "(expt 0.0 0)", newR(1.0));
		eqv  (l, "(expt 0 100000)", newZ(0));
		eqv  (l, "(expt 0 100000.0)", newR(0.0));
		eqv  (l, "(expt 0 +inf.0)", newR(0.0));
		eqv  (l, "(expt 0 -inf.0)", newR(0.0));
		eqv  (l, "(expt 0 100+100i)", newZ(0));
		eqv  (l, "(expt 0 +inf.0i)", newR(0.0));
		eqv  (l, "(expt 0 -inf.0i)", newR(0.0));

		eqv  (l, "(expt 100000 0)", newZ(1));
		eqv  (l, "(expt 100000.0 0)", newR(1.0));
		eqv  (l, "(expt +inf.0 0)", newR(1.0));
		eqv  (l, "(expt -inf.0 0)", newR(1.0));
		eqv  (l, "(expt 100+100i 0)", newZ(1));
		eqv  (l, "(expt +inf.0i 0)", newR(1.0));
		eqv  (l, "(expt -inf.0i 0)", newR(1.0));

		eqv  (l, "(expt 1 100000)", newZ(1));
		eqv  (l, "(expt 1 100000.0)", newR(1.0));
		eqv  (l, "(expt 1 +inf.0)", newR(1.0));
		eqv  (l, "(expt 1 -inf.0)", newR(1.0));
		eqv  (l, "(expt 1 100+100i)", newZ(1));
		eqv  (l, "(expt 1 +inf.0i)", newR(1.0));
		eqv  (l, "(expt 1 -inf.0i)", newR(1.0));

		eqv  (l, "(expt 100000 1)", newZ(100000));
		eqv  (l, "(expt 100000.0 1)", newR(100000.0));
		eqv  (l, "(expt 100000 1.0)", newR(100000.0));
		eqv  (l, "(expt +inf.0 1)", INF_0P);
		eqv  (l, "(expt -inf.0 1)", INF_0N);
		eqv  (l, "(expt 100+100i 1)", LispComplex.newComplex(newZ(100), newZ(100)));
		eqv  (l, "(expt +inf.0i 1)", newC(0, Double.POSITIVE_INFINITY));
		eqv  (l, "(expt -inf.0i 1)", newC(0, Double.NEGATIVE_INFINITY));

		eqv  (l, "(expt 0.1 +inf.0)", newR(0.0));
		eqv  (l, "(expt -0.1 +inf.0)", newR(0.0));
		eqv  (l, "(expt 0.1+0.3i +inf.0)", newR(0.0));
		eqv  (l, "(expt 2 +inf.0)", INF_0P);
		eqv  (l, "(expt +inf.0 +inf.0)", INF_0P);
		isNaN(l, "(expt -1 +inf.0)");
		isNaN(l, "(expt +i +inf.0)");
		isNaN(l, "(expt 2+3i +inf.0)");
		isNaN(l, "(expt -inf.0 +inf.0)");
		isNaN(l, "(expt +inf.0i +inf.0)");

		eqv  (l, "(expt 0.1 -inf.0)", INF_0P);
		eqv  (l, "(expt 2 -inf.0)", newR(0.0));
		eqv  (l, "(expt 2+3i -inf.0)", newR(0.0));
		eqv  (l, "(expt +inf.0 -inf.0)", newR(0.0));
		eqv  (l, "(expt -inf.0 -inf.0)", newR(0.0));
		eqv  (l, "(expt +inf.0i -inf.0)", newR(0.0));
		eqv  (l, "(expt -inf.0i -inf.0)", newR(0.0));
		isNaN(l, "(expt -0.1 -inf.0)");
		isNaN(l, "(expt 0.1+0.3i -inf.0)");
		isNaN(l, "(expt -1 -inf.0)");
		isNaN(l, "(expt +i -inf.0)");

		isNaN(l, "(expt 2 +inf.0i)");
		isNaN(l, "(expt 2 -inf.0i)");

		isNaN(l, "(expt +nan.0 0)");
		isNaN(l, "(expt 0 +nan.0)");

		eqv  (l, "(expt 4 1/2)", newZ(2));
		eqv  (l, "(expt 8 1/3)", newZ(2));
		eqv  (l, "(expt (expt 123456789 10) 1/10)", newZ(123456789));
		eqv  (l, "(expt 4/9 1/2)", newQ(2, 3));
		eqv  (l, "(expt 8/27 -1/3)", newQ(3, 2));
		eqv  (l, "(expt 8/27 2/3)", newQ(4, 9));

		lperr(l, "(expt 'a 1)");
		lperr(l, "(expt 1 'b)");
		lperr(l, "(expt 1)");
	}

	public void testSqrt() {
		Scheme l = Scheme.newInstance();

		eqv  (l, "(sqrt 4.0)", newR(2.0));
		eqv  (l, "(sqrt -4.0)", newC(0, 2.0));
		eqv  (l, "(sqrt +inf.0)", INF_0P);
		eqv  (l, "(sqrt -inf.0)", newC(0, Double.POSITIVE_INFINITY));
		eqv  (l, "(sqrt +inf.0i)", newC(
				Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY));
		eqv  (l, "(sqrt -inf.0i)", newC(
				Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY));
		isNaN(l, "(sqrt +nan.0)");

		eqv  (l, "(sqrt 4)", newZ(2));
		eqv  (l, "(sqrt (* 1234567 1234567))", newZ(1234567));

		lperr(l, "(sqrt 'a)");
		lperr(l, "(sqrt)");
	}

	public void testExp() {
		Scheme l = Scheme.newInstance();

		eqv  (l, "(exp 0.0)", newR(1.0));
		//eqv  (l, "(exp +" + Math.PI + "i)", newR(-1.0));
		eqv  (l, "(exp +inf.0)", INF_0P);
		eqv  (l, "(exp -inf.0)", newR(0.0));
		isNaN(l, "(exp +inf.0i)");
		isNaN(l, "(exp -inf.0i)");
		isNaN(l, "(exp +nan.0)");

		lperr(l, "(exp 'a)");
		lperr(l, "(exp)");
	}

	public void testLog() {
		Scheme l = Scheme.newInstance();

		eqv  (l, "(log 1.0)", newR(0.0));
		eqv  (l, "(log " + Math.E + ")", newR(1.0));
		eqv  (l, "(log +inf.0)", INF_0P);
		eqv  (l, "(log +0.0)", newC(Double.NEGATIVE_INFINITY, 0));
		eqv  (l, "(log -inf.0)", newC(
				Double.POSITIVE_INFINITY, Math.PI));
		eqv  (l, "(log -0.0)", newC(
				Double.NEGATIVE_INFINITY, Math.PI));
		eqv  (l, "(log +inf.0i)", newC(
				Double.POSITIVE_INFINITY, Math.PI / 2));
		//eqv  (l, "(log +0.0i)", newC(
		//		Double.NEGATIVE_INFINITY, Math.PI / 2));
		eqv  (l, "(log -inf.0i)", newC(
				Double.POSITIVE_INFINITY, -Math.PI / 2));
		//eqv  (l, "(log -0.0i)", newC(
		//		Double.NEGATIVE_INFINITY, -Math.PI / 2));
		isNaN(l, "(log +nan.0)");

		lperr(l, "(log 'a)");
		lperr(l, "(log)");
	}

	public void testSin() {
		Scheme l = Scheme.newInstance();

		eqv  (l, "(sin 0)", newR(0.0));
		eqv  (l, "(sin " + (Math.PI / 2) + ")", newR(1.0));
		eqv  (l, "(sin +i)", newC(0, Math.sinh(1)));
		isNaN(l, "(sin +inf.0)");
		isNaN(l, "(sin -inf.0)");
		eqv  (l, "(sin +inf.0i)", newC(0, Double.POSITIVE_INFINITY));
		eqv  (l, "(sin -inf.0i)", newC(0, Double.NEGATIVE_INFINITY));
		eqv  (l, "(sin 1+inf.0i)", newC(
				Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY));
		eqv  (l, "(sin 1-inf.0i)", newC(
				Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY));
		eqv  (l, "(sin 2+inf.0i)", newC(
				Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY));
		eqv  (l, "(sin 2-inf.0i)", newC(
				Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY));
		eqv  (l, "(sin 4+inf.0i)", newC(
				Double.NEGATIVE_INFINITY, Double.NEGATIVE_INFINITY));
		eqv  (l, "(sin 4-inf.0i)", newC(
				Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY));
		eqv  (l, "(sin 5+inf.0i)", newC(
				Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY));
		eqv  (l, "(sin 5-inf.0i)", newC(
				Double.NEGATIVE_INFINITY, Double.NEGATIVE_INFINITY));
		isNaN(l, "(sin +inf.0+inf.0i)");
		isNaN(l, "(sin -inf.0+inf.0i)");
		isNaN(l, "(sin +inf.0-inf.0i)");
		isNaN(l, "(sin -inf.0-inf.0i)");
		isNaN(l, "(sin +nan.0)");

		lperr(l, "(sin 'a)");
		lperr(l, "(sin)");
	}

	public void testCos() {
		Scheme l = Scheme.newInstance();

		eqv  (l, "(cos 0)", newR(1.0));
		eqv  (l, "(cos " + (Math.PI / 2) + ")", newR(
				Math.cos(Math.PI / 2)));
		eqv  (l, "(cos +i)", newC(Math.cosh(1.0), 0.0));
		isNaN(l, "(cos +inf.0)");
		isNaN(l, "(cos -inf.0)");
		eqv  (l, "(cos +inf.0i)", newC(Double.POSITIVE_INFINITY, 0.0));
		eqv  (l, "(cos -inf.0i)", newC(Double.POSITIVE_INFINITY, 0.0));
		eqv  (l, "(cos 1+inf.0i)", newC(
				Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY));
		eqv  (l, "(cos 1-inf.0i)", newC(
				Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY));
		eqv  (l, "(cos 2+inf.0i)", newC(
				Double.NEGATIVE_INFINITY, Double.NEGATIVE_INFINITY));
		eqv  (l, "(cos 2-inf.0i)", newC(
				Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY));
		eqv  (l, "(cos 4+inf.0i)", newC(
				Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY));
		eqv  (l, "(cos 4-inf.0i)", newC(
				Double.NEGATIVE_INFINITY, Double.NEGATIVE_INFINITY));
		eqv  (l, "(cos 5+inf.0i)", newC(
				Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY));
		eqv  (l, "(cos 5-inf.0i)", newC(
				Double.POSITIVE_INFINITY, Double.NEGATIVE_INFINITY));
		isNaN(l, "(cos +inf.0+inf.0i)");
		isNaN(l, "(cos -inf.0+inf.0i)");
		isNaN(l, "(cos +inf.0-inf.0i)");
		isNaN(l, "(cos -inf.0-inf.0i)");
		isNaN(l, "(cos +nan.0)");

		lperr(l, "(cos 'a)");
		lperr(l, "(cos)");
	}

	public void testTan() {
		Scheme l = Scheme.newInstance();

		eqv  (l, "(tan 0)", newR(0.0));
		eqv  (l, "(tan 1)", newR(Math.tan(1)));
		eqv  (l, "(tan +i)", newC(0, Math.tanh(1)));
		isNaN(l, "(tan +inf.0)");
		isNaN(l, "(tan -inf.0)");
		eqv  (l, "(tan +inf.0i)", newC(0.0, 1.0));
		eqv  (l, "(tan -inf.0i)", newC(0.0, -1.0));
		eqv  (l, "(tan 1+inf.0i)", newC(
				Math.tan(1), 1).div(newC(1, Math.tan(1))));
		eqv  (l, "(tan 1-inf.0i)", newC(
				Math.tan(1), -1).div(newC(1, -Math.tan(1))));
		isNaN(l, "(tan +inf.0+inf.0i)");
		isNaN(l, "(tan -inf.0+inf.0i)");
		isNaN(l, "(tan +inf.0-inf.0i)");
		isNaN(l, "(tan -inf.0-inf.0i)");
		isNaN(l, "(tan +nan.0)");

		lperr(l, "(tan 'a)");
		lperr(l, "(tan)");
	}

	public void testAsin() {
		Scheme l = Scheme.newInstance();

		eqv  (l, "(asin 0)", newR(0.0));
		eqv  (l, "(asin 1)", newR(Math.PI / 2));
		eqv  (l, "(asin -1)", newR(-Math.PI / 2));
		eqv  (l, "(asin +" + Math.sinh(1) + "i)", newC(0.0, 1.0));
		eqv  (l, "(asin +inf.0)", newC(
				Math.PI / 2, Double.NEGATIVE_INFINITY));
		eqv  (l, "(asin -inf.0)", newC(
				-Math.PI / 2, Double.POSITIVE_INFINITY));
		eqv  (l, "(asin +inf.0+i)", newC(
				Math.PI / 2, Double.POSITIVE_INFINITY));
		eqv  (l, "(asin -inf.0+i)", newC(
				-Math.PI / 2, Double.POSITIVE_INFINITY));
		eqv  (l, "(asin +inf.0-i)", newC(
				Math.PI / 2, Double.NEGATIVE_INFINITY));
		eqv  (l, "(asin -inf.0-i)", newC(
				-Math.PI / 2, Double.NEGATIVE_INFINITY));
		eqv  (l, "(asin +inf.0i)", newC(0, Double.POSITIVE_INFINITY));
		eqv  (l, "(asin -inf.0i)", newC(0, Double.NEGATIVE_INFINITY));
		isNaN(l, "(asin +nan.0)");

		lperr(l, "(asin 'a)");
		lperr(l, "(asin)");
	}

	public void testAcos() {
		Scheme l = Scheme.newInstance();

		eqv  (l, "(acos 0)", newR(Math.PI / 2));
		eqv  (l, "(acos 1)", newR(0.0));
		eqv  (l, "(acos -1)", newR(Math.PI));
		eqv  (l, "(acos +" + Math.sinh(1) + "i)", newC(
				Math.PI / 2, -1));
		eqv  (l, "(acos +inf.0)", newC(
				0, Double.POSITIVE_INFINITY));
		eqv  (l, "(acos -inf.0)", newC(
				Math.PI, Double.NEGATIVE_INFINITY));
		eqv  (l, "(acos +inf.0+i)", newC(
				0, Double.NEGATIVE_INFINITY));
		eqv  (l, "(acos -inf.0+i)", newC(
				Math.PI, Double.NEGATIVE_INFINITY));
		eqv  (l, "(acos +inf.0-i)", newC(
				0, Double.POSITIVE_INFINITY));
		eqv  (l, "(acos -inf.0-i)", newC(
				Math.PI, Double.POSITIVE_INFINITY));
		eqv  (l, "(acos +inf.0i)", newC(
				Math.PI / 2, Double.NEGATIVE_INFINITY));
		eqv  (l, "(acos -inf.0i)", newC(
				Math.PI / 2, Double.POSITIVE_INFINITY));
		isNaN(l, "(acos +nan.0)");

		lperr(l, "(acos 'a)");
		lperr(l, "(acos)");
	}

	public void testAtan() {
		Scheme l = Scheme.newInstance();

		eqv  (l, "(atan 0)", newR(0.0));
		eqv  (l, "(atan 1)", newR(Math.PI / 4));
		eqv  (l, "(atan +inf.0)", newR(Math.PI / 2));
		eqv  (l, "(atan -inf.0)", newR(-Math.PI / 2));
		eqv  (l, "(atan +1.0i)", newC(
				0, Double.POSITIVE_INFINITY));
		eqv  (l, "(atan -1.0i)", newC(
				0, Double.NEGATIVE_INFINITY));
		eqv  (l, "(atan +inf.0i)", newR(Math.PI / 2));
		eqv  (l, "(atan -inf.0i)", newR(-Math.PI / 2));
		isNaN(l, "(atan +nan.0)");

		eqv  (l, "(atan +0.0 +1.0)", newR(0.0));
		eqv  (l, "(atan +1.0 +1.0)", newR(Math.PI / 4));
		eqv  (l, "(atan +1.0 +0.0)", newR(Math.PI / 2));
		eqv  (l, "(atan +1.0 -1.0)", newR(Math.PI * 3 / 4));
		eqv  (l, "(atan +0.0 -1.0)", newR(Math.PI));
		eqv  (l, "(atan -0.0 -1.0)", newR(-Math.PI));
		eqv  (l, "(atan -1.0 -1.0)", newR(-Math.PI * 3 / 4));
		eqv  (l, "(atan -1.0  0.0)", newR(-Math.PI / 2));
		eqv  (l, "(atan -1.0 +1.0)", newR(-Math.PI / 4));
		eqv  (l, "(atan +0.0 +0.0)", newR(0.0));
		eqv  (l, "(atan -0.0 +0.0)", newR(-0.0));
		eqv  (l, "(atan +0.0 -0.0)", newR(Math.PI));
		eqv  (l, "(atan -0.0 -0.0)", newR(-Math.PI));

		lperr(l, "(atan 1 1+i)");
		lperr(l, "(atan 1+i 1)");
		lperr(l, "(atan 'a)");
		lperr(l, "(atan)");
	}

}
