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

import java.math.BigInteger;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispBigInt;
import net.morilib.lisp.LispComplex;
import net.morilib.lisp.LispRational;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.Scheme;

public class NumberTest extends TCSubr {

	public void testNumLessThan() {
		Scheme l = Scheme.newInstance();

		eq(l, "(< 1 2 3 4 5)", T);
		eq(l, "(< 1 2 2 4 5)", F);
		eq(l, "(< 5 4 3 2 1)", F);
		eq(l, "(< 5 4 4 2 1)", F);
		eq(l, "(< 1 1 1 1 1)", F);
		eq(l, "(< 1 2.0 3)", T);
		eq(l, "(< -inf.0 0 +inf.0)", T);
		eq(l, "(< -inf.0 -inf.0 +inf.0)", F);
		eq(l, "(< +inf.0 0 -inf.0)", F);
		eq(l, "(< +inf.0 +inf.0 -inf.0)", F);
		eq(l, "(< +inf.0 +inf.0)", F);
		eq(l, "(< 1 +nan.0 2)", F);
		lperr(l, "(< 1)");
		lperr(l, "(< 1 'a 2)");
		lperr(l, "(< 1 '(3) 4)");
		lperr(l, "(< 2 +2i)");
	}

	public void testNumLessThanEq() {
		Scheme l = Scheme.newInstance();

		eq(l, "(<= 1 2 3 4 5)", T);
		eq(l, "(<= 1 2 2 4 5)", T);
		eq(l, "(<= 5 4 3 2 1)", F);
		eq(l, "(<= 5 4 4 2 1)", F);
		eq(l, "(<= 1 1 1 1 1)", T);
		eq(l, "(<= 2 2.0 3)", T);
		eq(l, "(<= -inf.0 0 +inf.0)", T);
		eq(l, "(<= -inf.0 -inf.0 +inf.0)", T);
		eq(l, "(<= +inf.0 0 -inf.0)", F);
		eq(l, "(<= +inf.0 +inf.0 -inf.0)", F);
		eq(l, "(<= +inf.0 +inf.0)", T);
		eq(l, "(<= 1 +nan.0 2)", F);
		lperr(l, "(<= 1)");
		lperr(l, "(<= 1 'a 2)");
		lperr(l, "(<= 1 '(3) 4)");
		lperr(l, "(<= 2 +2i)");
	}

	public void testNumGreaterThan() {
		Scheme l = Scheme.newInstance();

		eq(l, "(> 1 2 3 4 5)", F);
		eq(l, "(> 1 2 2 4 5)", F);
		eq(l, "(> 5 4 3 2 1)", T);
		eq(l, "(> 5 4 4 2 1)", F);
		eq(l, "(> 1 1 1 1 1)", F);
		eq(l, "(> 3 2.0 1)", T);
		eq(l, "(> -inf.0 0 +inf.0)", F);
		eq(l, "(> -inf.0 -inf.0 +inf.0)", F);
		eq(l, "(> +inf.0 0 -inf.0)", T);
		eq(l, "(> +inf.0 +inf.0 -inf.0)", F);
		eq(l, "(> +inf.0 +inf.0)", F);
		eq(l, "(> 1 +nan.0 2)", F);
		lperr(l, "(> 1)");
		lperr(l, "(> 1 'a 2)");
		lperr(l, "(> 1 '(3) 4)");
		lperr(l, "(> 2 +2i)");
	}

	public void testNumGreaterThanEq() {
		Scheme l = Scheme.newInstance();

		eq(l, "(>= 1 2 3 4 5)", F);
		eq(l, "(>= 1 2 2 4 5)", F);
		eq(l, "(>= 5 4 3 2 1)", T);
		eq(l, "(>= 5 4 4 2 1)", T);
		eq(l, "(>= 1 1 1 1 1)", T);
		eq(l, "(>= 2 2.0 1)", T);
		eq(l, "(>= -inf.0 0 +inf.0)", F);
		eq(l, "(>= -inf.0 -inf.0 +inf.0)", F);
		eq(l, "(>= +inf.0 0 -inf.0)", T);
		eq(l, "(>= +inf.0 +inf.0 -inf.0)", T);
		eq(l, "(>= +inf.0 +inf.0)", T);
		eq(l, "(>= 1 +nan.0 2)", F);
		lperr(l, "(>= 1)");
		lperr(l, "(>= 1 'a 2)");
		lperr(l, "(>= 1 '(3) 4)");
		lperr(l, "(> 2 +2i)");
	}

	public void testNumEqual() {
		Scheme l = Scheme.newInstance();

		eq(l, "(= 1 2 3 4 5)", F);
		eq(l, "(= 1 2 2 4 5)", F);
		eq(l, "(= 5 4 3 2 1)", F);
		eq(l, "(= 5 4 4 2 1)", F);
		eq(l, "(= 1 1 1 1 1)", T);
		eq(l, "(= 2 2.0 4/2)", T);
		eq(l, "(= 2 +2i)", F);
		eq(l, "(= -inf.0 0 +inf.0)", F);
		eq(l, "(= -inf.0 -inf.0 +inf.0)", F);
		eq(l, "(= +inf.0 0 -inf.0)", F);
		eq(l, "(= +inf.0 +inf.0 -inf.0)", F);
		eq(l, "(= +inf.0 +inf.0)", T);
		eq(l, "(= 1 +nan.0 2)", F);
		eq(l, "(= +nan.0 +nan.0)", F);
//		lperr(l, "(= 1)");
		lperr(l, "(= 1 'a 2)");
		lperr(l, "(= 1 '(3) 4)");
	}

	public void testAdd() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(+ 1 2 3)", newZ(6));
		eqv(l, "(+ 1 2.0 3)", newR(6.0));
		eqv(l, "(+ 1/2 1/3)", newQ(5, 6));
		eqv(l, "(+ 1/2 0.5)", newR(1.0));
		eqv(l, "(+ 2 +3i)", newC(2, 3));
		eqv(l, "(+ 1+i 2+2i)", newC(3, 3));
		eqv(l, "(+ 1@1 2@1)", newCp(3, 1));
		eqv(l, "(+ 1.0@1.0 2.0@1.0)", newCp(3.0, 1.0));
		eqv(l, "(+ 1@1 -1@1)", newZ(0));
		eqv(l, "(+)", newZ(0));
		eqv(l, "(+ 1)", newZ(1));
		eqv(l, "(+ +inf.0 1)", INF_0P);
		eqv(l, "(+ +inf.0 +inf.0)", INF_0P);
		isNaN(l, "(+ +inf.0 -inf.0)");
		isNaN(l, "(+ +inf.0i -inf.0i)");
		isNaN(l, "(+ +nan.0 1)");
		isNaN(l, "(+ +nan.0 +nan.0)");
		lperr(l, "(+ 1 'a)");

		eqv (l, "(+ 1" +
				"   100000000000000000000000000)",
				newZ(new BigInteger("100000000000000000000000001")));
		eqv (l, "(+  100000000000000000000000001 "+
				"   -100000000000000000000000000)",
				newZ(1));
	}

	public void testSub() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(- 1 2 3)", newZ(-4));
		eqv(l, "(- 1 2.0 3)", newR(-4.0));
		eqv(l, "(- 1/2 1/3)", newQ(1, 6));
		eqv(l, "(- 1/2 0.5)", newR(0.0));
		eqv(l, "(- 2 +3i)", newC(2, -3));
		eqv(l, "(- 1+i 2+2i)", newC(-1, -1));
		eqv(l, "(- 2@1 1@1)", newCp(1, 1));
		eqv(l, "(- 2.0@1.0 1.0@1.0)", newCp(1.0, 1.0));
		eqv(l, "(- 1@1 1@1)", newZ(0));
		eqv(l, "(- 1)", newZ(-1));
		eqv(l, "(- +inf.0 1)", INF_0P);
		eqv(l, "(- +inf.0 -inf.0)", INF_0P);
		isNaN(l, "(- +inf.0 +inf.0)");
		isNaN(l, "(- +inf.0i +inf.0i)");
		isNaN(l, "(- +nan.0 1)");
		isNaN(l, "(- +nan.0 +nan.0)");
		lperr(l, "(-)");
		lperr(l, "(- 1 'a)");

		eqv (l, "(- 100000000000000000000000000" +
				"   1)",
				newZ(new BigInteger("99999999999999999999999999")));
		eqv (l, "(- 100000000000000000000000001 "+
				"   100000000000000000000000000)",
				newZ(1));
	}

	public void testMul() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(* 1 2 3)", newZ(6));
		eqv(l, "(* 1 2.0 3)", newR(6.0));
		eqv(l, "(* 1/2 1/3)", newQ(1, 6));
		eqv(l, "(* 1/2 0.5)", newR(0.25));
		eqv(l, "(* 2 +3i)", newC(0, 6));
		eqv(l, "(* 1+i 2+2i)", newC(0, 4));
		eqv(l, "(* 3@1 2@1)", newCp(6, 2));
		eqv(l, "(* 3.0@1.0 2.0@1.0)", newCp(6.0, 2.0));
		eqv(l, "(*)", newZ(1));
		eqv(l, "(* 1)", newZ(1));
		//eqv(l, "(* +inf.0 0)", newR(0));
		//eqv(l, "(* -inf.0 0)", newR(0));
		//eqv(l, "(* 0 +inf.0)", newR(0));
		//eqv(l, "(* 0 -inf.0)", newR(0));
		eqv(l, "(* +inf.0 1)", INF_0P);
		eqv(l, "(* +inf.0 +inf.0)", INF_0P);
		eqv(l, "(* +inf.0 -inf.0)", INF_0N);
		isNaN(l, "(* +nan.0 1)");
		isNaN(l, "(* +nan.0 +nan.0)");
		lperr(l, "(* 1 'a)");

		eqv (l, "(* 10000000" +
				"   10000000)",
				newZ(new BigInteger("100000000000000")));
		eqv (l, "(* 100000000000000000000000000 "+
				"   10)",
				newZ(new BigInteger("1000000000000000000000000000")));
	}

	public void testDiv() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(/ 1 2 3)", newQ(1, 6));
		eqv(l, "(/ 1 2.0 4)", newR(0.125));
		eqv(l, "(/ 1/2 1/3)", newQ(3, 2));
		eqv(l, "(/ 1/2 0.5)", newR(1.0));
		eqv(l, "(/ 2 +4.0i)", newC(0, -0.5));
		eqv(l, "(/ 1+i 2+2i)", newQ(1, 2));
		eqv(l, "(/ 4@1 2@2)", newCp(2, -1));
		eqv(l, "(/ 3.0@1.0 2.0@2.0)", newCp(1.5, -1.0));
		eqv(l, "(/ 2@1 2@1)", newZ(1));
		eqv(l, "(/ 2.0@1.0 2.0@1.0)", newR(1.0));
		eqv(l, "(/ 1.0 0)", INF_0P);
		eqv(l, "(/ -1.0 0)", INF_0N);
		eqv(l, "(/ 1.0 +inf.0)", newR(0.0));
		eqv(l, "(/ 1.0 -inf.0)", newR(-0.0));
		eqv(l, "(/ +inf.0 0)", INF_0P);
		eqv(l, "(/ -inf.0 0)", INF_0N);
		eqv(l, "(/ 0 +inf.0)", newR(0.0));
		eqv(l, "(/ 0 -inf.0)", newR(-0.0));
		isNaN(l, "(/ +inf.0 +inf.0)");
		isNaN(l, "(/ 0.0 0)");
		isNaN(l, "(/ +nan.0 1)");
		isNaN(l, "(/ +nan.0 +nan.0)");
		lperr(l, "(/ 1 0)");
		lperr(l, "(/ 0 0)");
		lperr(l, "(/ 1 'a)");
		lperr(l, "(/)");

		eqv (l, "(/ 1000000000000000000000000" +
				"   1000000000000000000000000)",
				newZ(1));
		eqv (l, "(/ 100000000000000000000000000 "+
				"   10)",
				newZ(new BigInteger("10000000000000000000000000")));
	}

	public void testIsExact() {
		Scheme l = Scheme.newInstance();

		eq(l, "(exact? 1)", T);
		eq(l, "(exact? 1/2)", T);
		eq(l, "(exact? #e0.25)", T);
		eq(l, "(exact? 0.25)", F);
		eq(l, "(exact? #i1/2)", F);
		eq(l, "(exact? 1.0+2i)", F);
		eq(l, "(exact? 'a)", F);
		eq(l, "(exact? +inf.0)", F);
		eq(l, "(exact? +nan.0)", F);
		lperr(l, "(exact?)");
	}

	public void testIsInexact() {
		Scheme l = Scheme.newInstance();

		eq(l, "(inexact? 1)", F);
		eq(l, "(inexact? 1/2)", F);
		eq(l, "(inexact? #e0.25)", F);
		eq(l, "(inexact? 0.25)", T);
		eq(l, "(inexact? #i1/2)", T);
		eq(l, "(inexact? 1.0+2i)", T);
		eq(l, "(inexact? 'a)", F);
		eq(l, "(inexact? +inf.0)", T);
		eq(l, "(inexact? +nan.0)", T);
		lperr(l, "(inexact?)");
	}

	public void testIsZero() {
		Scheme l = Scheme.newInstance();

		eq(l, "(zero? 0)", T);
		eq(l, "(zero? 0.0)", T);
		eq(l, "(zero? 1)", F);
		eq(l, "(zero? 1.0)", F);
		eq(l, "(zero? -1)", F);
		eq(l, "(zero? -1.0)", F);
		eq(l, "(zero? +1i)", F);
		eq(l, "(zero? +inf.0)", F);
		eq(l, "(zero? -inf.0)", F);
		eq(l, "(zero? +nan.0)", F);
		lperr(l, "(zero?)");
		lperr(l, "(zero? 'a)");
	}

	public void testIsPositive() {
		Scheme l = Scheme.newInstance();

		eq(l, "(positive? 0)", F);
		eq(l, "(positive? 0.0)", F);
		eq(l, "(positive? 1)", T);
		eq(l, "(positive? 1.0)", T);
		eq(l, "(positive? -1)", F);
		eq(l, "(positive? -1.0)", F);
		eq(l, "(positive? +inf.0)", T);
		eq(l, "(positive? -inf.0)", F);
		eq(l, "(positive? +nan.0)", F);
		lperr(l, "(positive? +1i)");
		lperr(l, "(positive?)");
		lperr(l, "(positive? 'a)");
	}

	public void testIsNegative() {
		Scheme l = Scheme.newInstance();

		eq(l, "(negative? 0)", F);
		eq(l, "(negative? 0.0)", F);
		eq(l, "(negative? 1)", F);
		eq(l, "(negative? 1.0)", F);
		eq(l, "(negative? -1)", T);
		eq(l, "(negative? -1.0)", T);
		eq(l, "(negative? +inf.0)", F);
		eq(l, "(negative? -inf.0)", T);
		eq(l, "(negative? +nan.0)", F);
		lperr(l, "(negative? +1i)");
		lperr(l, "(negative?)");
		lperr(l, "(negative? 'a)");
	}

	public void testIsEven() {
		Scheme l = Scheme.newInstance();

		eq(l, "(even? 2)", T);
		eq(l, "(even? 6.0)", T);
		eq(l, "(even? 3)", F);
		eq(l, "(even? 5.0)", F);
		lperr(l, "(even? 2.4)");
		lperr(l, "(even? 2/3)");
		lperr(l, "(even? +inf.0)");
		lperr(l, "(even? -inf.0)");
		lperr(l, "(even? +nan.0)");
		lperr(l, "(even? +2i)");
		lperr(l, "(even? 'a)");
		lperr(l, "(even?)");
	}

	public void testIsOdd() {
		Scheme l = Scheme.newInstance();

		eq(l, "(odd? 2)", F);
		eq(l, "(odd? 6.0)", F);
		eq(l, "(odd? 3)", T);
		eq(l, "(odd? 5.0)", T);
		lperr(l, "(odd? 2.4)");
		lperr(l, "(odd? 2/3)");
		lperr(l, "(odd? +inf.0)");
		lperr(l, "(odd? -inf.0)");
		lperr(l, "(odd? +nan.0)");
		lperr(l, "(odd? +2i)");
		lperr(l, "(odd? 'a)");
		lperr(l, "(odd?)");
	}

	public void testQuotient() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(quotient 10 3)", newZ(3));
		eqv(l, "(quotient 10.0 3)", newR(3.0));
		eqv(l, "(quotient 10.0 3.0)", newR(3.0));
		eqv(l, "(quotient -10 3)", newZ(-3));
		eqv(l, "(quotient 10 -3)", newZ(-3));
		eqv(l, "(quotient -10 -3)", newZ(3));
		eqv(l, "(quotient 10.3 3)", newR(3.0));
		eqv(l, "(quotient 10/3 3)", newZ(1));
		lperr(l, "(quotient +inf.0 3)");
		lperr(l, "(quotient -inf.0 3)");
		lperr(l, "(quotient +nan.0 3)");
		lperr(l, "(quotient +2i 3)");
		lperr(l, "(quotient 'a 3)");
		eqv(l, "(quotient 10 10.3)", newR(0.0));
		eqv(l, "(quotient 10 10/3)", newZ(3));
		lperr(l, "(quotient 10 +inf.0)");
		lperr(l, "(quotient 10 -inf.0)");
		lperr(l, "(quotient 10 +nan.0)");
		lperr(l, "(quotient 10 +2i)");
		lperr(l, "(quotient 10 'a)");
		lperr(l, "(quotient 10)");
	}

	public void testRemainder() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(remainder 10 3)", newZ(1));
		eqv(l, "(remainder 10.0 3)", newR(1.0));
		eqv(l, "(remainder 10.0 3.0)", newR(1.0));
		eqv(l, "(remainder -10 3)", newZ(-1));
		eqv(l, "(remainder 10 -3)", newZ(1));
		eqv(l, "(remainder -10 -3)", newZ(-1));
		//lperr(l, "(remainder 10.3 3)");
		eqv(l, "(remainder 10/3 3)", newQ(1, 3));
//		lperr(l, "(remainder +inf.0 3)");
//		lperr(l, "(remainder -inf.0 3)");
//		lperr(l, "(remainder +nan.0 3)");
		lperr(l, "(remainder +2i 3)");
		lperr(l, "(remainder 'a 3)");
		//lperr(l, "(remainder 10 10.3)");
		eqv(l, "(remainder 10 10/3)", newZ(0));
//		lperr(l, "(remainder 10 +inf.0)");
//		lperr(l, "(remainder 10 -inf.0)");
//		lperr(l, "(remainder 10 +nan.0)");
		lperr(l, "(remainder 10 +2i)");
		lperr(l, "(remainder 10 'a)");
		lperr(l, "(remainder 10)");
	}

	public void testModulo() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(modulo 10 3)", newZ(1));
		eqv(l, "(modulo 10.0 3)", newR(1.0));
		eqv(l, "(modulo 10.0 3.0)", newR(1.0));
		eqv(l, "(modulo -10 3)", newZ(2));
		eqv(l, "(modulo 10 -3)", newZ(-2));
		eqv(l, "(modulo -10 -3)", newZ(-1));
		//eqv(l, "(modulo 10.3 3)");
		eqv(l, "(modulo 10/3 3)", newQ(1, 3));
		lperr(l, "(modulo +inf.0 3)");
		lperr(l, "(modulo -inf.0 3)");
		lperr(l, "(modulo +nan.0 3)");
		lperr(l, "(modulo +2i 3)");
		lperr(l, "(modulo 'a 3)");
		//lperr(l, "(modulo 10 10.3)");
		eqv(l, "(modulo 10 10/3)", newZ(0));
		lperr(l, "(modulo 10 +inf.0)");
		lperr(l, "(modulo 10 -inf.0)");
		lperr(l, "(modulo 10 +nan.0)");
		lperr(l, "(modulo 10 +2i)");
		lperr(l, "(modulo 10 'a)");
		lperr(l, "(modulo 10)");
	}

	public void testTruncate() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(truncate 10)", newZ(10));
		eqv(l, "(truncate 5/4)", newZ(1));
		eqv(l, "(truncate -5/4)", newZ(-1));
		eqv(l, "(truncate 1.25)", newR(1.0));
		eqv(l, "(truncate -1.25)", newR(-1.0));
		eqv(l, "(truncate 7/4)", newZ(1));
		eqv(l, "(truncate -7/4)", newZ(-1));
		eqv(l, "(truncate 1.75)", newR(1.0));
		eqv(l, "(truncate -1.75)", newR(-1.0));
		eqv(l, "(truncate 9/2)", newZ(4));
		eqv(l, "(truncate 7/2)", newZ(3));
		eqv(l, "(truncate 4.5)", newR(4.0));
		eqv(l, "(truncate 3.5)", newR(3.0));
		lperr(l, "(truncate +2i)");
		lperr(l, "(truncate 'a)");
		lperr(l, "(truncate)");
	}

	public void testFloor() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(floor 10)", newZ(10));
		eqv(l, "(floor 5/4)", newZ(1));
		eqv(l, "(floor -5/4)", newZ(-2));
		eqv(l, "(floor 1.25)", newR(1.0));
		eqv(l, "(floor -1.25)", newR(-2.0));
		eqv(l, "(floor 7/4)", newZ(1));
		eqv(l, "(floor -7/4)", newZ(-2));
		eqv(l, "(floor 1.75)", newR(1.0));
		eqv(l, "(floor -1.75)", newR(-2.0));
		eqv(l, "(floor 9/2)", newZ(4));
		eqv(l, "(floor 7/2)", newZ(3));
		eqv(l, "(floor 4.5)", newR(4.0));
		eqv(l, "(floor 3.5)", newR(3.0));
		lperr(l, "(floor +2i)");
		lperr(l, "(floor 'a)");
		lperr(l, "(floor)");
	}

	public void testCeiling() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(ceiling 10)", newZ(10));
		eqv(l, "(ceiling 5/4)", newZ(2));
		eqv(l, "(ceiling -5/4)", newZ(-1));
		eqv(l, "(ceiling 1.25)", newR(2.0));
		eqv(l, "(ceiling -1.25)", newR(-1.0));
		eqv(l, "(ceiling 7/4)", newZ(2));
		eqv(l, "(ceiling -7/4)", newZ(-1));
		eqv(l, "(ceiling 1.75)", newR(2.0));
		eqv(l, "(ceiling -1.75)", newR(-1.0));
		eqv(l, "(ceiling 9/2)", newZ(5));
		eqv(l, "(ceiling 7/2)", newZ(4));
		eqv(l, "(ceiling 4.5)", newR(5.0));
		eqv(l, "(ceiling 3.5)", newR(4.0));
		lperr(l, "(ceiling +2i)");
		lperr(l, "(ceiling 'a)");
		lperr(l, "(ceiling)");
	}

	public void testRound() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(round 10)", newZ(10));
		eqv(l, "(round 5/4)", newZ(1));
		eqv(l, "(round -5/4)", newZ(-1));
		eqv(l, "(round 1.25)", newR(1.0));
		eqv(l, "(round -1.25)", newR(-1.0));
		eqv(l, "(round 7/4)", newZ(2));
		eqv(l, "(round -7/4)", newZ(-2));
		eqv(l, "(round 1.75)", newR(2.0));
		eqv(l, "(round -1.75)", newR(-2.0));
		eqv(l, "(round 9/2)", newZ(4));
		eqv(l, "(round 7/2)", newZ(4));
		eqv(l, "(round 4.5)", newR(4.0));
		eqv(l, "(round 3.5)", newR(4.0));
		lperr(l, "(round +2i)");
		lperr(l, "(round 'a)");
		lperr(l, "(round)");
	}

	public void testIsNumber() {
		Scheme l = Scheme.newInstance();

		eq(l, "(number? #f)", F);
		eq(l, "(number? 'a)", F);
		eq(l, "(number? #\\a)", F);
		eq(l, "(number? #(1 2))", F);
		eq(l, "(number? number?)", F);
		eq(l, "(number? '(1 2))", F);
		eq(l, "(number? 1)", T);
		eq(l, "(number? \"aaa\")", F);
		eq(l, "(number? (current-output-port))", F);

		eq(l, "(number? 1)", T);
		eq(l, "(number? 1.0)", T);
		eq(l, "(number? 3/4)", T);
		eq(l, "(number? .75)", T);
		eq(l, "(number? +inf.0)", T);
		eq(l, "(number? +nan.0)", T);
		eq(l, "(number? 1+2i)", T);
		eq(l, "(number? +inf.0-inf.0i)", T);

		lperr(l, "(number?)");
	}

	public void testIsInteger() {
		Scheme l = Scheme.newInstance();

		eq(l, "(integer? #f)", F);
		eq(l, "(integer? 'a)", F);
		eq(l, "(integer? #\\a)", F);
		eq(l, "(integer? #(1 2))", F);
		eq(l, "(integer? integer?)", F);
		eq(l, "(integer? '(1 2))", F);
		eq(l, "(integer? 1)", T);
		eq(l, "(integer? \"aaa\")", F);
		eq(l, "(integer? (current-output-port))", F);

		eq(l, "(integer? 1)", T);
		eq(l, "(integer? 1.0)", T);
		eq(l, "(integer? 3/4)", F);
		eq(l, "(integer? .75)", F);
		eq(l, "(integer? +inf.0)", F);
		eq(l, "(integer? +nan.0)", F);
		eq(l, "(integer? 1+2i)", F);
		eq(l, "(integer? +inf.0-inf.0i)", F);

		lperr(l, "(integer?)");
	}

	public void testIsRational() {
		Scheme l = Scheme.newInstance();

		eq(l, "(rational? #f)", F);
		eq(l, "(rational? 'a)", F);
		eq(l, "(rational? #\\a)", F);
		eq(l, "(rational? #(1 2))", F);
		eq(l, "(rational? rational?)", F);
		eq(l, "(rational? '(1 2))", F);
		eq(l, "(rational? 1)", T);
		eq(l, "(rational? \"aaa\")", F);
		eq(l, "(rational? (current-output-port))", F);

		eq(l, "(rational? 1)", T);
		eq(l, "(rational? 1.0)", T);
		eq(l, "(rational? 3/4)", T);
		eq(l, "(rational? .75)", T);
		eq(l, "(rational? +inf.0)", F);
		eq(l, "(rational? +nan.0)", F);
		eq(l, "(rational? 1+2i)", F);
		eq(l, "(rational? +inf.0-inf.0i)", F);

		lperr(l, "(rational?)");
	}

	public void testIsReal() {
		Scheme l = Scheme.newInstance();

		eq(l, "(real? #f)", F);
		eq(l, "(real? 'a)", F);
		eq(l, "(real? #\\a)", F);
		eq(l, "(real? #(1 2))", F);
		eq(l, "(real? real?)", F);
		eq(l, "(real? '(1 2))", F);
		eq(l, "(real? 1)", T);
		eq(l, "(real? \"aaa\")", F);
		eq(l, "(real? (current-output-port))", F);

		eq(l, "(real? 1)", T);
		eq(l, "(real? 1.0)", T);
		eq(l, "(real? 3/4)", T);
		eq(l, "(real? .75)", T);
		eq(l, "(real? +inf.0)", T);
		eq(l, "(real? +nan.0)", T);
		eq(l, "(real? 1+2i)", F);
		eq(l, "(real? +inf.0-inf.0i)", F);

		lperr(l, "(real?)");
	}

	public void testAbs() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(abs 1)", newZ(1));
		eqv(l, "(abs -1)", newZ(1));
		eqv(l, "(abs 0)", newZ(0));
		eqv(l, "(abs 1.0)", newR(1.0));
		eqv(l, "(abs 3+4i)", newZ(5));
		eqv(l, "(abs +inf.0)", INF_0P);
		eqv(l, "(abs 3-inf.0i)", INF_0P);
		eqv(l, "(abs +inf.0-inf.0i)", INF_0P);
		isNaN(l, "(abs +nan.0)");
		lperr(l, "(abs 'a)");
		lperr(l, "(abs)");
	}

	public void testMax() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(max 1 -1 2 -2 3 -3)", newZ(3));
		eqv(l, "(max 1 -1 2 -2 3 -3.0)", newR(3.0));
		eqv(l, "(max +inf.0 0 -inf.0)", INF_0P);
		eqv(l, "(max 1 -1 +nan.0)", newZ(1));
		eqv(l, "(max 1)", newZ(1));
		isNaN(l, "(max +nan.0)");
		lperr(l, "(max 1 -1i)");
		lperr(l, "(max 1 'a)");
		lperr(l, "(max)");
	}

	public void testMin() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(min 1 -1 2 -2 3 -3)", newZ(-3));
		eqv(l, "(min 1 -1 2 -2 3 -3.0)", newR(-3.0));
		eqv(l, "(min +inf.0 0 -inf.0)", INF_0N);
		eqv(l, "(min 1 -1 +nan.0)", newZ(-1));
		eqv(l, "(min 1)", newZ(1));
		isNaN(l, "(min +nan.0)");
		lperr(l, "(min 1 -1i)");
		lperr(l, "(min 1 'a)");
		lperr(l, "(min)");
	}

	public void testGcd() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(gcd 70 -42 28)", newZ(14));
		eqv(l, "(gcd 10)", newZ(10));
		eqv(l, "(gcd 70 -42 28.0)", newR(14.0));
		eqv(l, "(gcd)", newZ(0));
		eqv(l, "(gcd 100 0 2)", newZ(2));
		eqv(l, "(gcd 0)", newZ(0));
		eqv(l, "(gcd 0 0 0)", newZ(0));
		lperr(l, "(gcd 7.2 3)");
		lperr(l, "(gcd 7 +3i)");
	}

	public void testLcm() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(lcm 70 -42 28)", newZ(420));
		eqv(l, "(lcm 10)", newZ(10));
		eqv(l, "(lcm 70 -42 28.0)", newR(420.0));
		eqv(l, "(lcm)", newZ(1));
		eqv(l, "(lcm 100 0 2)", newZ(0));
		lperr(l, "(lcm 7.2 3)");
		lperr(l, "(lcm 7 +3i)");
	}

	public void testExactToInexact() {
		Scheme l = Scheme.newInstance();

		eqv  (l, "(exact->inexact 10)", newR(10.0));
		eqv  (l, "(exact->inexact 10.0)", newR(10.0));
		eqv  (l, "(exact->inexact -3/4)", newR(-0.75));
		eqv  (l, "(exact->inexact -.75)", newR(-0.75));
		eqv  (l, "(exact->inexact 1+i)", newC(1.0, 1.0));
		eqv  (l, "(exact->inexact +inf.0)", INF_0P);
		isNaN(l, "(exact->inexact +nan.0)");
		lperr(l, "(exact->inexact 'a)");
		lperr(l, "(exact->inexact)");
	}

	public void testInexactToExact() {
		Scheme l = Scheme.newInstance();

		eqv  (l, "(inexact->exact 10)", newZ(10));
		eqv  (l, "(inexact->exact 10.0)", newZ(10));
		eqv  (l, "(inexact->exact -3/4)", newQ(-3, 4));
		eqv  (l, "(inexact->exact -.75)", newQ(-3, 4));
		//eqv  (l, "(inexact->exact 1+i)", newC(1.0, 1.0));
		lperr(l, "(inexact->exact +inf.0)");
		lperr(l, "(inexact->exact +nan.0)");
		lperr(l, "(inexact->exact 'a)");
		lperr(l, "(inexact->exact)");
	}

	public void testNumerator() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(numerator 4)", newZ(4));
		eqv(l, "(numerator 4.0)", newR(4.0));
		eqv(l, "(numerator -3/4)", newZ(-3));
		eqv(l, "(numerator -0.75)", newR(-3.0));
		lperr(l, "(numerator +inf.0)");
		lperr(l, "(numerator +nan.0)");
		lperr(l, "(numerator 1+i)");
		lperr(l, "(numerator 'a)");
		lperr(l, "(numerator)");
	}

	public void testDenominator() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(denominator 4)", newZ(1));
		eqv(l, "(denominator 4.0)", newR(1.0));
		eqv(l, "(denominator -3/4)", newZ(4));
		eqv(l, "(denominator -0.75)", newR(4.0));
		lperr(l, "(denominator +inf.0)");
		lperr(l, "(denominator +nan.0)");
		lperr(l, "(denominator 1+i)");
		lperr(l, "(denominator 'a)");
		lperr(l, "(denominator)");
	}

	public void testRealPart() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(real-part 4)", newZ(4));
		eqv(l, "(real-part 4.0)", newR(4.0));
		eqv(l, "(real-part -3/4)", newQ(-3, 4));
		eqv(l, "(real-part -0.75)", newR(-0.75));
		eqv(l, "(real-part +inf.0)", INF_0P);
		eqv(l, "(real-part 1.0+2.0i)", newR(1.0));
		eqv(l, "(real-part +inf.0-inf.0i)", INF_0P);
		isNaN(l, "(real-part +nan.0)");
		lperr(l, "(real-part 'a)");
		lperr(l, "(real-part)");
	}

	public void testImagPart() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(imag-part 4)", newZ(0));
		eqv(l, "(imag-part 4.0)", newR(0.0));
		eqv(l, "(imag-part -3/4)", newZ(0));
		eqv(l, "(imag-part -0.75)", newR(0.0));
		eqv(l, "(imag-part +inf.0)", newR(0.0));
		eqv(l, "(imag-part 1.0+2.0i)", newR(2.0));
		eqv(l, "(imag-part +inf.0-inf.0i)", INF_0N);
		isNaN(l, "(imag-part +nan.0)");
		lperr(l, "(imag-part 'a)");
		lperr(l, "(imag-part)");
	}

	public void testMakeRectangular() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(make-rectangular 1.0 2.0)", newC(1.0, 2.0));
		eqv(l, "(make-rectangular 2.0 0)", newC(2.0, 0.0));
		isNaN(l, "(make-rectangular +nan.0 2)");
		isNaN(l, "(make-rectangular 2 +nan.0)");
		lperr(l, "(make-rectangular 1 2+3i)");
		lperr(l, "(make-rectangular 2+3i 1)");
		lperr(l, "(make-rectangular 1 'a)");
		lperr(l, "(make-rectangular 'a 1)");
		lperr(l, "(make-rectangular 1)");
	}

	public void testMakePolar() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(make-polar 2.0 0)", newR(2.0));
		eqv(l, "(make-polar +inf.0 0)", INF_0P);
//		eqv(l, "(make-polar +inf.0 1)", newC(
//				Double.POSITIVE_INFINITY,
//				Double.POSITIVE_INFINITY));
//		eqv(l, "(make-polar +inf.0 2)", newC(
//				Double.NEGATIVE_INFINITY,
//				Double.POSITIVE_INFINITY));
//		eqv(l, "(make-polar +inf.0 4)", newC(
//				Double.NEGATIVE_INFINITY,
//				Double.NEGATIVE_INFINITY));
//		eqv(l, "(make-polar +inf.0 6)", newC(
//				Double.POSITIVE_INFINITY,
//				Double.NEGATIVE_INFINITY));
		eqv(l, "(make-polar 0 +inf.0)", newR(0.0));
		isNaN(l, "(make-polar 1 +inf.0)");
		isNaN(l, "(make-polar +nan.0 1)");
		isNaN(l, "(make-polar 1 +nan.0)");
		lperr(l, "(make-polar 1 2+3i)");
		lperr(l, "(make-polar 2+3i 1)");
		lperr(l, "(make-polar 1 'a)");
		lperr(l, "(make-polar 'a 1)");
		lperr(l, "(make-polar 1)");
	}

	public void testAngle() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(angle -1)", newR(Math.PI));
		eqv(l, "(angle +i)", newR(Math.PI / 2));
		eqv(l, "(angle +inf.0+3i)", newR(0.0));
		eqv(l, "(angle -inf.0+3i)", newR(Math.PI));
		eqv(l, "(angle 2+inf.0i)", newR(Math.PI / 2));
		eqv(l, "(angle 2-inf.0i)", newR(-Math.PI / 2));
		eqv(l, "(angle 4@2)", newZ(2));
		eqv(l, "(angle 4@2.0)", newR(2.0));
		//isNaN(l, "(angle +inf.0+inf.0i)");
		isNaN(l, "(angle +nan.0)");
		lperr(l, "(angle 'a)");
		lperr(l, "(angle)");
	}

	public void testMagnitude() {
		Scheme l = Scheme.newInstance();

		eqv(l, "(magnitude 1)", newZ(1));
		eqv(l, "(magnitude -1)", newZ(1));
		eqv(l, "(magnitude 0)", newZ(0));
		eqv(l, "(magnitude 1.0)", newR(1.0));
		eqv(l, "(magnitude 3+4i)", newZ(5));
		eqv(l, "(magnitude +inf.0)", INF_0P);
		eqv(l, "(magnitude 3-inf.0i)", INF_0P);
		eqv(l, "(magnitude +inf.0-inf.0i)", INF_0P);
		eqv(l, "(magnitude 4@2)", newZ(4));
		eqv(l, "(magnitude 4.0@2.0)", newR(4.0));
		isNaN(l, "(magnitude +nan.0)");
		lperr(l, "(magnitude 'a)");
		lperr(l, "(magnitude)");
	}

	public void testNumberToString() {
		Scheme l = Scheme.newInstance();

		equal(l, "(number->string 12)", str("12"));
		equal(l, "(number->string 12.0)", str("12.0"));
		equal(l, "(number->string 3/4)", str("3/4"));
		equal(l, "(number->string 3.0+4.0i)", str("3.0+4.0i"));
		equal(l, "(number->string +inf.0)", str("+inf.0"));
		equal(l, "(number->string +nan.0)", str("+nan.0"));
		equal(l, "(number->string +inf.0+inf.0i)", str("+inf.0+inf.0i"));
		equal(l, "(number->string 15/16 16)", str("f/10"));
		equal(l, "(number->string 8/9 2)", str("1000/1001"));
		lperr(l, "(number->string 'a)");
		lperr(l, "(number->string 2 'b)");
		lperr(l, "(number->string 2 1)");
		lperr(l, "(number->string 2 17)");
		lperr(l, "(number->string)");
	}

	private void isFix(Datum d, int v) {
		if(d instanceof LispSmallInt) {
			eq(d.getInt(), v);
		} else {
			fail();
		}
	}

	private void isBig(Datum d, String v) {
		if(d instanceof LispBigInt) {
			eq(d.getBigInteger(), new BigInteger(v));
		} else {
			fail();
		}
	}

	private void isRat(Datum d, String n, String m) {
		if(d instanceof LispRational) {
			eq(((LispRational)d).getNumerator(),   new BigInteger(n));
			eq(((LispRational)d).getDenominator(), new BigInteger(m));
		} else {
			fail();
		}
	}

	private void isFlo(Datum d, double v) {
		if(d instanceof LispReal) {
			eq(d.getRealDouble(), v);
		} else {
			fail(d.toString());
		}
	}

	private void isCom(Datum d, double r, double i) {
		if(d instanceof LispComplex) {
			eq(d.getRealDouble(), r);
			eq(d.getImagDouble(), i);
		} else {
			fail(d.toString());
		}
	}

	public void testRange() {
		Scheme l = Scheme.newInstance();

		isBig(l.exec("(+  2147483647 1)"), "2147483648");
		isFix(l.exec("(-  2147483648 1)"), 2147483647);
		isBig(l.exec("(- -2147483648 1)"), "-2147483649");
		isFix(l.exec("(+ -2147483649 1)"), -2147483648);
		isBig(l.exec("(*  1073741824 2)"), "2147483648");
		isFix(l.exec("(/  2147483648 2)"), 1073741824);
		isFix(l.exec("(* -1073741824 2)"), -2147483648);
		isBig(l.exec("(* -1073741825 2)"), "-2147483650");
		isFix(l.exec("(/ -2147483650 2)"), -1073741825);
		isRat(l.exec("(/ 5 2)"), "5", "2");
		isFix(l.exec("(/ 6 2)"), 3);
		isBig(l.exec("(/ 4294967296 2)"), "2147483648");
		isFix(l.exec("(* 5/2 2)"), 5);
		isBig(l.exec("(* -2147483649/2 2)"), "-2147483649");
		isFix(l.exec("(+ 5/2 5/2)"), 5);
		isBig(l.exec("(+ -2147483649/2 -2147483649/2)"), "-2147483649");
		isFix(l.exec("(- 5/2 5/2)"), 0);
		isBig(l.exec("(- -2147483649/2 2147483649/2)"), "-2147483649");
		isFlo(l.exec("(+ 1 2.0)"), 3.0);
		isFlo(l.exec("(+ 1/2 0.5)"), 1.0);
		isFlo(l.exec("(- 1 2.0)"), -1.0);
		isFlo(l.exec("(- 1/2 0.5)"), 0.0);
		isFlo(l.exec("(* 1 2.0)"), 2.0);
		isFlo(l.exec("(* 1/2 0.5)"), 0.25);
		isFlo(l.exec("(/ 1 2.0)"), 0.5);
		isFlo(l.exec("(/ 1/2 0.5)"), 1.0);
		isCom(l.exec("(+ 1 1+i)"), 2.0, 1.0);
		isCom(l.exec("(+ 1/2 1+i)"), 1.5, 1.0);
		isFlo(l.exec("(+ 1+i 1-i)"), 2.0);
		isCom(l.exec("(- 1 1+i)"), 0.0, -1.0);
		isCom(l.exec("(- 1/2 1+i)"), -0.5, -1.0);
		isFlo(l.exec("(- 1+i 1+i)"), 0.0);
		isCom(l.exec("(* 1 1+i)"), 1.0, 1.0);
		isCom(l.exec("(* 1/2 1+i)"), 0.5, 0.5);
		isFlo(l.exec("(* 1+i 1-i)"), 2.0);
		isCom(l.exec("(/ 1+i 2)"), 0.5, 0.5);
		isCom(l.exec("(/ 1+i 1/2)"), 2.0, 2.0);
		isFlo(l.exec("(/ 1+i 1+i)"), 1.0);
	}

}
