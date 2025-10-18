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
 * @author MORIGUCHI, Yuichiro 2011/07/02
 */
public class SRFI94Test extends TCSubr {

	public void testRealExp() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		l.exec ("(print 'real-exp)");
		equal(l,"(real-exp 0)", newZ(1));
		l.exec ("(print (real-exp 1))");
		lperr(l,"(real-exp 1+i)");
	}

	public void testRealLn() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		l.exec ("(print 'real-ln)");
		equal(l,"(real-ln 1)", newR(0.0));
		l.exec ("(print (real-ln (real-exp 1)))");
		lperr(l,"(real-ln 0)");
		lperr(l,"(real-ln -1)");
		lperr(l,"(real-ln 1+i)");
	}

	public void testRealSin() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		l.exec ("(print 'real-sin)");
		equal(l,"(real-sin 0)", newR(0.0));
		l.exec ("(print (real-sin 3.14159265))");
		lperr(l,"(real-sin 1+i)");
	}

	public void testRealCos() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		l.exec ("(print 'real-cos)");
		equal(l,"(real-cos 0)", newR(1.0));
		l.exec ("(print (real-cos 3.14159265))");
		lperr(l,"(real-sin 1+i)");
	}

	public void testRealTan() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		l.exec ("(print 'real-tan)");
		equal(l,"(real-tan 0)", newR(0.0));
		l.exec ("(print (real-tan 3.14159265))");
		lperr(l,"(real-tan 1+i)");
	}

	public void testRealAsin() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		l.exec ("(print 'real-asin)");
		equal(l,"(real-asin 0)", newR(0.0));
		l.exec ("(print (real-asin 1))");
		lperr(l,"(real-asin -1.01)");
		lperr(l,"(real-asin 1.01)");
		lperr(l,"(real-asin 1+i)");
	}

	public void testRealAcos() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		l.exec ("(print 'real-acos)");
		equal(l,"(real-acos 1)", newR(0.0));
		l.exec ("(print (real-acos -1))");
		lperr(l,"(real-acos -1.01)");
		lperr(l,"(real-acos 1.01)");
		lperr(l,"(real-acos 1+i)");
	}

	public void testRealAtan() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		l.exec ("(print 'real-atan)");
		equal(l,"(real-atan 0)", newR(0.0));
		l.exec ("(print (* (real-atan 1) 4))");
		lperr(l,"(real-atan 1+i)");
	}

	public void testAbs() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		equal(l,"(abs -2)", newZ(2));
		equal(l,"(abs 2)", newZ(2));
		equal(l,"(abs -2.0)", newR(2.0));
		equal(l,"(abs 2.0)", newR(2.0));
		lperr(l,"(abs 1+i)");
	}

	public void testIntegerSqrt() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		equal(l,"(integer-sqrt 4)", newZ(2));
		lperr(l,"(integer-sqrt 3)");
		lperr(l,"(integer-sqrt -4)");
		lperr(l,"(integer-sqrt 4.0)");
		lperr(l,"(integer-sqrt 1+i)");
	}

	public void testRealSqrt() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		l.exec ("(print 'real-sqrt)");
		equal(l,"(real-sqrt 4)", newZ(2));
		equal(l,"(real-sqrt 4.0)", newR(2.0));
		l.exec ("(print (real-sqrt 3))");
		lperr(l,"(real-sqrt -4)");
		lperr(l,"(real-sqrt 1+i)");
	}

	public void testIntegerExpt() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		equal(l,"(integer-expt 2 4)", newZ(16));
		lperr(l,"(integer-expt 2.0 4)");
		lperr(l,"(integer-expt 2 4.0)");
		lperr(l,"(integer-expt 4 1/2)");
		lperr(l,"(integer-expt 1+i 2)");
		lperr(l,"(integer-expt 2 2+i)");
	}

	public void testRealExpt() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		equal(l,"(real-expt 2 4)", newZ(16));
		equal(l,"(real-expt 2.0 4)", newR(16.0));
		equal(l,"(real-expt 2 4.0)", newR(16.0));
		equal(l,"(real-expt 4 1/2)", newZ(2));
		lperr(l,"(real-expt 1+i 2)");
		lperr(l,"(real-expt 2 2+i)");
	}

	public void testIntegerLog() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		equal(l,"(integer-log 8 2)", newZ(3));
		lperr(l,"(integer-log 8.0 2)");
		lperr(l,"(integer-log 8 2.0)");
		lperr(l,"(integer-log 8 1/2)");
		lperr(l,"(integer-log 1+i 2)");
		lperr(l,"(integer-log 2 2+i)");
	}

	public void testRealLog() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		l.exec ("(print 'real-log)");
		equal(l,"(real-log 8 2)", newZ(3));
		l.exec ("(print (real-log 8.0 2))");
		lperr(l,"(real-log 1+i 2)");
		lperr(l,"(real-log 2 2+i)");
	}

	public void testQuo() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		equal(l,"(quo 2/3 1/5)", newZ(3));
		equal(l,"(quo -2/3 1/5)", newZ(-3));
		equal(l,"(quo 2/3 -1/5)", newZ(-3));
		equal(l,"(quo -2/3 -1/5)", newZ(3));
		equal(l,"(quo 3 0.8)", newR(3.0));
		equal(l,"(quo -3 0.8)", newR(-3.0));
		equal(l,"(quo 3 -0.8)", newR(-3.0));
		equal(l,"(quo -3 -0.8)", newR(3.0));
		lperr(l,"(quo 1 0)");
		lperr(l,"(quo 1+i 2)");
		lperr(l,"(quo 2 2+i)");
	}

	public void testRem() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		l.exec ("(print 'rem)");
		equal(l,"(rem 2/3 1/5)", newQ(1, 15));
		equal(l,"(rem -2/3 1/5)", newQ(-1, 15));
		equal(l,"(rem 2/3 -1/5)", newQ(1, 15));
		equal(l,"(rem -2/3 -1/5)", newQ(-1, 15));
		l.exec ("(print (rem 3 0.8))");
		l.exec ("(print (rem -3 0.8))");
		l.exec ("(print (rem 3 -0.8))");
		l.exec ("(print (rem -3 -0.8))");
		lperr(l,"(rem 1 0)");
		lperr(l,"(rem 1+i 2)");
		lperr(l,"(rem 2 2+i)");
	}

	public void testMod() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		l.exec ("(print 'mod)");
		equal(l,"(mod 2/3 1/5)", newQ(1, 15));
		equal(l,"(mod -2/3 1/5)", newQ(2, 15));
		equal(l,"(mod 2/3 -1/5)", newQ(-2, 15));
		equal(l,"(mod -2/3 -1/5)", newQ(-1, 15));
		l.exec ("(print (mod 3 0.8))");
		l.exec ("(print (mod -3 0.8))");
		l.exec ("(print (mod 3 -0.8))");
		l.exec ("(print (mod -3 -0.8))");
		lperr(l,"(mod 1 0)");
		lperr(l,"(mod 1+i 2)");
		lperr(l,"(mod 2 2+i)");
	}

	public void testQuotient() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		equal(l,"(quotient 8 3)", newZ(2));
		lperr(l,"(quotient 2/3 1/5)");
		lperr(l,"(quotient 3 0.8)");
		lperr(l,"(quotient 1 0)");
		lperr(l,"(quotient 1+i 2)");
		lperr(l,"(quotient 2 2+i)");
	}

	public void testRemainder() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		equal(l,"(remainder 8 3)", newZ(2));
//		lperr(l,"(remainder 2/3 1/5)");
//		lperr(l,"(remainder 3 0.8)");
//		lperr(l,"(remainder 1 0)");
		lperr(l,"(remainder 1+i 2)");
		lperr(l,"(remainder 2 2+i)");
	}

	public void testModulo() {
		Scheme l = Scheme.newInstance();

		l.exec ("(use srfi-94)");
		equal(l,"(modulo 8 3)", newZ(2));
		lperr(l,"(modulo 2/3 1/5)");
		lperr(l,"(modulo 3 0.8)");
		lperr(l,"(modulo 1 0)");
		lperr(l,"(modulo 1+i 2)");
		lperr(l,"(modulo 2 2+i)");
	}

}
