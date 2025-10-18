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
package net.morilib.lisp.test.math;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/11/13
 */
public class Polynomial1Test extends TCSubr {

	public void testAdd() {
		Scheme l = Scheme.newInstance();

		equal(l,"(eqv? (+ (polynomial1 1 2 3) (polynomial1 1 1 1 1))" +
				" (polynomial1 1 2 3 4))", T);
		equal(l,"(eqv? (+ (polynomial1 1 2 3) (polynomial1 -1 -1 -1))" +
				" (polynomial1 1 2))", T);
		equal(l,"(eqv? (+ (polynomial1 1 1 1) (polynomial1 -1 -1 -1))" +
				" (polynomial1 0))", T);
		equal(l,"(eqv? (+ (polynomial1 1 1 1) (polynomial1 -1 -1 -1))" +
				" (polynomial1))", T);
		equal(l,"(eqv? (+ (polynomial1 1 1 1) (polynomial1))" +
				" (polynomial1 1 1 1))", T);
	}

	public void testSub() {
		Scheme l = Scheme.newInstance();

		equal(l,"(eqv? (- (polynomial1 1 2 3) (polynomial1 -1 -1 -1 -1))" +
				" (polynomial1 1 2 3 4))", T);
		equal(l,"(eqv? (- (polynomial1 1 2 3) (polynomial1 1 1 1))" +
				" (polynomial1 1 2))", T);
		equal(l,"(eqv? (- (polynomial1 1 1 1) (polynomial1 1 1 1))" +
				" (polynomial1 0))", T);
		equal(l,"(eqv? (- (polynomial1 1 1 1) (polynomial1))" +
				" (polynomial1 1 1 1))", T);
		equal(l,"(eqv? (- (polynomial1) (polynomial1 -1 -1 -1))" +
				" (polynomial1 1 1 1))", T);
		equal(l,"(eqv? (- (polynomial1 -1 -1 -1))" +
				" (polynomial1 1 1 1))", T);
	}

	public void testMul() {
		Scheme l = Scheme.newInstance();

		equal(l,"(eqv? (* (polynomial1 1 1) (polynomial1 1 1))" +
				" (polynomial1 1 2 1))", T);
		equal(l,"(eqv? (* (polynomial1 1 1) (polynomial1 1 1) (polynomial1 1 1))" +
				" (polynomial1 1 3 3 1))", T);
		equal(l,"(eqv? (* (polynomial1 1 1))" +
				" (polynomial1 1 1))", T);
		equal(l,"(eqv? (* (polynomial1 1 1) (polynomial1))" +
				" (polynomial1))", T);
	}

	public void testQuotient() {
		Scheme l = Scheme.newInstance();

		equal(l,"(eqv? (quotient (polynomial1 1 3 3 1) (polynomial1 1 1))" +
				" (polynomial1 1 2 1))", T);
		equal(l,"(eqv? (quotient (polynomial1 1 3 3 2) (polynomial1 1 1))" +
				" (polynomial1 1 2 1))", T);
		equal(l,"(eqv? (quotient (polynomial1 1 4 6 4 1) (polynomial1 1 2 1))" +
				" (polynomial1 1 2 1))", T);
		equal(l,"(eqv? (quotient (polynomial1 1 4 6 5 1) (polynomial1 1 2 1))" +
				" (polynomial1 1 2 1))", T);
	}

	public void testRemainder() {
		Scheme l = Scheme.newInstance();

		equal(l,"(eqv? (remainder (polynomial1 1 3 3 1) (polynomial1 1 1))" +
				" (polynomial1))", T);
		equal(l,"(eqv? (remainder (polynomial1 1 3 3 2) (polynomial1 1 1))" +
				" (polynomial1 1))", T);
		equal(l,"(eqv? (remainder (polynomial1 1 4 6 4 1) (polynomial1 1 2 1))" +
				" (polynomial1))", T);
		equal(l,"(eqv? (remainder (polynomial1 1 4 6 5 1) (polynomial1 1 2 1))" +
				" (polynomial1 1 0))", T);
	}

	public void testDifferenciate1() {
		Scheme l = Scheme.newInstance();

		equal(l,"(eqv? (differenciate1 (polynomial1 1 2 3 4))" +
				" (polynomial1 3 4 3))", T);
		equal(l,"(eqv? (differenciate1 (polynomial1 4))" +
				" (polynomial1))", T);
		equal(l,"(eqv? (differenciate1 (polynomial1))" +
				" (polynomial1))", T);
	}

	public void testIntegrate1() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(integrate1 (polynomial1 3 0 0) 0 2)", 8);
		eqi  (l,"(integrate1 (polynomial1 3 0 0) 1 2)", 7);
		eqi  (l,"(integrate1 (polynomial1 3 0 0) 2 0)", -8);
		eqi  (l,"(integrate1 (polynomial1) 1 2)", 0);
	}

	public void testSubstitute1() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(substitute1 (polynomial1 1 2 3) 3)", 9+6+3);
		eqi  (l,"(substitute1 (polynomial1 3) 3)", 3);
		eqi  (l,"(substitute1 (polynomial1) 3)", 0);
	}

	public void testDegreePolynomial1() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(degree-polynomial1 (polynomial1 1 2 3))", 2);
		eqi  (l,"(degree-polynomial1 (polynomial1 1))", 0);
		eqi  (l,"(degree-polynomial1 (polynomial1 0))", -1);
	}

	public void testIntegratePolynomial1() {
		Scheme l = Scheme.newInstance();

		equal(l,"(eqv? (integrate-polynomial1 (polynomial1 3 2 1))" +
				" (polynomial1 1 1 1 0))", T);
		equal(l,"(eqv? (integrate-polynomial1 (polynomial1 3 2 1) 2)" +
				" (polynomial1 1 1 1 2))", T);
		equal(l,"(eqv? (integrate-polynomial1 (polynomial1) 2)" +
				" (polynomial1 2))", T);
	}

	public void testPolynomial1Ref() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(polynomial1-ref (polynomial1 1 2 3) 2)", 1);
		eqi  (l,"(polynomial1-ref (polynomial1 1 2 3) 1)", 2);
		eqi  (l,"(polynomial1-ref (polynomial1 1 2 3) 0)", 3);
		eqi  (l,"(polynomial1-ref (polynomial1 1 2 3) 3)", 0);
		lperr(l,"(polynomial1-ref (polynomial1 1 2 3) -1)");
	}

}
