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

import net.morilib.lisp.LispComplex;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/09
 */
public class Complex2Test extends TCSubr {

	public void testInfAdd() {
		Scheme l = Scheme.newInstance();

		eqv  (l,"(+ +inf.0@1 1)", LispComplex.INFINITY);
		eqv  (l,"(+ +inf.0@1 -1)", LispComplex.INFINITY);
		eqv  (l,"(+ +inf.0@1 1+i)", LispComplex.INFINITY);
		isNaN(l,"(+ +inf.0@1 1+i+j)");
		eqv  (l,"(+ 1 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(+ 9999999999999999999999999999999 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(+ 1/2 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(+ 1.0 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(+ 1.0f0 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(+ 1.0s0 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(+ 1.0df0 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(+ 1.0dd0 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(+ 1+i +inf.0@1)", LispComplex.INFINITY);
		isNaN(l,"(+ 1+i+j +inf.0@1)");
		isNaN(l,"(+ +inf.0@1 +inf.0@1)");
	}

	public void testInfSub() {
		Scheme l = Scheme.newInstance();

		eqv  (l,"(- +inf.0@1 1)", LispComplex.INFINITY);
		eqv  (l,"(- +inf.0@1 -1)", LispComplex.INFINITY);
		eqv  (l,"(- +inf.0@1 1+i)", LispComplex.INFINITY);
		isNaN(l,"(- +inf.0@1 1+i+j)");
		eqv  (l,"(- 1 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(- 9999999999999999999999999999999 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(- 1/2 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(- 1.0 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(- 1.0f0 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(- 1.0s0 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(- 1.0df0 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(- 1.0dd0 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(- 1+i +inf.0@1)", LispComplex.INFINITY);
		isNaN(l,"(- 1+i+j +inf.0@1)");
		isNaN(l,"(- +inf.0@1 +inf.0@1)");
		eqv  (l,"(- +inf.0@1)", LispComplex.INFINITY);
	}

	public void testInfMul() {
		Scheme l = Scheme.newInstance();

		eqv  (l,"(* +inf.0@1 1)", LispComplex.INFINITY);
		eqv  (l,"(* +inf.0@1 -1)", LispComplex.INFINITY);
		eqv  (l,"(* +inf.0@1 1+i)", LispComplex.INFINITY);
		isNaN(l,"(* +inf.0@1 1+i+j)");
		eqv  (l,"(* 1 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(* 9999999999999999999999999999999 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(* 1/2 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(* 1.0 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(* 1.0f0 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(* 1.0s0 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(* 1.0df0 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(* 1.0dd0 +inf.0@1)", LispComplex.INFINITY);
		eqv  (l,"(* 1+i +inf.0@1)", LispComplex.INFINITY);
		isNaN(l,"(* 1+i+j +inf.0@1)");
		isNaN(l,"(* +inf.0@1 +inf.0@1)");
	}

	public void testInfDiv() {
		Scheme l = Scheme.newInstance();

		eqv  (l,"(/ +inf.0@1 1)", LispComplex.INFINITY);
		eqv  (l,"(/ +inf.0@1 -1)", LispComplex.INFINITY);
		eqv  (l,"(/ +inf.0@1 1+i)", LispComplex.INFINITY);
		isNaN(l,"(/ +inf.0@1 1+i+j)");
		eqv  (l,"(/ 1 +inf.0@1)", LispInteger.ZERO);
		eqv  (l,"(/ 9999999999999999999999999999999 +inf.0@1)", LispInteger.ZERO);
		eqv  (l,"(/ 1/2 +inf.0@1)", LispInteger.ZERO);
		eqv  (l,"(/ 1.0 +inf.0@1)", LispDouble.ZERO);
		eqv  (l,"(/ 1.0f0 +inf.0@1)", LispDouble.ZERO);
		eqv  (l,"(/ 1.0s0 +inf.0@1)", LispDouble.ZERO);
		eqv  (l,"(/ 1.0df0 +inf.0@1)", LispDouble.ZERO);
		eqv  (l,"(/ 1.0dd0 +inf.0@1)", LispDouble.ZERO);
		eqv  (l,"(/ 1+i +inf.0@1)", LispInteger.ZERO);
		isNaN(l,"(/ 1+i+j +inf.0@1)");
		isNaN(l,"(/ +inf.0@1 +inf.0@1)");
		eqv  (l,"(/ +inf.0@1)", LispInteger.ZERO);
	}

}
