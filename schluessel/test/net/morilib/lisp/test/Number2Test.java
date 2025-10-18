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
package net.morilib.lisp.test;

import net.morilib.lang.Decimal32;
import net.morilib.lang.Decimal64;
import net.morilib.lang.DoubleUtils;
import net.morilib.lisp.Datum;
import net.morilib.lisp.LispDecimal32;
import net.morilib.lisp.LispDecimal64;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispFloat;
import net.morilib.lisp.LispHalf;
import net.morilib.lisp.Scheme;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/11/11
 */
public class Number2Test extends TCSubr {

	private void eqh(Scheme l, String s, double x) {
		Datum d = l.exec(s);

		if(d instanceof LispHalf) {
			eq(d, new LispHalf(DoubleUtils.toHalf(x)));
		} else {
			fail(d.toString());
		}
	}

	private void eqf(Scheme l, String s, double x) {
		Datum d = l.exec(s);

		if(d instanceof LispFloat) {
			eq(d, new LispFloat((float)x));
		} else {
			fail(d.toString());
		}
	}

	private void eqd(Scheme l, String s, double x) {
		Datum d = l.exec(s);

		if(d instanceof LispDouble) {
			eq(d, new LispDouble(x));
		} else {
			fail(d.toString());
		}
	}

	private void eq3(Scheme l, String s, double x) {
		Datum d = l.exec(s);

		if(d instanceof LispDecimal32) {
			eq(d, new LispDecimal32(Decimal32.toDecimal(x)));
		} else {
			fail(d.toString());
		}
	}

	private void eq6(Scheme l, String s, double x) {
		Datum d = l.exec(s);

		if(d instanceof LispDecimal64) {
			eq(d, new LispDecimal64(Decimal64.toDecimal(x)));
		} else {
			fail(d.toString());
		}
	}

	public void testAdd() {
		Scheme l = Scheme.newInstance();

		eqh(l, "(+ 3s0  3s0)", 6);
		eqf(l, "(+ 3f0  3s0)", 6);
		eqd(l, "(+ 3d0  3s0)", 6);
		eqd(l, "(+ 3l0  3s0)", 6);
		eq3(l, "(+ 3df0 3s0)", 6);
		eq6(l, "(+ 3dd0 3s0)", 6);
		eqh(l, "(+ 3    3s0)", 6);

		eqf(l, "(+ 3s0  3f0)", 6);
		eqf(l, "(+ 3f0  3f0)", 6);
		eqd(l, "(+ 3d0  3f0)", 6);
		eqd(l, "(+ 3l0  3f0)", 6);
		eq3(l, "(+ 3df0 3f0)", 6);
		eq6(l, "(+ 3dd0 3f0)", 6);
		eqf(l, "(+ 3    3f0)", 6);

		eqd(l, "(+ 3s0  3d0)", 6);
		eqd(l, "(+ 3f0  3d0)", 6);
		eqd(l, "(+ 3d0  3d0)", 6);
		eqd(l, "(+ 3l0  3d0)", 6);
		eqd(l, "(+ 3df0 3d0)", 6);
		eq6(l, "(+ 3dd0 3d0)", 6);
		eqd(l, "(+ 3    3d0)", 6);

		eqd(l, "(+ 3s0  3l0)", 6);
		eqd(l, "(+ 3f0  3l0)", 6);
		eqd(l, "(+ 3d0  3l0)", 6);
		eqd(l, "(+ 3l0  3l0)", 6);
		eqd(l, "(+ 3df0 3l0)", 6);
		eq6(l, "(+ 3dd0 3l0)", 6);
		eqd(l, "(+ 3    3l0)", 6);

		eq3(l, "(+ 3s0  3df0)", 6);
		eq3(l, "(+ 3f0  3df0)", 6);
		eq3(l, "(+ 3d0  3df0)", 6);
		eq3(l, "(+ 3l0  3df0)", 6);
		eq3(l, "(+ 3df0 3df0)", 6);
		eq6(l, "(+ 3dd0 3df0)", 6);
		eq3(l, "(+ 3    3df0)", 6);

		eq6(l, "(+ 3s0  3dd0)", 6);
		eq6(l, "(+ 3f0  3dd0)", 6);
		eq6(l, "(+ 3d0  3dd0)", 6);
		eq6(l, "(+ 3l0  3dd0)", 6);
		eq6(l, "(+ 3df0 3dd0)", 6);
		eq6(l, "(+ 3dd0 3dd0)", 6);
		eq6(l, "(+ 3    3dd0)", 6);
	}

	public void testSub() {
		Scheme l = Scheme.newInstance();

		eqh(l, "(- 3s0  2s0)", 1);
		eqf(l, "(- 3f0  2s0)", 1);
		eqd(l, "(- 3d0  2s0)", 1);
		eqd(l, "(- 3l0  2s0)", 1);
		eq3(l, "(- 3df0 2s0)", 1);
		eq6(l, "(- 3dd0 2s0)", 1);
		eqh(l, "(- 3    2s0)", 1);

		eqf(l, "(- 3s0  2f0)", 1);
		eqf(l, "(- 3f0  2f0)", 1);
		eqd(l, "(- 3d0  2f0)", 1);
		eqd(l, "(- 3l0  2f0)", 1);
		eq3(l, "(- 3df0 2f0)", 1);
		eq6(l, "(- 3dd0 2f0)", 1);
		eqf(l, "(- 3    2f0)", 1);

		eqd(l, "(- 3s0  2d0)", 1);
		eqd(l, "(- 3f0  2d0)", 1);
		eqd(l, "(- 3d0  2d0)", 1);
		eqd(l, "(- 3l0  2d0)", 1);
		eqd(l, "(- 3df0 2d0)", 1);
		eq6(l, "(- 3dd0 2d0)", 1);
		eqd(l, "(- 3    2d0)", 1);

		eqd(l, "(- 3s0  2l0)", 1);
		eqd(l, "(- 3f0  2l0)", 1);
		eqd(l, "(- 3d0  2l0)", 1);
		eqd(l, "(- 3l0  2l0)", 1);
		eqd(l, "(- 3df0 2l0)", 1);
		eq6(l, "(- 3dd0 2l0)", 1);
		eqd(l, "(- 3    2l0)", 1);

		eq3(l, "(- 3s0  2df0)", 1);
		eq3(l, "(- 3f0  2df0)", 1);
		eq3(l, "(- 3d0  2df0)", 1);
		eq3(l, "(- 3l0  2df0)", 1);
		eq3(l, "(- 3df0 2df0)", 1);
		eq6(l, "(- 3dd0 2df0)", 1);
		eq3(l, "(- 3    2df0)", 1);

		eq6(l, "(- 3s0  2dd0)", 1);
		eq6(l, "(- 3f0  2dd0)", 1);
		eq6(l, "(- 3d0  2dd0)", 1);
		eq6(l, "(- 3l0  2dd0)", 1);
		eq6(l, "(- 3df0 2dd0)", 1);
		eq6(l, "(- 3dd0 2dd0)", 1);
		eq6(l, "(- 3    2dd0)", 1);
	}

	public void testMul() {
		Scheme l = Scheme.newInstance();

		eqh(l, "(* 3s0  3s0)", 9);
		eqf(l, "(* 3f0  3s0)", 9);
		eqd(l, "(* 3d0  3s0)", 9);
		eqd(l, "(* 3l0  3s0)", 9);
		eq3(l, "(* 3df0 3s0)", 9);
		eq6(l, "(* 3dd0 3s0)", 9);
		eqh(l, "(* 3    3s0)", 9);

		eqf(l, "(* 3s0  3f0)", 9);
		eqf(l, "(* 3f0  3f0)", 9);
		eqd(l, "(* 3d0  3f0)", 9);
		eqd(l, "(* 3l0  3f0)", 9);
		eq3(l, "(* 3df0 3f0)", 9);
		eq6(l, "(* 3dd0 3f0)", 9);
		eqf(l, "(* 3    3f0)", 9);

		eqd(l, "(* 3s0  3d0)", 9);
		eqd(l, "(* 3f0  3d0)", 9);
		eqd(l, "(* 3d0  3d0)", 9);
		eqd(l, "(* 3l0  3d0)", 9);
		eqd(l, "(* 3df0 3d0)", 9);
		eq6(l, "(* 3dd0 3d0)", 9);
		eqd(l, "(* 3    3d0)", 9);

		eqd(l, "(* 3s0  3l0)", 9);
		eqd(l, "(* 3f0  3l0)", 9);
		eqd(l, "(* 3d0  3l0)", 9);
		eqd(l, "(* 3l0  3l0)", 9);
		eqd(l, "(* 3df0 3l0)", 9);
		eq6(l, "(* 3dd0 3l0)", 9);
		eqd(l, "(* 3    3l0)", 9);

		eq3(l, "(* 3s0  3df0)", 9);
		eq3(l, "(* 3f0  3df0)", 9);
		eq3(l, "(* 3d0  3df0)", 9);
		eq3(l, "(* 3l0  3df0)", 9);
		eq3(l, "(* 3df0 3df0)", 9);
		eq6(l, "(* 3dd0 3df0)", 9);
		eq3(l, "(* 3    3df0)", 9);

		eq6(l, "(* 3s0  3dd0)", 9);
		eq6(l, "(* 3f0  3dd0)", 9);
		eq6(l, "(* 3d0  3dd0)", 9);
		eq6(l, "(* 3l0  3dd0)", 9);
		eq6(l, "(* 3df0 3dd0)", 9);
		eq6(l, "(* 3dd0 3dd0)", 9);
		eq6(l, "(* 3    3dd0)", 9);
	}

	public void testDiv() {
		Scheme l = Scheme.newInstance();

		eqh(l, "(/ 3s0  3s0)", 1);
		eqf(l, "(/ 3f0  3s0)", 1);
		eqd(l, "(/ 3d0  3s0)", 1);
		eqd(l, "(/ 3l0  3s0)", 1);
		eq3(l, "(/ 3df0 3s0)", 1);
		eq6(l, "(/ 3dd0 3s0)", 1);
		eqh(l, "(/ 3    3s0)", 1);

		eqf(l, "(/ 3s0  3f0)", 1);
		eqf(l, "(/ 3f0  3f0)", 1);
		eqd(l, "(/ 3d0  3f0)", 1);
		eqd(l, "(/ 3l0  3f0)", 1);
		eq3(l, "(/ 3df0 3f0)", 1);
		eq6(l, "(/ 3dd0 3f0)", 1);
		eqf(l, "(/ 3    3f0)", 1);

		eqd(l, "(/ 3s0  3d0)", 1);
		eqd(l, "(/ 3f0  3d0)", 1);
		eqd(l, "(/ 3d0  3d0)", 1);
		eqd(l, "(/ 3l0  3d0)", 1);
		eqd(l, "(/ 3df0 3d0)", 1);
		eq6(l, "(/ 3dd0 3d0)", 1);
		eqd(l, "(/ 3    3d0)", 1);

		eqd(l, "(/ 3s0  3l0)", 1);
		eqd(l, "(/ 3f0  3l0)", 1);
		eqd(l, "(/ 3d0  3l0)", 1);
		eqd(l, "(/ 3l0  3l0)", 1);
		eqd(l, "(/ 3df0 3l0)", 1);
		eq6(l, "(/ 3dd0 3l0)", 1);
		eqd(l, "(/ 3    3l0)", 1);

		eq3(l, "(/ 3s0  3df0)", 1);
		eq3(l, "(/ 3f0  3df0)", 1);
		eq3(l, "(/ 3d0  3df0)", 1);
		eq3(l, "(/ 3l0  3df0)", 1);
		eq3(l, "(/ 3df0 3df0)", 1);
		eq6(l, "(/ 3dd0 3df0)", 1);
		eq3(l, "(/ 3    3df0)", 1);

		eq6(l, "(/ 3s0  3dd0)", 1);
		eq6(l, "(/ 3f0  3dd0)", 1);
		eq6(l, "(/ 3d0  3dd0)", 1);
		eq6(l, "(/ 3l0  3dd0)", 1);
		eq6(l, "(/ 3df0 3dd0)", 1);
		eq6(l, "(/ 3dd0 3dd0)", 1);
		eq6(l, "(/ 3    3dd0)", 1);
	}

}
