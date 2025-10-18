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

import java.io.IOException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispQuaternion;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.Parser;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/22
 */
public class QuaternionTest extends TCSubr {
	
	private static Datum read(Parser p, String exp) throws IOException {
		p.clear();
		p.read(exp);
		return p.parse() ? p.getDatum() : null;
	}

	private void eqquo(Datum d, LispReal r, LispReal i, LispReal j,
			LispReal k) {
		LispQuaternion q = (LispQuaternion)d;
		LispReal[] im = q.getImagsAsQuaternion();

		eqv(q.getReal(), r);
		eqv(im[0], i);
		eqv(im[1], j);
		eqv(im[2], k);
	}

	private void eqquo(Scheme l, String s, LispReal r, LispReal i,
			LispReal j, LispReal k) {
		eqquo(l.input(s), r, i, j, k);
	}

	private void eqs2n(Scheme l, String s, LispReal r, LispReal i,
			LispReal j, LispReal k) {
		eqquo(l.input("(string->number \"" + s + "\")"), r, i, j, k);
	}

//	private LispQuaternion quo(LispReal r, LispReal i, LispReal j,
//			LispReal k) {
//		return LispQuaternion.newQuaternion(r, i, j, k);
//	}

	public void testReader() throws IOException {
		Parser p = new Parser();

		eqquo(read(p, "1+2i+3j+4k"), newZ(1), newZ(2), newZ(3), newZ(4));
		eqquo(read(p, "1-2i+3j-4k"), newZ(1), newZ(-2), newZ(3), newZ(-4));
		eqquo(read(p, "1+i+j+k"), newZ(1), newZ(1), newZ(1), newZ(1));
		eqquo(read(p, "1-i+j-k"), newZ(1), newZ(-1), newZ(1), newZ(-1));
		eqquo(read(p, "1+i+k"), newZ(1), newZ(1), newZ(0), newZ(1));
		eqquo(read(p, "1+i"), newZ(1), newZ(1), newZ(0), newZ(0));
		eqquo(read(p, "+i+j+k"), newZ(0), newZ(1), newZ(1), newZ(1));
		eqquo(read(p, "1+3/2j"), newZ(1), newZ(0), newQ(3, 2), newZ(0));
		eqquo(read(p, "1+2.3j"), newR(1.0), newR(0.0), newR(2.3), newR(0.0));
		eqquo(read(p, "1+inf.0j"), newR(1.0), newR(0.0), newR(Double.POSITIVE_INFINITY), newR(0.0));
		eqquo(read(p, "1-inf.0j"), newR(1.0), newR(0.0), newR(Double.NEGATIVE_INFINITY), newR(0.0));
		eqquo(read(p, "1+inf.0i"), newR(1.0), newR(Double.POSITIVE_INFINITY), newR(0.0), newR(0.0));
		eqquo(read(p, "1-inf.0i"), newR(1.0), newR(Double.NEGATIVE_INFINITY), newR(0.0), newR(0.0));
		eq   (read(p, "1+i+i"), sym("1+i+i"));
	}

	public void testAdd() {
		Scheme l = Scheme.newInstance();

		eqquo(l,"(+ 1+i+j+k +2i+4j+7k)", newZ(1), newZ(3), newZ(5), newZ(8));
		eqquo(l,"(+ 1+i+j+k +2.0i+4j+7k)", newR(1.0), newR(3.0), newR(5.0), newR(8.0));
		eqquo(l,"(+ 1+i+j+k -1-i)", newZ(0), newZ(0), newZ(1), newZ(1));
		eqquo(l,"(+ 1+i+j+k -j-k)", newZ(1), newZ(1), newZ(0), newZ(0));
	}

	public void testSub() {
		Scheme l = Scheme.newInstance();

		eqquo(l,"(- 1+i+j+k +2i+4j+7k)", newZ(1), newZ(-1), newZ(-3), newZ(-6));
		eqquo(l,"(- 1+i+j+k +2.0i+4j+7k)", newR(1.0), newR(-1.0), newR(-3.0), newR(-6.0));
		eqquo(l,"(- 1+i+j+k 1+i)", newZ(0), newZ(0), newZ(1), newZ(1));
		eqquo(l,"(- 1+i+j+k +j+k)", newZ(1), newZ(1), newZ(0), newZ(0));
	}

	public void testMulDiv() {
		Scheme l = Scheme.newInstance();

		eqquo(l,"(/ 2+2i+2j-2k)", newQ(1, 8), newQ(-1, 8), newQ(-1, 8), newQ(1, 8));
		eqv  (l,"(/ -2-2i+2j+2k 1+i-j-k)", newZ(-2));
		eqv  (l,"(* 2+2i+2j-2k (/ 2+2i+2j-2k))", newZ(1));
		eqquo(l,"(/ 2.0+2i+2j-2k)", newR(0.125), newR(-.125), newR(-.125), newR(.125));
		eqv  (l,"(/ -2.0-2i+2j+2k 1+i-j-k)", newC(-2.0, 0.0));
		eqv  (l,"(* 2.0+2i+2j-2k (/ 2+2i+2j-2k))", newC(1.0, 0.0));
	}

	public void testStringToNumber() throws IOException {
		Scheme l = Scheme.newInstance();

		eqs2n(l,"1+2i+3j+4k", newZ(1), newZ(2), newZ(3), newZ(4));
		eqs2n(l,"1-2i+3j-4k", newZ(1), newZ(-2), newZ(3), newZ(-4));
		eqs2n(l,"1+i+j+k", newZ(1), newZ(1), newZ(1), newZ(1));
		eqs2n(l,"1-i+j-k", newZ(1), newZ(-1), newZ(1), newZ(-1));
		eqs2n(l,"1+i+k", newZ(1), newZ(1), newZ(0), newZ(1));
		eqs2n(l,"1+i", newZ(1), newZ(1), newZ(0), newZ(0));
		eqs2n(l,"+i+j+k", newZ(0), newZ(1), newZ(1), newZ(1));
		eqs2n(l,"1+3/2j", newZ(1), newZ(0), newQ(3, 2), newZ(0));
		eqs2n(l,"1+2.3j", newR(1.0), newR(0.0), newR(2.3), newR(0.0));
		eqs2n(l,"1+inf.0j", newR(1.0), newR(0.0), newR(Double.POSITIVE_INFINITY), newR(0.0));
		eqs2n(l,"1-inf.0j", newR(1.0), newR(0.0), newR(Double.NEGATIVE_INFINITY), newR(0.0));
		eqs2n(l,"1+inf.0i", newR(1.0), newR(Double.POSITIVE_INFINITY), newR(0.0), newR(0.0));
		eqs2n(l,"1-inf.0i", newR(1.0), newR(Double.NEGATIVE_INFINITY), newR(0.0), newR(0.0));
		lperr(l,"(number->string \"1+i+i\")");
	}

	public void testEqual() {
		Scheme l = Scheme.newInstance();

		equal(l,"(= +i+j +i+j)", T);
		equal(l,"(= +i+j +i-j)", F);
		equal(l,"(= +i+j 1.0i+j)", F);
		equal(l,"(= +1.0i+j +i+j)", T);
	}

}
