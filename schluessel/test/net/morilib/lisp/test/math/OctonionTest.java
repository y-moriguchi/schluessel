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
import net.morilib.lisp.LispOctonion;
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
public class OctonionTest extends TCSubr {
	
	private static Datum read(Parser p, String exp) throws IOException {
		p.clear();
		p.read(exp);
		return p.parse() ? p.getDatum() : null;
	}

	private void eqcmp(Datum d, LispReal r, LispReal i) {
		LispQuaternion q = (LispQuaternion)d;
		LispReal im = q.getImag();

		eqv(q.getReal(), r);
		eqv(im, i);
	}

	private void eqcmp(Scheme l, String s, LispReal r, LispReal i) {
		eqcmp(l.input(s), r, i);
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

	private void eqoct(Datum d, LispReal r, LispReal i, LispReal j,
			LispReal k, LispReal o, LispReal io, LispReal jo,
			LispReal ko) {
		LispOctonion q = (LispOctonion)d;
		LispReal[] im = q.getImagsAsOctonion();

		eqv(q.getReal(), r);
		eqv(im[0], i);
		eqv(im[1], j);
		eqv(im[2], k);
		eqv(im[3], o);
		eqv(im[4], io);
		eqv(im[5], jo);
		eqv(im[6], ko);
	}

	private void eqoct(Scheme l, String s, LispReal r, LispReal i,
			LispReal j, LispReal k, LispReal o, LispReal io,
			LispReal jo, LispReal ko) {
		eqoct(l.input(s), r, i, j, k, o, io, jo, ko);
	}

	private void eqs2n(Scheme l, String s, LispReal r, LispReal i,
			LispReal j, LispReal k, LispReal o, LispReal io,
			LispReal jo, LispReal ko) {
		eqoct(l.input("(string->number \"" + s + "\")"),
				r, i, j, k, o, io, jo, ko);
	}

//	private LispQuaternion quo(LispReal r, LispReal i, LispReal j,
//			LispReal k) {
//		return LispQuaternion.newQuaternion(r, i, j, k);
//	}

	public void testReader() throws IOException {
		Parser p = new Parser();

		eqoct(read(p, "1+2i+3j+4k+5o+6io+7jo+8ko"),
				newZ(1), newZ(2), newZ(3), newZ(4),
				newZ(5), newZ(6), newZ(7), newZ(8));
		eqoct(read(p, "1-2i+3j-4k+5o-6io+7jo-8ko"),
				newZ(1), newZ(-2), newZ(3), newZ(-4),
				newZ(5), newZ(-6), newZ(7), newZ(-8));
		eqoct(read(p, "1+i+j+k+o+io+jo+ko"),
				newZ(1), newZ(1), newZ(1), newZ(1),
				newZ(1), newZ(1), newZ(1), newZ(1));
		eqoct(read(p, "1-i+j-k+o-io+jo-ko"),
				newZ(1), newZ(-1), newZ(1), newZ(-1),
				newZ(1), newZ(-1), newZ(1), newZ(-1));
		eqoct(read(p, "1+i+k+io"),
				newZ(1), newZ(1), newZ(0), newZ(1),
				newZ(0), newZ(1), newZ(0), newZ(0));
		eqoct(read(p, "+i+j+k+io"),
				newZ(0), newZ(1), newZ(1), newZ(1),
				newZ(0), newZ(1), newZ(0), newZ(0));
		eqoct(read(p, "1+3/2io"),
				newZ(1), newZ(0), newZ(0), newZ(0),
				newZ(0), newQ(3, 2), newZ(0), newZ(0));
		eqoct(read(p, "1+2.3io"),
				newR(1.0), newR(0.0), newR(0.0), newR(0.0),
				newR(0.0), newR(2.3), newR(0.0), newR(0.0));
		eqoct(read(p, "1+inf.0io"),
				newR(1.0), newR(0.0), newR(0.0), newR(0.0),
				newR(0.0), newR(Double.POSITIVE_INFINITY), newR(0.0), newR(0.0));
		eqoct(read(p, "1-inf.0io"),
				newR(1.0), newR(0.0), newR(0.0), newR(0.0),
				newR(0.0), newR(Double.NEGATIVE_INFINITY), newR(0.0), newR(0.0));
		eq   (read(p, "1+io+io"), sym("1+i+i"));
	}

	public void testAdd() {
		Scheme l = Scheme.newInstance();

		eqoct(l,"(+ 1+i+j+k+o+io+jo+ko +2i+4j+7k+io+jo+ko)",
				newZ(1), newZ(3), newZ(5), newZ(8),
				newZ(1), newZ(2), newZ(2), newZ(2));
		eqoct(l,"(+ 1+i+j+k+o+io+jo+ko +2.0i+4j+7k+io+jo+ko)",
				newR(1.0), newR(3.0), newR(5.0), newR(8.0),
				newR(1.0), newR(2.0), newR(2.0), newR(2.0));
		eqoct(l,"(+ 1+i+o+io -1-i)",
				newZ(0), newZ(0), newZ(0), newZ(0), 
				newZ(1), newZ(1), newZ(0), newZ(0));
		eqoct(l,"(+ 1+i+o+io -o-io)",
				newZ(1), newZ(1), newZ(0), newZ(0),
				newZ(0), newZ(0), newZ(0), newZ(0));
	}

	public void testSub() {
		Scheme l = Scheme.newInstance();

		eqoct(l,"(- 1+i+j+k+o+io+jo+ko +2i+4j+7k+io+jo+ko)",
				newZ(1), newZ(-1), newZ(-3), newZ(-6),
				newZ(1), newZ(0), newZ(0), newZ(0));
		eqoct(l,"(- 1+i+j+k+o+io+jo+ko +2.0i+4j+7k+io+jo+ko)",
				newR(1.0), newR(-1.0), newR(-3.0), newR(-6.0),
				newR(1.0), newR(0.0), newR(0.0), newR(0.0));
		eqoct(l,"(- 1+i+o+io 1+i)",
				newZ(0), newZ(0), newZ(0), newZ(0), 
				newZ(1), newZ(1), newZ(0), newZ(0));
		eqoct(l,"(- 1+i+o+io +o+io)",
				newZ(1), newZ(1), newZ(0), newZ(0),
				newZ(0), newZ(0), newZ(0), newZ(0));
	}

	public void testMulDiv() {
		Scheme l = Scheme.newInstance();

		eqoct(l,"(/ 2+2i+2j-2k+2o+2io+2jo+2ko)",
				newQ(1, 16), newQ(-1, 16), newQ(-1, 16), newQ(1, 16),
				newQ(-1, 16), newQ(-1, 16), newQ(-1, 16), newQ(-1, 16));
		eqv  (l,"(/ -2-2i+2j+2k+2o+2io+2jo+2ko 1+i-j-k-o-io-jo-ko)", newZ(-2));
		eqv  (l,"(* 2+2i+2j-2k+2o+2io+2jo+2ko (/ 2+2i+2j-2k+2o+2io+2jo+2ko))", newZ(1));
		eqoct(l,"(/ 2.0+2i+2j-2k+2o+2io+2jo+2ko)",
				newR(0.0625), newR(-.0625), newR(-.0625), newR(.0625),
				newR(-.0625), newR(-.0625), newR(-.0625), newR(-.0625));
		eqv  (l,"(/ -2.0-2i+2j+2k+2o+2io+2jo+2ko 1+i-j-k-o-io-jo-ko)", newC(-2.0, 0.0));
		eqv  (l,"(* 2.0+2i+2j-2k+2o+2io+2jo+2ko (/ 2+2i+2j-2k+2o+2io+2jo+2ko))", newC(1.0, 0.0));
	}

	public void testStringToNumber() throws IOException {
		Scheme l = Scheme.newInstance();

		eqs2n(l,"1+2i+3j+4k+5o+6io+7jo+8ko",
				newZ(1), newZ(2), newZ(3), newZ(4),
				newZ(5), newZ(6), newZ(7), newZ(8));
		eqs2n(l,"1-2i+3j-4k+5o-6io+7jo-8ko",
				newZ(1), newZ(-2), newZ(3), newZ(-4),
				newZ(5), newZ(-6), newZ(7), newZ(-8));
		eqs2n(l,"1+i+j+k+o+io+jo+ko",
				newZ(1), newZ(1), newZ(1), newZ(1),
				newZ(1), newZ(1), newZ(1), newZ(1));
		eqs2n(l,"1-i+j-k+o-io+jo-ko",
				newZ(1), newZ(-1), newZ(1), newZ(-1),
				newZ(1), newZ(-1), newZ(1), newZ(-1));
		eqs2n(l,"1+i+k+io",
				newZ(1), newZ(1), newZ(0), newZ(1),
				newZ(0), newZ(1), newZ(0), newZ(0));
		eqs2n(l,"+i+j+k+io",
				newZ(0), newZ(1), newZ(1), newZ(1),
				newZ(0), newZ(1), newZ(0), newZ(0));
		eqs2n(l,"1+3/2io",
				newZ(1), newZ(0), newZ(0), newZ(0),
				newZ(0), newQ(3, 2), newZ(0), newZ(0));
		eqs2n(l,"1+2.3io",
				newR(1.0), newR(0.0), newR(0.0), newR(0.0),
				newR(0.0), newR(2.3), newR(0.0), newR(0.0));
		eqs2n(l,"1+inf.0io",
				newR(1.0), newR(0.0), newR(0.0), newR(0.0),
				newR(0.0), newR(Double.POSITIVE_INFINITY), newR(0.0), newR(0.0));
		eqs2n(l,"1-inf.0io",
				newR(1.0), newR(0.0), newR(0.0), newR(0.0),
				newR(0.0), newR(Double.NEGATIVE_INFINITY), newR(0.0), newR(0.0));
		lperr(l,"(number->string \"1+io+io\")");
	}

	public void testEqual() {
		Scheme l = Scheme.newInstance();

		equal(l,"(= +i+jo +i+jo)", T);
		equal(l,"(= +i+jo +i-jo)", F);
		equal(l,"(= +i+jo 1.0i+jo)", F);
		equal(l,"(= +1.0i+jo +i+jo)", T);
	}

	public void testAddSmallInt() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(+ 2 1+2i)", newZ(3), newZ(2));
		eqcmp(l,"(+ 2 1.0+2i)", newR(3.0), newR(2.0));
		eqquo(l,"(+ 2 1+2i+3j+4k)", newZ(3), newZ(2), newZ(3), newZ(4));
		eqquo(l,"(+ 2 1.0+2i+3j+4k)", newR(3.0), newR(2.0), newR(3.0), newR(4.0));
		eqoct(l,"(+ 2 1+2i+3j+4k+o+io+jo+ko)",
				newZ(3), newZ(2), newZ(3), newZ(4),
				newZ(1), newZ(1), newZ(1), newZ(1));
		eqoct(l,"(+ 2 1.0+2i+3j+4k+o+io+jo+ko)",
				newR(3.0), newR(2.0), newR(3.0), newR(4.0),
				newR(1.0), newR(1.0), newR(1.0), newR(1.0));
	}

	public void testAddBigInt() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(+ 20000000000000 1+2i)", newZ(20000000000001l), newZ(2));
		eqcmp(l,"(+ 20000000000000 1.0+2i)", newR(20000000000001.0), newR(2.0));
		eqquo(l,"(+ 20000000000000 1+2i+3j+4k)",
				newZ(20000000000001l), newZ(2), newZ(3), newZ(4));
		eqquo(l,"(+ 20000000000000 1.0+2i+3j+4k)",
				newR(20000000000001.0), newR(2.0), newR(3.0), newR(4.0));
		eqoct(l,"(+ 20000000000000 1+2i+3j+4k+o+io+jo+ko)",
				newZ(20000000000001l), newZ(2), newZ(3), newZ(4),
				newZ(1), newZ(1), newZ(1), newZ(1));
		eqoct(l,"(+ 20000000000000 1.0+2i+3j+4k+o+io+jo+ko)",
				newR(20000000000001.0), newR(2.0), newR(3.0), newR(4.0),
				newR(1.0), newR(1.0), newR(1.0), newR(1.0));
	}

	public void testAddRational() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(+ 1/2 1+2i)", newQ(3, 2), newZ(2));
		eqcmp(l,"(+ 1/2 1.0+2i)", newR(1.5), newR(2.0));
		eqquo(l,"(+ 1/2 1+2i+3j+4k)", newQ(3, 2), newZ(2), newZ(3), newZ(4));
		eqquo(l,"(+ 1/2 1.0+2i+3j+4k)", newR(1.5), newR(2.0), newR(3.0), newR(4.0));
		eqoct(l,"(+ 1/2 1+2i+3j+4k+o+io+jo+ko)",
				newQ(3, 2), newZ(2), newZ(3), newZ(4),
				newZ(1), newZ(1), newZ(1), newZ(1));
		eqoct(l,"(+ 1/2 1.0+2i+3j+4k+o+io+jo+ko)",
				newR(1.5), newR(2.0), newR(3.0), newR(4.0),
				newR(1.0), newR(1.0), newR(1.0), newR(1.0));
	}

	public void testAddDouble() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(+ 2.0 1+2i)", newR(3.0), newR(2.0));
		eqcmp(l,"(+ 2.0 1.0+2i)", newR(3.0), newR(2.0));
		eqquo(l,"(+ 2.0 1+2i+3j+4k)", newR(3.0), newR(2.0), newR(3.0), newR(4.0));
		eqquo(l,"(+ 2.0 1.0+2i+3j+4k)", newR(3.0), newR(2.0), newR(3.0), newR(4.0));
		eqoct(l,"(+ 2.0 1+2i+3j+4k+o+io+jo+ko)",
				newR(3.0), newR(2.0), newR(3.0), newR(4.0),
				newR(1.0), newR(1.0), newR(1.0), newR(1.0));
		eqoct(l,"(+ 2.0 1.0+2i+3j+4k+o+io+jo+ko)",
				newR(3.0), newR(2.0), newR(3.0), newR(4.0),
				newR(1.0), newR(1.0), newR(1.0), newR(1.0));
	}

	public void testAddExactComplex() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(+ 2+2i 2)", newZ(4), newZ(2));
		eqcmp(l,"(+ 2+2i 20000000000000)", newZ(20000000000002l), newZ(2));
		eqcmp(l,"(+ 2+2i 1/2)", newQ(5, 2), newZ(2));
		eqcmp(l,"(+ 2+2i 1.0)", newR(3.0), newR(2.0));
		equal(l,"(+ 1-2i 1+2i)", newZ(2));
		equal(l,"(+ 1-2i 1.0+2i)", newC(2.0, 0.0));
		eqquo(l,"(+ 1-i 1+i+j+k)", newZ(2), newZ(0), newZ(1), newZ(1));
		eqquo(l,"(+ 1-i 1.0+i+j+k)", newR(2.0), newR(0.0), newR(1.0), newR(1.0));
	}

	public void testAddDoubleComplex() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(+ 2.0+2i 2)", newR(4.0), newR(2.0));
		eqcmp(l,"(+ 2.0+2i 20000000000000)", newR(20000000000002.0), newR(2.0));
		eqcmp(l,"(+ 2.0+2i 1/2)", newR(2.5), newR(2.0));
		eqcmp(l,"(+ 2.0+2i 1.0)", newR(3.0), newR(2.0));
		equal(l,"(+ 1.0-2i 1+2i)", newC(2.0, 0.0));
		equal(l,"(+ 1.0-2i 1.0+2i)", newC(2.0, 0.0));
		eqquo(l,"(+ 1.0-i 1+i+j+k)", newR(2.0), newR(0.0), newR(1.0), newR(1.0));
		eqquo(l,"(+ 1.0-i 1.0+i+j+k)", newR(2.0), newR(0.0), newR(1.0), newR(1.0));
	}

	public void testAddExactQuaternion() {
		Scheme l = Scheme.newInstance();

		eqquo(l,"(+ 2+2i+2j+2k 2)", newZ(4), newZ(2), newZ(2), newZ(2));
		eqquo(l,"(+ 2+2i+2j+2k 20000000000000)",
				newZ(20000000000002l), newZ(2), newZ(2), newZ(2));
		eqquo(l,"(+ 2+2i+2j+2k 1/2)", newQ(5, 2), newZ(2), newZ(2), newZ(2));
		eqquo(l,"(+ 2+2i+2j+2k 1.0)", newR(3.0), newR(2.0), newR(2.0), newR(2.0));
		eqquo(l,"(+ 1+i+j+k 1-i)", newZ(2), newZ(0), newZ(1), newZ(1));
		eqquo(l,"(+ 1+i+j+k 1.0-i)", newR(2.0), newR(0.0), newR(1.0), newR(1.0));
		equal(l,"(+ 1-i-j-k 1+i+j+k)", newZ(2));
		equal(l,"(+ 1-i-j-k 1.0+i+j+k)", newC(2.0, 0.0));
	}

	public void testAddDoubleQuaternion() {
		Scheme l = Scheme.newInstance();

		eqquo(l,"(+ 2.0+2i+2j+2k 2)", newR(4.0), newR(2.0), newR(2.0), newR(2.0));
		eqquo(l,"(+ 2.0+2i+2j+2k 20000000000000)",
				newR(20000000000002.0), newR(2.0), newR(2.0), newR(2.0));
		eqquo(l,"(+ 2.0+2i+2j+2k 1/2)", newR(2.5), newR(2.0), newR(2.0), newR(2.0));
		eqquo(l,"(+ 2.0+2i+2j+2k 1.0)", newR(3.0), newR(2.0), newR(2.0), newR(2.0));
		eqquo(l,"(+ 1.0+i+j+k 1-i)", newR(2.0), newR(0.0), newR(1.0), newR(1.0));
		eqquo(l,"(+ 1.0+i+j+k 1.0-i)", newR(2.0), newR(0.0), newR(1.0), newR(1.0));
		equal(l,"(+ 1.0-i-j-k 1+i+j+k)", newC(2.0, 0.0));
		equal(l,"(+ 1.0-i-j-k 1.0+i+j+k)", newC(2.0, 0.0));
	}

	public void testSubSmallInt() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(- 2 1+2i)", newZ(1), newZ(-2));
		eqcmp(l,"(- 2 1.0+2i)", newR(1.0), newR(-2.0));
		eqquo(l,"(- 2 1+2i+3j+4k)", newZ(1), newZ(-2), newZ(-3), newZ(-4));
		eqquo(l,"(- 2 1.0+2i+3j+4k)", newR(1.0), newR(-2.0), newR(-3.0), newR(-4.0));
		eqoct(l,"(- 2 1+2i+3j+4k+o+io+jo+ko)",
				newZ(1), newZ(-2), newZ(-3), newZ(-4),
				newZ(-1), newZ(-1), newZ(-1), newZ(-1));
		eqoct(l,"(- 2 1.0+2i+3j+4k+o+io+jo+ko)",
				newR(1.0), newR(-2.0), newR(-3.0), newR(-4.0),
				newR(-1.0), newR(-1.0), newR(-1.0), newR(-1.0));
	}

	public void testSubBigInt() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(- 20000000000000 1+2i)",   newZ(19999999999999l), newZ(-2));
		eqcmp(l,"(- 20000000000000 1.0+2i)", newR(19999999999999.0), newR(-2.0));
		eqquo(l,"(- 20000000000000 1+2i+3j+4k)",
				newZ(19999999999999l), newZ(-2), newZ(-3), newZ(-4));
		eqquo(l,"(- 20000000000000 1.0+2i+3j+4k)",
				newR(19999999999999.0), newR(-2.0), newR(-3.0), newR(-4.0));
		eqoct(l,"(- 20000000000000 1+2i+3j+4k+o+io+jo+ko)",
				newZ(19999999999999l), newZ(-2), newZ(-3), newZ(-4),
				newZ(-1), newZ(-1), newZ(-1), newZ(-1));
		eqoct(l,"(- 20000000000000 1.0+2i+3j+4k+o+io+jo+ko)",
				newR(19999999999999.0), newR(-2.0), newR(-3.0), newR(-4.0),
				newR(-1.0), newR(-1.0), newR(-1.0), newR(-1.0));
	}

	public void testSubRational() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(- 1/2 1+2i)", newQ(-1, 2), newZ(-2));
		eqcmp(l,"(- 1/2 1.0+2i)", newR(-0.5), newR(-2.0));
		eqquo(l,"(- 1/2 1+2i+3j+4k)", newQ(-1, 2), newZ(-2), newZ(-3), newZ(-4));
		eqquo(l,"(- 1/2 1.0+2i+3j+4k)", newR(-0.5), newR(-2.0), newR(-3.0), newR(-4.0));
		eqoct(l,"(- 1/2 1+2i+3j+4k+o+io+jo+ko)",
				newQ(-1, 2), newZ(-2), newZ(-3), newZ(-4),
				newZ(-1), newZ(-1), newZ(-1), newZ(-1));
		eqoct(l,"(- 1/2 1.0+2i+3j+4k+o+io+jo+ko)",
				newR(-0.5), newR(-2.0), newR(-3.0), newR(-4.0),
				newR(-1.0), newR(-1.0), newR(-1.0), newR(-1.0));
	}

	public void testSubDouble() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(- 2.0 1+2i)", newR(1.0), newR(-2.0));
		eqcmp(l,"(- 2.0 1.0+2i)", newR(1.0), newR(-2.0));
		eqquo(l,"(- 2.0 1+2i+3j+4k)", newR(1.0), newR(-2.0), newR(-3.0), newR(-4.0));
		eqquo(l,"(- 2.0 1.0+2i+3j+4k)", newR(1.0), newR(-2.0), newR(-3.0), newR(-4.0));
		eqoct(l,"(- 2.0 1+2i+3j+4k+o+io+jo+ko)",
				newR(1.0), newR(-2.0), newR(-3.0), newR(-4.0),
				newR(-1.0), newR(-1.0), newR(-1.0), newR(-1.0));
		eqoct(l,"(- 2.0 1.0+2i+3j+4k+o+io+jo+ko)",
				newR(1.0), newR(-2.0), newR(-3.0), newR(-4.0),
				newR(-1.0), newR(-1.0), newR(-1.0), newR(-1.0));
	}

	public void testSubExactComplex() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(- 2+2i 2)", newZ(0), newZ(2));
		eqcmp(l,"(- 2+2i 20000000000000)", newZ(-19999999999998l), newZ(2));
		eqcmp(l,"(- 2+2i 1/2)", newQ(3, 2), newZ(2));
		eqcmp(l,"(- 2+2i 1.0)", newR(1.0), newR(2.0));
		equal(l,"(- 1+2i -1+2i)", newZ(2));
		equal(l,"(- 1+2i -1.0+2i)", newC(2.0, 0.0));
		eqquo(l,"(- 1-i -1-i+j+k)", newZ(2), newZ(0), newZ(-1), newZ(-1));
		eqquo(l,"(- 1-i -1.0-i+j+k)", newR(2.0), newR(0.0), newR(-1.0), newR(-1.0));
	}

	public void testSubDoubleComplex() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(- 2.0+2i 2)", newR(0.0), newR(2.0));
		eqcmp(l,"(- 2.0+2i 20000000000000)", newR(-19999999999998.0), newR(2.0));
		eqcmp(l,"(- 2.0+2i 1/2)", newR(1.5), newR(2.0));
		eqcmp(l,"(- 2.0+2i 1.0)", newR(1.0), newR(2.0));
		equal(l,"(- 1.0-2i -1-2i)", newC(2.0, 0.0));
		equal(l,"(- 1.0-2i -1.0-2i)", newC(2.0, 0.0));
		eqquo(l,"(- 1.0-i -1-i-j-k)", newR(2.0), newR(0.0), newR(1.0), newR(1.0));
		eqquo(l,"(- 1.0-i -1.0-i-j-k)", newR(2.0), newR(0.0), newR(1.0), newR(1.0));
	}

	public void testSubExactQuaternion() {
		Scheme l = Scheme.newInstance();

		eqquo(l,"(- 2+2i+2j+2k 2)", newZ(0), newZ(2), newZ(2), newZ(2));
		eqquo(l,"(- 2+2i+2j+2k 20000000000000)",
				newZ(-19999999999998l), newZ(2), newZ(2), newZ(2));
		eqquo(l,"(- 2+2i+2j+2k 1/2)", newQ(3, 2), newZ(2), newZ(2), newZ(2));
		eqquo(l,"(- 2+2i+2j+2k 1.0)", newR(1.0), newR(2.0), newR(2.0), newR(2.0));
		eqquo(l,"(- 1+i+j+k -1+i)", newZ(2), newZ(0), newZ(1), newZ(1));
		eqquo(l,"(- 1+i+j+k -1.0+i)", newR(2.0), newR(0.0), newR(1.0), newR(1.0));
		equal(l,"(- 1-i-j-k -1-i-j-k)", newZ(2));
		equal(l,"(- 1-i-j-k -1.0-i-j-k)", newC(2.0, 0.0));
	}

	public void testSubDoubleQuaternion() {
		Scheme l = Scheme.newInstance();

		eqquo(l,"(- 2.0+2i+2j+2k 2)", newR(0.0), newR(2.0), newR(2.0), newR(2.0));
		eqquo(l,"(- 2.0+2i+2j+2k 20000000000000)",
				newR(-19999999999998.0), newR(2.0), newR(2.0), newR(2.0));
		eqquo(l,"(- 2.0+2i+2j+2k 1/2)", newR(1.5), newR(2.0), newR(2.0), newR(2.0));
		eqquo(l,"(- 2.0+2i+2j+2k 1.0)", newR(1.0), newR(2.0), newR(2.0), newR(2.0));
		eqquo(l,"(- 1.0+i+j+k -1+i)", newR(2.0), newR(0.0), newR(1.0), newR(1.0));
		eqquo(l,"(- 1.0+i+j+k -1.0+i)", newR(2.0), newR(0.0), newR(1.0), newR(1.0));
		equal(l,"(- 1.0-i-j-k -1-i-j-k)", newC(2.0, 0.0));
		equal(l,"(- 1.0-i-j-k -1.0-i-j-k)", newC(2.0, 0.0));
	}

	public void testMulSmallInt() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(* 2 1+2i)", newZ(2), newZ(4));
		eqcmp(l,"(* 2 1.0+2i)", newR(2.0), newR(4.0));
		eqquo(l,"(* 2 1+2i+3j+4k)", newZ(2), newZ(4), newZ(6), newZ(8));
		eqquo(l,"(* 2 1.0+2i+3j+4k)", newR(2.0), newR(4.0), newR(6.0), newR(8.0));
		eqoct(l,"(* 2 1+2i+3j+4k+o+io+jo+ko)",
				newZ(2), newZ(4), newZ(6), newZ(8),
				newZ(2), newZ(2), newZ(2), newZ(2));
		eqoct(l,"(* 2 1.0+2i+3j+4k+o+io+jo+ko)",
				newR(2.0), newR(4.0), newR(6.0), newR(8.0),
				newR(2.0), newR(2.0), newR(2.0), newR(2.0));
	}

	public void testMulBigInt() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(* 20000000000000 1+2i)",
				newZ(20000000000000l), newZ(40000000000000l));
		eqcmp(l,"(* 20000000000000 1.0+2i)",
				newR(20000000000000.0), newR(40000000000000.0));
		eqquo(l,"(* 20000000000000 1+2i+3j+4k)",
				newZ(20000000000000l), newZ(40000000000000l),
				newZ(60000000000000l), newZ(80000000000000l));
		eqquo(l,"(* 20000000000000 1.0+2i+3j+4k)",
				newR(20000000000000.0), newR(40000000000000.0),
				newR(60000000000000.0), newR(80000000000000.0));
		eqoct(l,"(* 20000000000000 1+2i+3j+4k+o+io+jo+ko)",
				newZ(20000000000000l), newZ(40000000000000l),
				newZ(60000000000000l), newZ(80000000000000l),
				newZ(20000000000000l), newZ(20000000000000l),
				newZ(20000000000000l), newZ(20000000000000l));
		eqoct(l,"(* 20000000000000 1.0+2i+3j+4k+o+io+jo+ko)",
				newR(20000000000000.0), newR(40000000000000.0),
				newR(60000000000000.0), newR(80000000000000.0),
				newR(20000000000000.0), newR(20000000000000.0),
				newR(20000000000000.0), newR(20000000000000.0));
	}

	public void testMulRational() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(* 1/2 1+2i)", newQ(1, 2), newZ(1));
		eqcmp(l,"(* 1/2 1.0+2i)", newR(0.5), newR(1.0));
		eqquo(l,"(* 1/2 1+2i+3j+4k)", newQ(1, 2), newZ(1), newQ(3, 2), newZ(2));
		eqquo(l,"(* 1/2 1.0+2i+3j+4k)", newR(0.5), newR(1.0), newR(1.5), newR(2.0));
		eqoct(l,"(* 1/2 1+2i+3j+4k+o+io+jo+ko)",
				newQ(1, 2), newZ(1), newQ(3, 2), newZ(2),
				newQ(1, 2), newQ(1, 2), newQ(1, 2), newQ(1, 2));
		eqoct(l,"(* 1/2 1.0+2i+3j+4k+o+io+jo+ko)",
				newR(0.5), newR(1.0), newR(1.5), newR(2.0),
				newR(0.5), newR(0.5), newR(0.5), newR(0.5));
	}

	public void testMulDouble() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(* 2.0 1+2i)", newR(2.0), newR(4.0));
		eqcmp(l,"(* 2.0 1.0+2i)", newR(2.0), newR(4.0));
		eqquo(l,"(* 2.0 1+2i+3j+4k)", newR(2.0), newR(4.0), newR(6.0), newR(8.0));
		eqquo(l,"(* 2.0 1.0+2i+3j+4k)", newR(2.0), newR(4.0), newR(6.0), newR(8.0));
		eqoct(l,"(* 2.0 1+2i+3j+4k+o+io+jo+ko)",
				newR(2.0), newR(4.0), newR(6.0), newR(8.0),
				newR(2.0), newR(2.0), newR(2.0), newR(2.0));
		eqoct(l,"(* 2.0 1.0+2i+3j+4k+o+io+jo+ko)",
				newR(2.0), newR(4.0), newR(6.0), newR(8.0),
				newR(2.0), newR(2.0), newR(2.0), newR(2.0));
	}

	public void testMulExactComplex() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(* 2+2i 2)", newZ(4), newZ(4));
		eqcmp(l,"(* 2+2i 20000000000000)", newZ(40000000000000l), newZ(40000000000000l));
		eqcmp(l,"(* 2+2i 1/2)", newZ(1), newZ(1));
		eqcmp(l,"(* 2+2i 1.0)", newR(2.0), newR(2.0));
		equal(l,"(* 1-2i 1+2i)", newZ(5));
		equal(l,"(* 1-2i 1.0+2i)", newC(5.0, 0.0));
		eqquo(l,"(* 1-i 1+i+j+k)", newZ(2), newZ(0), newZ(2), newZ(0));
		eqquo(l,"(* 1-i 1.0+i+j+k)", newR(2.0), newR(0.0), newR(2.0), newR(0.0));
	}

	public void testMulDoubleComplex() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(* 2.0+2i 2)", newR(4.0), newR(4.0));
		eqcmp(l,"(* 2.0+2i 20000000000000)", newR(40000000000000.0), newR(40000000000000.0));
		eqcmp(l,"(* 2.0+2i 1/2)", newR(1.0), newR(1.0));
		eqcmp(l,"(* 2.0+2i 1.0)", newR(2.0), newR(2.0));
		equal(l,"(* 1.0-2i 1+2i)", newC(5.0, 0.0));
		equal(l,"(* 1.0-2i 1.0+2i)", newC(5.0, 0.0));
		eqquo(l,"(* 1.0-i 1+i+j+k)", newR(2.0), newR(0.0), newR(2.0), newR(0.0));
		eqquo(l,"(* 1.0-i 1.0+i+j+k)", newR(2.0), newR(0.0), newR(2.0), newR(0.0));
	}

	public void testMulExactQuaternion() {
		Scheme l = Scheme.newInstance();

		eqquo(l,"(* 2+2i+2j+2k 2)", newZ(4), newZ(4), newZ(4), newZ(4));
		eqquo(l,"(* 2+2i+2j+2k 20000000000000)",
				newZ(40000000000000l), newZ(40000000000000l),
				newZ(40000000000000l), newZ(40000000000000l));
		eqquo(l,"(* 2+2i+2j+2k 1/2)", newZ(1), newZ(1), newZ(1), newZ(1));
		eqquo(l,"(* 2+2i+2j+2k 1.0)", newR(2.0), newR(2.0), newR(2.0), newR(2.0));
		eqquo(l,"(* 1+i+j+k 1-i)", newZ(2), newZ(0), newZ(0), newZ(2));
		eqquo(l,"(* 1+i+j+k 1.0-i)", newR(2.0), newR(0.0), newR(0.0), newR(2.0));
		equal(l,"(* 1-i-j-k 1+i+j+k)", newZ(4));
		equal(l,"(* 1-i-j-k 1.0+i+j+k)", newC(4.0, 0.0));
	}

	public void testMulDoubleQuaternion() {
		Scheme l = Scheme.newInstance();

		eqquo(l,"(* 2.0+2i+2j+2k 2)", newR(4.0), newR(4.0), newR(4.0), newR(4.0));
		eqquo(l,"(* 2.0+2i+2j+2k 20000000000000)",
				newR(40000000000000.0), newR(40000000000000.0),
				newR(40000000000000.0), newR(40000000000000.0));
		eqquo(l,"(* 2.0+2i+2j+2k 1/2)", newR(1.0), newR(1.0), newR(1.0), newR(1.0));
		eqquo(l,"(* 2.0+2i+2j+2k 1.0)", newR(2.0), newR(2.0), newR(2.0), newR(2.0));
		eqquo(l,"(* 1.0+i+j+k 1-i)", newR(2.0), newR(0.0), newR(0.0), newR(2.0));
		eqquo(l,"(* 1.0+i+j+k 1.0-i)", newR(2.0), newR(0.0), newR(0.0), newR(2.0));
		equal(l,"(* 1.0-i-j-k 1+i+j+k)", newC(4.0, 0.0));
		equal(l,"(* 1.0-i-j-k 1.0+i+j+k)", newC(4.0, 0.0));
	}

	public void testDivSmallInt() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(/ 2 1+i)", newZ(1), newZ(-1));
		eqcmp(l,"(/ 2 1.0+i)", newR(1.0), newR(-1.0));
		eqquo(l,"(/ 4 1+i+j+k)", newZ(1), newZ(-1), newZ(-1), newZ(-1));
		eqquo(l,"(/ 4 1.0+i+j+k)", newR(1.0), newR(-1.0), newR(-1.0), newR(-1.0));
		eqoct(l,"(/ 8 1+i+j+k+o+io+jo+ko)",
				newZ(1), newZ(-1), newZ(-1), newZ(-1),
				newZ(-1), newZ(-1), newZ(-1), newZ(-1));
		eqoct(l,"(/ 8 1.0+i+j+k+o+io+jo+ko)",
				newR(1.0), newR(-1.0), newR(-1.0), newR(-1.0),
				newR(-1.0), newR(-1.0), newR(-1.0), newR(-1.0));
	}

	public void testDivBigInt() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(/ 20000000000000 1+i)",
				newZ(10000000000000l), newZ(-10000000000000l));
		eqcmp(l,"(/ 20000000000000 1.0+i)",
				newR(10000000000000.0), newR(-10000000000000.0));
		eqquo(l,"(/ 40000000000000 1+i+j+k)",
				newZ(10000000000000l), newZ(-10000000000000l),
				newZ(-10000000000000l), newZ(-10000000000000l));
		eqquo(l,"(/ 40000000000000 1.0+i+j+k)",
				newR(10000000000000.0), newR(-10000000000000.0),
				newR(-10000000000000.0), newR(-10000000000000.0));
		eqoct(l,"(/ 80000000000000 1+i+j+k+o+io+jo+ko)",
				newZ(10000000000000l), newZ(-10000000000000l),
				newZ(-10000000000000l), newZ(-10000000000000l),
				newZ(-10000000000000l), newZ(-10000000000000l),
				newZ(-10000000000000l), newZ(-10000000000000l));
		eqoct(l,"(/ 80000000000000 1.0+i+j+k+o+io+jo+ko)",
				newR(10000000000000.0), newR(-10000000000000.0),
				newR(-10000000000000.0), newR(-10000000000000.0),
				newR(-10000000000000.0), newR(-10000000000000.0),
				newR(-10000000000000.0), newR(-10000000000000.0));
	}

	public void testDivRational() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(/ 1/2 1+i)", newQ(1, 4), newQ(-1, 4));
		eqcmp(l,"(/ 1/2 1.0+i)", newR(0.25), newR(-0.25));
		eqquo(l,"(/ 1/2 1+i+j+k)", newQ(1, 8), newQ(-1, 8), newQ(-1, 8), newQ(-1, 8));
		eqquo(l,"(/ 1/2 1.0+i+j+k)",
				newR(0.125), newR(-0.125), newR(-0.125), newR(-0.125));
		eqoct(l,"(/ 1/2 1+i+j+k+o+io+jo+ko)",
				newQ(1, 16), newQ(-1, 16), newQ(-1, 16), newQ(-1, 16),
				newQ(-1, 16), newQ(-1, 16), newQ(-1, 16), newQ(-1, 16));
		eqoct(l,"(/ 1/2 1.0+i+j+k+o+io+jo+ko)",
				newR(0.0625), newR(-0.0625), newR(-0.0625), newR(-0.0625),
				newR(-0.0625), newR(-0.0625), newR(-0.0625), newR(-0.0625));
	}

	public void testDivDouble() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(/ 2.0 1+i)", newR(1.0), newR(-1.0));
		eqcmp(l,"(/ 2.0 1.0+i)", newR(1.0), newR(-1.0));
		eqquo(l,"(/ 4.0 1+i+j+k)", newR(1.0), newR(-1.0), newR(-1.0), newR(-1.0));
		eqquo(l,"(/ 4.0 1.0+i+j+k)", newR(1.0), newR(-1.0), newR(-1.0), newR(-1.0));
		eqoct(l,"(/ 8.0 1+i+j+k+o+io+jo+ko)",
				newR(1.0), newR(-1.0), newR(-1.0), newR(-1.0),
				newR(-1.0), newR(-1.0), newR(-1.0), newR(-1.0));
		eqoct(l,"(/ 8.0 1.0+i+j+k+o+io+jo+ko)",
				newR(1.0), newR(-1.0), newR(-1.0), newR(-1.0),
				newR(-1.0), newR(-1.0), newR(-1.0), newR(-1.0));
	}

	public void testDivExactComplex() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(/ 2+2i 2)", newZ(1), newZ(1));
		eqcmp(l,"(/ 20000000000000+20000000000000i 20000000000000)", newZ(1), newZ(1));
		eqcmp(l,"(/ 2+2i 1/2)", newZ(4), newZ(4));
		eqcmp(l,"(/ 2+2i 2.0)", newR(1.0), newR(1.0));
		equal(l,"(/ 1-2i 1-2i)", newZ(1));
		equal(l,"(/ 1-2i 1.0-2i)", newC(1.0, 0.0));
		eqquo(l,"(/ 4+4i 1+i+j+k)", newZ(2), newZ(0), newZ(-2), newZ(0));
		eqquo(l,"(/ 4+4i 1.0+i+j+k)", newR(2.0), newR(0.0), newR(-2.0), newR(0.0));
	}

	public void testDivDoubleComplex() {
		Scheme l = Scheme.newInstance();

		eqcmp(l,"(/ 2.0+2i 2)", newR(1.0), newR(1.0));
		eqcmp(l,"(/ 2.0+2i 1/2)", newR(4.0), newR(4.0));
		eqcmp(l,"(/ 2.0+2i 2.0)", newR(1.0), newR(1.0));
		equal(l,"(/ 1.0-2i 1-2i)", newC(1.0, 0.0));
		equal(l,"(/ 1.0-2i 1.0-2i)", newC(1.0, 0.0));
		eqquo(l,"(/ 4.0+4i 1+i+j+k)", newR(2.0), newR(0.0), newR(-2.0), newR(0.0));
		eqquo(l,"(/ 4.0+4i 1.0+i+j+k)", newR(2.0), newR(0.0), newR(-2.0), newR(0.0));
	}

	public void testDivExactQuaternion() {
		Scheme l = Scheme.newInstance();

		eqquo(l,"(/ 2+2i+2j+2k 2)", newZ(1), newZ(1), newZ(1), newZ(1));
		eqquo(l,"(/ 20000000000000+20000000000000i+20000000000000j+20000000000000k 20000000000000)",
				newZ(1), newZ(1), newZ(1), newZ(1));
		eqquo(l,"(/ 1/2+1/2i+1/2j+1/2k 1/2)", newZ(1), newZ(1), newZ(1), newZ(1));
		eqquo(l,"(/ 2+2i+2j+2k 2.0)", newR(1.0), newR(1.0), newR(1.0), newR(1.0));
		eqquo(l,"(/ 1+i+j+k 1+i)", newZ(1), newZ(0), newZ(1), newZ(0));
		eqquo(l,"(/ 1+i+j+k 1.0+i)", newR(1.0), newR(0.0), newR(1.0), newR(0.0));
		equal(l,"(/ 1-i-j-k 1-i-j-k)", newZ(1));
		equal(l,"(/ 1-i-j-k 1.0-i-j-k)", newC(1.0, 0.0));
	}

	public void testDivDoubleQuaternion() {
		Scheme l = Scheme.newInstance();

		eqquo(l,"(/ 2.0+2.0i+2.0j+2.0k 2)", newR(1.0), newR(1.0), newR(1.0), newR(1.0));
		eqquo(l,"(/ 0.5+0.5i+0.5j+0.5k 1/2)", newR(1.0), newR(1.0), newR(1.0), newR(1.0));
		eqquo(l,"(/ 2.0+2i+2j+2k 2.0)", newR(1.0), newR(1.0), newR(1.0), newR(1.0));
		eqquo(l,"(/ 1.0+i+j+k 1+i)", newR(1.0), newR(0.0), newR(1.0), newR(0.0));
		eqquo(l,"(/ 1.0+i+j+k 1.0+i)", newR(1.0), newR(0.0), newR(1.0), newR(0.0));
		equal(l,"(/ 1.0-i-j-k 1-i-j-k)", newC(1.0, 0.0));
		equal(l,"(/ 1.0-i-j-k 1.0-i-j-k)", newC(1.0, 0.0));
	}

}
