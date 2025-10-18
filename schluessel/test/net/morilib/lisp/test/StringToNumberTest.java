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

import java.io.IOException;
import java.math.BigInteger;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispComplex;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.Scheme;

public class StringToNumberTest extends TCSubr {

	private static void eqsym(Scheme p, String s) {
		eq(p, "(string->number \"" + s + "\")", F);
	}

	private static void rrerr(Scheme p, String s) {
		eq(p, "(string->number \"" + s + "\")", F);
	}

	private static void eqval(Scheme p, String s, Datum d) {
		eqv(p, "(string->number \"" + s + "\")", d);
	}

	private static void eqrat(Scheme p, String s, String n, String d) {
		eqval(p, s, newQ(new BigInteger(n), new BigInteger(d)));
	}

	private static void eqrat(Scheme p, String s, int n, int d) {
		eqval(p, s, newQ(n, d));
	}

	private static void eqbig(Scheme p, String v) {
		eqval(p, v, newZ(new BigInteger(v.replaceFirst("^\\+", ""))));
	}

	private static void eqint(Scheme p, String v, int n) {
		eqval(p, v, newZ(n));
	}

	private static void eqdbl(Scheme p, String v, double d) {
		eqval(p, v, newR(d));
	}

	private static void eqimg(Scheme p, String v, double d) {
		eqval(p, v, newC(0, d));
	}

	private static void eqimg(Scheme p, String v, LispReal d) {
		eqval(p, v, LispComplex.newComplex(newZ(0), d));
	}

	private static void eqpol(Scheme p, String v, double r, double t) {
		eqval(p, v, newC(r * Math.cos(t), r * Math.sin(t)));
	}

	private static void eqcmp(Scheme p, String v, double r, double i) {
		eqval(p, v, newC(r, i));
	}

	private static void eqcmp(Scheme p, String v, LispReal r, LispReal i) {
		eqval(p, v, LispComplex.newComplex(r, i));
	}

	private static void eqnan(Scheme p, String v) {
		isNaN(p, "(string->number \"" + v + "\")");
	}


	public void testSymbol() throws Exception {
		Scheme p = Scheme.newInstance();

		eqsym(p, "symbol");
		eqsym(p, "*");
		eqsym(p, "+");       eqsym(p, "-");
		eqsym(p, "i");
		eqsym(p, "1a");
		eqsym(p, "+1a");     eqsym(p, "-1a");
		eqsym(p, "+-1");
		eqsym(p, ".sym");
		eqsym(p, "+.a");
		eqsym(p, "1.a");
		eqsym(p, "+1.a");    eqsym(p, "-1.a");
		eqsym(p, "1.1a");
		eqsym(p, "1.1e");
		eqsym(p, "1.1e+");   eqsym(p, "1.1e-");
		eqsym(p, "1.1e+a");  eqsym(p, "1.1e-1a");
		eqsym(p, "1.1e+1.1");
		eqsym(p, "/");
		eqsym(p, "1/");
		eqsym(p, "1/2a");
		eqsym(p, "1/2.1");
		eqsym(p, "1.1@");
		eqsym(p, "1.1@+");      eqsym(p, "1.1@-");
		eqsym(p, "1.1@a");      eqsym(p, "1.1@1a");
		eqsym(p, "1.1@.");
		eqsym(p, "1.1@+.");     eqsym(p, "1.1@-.");
		eqsym(p, "1.1@1.");
		eqsym(p, "1.1@1.1e");
		eqsym(p, "1.1@1.1e+");  eqsym(p, "1.1@1.1e-");
		eqsym(p, "1.1@1.1e+1a");
		eqsym(p, "1.1@1.1e+1.1");
		eqsym(p, "1.1@1/");
		eqsym(p, "1.1@1/2a");
		eqsym(p, "1.1@1/2.1");
		eqsym(p, "inf.0");
		eqsym(p, "+in");        eqsym(p, "-in");
		eqsym(p, "+inf");       eqsym(p, "-inf");
		eqsym(p, "+inf.");      eqsym(p, "-inf.");
		eqsym(p, "+inf.00");    eqsym(p, "-inf.00");
		eqsym(p, "+inf.0ii");
		eqsym(p, "nan.0");
		eqsym(p, "+n");
		eqsym(p, "+na");
		eqsym(p, "+nan");
		eqsym(p, "+nan.");
		eqsym(p, "+nan.00");
		eqsym(p, "2i");
		eqsym(p, ".5i");
		eqsym(p, "1e+3i");
		eqsym(p, "1.1ii");
		eqsym(p, "+ii");        eqsym(p, "-ii");
		eqsym(p, "+1.1+1");     eqsym(p, "+1.1-1");
		eqsym(p, "+1.1+ai");    eqsym(p, "+1.1-ai");
		eqsym(p, "+1.1+.i");    eqsym(p, "+1.1-.i");
		eqsym(p, "+1.1+1.1");   eqsym(p, "+1.1-1.1");
		eqsym(p, "+1.1+1.ai");  eqsym(p, "+1.1-1.ai");
		eqsym(p, "1+1e");       eqsym(p, "1-1e");
		eqsym(p, "1+1ei");      eqsym(p, "1-1ei");
		eqsym(p, "1+1e+");      eqsym(p, "1-1e+");
		eqsym(p, "1+1e+i");     eqsym(p, "1-1e+i");
		eqsym(p, "1+1e-i");     eqsym(p, "1-1e-i");
		eqsym(p, "1+1e1");      eqsym(p, "1-1e1");
		eqsym(p, "1+1e+1");     eqsym(p, "1-1e+1");
		eqsym(p, "1+1e-1");     eqsym(p, "1-1e-1");
		eqsym(p, "1+1e+ai");    eqsym(p, "1-1e+ai");
		eqsym(p, "1+1e-ai");    eqsym(p, "1-1e-ai");
		eqsym(p, "1+/");        eqsym(p, "1-/");
		eqsym(p, "1+/i");       eqsym(p, "1-/i");
		eqsym(p, "1+1/");       eqsym(p, "1-1/");
		eqsym(p, "1+1/i");      eqsym(p, "1-1/i");
		eqsym(p, "1+1/a");      eqsym(p, "1-1/a");
		eqsym(p, "1+1/ai");     eqsym(p, "1-1/ai");
		eqsym(p, "1+in");       eqsym(p, "1-in");
		eqsym(p, "1+inf");      eqsym(p, "1-inf");
		eqsym(p, "1+inf.");     eqsym(p, "1-inf.");
		eqsym(p, "1+inf.0");    eqsym(p, "1-inf.0");
		eqsym(p, "1+inf.00i");  eqsym(p, "1-inf.00i");
		eqsym(p, "1+ii");       eqsym(p, "1-ii");
		eqsym(p, "1+1ii");      eqsym(p, "1-1ii");
	}

	public void testInteger() throws Exception {
		Scheme p = Scheme.newInstance();

		eqbig(p, "999999999999999999999999999999999999999999999999999");
		eqbig(p, "+99999999999999999999");
		eqbig(p, "-99999999999999999999");
	}

	public void testRational() throws Exception {
		Scheme p = Scheme.newInstance();
		final String N = "1111111111111111111111111111111111111111111";
		final String D = "1234567890123456789012345678901234567890123";

		eqrat(p, N + "/" + D, N, D);
		eqval(p, "10/10", newZ(1));
		eqval(p, "+13/11", newQ(13, 11));
		eqval(p, "-13/11", newQ(-13, 11));
	}

	public void testDouble() throws Exception {
		Scheme p = Scheme.newInstance();

		eqdbl(p, ".5", 0.5);
		eqdbl(p, "+.5", 0.5);     eqdbl(p, "-.5", -0.5);
		eqdbl(p, "1.5", 1.5);
		eqdbl(p, "+1.5", 1.5);    eqdbl(p, "-1.5", -1.5);
		eqdbl(p, "1e3", 1e3);
		eqdbl(p, "1e+3", 1e3);    eqdbl(p, "1e-3", 1e-3);
	}

	public void testImaginary() throws Exception {
		Scheme p = Scheme.newInstance();

		eqimg(p, "+i", newZ(1));  eqimg(p, "-i", newZ(-1));
		eqimg(p, "+.5i", 0.5);    eqimg(p, "-.5i", -0.5);
		eqimg(p, "+1.5i", 1.5);   eqimg(p, "-1.5i", -1.5);
		eqimg(p, "+1e+3i", 1e3);  eqimg(p, "-1e-3i", -1e-3);

		eqimg(p, "+1/2i", newQ(1, 2));
		eqimg(p, "-1/2i", newQ(-1, 2));
		eqimg(p, "+3/3i", newZ(1));
	}

	public void testPolar() throws Exception {
		Scheme p = Scheme.newInstance();

		eqpol(p, "1@0.7", 1.0, 0.7);
		eqpol(p, "1@+0.7", 1.0, 0.7);   eqpol(p, "1@-0.7", 1.0, -0.7);
		eqpol(p, "1@.7", 1.0, 0.7);
		eqpol(p, "1@+.7", 1.0, 0.7);    eqpol(p, "1@-.7", 1.0, -0.7);
		eqpol(p, "1@1", 1.0, 1.0);
		eqpol(p, "1@+1", 1.0, 1.0);     eqpol(p, "1@-1", 1.0, -1.0);
		eqpol(p, "1@1e1", 1.0, 1e1);
		eqpol(p, "1@1e+1", 1.0, 1e1);   eqpol(p, "1@1e-1", 1.0, 1e-1);

		eqpol(p, "1@1/2", 1.0, 0.5);
		eqpol(p, "1@+1/2", 1.0, 0.5);   eqpol(p, "1@-1/2", 1.0, -0.5);
		eqpol(p, "1@3/3", 1.0, 1.0);
	}

	public void testComplex() throws IOException {
		Scheme p = Scheme.newInstance();

		eqcmp(p, "1+0.7i", 1.0, 0.7);   eqcmp(p, "1-0.7i", 1.0, -0.7);
		eqcmp(p, "1+.7i", 1.0, 0.7);    eqcmp(p, "1-.7i", 1.0, -0.7);
		eqcmp(p, "1+1i", newZ(1), newZ(1));
		eqcmp(p, "1-1i", newZ(1), newZ(-1));
		eqcmp(p, "1+i", newZ(1), newZ(1));
		eqcmp(p, "1-i", newZ(1), newZ(-1));
		eqcmp(p, "1+1e+1i", 1.0, 1e1);  eqcmp(p, "1+1e-1i", 1.0, 1e-1);

		eqcmp(p, "1+1/2i", newZ(1), newQ(1, 2));
		eqcmp(p, "1-1/2i", newZ(1), newQ(-1, 2));
		eqcmp(p, "1+3/3i", newZ(1), newZ(1));
	}

	public void testInfinity() throws IOException {
		Scheme p = Scheme.newInstance();

		eqdbl(p, "+inf.0", Double.POSITIVE_INFINITY);
		eqdbl(p, "-inf.0", Double.NEGATIVE_INFINITY);
		eqimg(p, "+inf.0i", Double.POSITIVE_INFINITY);
		eqimg(p, "-inf.0i", Double.NEGATIVE_INFINITY);
		eqcmp(p, "1+inf.0i", 1.0, Double.POSITIVE_INFINITY);
		eqcmp(p, "1-inf.0i", 1.0, Double.NEGATIVE_INFINITY);
		eqcmp(p, "+inf.0+inf.0i",
				Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY);
		eqcmp(p, "-inf.0-inf.0i",
				Double.NEGATIVE_INFINITY, Double.NEGATIVE_INFINITY);
		eqnan(p, "+nan.0");
	}

	public void testRadix() throws IOException {
		Scheme p = Scheme.newInstance();

		eqval(p, "#b10", newZ(2));
		eqval(p, "#b10/11", newQ(2, 3));
		eqval(p, "#b12", F);

		eqval(p, "#o10", newZ(8));
		eqval(p, "#o10/11", newQ(8, 9));
		eqval(p, "#o17", newZ(15));
		eqval(p, "#o18", F);

		eqval(p, "#d10", newZ(10));
		eqval(p, "#d10/11", newQ(10, 11));
		eqval(p, "#d17", newZ(17));
		eqval(p, "#d19", newZ(19));
		eqval(p, "#d1a", F);

		eqval(p, "#x10", newZ(16));
		eqval(p, "#x10/11", newQ(16, 17));
		eqval(p, "#x17", newZ(23));
		eqval(p, "#x19", newZ(25));
		eqval(p, "#x1f", newZ(31));
		eqval(p, "#x1g", F);
	}

	public void testExactness() throws IOException {
		Scheme p = Scheme.newInstance();

		eqdbl(p, "#i1", 1.0);
		eqdbl(p, "#i1/16", 0.0625);
		eqdbl(p, "#i#d1/16", 0.0625);
		eqdbl(p, "#i#b1/10", 0.5);
		eqdbl(p, "#i#o1/10", 0.125);
		eqdbl(p, "#i#x1/10", 0.0625);
		eqdbl(p, "#d#i1/16", 0.0625);
		eqdbl(p, "#b#i1/10", 0.5);
		eqdbl(p, "#o#i1/10", 0.125);
		eqdbl(p, "#x#i1/10", 0.0625);
		eqcmp(p, "#i1/2+1/2i", 0.5, 0.5);

		eqrat(p, "#e0.5", 1, 2);
		eqint(p, "#e1.0", 1);
		eqint(p, "#e1e1", 10);
		eqrat(p, "#e1e-1", 1, 10);
		eqrat(p, "#e#d1/16", 1, 16);
		eqrat(p, "#e#b1/10000", 1, 16);
		eqrat(p, "#e#o1/20", 1, 16);
		eqrat(p, "#e#x1/10", 1, 16);
		eqrat(p, "#d#e1/16", 1, 16);
		eqrat(p, "#b#e1/10000", 1, 16);
		eqrat(p, "#o#e1/20", 1, 16);
		eqrat(p, "#x#e1/10", 1, 16);

		rrerr(p, "#i#e1");  rrerr(p, "#e#i1");
		rrerr(p, "#b#o1");  rrerr(p, "#b#d1");  rrerr(p, "#b#x1");
		rrerr(p, "#o#b1");  rrerr(p, "#o#d1");  rrerr(p, "#o#x1");
		rrerr(p, "#d#b1");  rrerr(p, "#d#o1");  rrerr(p, "#d#x1");
		rrerr(p, "#x#b1");  rrerr(p, "#x#o1");  rrerr(p, "#x#d1");
		rrerr(p, "#i1i");
	}

	public void testRadix2() {
		Scheme p = Scheme.newInstance();

		eqv(p, "(string->number \"10\" 2)", newZ(2));
		eqv(p, "(string->number \"10\" 3)", newZ(3));
		eqv(p, "(string->number \"10\" 4)", newZ(4));
		eqv(p, "(string->number \"10\" 5)", newZ(5));
		eqv(p, "(string->number \"10\" 6)", newZ(6));
		eqv(p, "(string->number \"10\" 7)", newZ(7));
		eqv(p, "(string->number \"10\" 8)", newZ(8));
		eqv(p, "(string->number \"10\" 9)", newZ(9));
		eqv(p, "(string->number \"10\" 10)", newZ(10));
		eqv(p, "(string->number \"10\" 11)", newZ(11));
		eqv(p, "(string->number \"10\" 12)", newZ(12));
		eqv(p, "(string->number \"10\" 13)", newZ(13));
		eqv(p, "(string->number \"10\" 14)", newZ(14));
		eqv(p, "(string->number \"10\" 15)", newZ(15));
		eqv(p, "(string->number \"10\" 16)", newZ(16));

		eq(p, "(string->number \"12\" 2)", F);
		eq(p, "(string->number \"13\" 3)", F);
		eq(p, "(string->number \"14\" 4)", F);
		eq(p, "(string->number \"15\" 5)", F);
		eq(p, "(string->number \"16\" 6)", F);
		eq(p, "(string->number \"17\" 7)", F);
		eq(p, "(string->number \"18\" 8)", F);
		eq(p, "(string->number \"19\" 9)", F);
		eq(p, "(string->number \"1a\" 10)", F);
		eq(p, "(string->number \"1b\" 11)", F);
		eq(p, "(string->number \"1c\" 12)", F);
		eq(p, "(string->number \"1d\" 13)", F);
		eq(p, "(string->number \"1e\" 14)", F);
		eq(p, "(string->number \"1f\" 15)", F);
		eq(p, "(string->number \"1g\" 16)", F);

		lperr(p, "(string->number \"10\" 1)");
		lperr(p, "(string->number \"10\" 17)");
		lperr(p, "(string->number \"10\" 'a)");
		lperr(p, "(string->number 'a 2)");
		lperr(p, "(string->number)");
	}

}
