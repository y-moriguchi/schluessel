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

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.LispComplex;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispExactReal;
import net.morilib.lisp.LispFloat;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispRational;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.uvector.ILispBytevector;

public abstract class TCLisp extends TC {

	public static final LispBoolean T = LispBoolean.TRUE;

	public static final LispBoolean F = LispBoolean.FALSE;

	public static final Symbol QT = Symbol.QUOTE;

	public static final Symbol QQT = Symbol.QUASIQUOTE;

	public static final Symbol UQT = Symbol.UNQUOTE;

	public static final Symbol UQTS = Symbol.UNQUOTE_SPLICING;

	public static final LispDouble INF_0P =
		LispDouble.POSITIVE_INFINITY;

	public static final LispDouble INF_0N =
		LispDouble.NEGATIVE_INFINITY;

	public static final LispDouble NAN_0 = LispDouble.NaN;


	private static String getStr(Datum d1, Datum d2) {
		return ("<" + LispUtils.getResult(d2) + "> but <" +
				LispUtils.getResult(d1) + ">");
	}

	public static Symbol sym(String name) {
		return Symbol.getSymbol(name);
	}

	public static LispString str(String v) {
		return new LispString(v);
	}

	public static LispCharacter chr(char v) {
		return new LispCharacter(v);
	}

	public static LispInteger newZ(int v) {
		return LispInteger.valueOf(v);
	}

	public static LispInteger newZ(long v) {
		return LispInteger.valueOf(v);
	}

	public static LispInteger newZ(BigInteger v) {
		return LispInteger.valueOf(v);
	}

	public static LispInteger newZ(String v) {
		return LispInteger.valueOf(new BigInteger(v));
	}

	public static LispExactReal newQ(BigInteger n, BigInteger d) {
		return LispRational.newRational(n, d);
	}

	public static LispExactReal newQ(int n, int d) {
		return newQ(BigInteger.valueOf(n), BigInteger.valueOf(d));
	}

	public static LispDouble newR(double v) {
		return new LispDouble(v);
	}

	public static LispFloat newR(float v) {
		return new LispFloat(v);
	}

	public static LispComplex newC(double r, double i) {
		return LispComplex.newComplex(r, i);
	}

	public static LispComplex newC(float r, float i) {
		return LispComplex.newComplex(r, i);
	}

	public static LispComplex newC(int r, int i) {
		return LispComplex.newComplex(
				LispInteger.valueOf(r), LispInteger.valueOf(i));
	}

	public static LispComplex newCp(double r, double i) {
		return LispComplex.newPolar(r, i);
	}

	public static LispComplex newCp(float r, float i) {
		return LispComplex.newPolar(r, i);
	}

	public static LispComplex newCp(int r, int i) {
		return LispComplex.newPolar(
				LispInteger.valueOf(r), LispInteger.valueOf(i));
	}

	public static Datum list(Object... lst) {
		return LispUtils.list(lst);
	}

	public static Datum listDot(Object d, Object... lst) {
		return LispUtils.listDot(d, lst);
	}

	public static Cons cons(Object car, Object cdr) {
		return LispUtils.cons(car, cdr);
	}

	public static Datum lsym(String... lst) {
		Object[] s = new Object[lst.length];

		for(int i = 0; i < s.length; i++) {
			s[i] = Symbol.getSymbol(lst[i]);
		}
		return list(s);
	}

	public static LispVector vec(Object... lst) {
		return LispUtils.vector(lst);
	}

	public static Datum qt(Symbol q, Object o) {
		return new Cons(q, new Cons(LispUtils.toDatum(o), Nil.NIL));
	}

	public static Datum qt(Object o) {
		return qt(QT, o);
	}

	public static Datum qq(Object o) {
		return qt(QQT, o);
	}

	public static Datum uq(Object o) {
		return qt(UQT, o);
	}

	public static Datum uqs(Object o) {
		return qt(UQTS, o);
	}

	public static void eqv(Datum d1, Datum d2) {
		assertTrue(getStr(d1, d2), d1.isEqv(d2));
	}

	public static void eqstr(Datum d1, Datum d2) {
		String s1, s2;

		try {
			s1 = ((LispString)d1).getString();
			s2 = ((LispString)d2).getString();
			assertTrue(
					("<" + LispUtils.getResult(d1) + "> but <" +
							LispUtils.getResult(d2) + ">"),
					s1.equals(s2));
		} catch(ClassCastException e) {
			fail("<" + LispUtils.getResult(d1) + "> but <" +
					LispUtils.getResult(d2) + ">");
		}
	}

	public static void equal(Datum d1, Datum d2) {
		assertTrue(getStr(d1, d2), LispUtils.equals(d1, d2));
	}

	public static void isNaN(Datum d1) {
		if(d1 instanceof LispNumber) {
			if(Double.isNaN(((LispNumber)d1).getRealDouble())) {
				return;
			}
		}
		fail("<+nan.0>" + LispUtils.getResult(d1));
	}

	public static void eqbv(Datum d1, Datum d2) {
		eq(((ILispBytevector)d1).toBytes(),
				((ILispBytevector)d2).toBytes());
	}

	public static void eqbv(Datum d1, byte[] d2) {
		eq(((ILispBytevector)d1).toBytes(), d2);
	}

}
