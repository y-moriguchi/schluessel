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
package net.morilib.lisp.compare;

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.ILispVector;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Symbol;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/20
 */
public final class SRFI67 {

	//
	private static final Symbol DYNFLG =
		Symbol.getSymbol("*refer-default-compare-dynamic*");
	private static final Symbol DEFCMP =
		Symbol.getSymbol("default-compare");

	/**
	 * 
	 */
	public static final LispInteger GREATER = LispInteger.ONE;

	/**
	 * 
	 */
	public static final LispInteger EQUAL = LispInteger.ZERO;

	/**
	 * 
	 */
	public static final LispInteger LESS = LispInteger.valueOf(-1);

	//
	private static boolean isDynamic(Environment env) {
		Datum d = env.findDatum(DYNFLG);

		return (d == null) ? false : d.isTrue();
	}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static int getValue(Datum d, LispMessage mesg) {
		if(!(d instanceof LispSmallInt)) {
			throw mesg.getError("err.srfi67.return.value3", d);
		} else if(d.getInt() > 1 || d.getInt() < -1) {
			throw mesg.getError("err.srfi67.return.value3", d);
		} else {
			return d.getInt();
		}
	}

	/**
	 * @param compareTo
	 * @return
	 */
	public static LispInteger getInstance(int x) {
		return (x > 0) ? GREATER : (x < 0) ? LESS : EQUAL;
	}

	/**
	 * @param character
	 * @param character2
	 * @return
	 */
	public static LispInteger compareChar(char x1, char x2) {
		return (x1 > x2) ? GREATER : (x1 < x2) ? LESS : EQUAL;
	}

	/**
	 * @param character
	 * @param character2
	 * @return
	 */
	public static LispInteger compareInt(int x1, int x2) {
		return (x1 > x2) ? GREATER : (x1 < x2) ? LESS : EQUAL;
	}

	/**
	 * @param character
	 * @param character2
	 * @return
	 */
	public static<T extends Comparable<T>> LispInteger compareTo(T x1,
			T x2) {
		return getInstance(x1.compareTo(x2));
	}

	/**
	 * 
	 * @param x1
	 * @param x2
	 * @return
	 */
	public static LispInteger compareReal(LispReal x1, LispReal x2) {
		return SRFI67.getInstance(x1.subtract(x2).signum());
	}

	/**
	 * 
	 * @param x1
	 * @param x2
	 * @param env
	 * @param mesg
	 * @return
	 */
	public static LispInteger callCompareDefault(Datum x1, Datum x2,
			Environment env, LispMessage mesg) {
		boolean d = isDynamic(env);
		Datum f;

		f = d ? env.findDatum(DEFCMP) : env.getDatumTop(DEFCMP);
		return callCompare(f, x1, x2, env, mesg);
	}

	/**
	 * 
	 * @param env
	 * @param mesg
	 * @return
	 */
	public static Procedure getDefaultCompare(Environment env,
			LispMessage mesg) {
		boolean d = isDynamic(env);
		Datum f;

		f = d ? env.findDatum(DEFCMP) : env.getDatumTop(DEFCMP);
		if(f instanceof Procedure) {
			return (Procedure)f;
		} else {
			throw mesg.getError("err.require.procedure", f);
		}
	}

	/**
	 * 
	 * @param f
	 * @param x1
	 * @param x2
	 * @param env
	 * @param mesg
	 * @return
	 */
	public static LispInteger callCompare(Datum f, Datum x1, Datum x2,
			Environment env, LispMessage mesg) {
		Datum r;

		if(f != null) {
			r = Scheme.callva(f, env, mesg, x1, x2);
			if(!(r instanceof LispSmallInt)) {
				throw mesg.getError("err.srfi67.return.value3", r);
			} else if(r.getInt() < -1 || r.getInt() > 1) {
				throw mesg.getError("err.srfi67.return.value3", r);
			} else {
				return (LispInteger)r;
			}
		} else {
			return callCompareDefault(x1, x2, env, mesg);
		}
	}

	//
	/*package*/ static int callSize(Datum f, ILispVector x,
			Environment env, LispMessage mesg) {
		Datum r;

		if(f == null) {
			return x.size();
		} else {
			r = Scheme.callva(f, env, mesg, (Datum)x);
			if(!(r instanceof LispSmallInt)) {
				throw mesg.getError("err.srfi67.return.nonnegative",
						r);
			} else if(r.getInt() < 0) {
				throw mesg.getError("err.srfi67.return.nonnegative",
						r);
			} else {
				return r.getInt();
			}
		}
	}

	//
	/*package*/ static Datum callRef(Datum f, ILispVector x, int i,
			Environment env, LispMessage mesg) {
		if(f == null) {
			return x.get(i);
		} else {
			return Scheme.callva(f, env, mesg, (Datum)x,
					LispInteger.valueOf(i));
		}
	}

	//
	/*package*/ static boolean callIsNull(Datum f, Datum x,
			Environment env, LispMessage mesg) {
		if(f == null) {
			return x.isNil();
		} else {
			return Scheme.callva(f, env, mesg, x).isTrue();
		}
	}

	//
	/*package*/ static Datum callCar(Datum f, Datum x,
			Environment env, LispMessage mesg) {
		if(f != null) {
			return Scheme.callva(f, env, mesg, x);
		} else if(x instanceof Cons) {
			return ((Cons)x).getCar();
		} else {
			throw mesg.getError("err.require.pair", x);
		}
	}

	//
	/*package*/ static Datum callCdr(Datum f, Datum x,
			Environment env, LispMessage mesg) {
		if(f != null) {
			return Scheme.callva(f, env, mesg, x);
		} else if(x instanceof Cons) {
			return ((Cons)x).getCdr();
		} else {
			throw mesg.getError("err.require.pair", x);
		}
	}

}
