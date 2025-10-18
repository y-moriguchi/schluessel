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
package net.morilib.lisp.r6rs.flonum;

import java.math.BigInteger;
import java.util.Iterator;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/31
 */
public final class FlonumUtils {

	//
	private static final BigInteger BTWO = BigInteger.valueOf(2);

	//
	private FlonumUtils() {}

	/**
	 * 
	 * @param d
	 * @param mesg
	 * @return
	 */
	public static double getFlonum(Datum d, LispMessage mesg) {
		if(d instanceof ILispFlonum) {
			return ((ILispFlonum)d).getFlonum();
		} else {
			throw mesg.getError("err.r6rs.require.flonum", d);
		}
	}

	/**
	 * 
	 * @param itr
	 * @param body
	 * @param mesg
	 * @return
	 */
	public static double nextFlonum(Iterator<Datum> itr, Datum body,
			LispMessage mesg) {
		if(itr.hasNext()) {
			return getFlonum(itr.next(), mesg);
		} else {
			throw mesg.getError("err.argument", body);
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static Datum flonum(double x) {
		return new LispDouble(x);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isInteger(double x) {
		return LispUtils.toIntegerExact(x) != null;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isOdd(double x) {
		BigInteger r = LispUtils.toIntegerExact(x);

		return r != null && r.remainder(BTWO).signum() != 0;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isEven(double x) {
		BigInteger r = LispUtils.toIntegerExact(x);

		return r != null && r.remainder(BTWO).signum() == 0;
	}

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public static double div(double x, double y) {
//		double r, q;

		if(y == 0.0) {
			return Double.NaN;
		} else if(Double.isInfinite(x)) {
			return Double.NaN;
		} else if(y == Double.POSITIVE_INFINITY) {
			return (x > 0) ? 0.0 : (x < 0) ? -0.0 : x;
		} else if(y == Double.NEGATIVE_INFINITY) {
			return (x > 0) ? -0.0 : (x < 0) ? 0.0 : -x;
		} else if(Double.isNaN(x) || Double.isNaN(y)) {
			return Double.NaN;
		} else if(y > 0) {
			return Math.floor(x / y);
		} else {
			return Math.ceil(x / y);
		}
//		r = Math.IEEEremainder(x, y);
//		q = (x - r) / y;
//		return (x >= 0) ? q : (y > 0) ? q - 1.0 : q + 1.0;
	}

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public static double mod(double x, double y) {
//		double r;

		if(y == 0.0) {
			return Double.NaN;
		} else if(Double.isInfinite(x)) {
			return Double.NaN;
		} else if(y == Double.POSITIVE_INFINITY) {
			return (x >= 0) ? x : Double.POSITIVE_INFINITY;
		} else if(y == Double.NEGATIVE_INFINITY) {
			return (x >= 0) ? x : Double.POSITIVE_INFINITY;
		} else if(Double.isNaN(x) || Double.isNaN(y)) {
			return Double.NaN;
		} else if(y > 0) {
			return x - y * Math.floor(x / y);
		} else {
			return x - y * Math.ceil(x / y);
		}
//		r = Math.IEEEremainder(x, y);
//		return (x >= 0) ? r : (y > 0) ? r + y : r - y;
	}

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public static double div0(double x, double y) {
		if(y == 0.0) {
			return Double.NaN;
		} else if(Double.isInfinite(x)) {
			return Double.NaN;
		} else if(y == Double.POSITIVE_INFINITY) {
			return (x > 0) ? 0.0 : (x < 0) ? -0.0 : x;
		} else if(y == Double.NEGATIVE_INFINITY) {
			return (x > 0) ? -0.0 : (x < 0) ? 0.0 : -x;
		} else if(Double.isNaN(x) || Double.isNaN(y)) {
			return Double.NaN;
		}
		return truncate(x / y);
	}

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public static double mod0(double x, double y) {
		if(y == 0.0) {
			return Double.NaN;
		} else if(Double.isInfinite(x)) {
			return Double.NaN;
		} else if(y == Double.POSITIVE_INFINITY) {
			return x;
		} else if(y == Double.NEGATIVE_INFINITY) {
			return -x;
		} else if(Double.isNaN(x) || Double.isNaN(y)) {
			return Double.NaN;
		}
		return x - y * truncate(x / y);
//		return Math.IEEEremainder(x, y);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static double getNumerator(double x) {
		if(x == 0.0) {
			return x;
		} else if(Double.isInfinite(x)) {
			return x;
		} else if(Double.isNaN(x)) {
			return Double.NaN;
		}
		return LispDouble.toExact(x).getNumerator().doubleValue();
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static double getDenominator(double x) {
		if(x == 0.0) {
			return 1.0;
		} else if(Double.isInfinite(x)) {
			return 1.0;
		} else if(Double.isNaN(x)) {
			return Double.NaN;
		}
		return LispDouble.toExact(x).getDenominator().doubleValue();
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static double truncate(double x) {
		return (x > 0) ? Math.floor(x) : Math.ceil(x);
	}

}
