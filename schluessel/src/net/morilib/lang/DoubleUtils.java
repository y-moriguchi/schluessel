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
package net.morilib.lang;

import net.morilib.util.BitUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/16
 */
public final class DoubleUtils {

	//
	private static final int EMAX = 2047;
	private static final int EXPBIT = 52;
	private static final long EXPMASK = 0x7ff0000000000000l;
	private static final long SIGMASK = 0x000fffffffffffffl;
	private static final long SGNMASK = 0x8000000000000000l;

	/**
	 * 
	 */
	public static final int MAX_EXPONENT = 1023;

	/**
	 * 
	 */
	public static final int MIN_NORMALIZED_EXPONENT = -1022;

	/**
	 * 
	 */
	public static final int MIN_EXPONENT = -1022 - 52;

	/**
	 * 
	 */
	public static final int FRACTION_BITS = 52;

	/**
	 * 
	 */
	public static final int BIAS = 1023;

	/**
	 * 
	 */
	public static final long POSITIVE_INFINITY_BY_LONG =
		0x7ff0000000000000l;

	/**
	 * 
	 */
	public static final long NEGATIVE_INFINITY_BY_LONG =
		0xfff0000000000000l;

	/**
	 * 
	 */
	public static final long MAX_VALUE_BY_SHORT =
		0x7fefffffffffffffl;

	/**
	 * 
	 */
	public static final long MIN_VALUE_BY_SHORT =
		0xffefffffffffffffl;

	/**
	 * 
	 */
	public static final long ZERO = 0;

	/**
	 * 
	 */
	public static final long MINUS_ZERO = 0xf000000000000000l;

	//
	private static final int DTOH = FRACTION_BITS - Half.FRACTION_BITS;

	//
	/*package*/ static double _getDouble(
			int sign, int exp, long frac) {
		long l = 0;

		l |= (sign == 1) ? SGNMASK : 0;
		if(exp > MAX_EXPONENT) {
			return (l | POSITIVE_INFINITY_BY_LONG);
		} else if(exp >= MIN_NORMALIZED_EXPONENT) {
			l |= (long)(exp + BIAS) << EXPBIT;
		} else if(exp < MIN_EXPONENT) {
			return Double.longBitsToDouble(l);
		}
		l |= (frac & SIGMASK);
		return Double.longBitsToDouble(l);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getSignumField(long x) {
		return (x < 0) ? 1 : 0;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getExponentField(long x) {
		return (int)((x & EXPMASK) >> EXPBIT);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static long getFractionField(long x) {
		return (long)(x & SIGMASK);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getSignum(long x) {
		return (getSignumField(x) == 0) ? 1 : -1;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getExponent(long x) {
		int e = getExponentField(x);
		long f = getFractionField(x);

		if(e == EMAX) {
			if(f == 0) {
				return MAX_EXPONENT + 1;  // infinity
			} else {
				//throw new ArithmeticException("Not a number");
				return -1;
			}
		} else if(e > 0) {  // normalized
			return e;
		} else {   // unnormalized
			return MIN_EXPONENT - (BitUtils.getMsb(f) - 1);
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isNormalized(long x) {
		int  e = getExponentField(x);

		return e > 0 && e < EMAX;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isZero(long x) {
		return (x & ~SGNMASK) == 0;
	}

	/**
	 * @param point
	 * @return
	 */
	public static double inclement(double x) {
		long l = Double.doubleToLongBits(x);
		int  s = getSignumField(l);
		int  e = getExponentField(l);
		long f = getFractionField(l);

		if(x == Double.POSITIVE_INFINITY) {
			return x;
		} else if(x == Double.NEGATIVE_INFINITY) {
			return Double.MIN_VALUE;
		} else if(x == Double.MAX_VALUE) {
			return Double.POSITIVE_INFINITY;
		} else if(Double.isNaN(x)) {
			return x;
		}

		if(f == SIGMASK) {
			return _getDouble(s, e + 1, f << 1);
		} else {
			return _getDouble(s, e, f + 1);
		}
	}

	/**
	 * @param point
	 * @return
	 */
	public static double declement(double x) {
		long l = Double.doubleToLongBits(x);
		int  s = getSignumField(l);
		int  e = getExponentField(l);
		long f = getFractionField(l);

		if(x == Double.POSITIVE_INFINITY) {
			return Double.MAX_VALUE;
		} else if(x == Double.NEGATIVE_INFINITY) {
			return x;
		} else if(x == Double.MIN_VALUE) {
			return Double.NEGATIVE_INFINITY;
		} else if(Double.isNaN(x)) {
			return x;
		}

		if(f == 0l) {
			return _getDouble(s, e - 1, 1);
		} else {
			return _getDouble(s, e, f - 1);
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getSignumField(double x) {
		return getSignumField(Double.doubleToRawLongBits(x));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getExponentField(double x) {
		return getExponentField(Double.doubleToRawLongBits(x));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static long getFractionField(double x) {
		return getFractionField(Double.doubleToRawLongBits(x));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getSignum(double x) {
		return getSignum(Double.doubleToRawLongBits(x));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getExponent(double d) {
		//return getExponent(Double.doubleToRawLongBits(x));
		long x = Double.doubleToLongBits(d);
		int  e = getExponentField(x);
		long f = getFractionField(x);

		if(isZero(x)) {
			return 0;
		} else if(e == EMAX) {
			if(f == 0) {
				return MAX_EXPONENT + 1;  // infinity
			} else {
				//throw new ArithmeticException("Not a number");
				return Integer.MIN_VALUE;
			}
		} else if(e > 0) {  // normalized
			return e - BIAS;
		} else {   // unnormalized
			return MIN_EXPONENT + (BitUtils.getMsb(f) - 1);
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isNormalized(double x) {
		return isNormalized(Double.doubleToRawLongBits(x));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static short toHalf(double x) {
		int  e = getExponent(x);
		long l = getFractionField(x);

		if(e == Integer.MIN_VALUE) {
			return Half.NaN_BY_SHORT;
		} else if(x > Half.MAX_VALUE.doubleValue()) {
			return Half.POSITIVE_INFINITY_BY_SHORT;
		} else if(x < Half.NEGATIVE_MAX_VALUE.doubleValue()) {
			return Half.NEGATIVE_INFINITY_BY_SHORT;
		} else if(Math.abs(x) <
				Half.MIN_VALUE.doubleValue()) {
			return (x > 0) ?
					Half.ZERO_BY_SHORT : Half.MINUS_ZERO_BY_SHORT;
		} else if(e >= Half.MIN_NORMALIZED_EXPONENT) {
			return Half._getHalf(getSignumField(x), e,
					(int)(l >> DTOH));
		} else if(e >= Half.MIN_EXPONENT) {
			short r = (short)((l | (1l << FRACTION_BITS)) >>
					(FRACTION_BITS - (Half.MIN_EXPONENT - e)));

			return (x > 0) ? r : Half.neg(r);
		} else {
			return (x > 0) ?
					Half.ZERO_BY_SHORT : Half.MINUS_ZERO_BY_SHORT;
		}
	}

}
