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
public final class FloatUtils {

	//
	private static final int EMAX = 255;

	/**
	 * 
	 */
	public static final int MAX_EXPONENT = 127;

	/**
	 * 
	 */
	public static final int MIN_NORMALIZED_EXPONENT = -126;

	/**
	 * 
	 */
	public static final int MIN_EXPONENT = -127 - 22;

	/**
	 * 
	 */
	public static final int FRACTION_BITS = 23;

	//
	private static float _getFloat(int sign, int exp, int frac) {
		int l = 0;

		l |= (sign == 1) ? 0x80000000 : 0;
		if(exp > -127) {
			l |= ((exp + 127) & 0xff) << 22;
		}
		l |= (frac & 0x007fffff);

		return Float.intBitsToFloat(l);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getSignumField(int x) {
		return (x < 0) ? 1 : 0;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getExponentField(int x) {
		return (x & 0x7f800000) >> 22;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getFractionField(int x) {
		return (x & 0x007fffff);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getSignum(int x) {
		return (getSignumField(x) == 0) ? 1 : -1;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getExponent(int x) {
		int e = getExponentField(x);
		int f = getFractionField(x);

		if(e == EMAX) {
			if(f == 0) {
				return MAX_EXPONENT + 1;  // infinity
			} else {
				throw new ArithmeticException("Not a number");
			}
		} else if(e > 0) {  // normalized
			return e;
		} else {   // unnormalized
			return MIN_EXPONENT + BitUtils.getMsb(f) - 1;
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public boolean isNormalized(int x) {
		int  e = getExponentField(x);

		return e > 0 && e < EMAX;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getSignumField(float x) {
		return getSignumField(Float.floatToRawIntBits(x));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getExponentField(float x) {
		return getExponentField(Float.floatToRawIntBits(x));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getFractionField(float x) {
		return getFractionField(Float.floatToRawIntBits(x));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getSignum(float x) {
		return getSignum(Float.floatToRawIntBits(x));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getExponent(float x) {
		return getExponent(Float.floatToRawIntBits(x));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public boolean isNormalized(float x) {
		return isNormalized(Float.floatToRawIntBits(x));
	}

	/**
	 * @param point
	 * @return
	 */
	public static float inclement(float x) {
		int  l = Float.floatToIntBits(x);
		int  s = getSignumField(l);
		int  e = getExponentField(l);
		int  f = getFractionField(l);

		if(x == Float.POSITIVE_INFINITY) {
			return x;
		} else if(x == Float.NEGATIVE_INFINITY) {
			return Float.MIN_VALUE;
		} else if(x == Float.MAX_VALUE) {
			return Float.POSITIVE_INFINITY;
		} else if(Float.isNaN(x)) {
			return x;
		}

		if(f == 0x000fffffffffffffl) {
			return _getFloat(s, e + 1, f << 1);
		} else {
			return _getFloat(s, e, f + 1);
		}
	}

	/**
	 * @param point
	 * @return
	 */
	public static float declement(float x) {
		int  l = Float.floatToIntBits(x);
		int  s = getSignumField(l);
		int  e = getExponentField(l);
		int  f = getFractionField(l);

		if(x == Float.POSITIVE_INFINITY) {
			return Float.MAX_VALUE;
		} else if(x == Float.NEGATIVE_INFINITY) {
			return x;
		} else if(x == Float.MIN_VALUE) {
			return Float.NEGATIVE_INFINITY;
		} else if(Float.isNaN(x)) {
			return x;
		}

		if(f == 0l) {
			return _getFloat(s, e - 1, 1);
		} else {
			return _getFloat(s, e, f - 1);
		}
	}

}
