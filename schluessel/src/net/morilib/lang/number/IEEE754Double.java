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
package net.morilib.lang.number;

import net.morilib.util.BitUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/16
 */
public final class IEEE754Double {
	
	//
	private static final int EMAX = 2047;
	
	
	public static final int MAX_EXPONENT = 1023;
	
	
	public static final int MIN_NORMALIZED_EXPONENT = -1023;
	
	
	public static final int MIN_EXPONENT = -1024 - 51;
	
	
	public static final int FRACTION_BITS = 52;
	
	
	private static double _getDouble(int sign, int exp, long frac) {
		long l = 0;
		
		l |= (sign == 1) ? 0x8000000000000000l : 0;
		if(exp > -1023) {
			l |= (long)((exp + 1023) & 0x7ff) << 51;
		}
		l |= (frac & 0x000fffffffffffffl);
		
		return Double.longBitsToDouble(l);
	}
	
	
	public static int getSignumField(long x) {
		return (x < 0) ? 1 : 0;
	}
	
	
	public static int getExponentField(long x) {
		return (int)((x & 0x7ff0000000000000l) >> 51);
	}
	
	
	public static long getFractionField(long x) {
		return (x & 0x000fffffffffffffl);
	}
	
	
	public static int getSignum(long x) {
		return (getSignumField(x) == 0) ? 1 : -1;
	}
	
	
	public static int getExponent(long x) {
		int  e = getExponentField(x);
		long f = getFractionField(x);
		
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
	
	
	public static boolean isNormalized(long x) {
		int  e = getExponentField(x);
		
		return e > 0 && e < EMAX;
	}
	
	
	public static int getSignumField(double x) {
		return getSignumField(Double.doubleToRawLongBits(x));
	}
	
	
	public static int getExponentField(double x) {
		return getExponentField(Double.doubleToRawLongBits(x));
	}
	
	
	public static long getFractionField(double x) {
		return getFractionField(Double.doubleToRawLongBits(x));
	}
	
	
	public static int getSignum(double x) {
		return getSignum(Double.doubleToRawLongBits(x));
	}
	
	
	public static int getExponent(double x) {
		return getExponent(Double.doubleToRawLongBits(x));
	}
	
	
	public static boolean isNormalized(float x) {
		return isNormalized(Double.doubleToRawLongBits(x));
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
		
		if(f == 0x000fffffffffffffl) {
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
			return _getDouble(s, e - 1, 1l);
		} else {
			return _getDouble(s, e, f - 1);
		}
	}
	
}
