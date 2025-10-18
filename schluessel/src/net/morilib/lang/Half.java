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
public final class Half extends Number implements Comparable<Half> {

	//
	private static final int EMAX = 31;
	private static final int EXPBIT = 10;
	private static final short EXPMASK = 0x7c00;
	private static final short SIGMASK = 0x03ff;
	private static final short SGNMASK = (short)0x8000;

	/**
	 * 
	 */
	public static final int MAX_EXPONENT = 15;

	/**
	 * 
	 */
	public static final int MIN_NORMALIZED_EXPONENT = -14;

	/**
	 * 
	 */
	public static final int MIN_EXPONENT = -14 - 10;

	/**
	 * 
	 */
	public static final int FRACTION_BITS = 10;

	/**
	 * 
	 */
	public static final int BIAS = 15;

	/**
	 * 
	 */
	public static final short POSITIVE_INFINITY_BY_SHORT = 0x7c00;

	/**
	 * 
	 */
	public static final short NEGATIVE_INFINITY_BY_SHORT =
		(short)0xfc00;

	/**
	 * 
	 */
	public static final short MAX_VALUE_BY_SHORT = (short)0x7bff;

	/**
	 * 
	 */
	public static final short NEGATIVE_MAX_VALUE_BY_SHORT =
		(short)0xfbff;

	/**
	 * 
	 */
	public static final short MIN_VALUE_BY_SHORT = (short)0x0001;

	/**
	 * 
	 */
	public static final short ZERO_BY_SHORT = 0;

	/**
	 * 
	 */
	public static final short MINUS_ZERO_BY_SHORT = (short)0x8000;

	/**
	 * 
	 */
	public static final short NaN_BY_SHORT = (short)0x7fff;

	/**
	 * 
	 */
	public static final Half MAX_VALUE = new Half(MAX_VALUE_BY_SHORT);

	/**
	 * 
	 */
	public static final Half NEGATIVE_MAX_VALUE =
		new Half(NEGATIVE_MAX_VALUE_BY_SHORT);

	/**
	 * 
	 */
	public static final Half MIN_VALUE = new Half(MIN_VALUE_BY_SHORT);

	/**
	 * 
	 */
	public static final int SIZE = 16;

	//
	/*package*/ static short _getHalf(int sign, int exp, int frac) {
		int l = 0;

		l |= (sign == 1) ? SGNMASK : 0;
		if(exp > MAX_EXPONENT) {
			return (short)(l | POSITIVE_INFINITY_BY_SHORT);
		} else if(exp >= MIN_NORMALIZED_EXPONENT) {
			l |= ((exp + BIAS) & EMAX) << EXPBIT;
		} else if(exp < MIN_EXPONENT) {
			return (short)l;
		} else {
			l |= ((frac << (exp - MIN_EXPONENT)) & SIGMASK);
			l |= 1 << (exp - MIN_EXPONENT);
		}
		l |= (frac & SIGMASK);
		return (short)l;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getSignumField(short x) {
		return (x < 0) ? 1 : 0;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getExponentField(short x) {
		return (x & EXPMASK) >> EXPBIT;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getFractionField(int x) {
		return (x & SIGMASK);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getSignum(short x) {
		if(isZero(x)) {
			return 0;
		} else {
			return (getSignumField(x) == 0) ? 1 : -1;
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getExponent(short x) {
		int e = getExponentField(x);
		int f = getFractionField(x);

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
	public static boolean isNormalized(short x) {
		int  e = getExponentField(x);

		return e > 0 && e < EMAX;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isNaN(short x) {
		return getExponent(x) == Integer.MIN_VALUE;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isZero(short x) {
		return (x & ~SGNMASK) == 0;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isInfinite(short x) {
		return (getExponentField(x) == EMAX &&
				getFractionField(x) == 0);
	}

	/**
	 * @param point
	 * @return
	 */
	public static short inclement(short l) {
		int  s = getSignumField(l);
		int  e = getExponent(l);
		int  f = getFractionField(l);

		if(l == POSITIVE_INFINITY_BY_SHORT) {
			return l;
		} else if(l == NEGATIVE_INFINITY_BY_SHORT) {
			return NEGATIVE_MAX_VALUE_BY_SHORT;
		} else if(l == MAX_VALUE_BY_SHORT) {
			return POSITIVE_INFINITY_BY_SHORT;
		} else if(isNaN(l)) {
			return l;
		}

		if(f == SIGMASK) {
			return _getHalf(s, e + 1, 0);
		} else {
			return _getHalf(s, e, f + 1);
		}
	}

	/**
	 * @param point
	 * @return
	 */
	public static short declement(short l) {
		int  s = getSignumField(l);
		int  e = getExponent(l);
		int  f = getFractionField(l);

		if(l == POSITIVE_INFINITY_BY_SHORT) {
			return MAX_VALUE_BY_SHORT;
		} else if(l == NEGATIVE_INFINITY_BY_SHORT) {
			return l;
		} else if(l == NEGATIVE_MAX_VALUE_BY_SHORT) {
			return NEGATIVE_INFINITY_BY_SHORT;
		} else if(isNaN(l)) {
			return l;
		}

		if(f == 0l) {
			return _getHalf(s, e - 1, SIGMASK);
		} else {
			return _getHalf(s, e, f - 1);
		}
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static int compare(short a, short b) {
		int as = getSignum(a);
		int bs = getSignum(b);
		int ea = getExponent(a);
		int eb = getExponent(b);
		int fa = getFractionField(a);
		int fb = getFractionField(b);

		if(isNaN(a)) {
			return isNaN(b) ? 0 : 1;
		} else if(isNaN(b)) {
			return -1;
		} else if(as > bs) {
			return 1;
		} else if(as < bs) {
			return -1;
		} else if(ea > eb) {
			return 1;
		} else if(ea < eb) {
			return -1;
		} else if(fa > fb) {
			return 1;
		} else if(fa < fb) {
			return -1;
		} else {
			return 0;
		}
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static boolean equals(short a, short b) {
		return !isNaN(a) && !isNaN(b) &&
				(isZero(a) && isZero(b)) || a == b;
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static short max(short a, short b) {
		return (compare(a, b) > 0) ? a : b;
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static short min(short a, short b) {
		return (compare(a, b) < 0) ? a : b;
	}

	//
	private static short unnormalizedToF(int a) {
		int b = BitUtils.getMsb(a);
		int e;
		short r;

		if(b > FRACTION_BITS) {
			e = b - FRACTION_BITS + MIN_NORMALIZED_EXPONENT - 1;
			r = _getHalf(0, e,
					(a >> (e - MIN_NORMALIZED_EXPONENT)) & SIGMASK);
//			if((a & (1 << (b - FRACTION_BITS - 1))) > 0) {
//				return inclement(r);
//			} else {
//				return r;
//			}
			return r;
		} else {
			return (short)a;
		}
	}

	// compute a + b   if a > 0 and b > 0
	//         a - |b| if a < 0 and b < 0
	// always |a| >= |b|
	private static short addsamesign(short a, short b) {
		int as = getSignumField(a);
		int ea = getExponent(a);
		int eb = getExponent(b);
		int fa = getFractionField(a);
		int fb = getFractionField(b);
		int an, bn, cr;

		if(ea - eb > EXPBIT) {
			return a;
		} else if(ea - eb == EXPBIT) {
			if((fb & (1 << (FRACTION_BITS - 1))) > 0) {
				return inclement(a);
			} else {
				return a;
			}
		} else if(!isNormalized(a)) {
			// b is also unnormalized
			an = unnormalizedToF(fa + fb);
			an = an | ((as > 0) ? SGNMASK : 0);
			return (short)an;
		} else if(!isNormalized(b)) {
			an = (fa | (1 << EXPBIT)) << (ea - MIN_EXPONENT - EXPBIT);
			an = an + fb;
			an = an | ((as > 0) ? SGNMASK : 0);
			return unnormalizedToF(an);
		} else {
			// a and b are normalized
			an = (fa | (1 << EXPBIT)) << EXPBIT;
			bn = (fb | (1 << EXPBIT)) << (EXPBIT - (ea - eb));
			an = an + bn;
			//cr = ((an & (1 << (EXPBIT * 2 + 1))) > 0) ? 1 : 0;
			cr = an >> (EXPBIT * 2 + 1);
			an >>= (EXPBIT + cr);
			an = an & SIGMASK;
			return _getHalf(as, ea + cr, an);
		}
	}

	// compute a - |b| if a > 0 and b < 0
	//         a + |b| if a < 0 and b > 0
	// always |a| >= |b|
	private static short adddiffsign(short a, short b) {
		int as = getSignumField(a);
		int ea = getExponent(a);
		int eb = getExponent(b);
		int fa = getFractionField(a);
		int fb = getFractionField(b);
		int an, bn, cr;

		if(ea - eb > EXPBIT) {
			return a;
		} else if(ea - eb == EXPBIT) {
			if((fb & (1 << (FRACTION_BITS - 1))) > 0) {
				return declement(a);
			} else {
				return a;
			}
		} else if(!isNormalized(a)) {
			// b is also unnormalized
			an = fa - fb;
			an = an | ((as > 0) ? SGNMASK : 0);
			return (short)an;
		} else if(!isNormalized(b)) {
			an = (fa | (1 << EXPBIT)) << (ea - MIN_EXPONENT - EXPBIT);
			an = an - fb;
			an = an | ((as > 0) ? SGNMASK : 0);
			return (short)an;
		} else {
			// a and b are normalized
			an = (fa | (1 << EXPBIT)) << EXPBIT;
			bn = (fb | (1 << EXPBIT)) << (EXPBIT - (ea - eb));
			an = an - bn;
			if(an > 0) {
				cr = ((an & (1 << (EXPBIT * 2))) > 0) ? 0 : 1;
				an >>= EXPBIT - 2;
				return _getHalf(as, ea - cr, an >> 1);
			} else {
				return ZERO_BY_SHORT;
			}
		}
	}

	/**
	 * 
	 * @param a
	 * @return
	 */
	public static short neg(short a) {
		if(isNaN(a)) {
			return NaN_BY_SHORT;
		} else {
			return (short)(a ^ SGNMASK);
		}
	}

	/**
	 * 
	 * @param a
	 * @return
	 */
	public static short abs(short a) {
		if(isNaN(a)) {
			return NaN_BY_SHORT;
		} else {
			return (short)(a & ~SGNMASK);
		}
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static short add(short a, short b) {
		int as = getSignumField(a);
		int bs = getSignumField(b);
		short za = abs(a);
		short zb = abs(b);

		if(isNaN(a) || isNaN(b)) {
			return NaN_BY_SHORT;
		} else if(a == POSITIVE_INFINITY_BY_SHORT) {
			return (b == NEGATIVE_INFINITY_BY_SHORT) ?
					NaN_BY_SHORT : a;
		} else if(a == NEGATIVE_INFINITY_BY_SHORT) {
			return (b == POSITIVE_INFINITY_BY_SHORT) ?
					NaN_BY_SHORT : a;
		} else if(as == bs) {
			return (compare(za, zb) > 0) ?
					addsamesign(a, b) : addsamesign(b, a);
		} else {
			return (compare(za, zb) > 0) ?
					adddiffsign(a, b) : adddiffsign(b, a);
		}
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static short sub(short a, short b) {
		int as = getSignumField(a);
		int bs = getSignumField(b);
		short za = abs(a);
		short zb = abs(b);

		if(isNaN(a) || isNaN(b)) {
			return NaN_BY_SHORT;
		} else if(a == POSITIVE_INFINITY_BY_SHORT) {
			return (b == POSITIVE_INFINITY_BY_SHORT) ?
					NaN_BY_SHORT : a;
		} else if(a == NEGATIVE_INFINITY_BY_SHORT) {
			return (b == NEGATIVE_INFINITY_BY_SHORT) ?
					NaN_BY_SHORT : a;
		} else if(as == bs) {
			return (compare(za, zb) > 0) ?
					adddiffsign(a, neg(b)) : adddiffsign(neg(b), a);
		} else {
			return (compare(za, zb) > 0) ?
					addsamesign(a, neg(b)) : addsamesign(neg(b), a);
		}
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static short mul(short a, short b) {
		int sg = getSignumField(a) ^ getSignumField(b);
		int ea = getExponent(a);
		int eb = getExponent(b);
		int fa = getFractionField(a);
		int fb = getFractionField(b);
		int an, bt;

		if(isNaN(a) || isNaN(b)) {
			return NaN_BY_SHORT;
		} else if(isInfinite(a)) {
			switch(getSignum(b)) {
			case -1:  return neg(a);
			case 0:
				return (sg > 0) ? ZERO_BY_SHORT : MINUS_ZERO_BY_SHORT;
			case 1:   return a;
			default:  throw new RuntimeException();
			}
		} else if(ea + eb > MAX_EXPONENT) {
			if(sg > 0) {
				return NEGATIVE_INFINITY_BY_SHORT;
			} else {
				return POSITIVE_INFINITY_BY_SHORT;
			}
		} else if(ea + eb < MIN_EXPONENT) {
			return (sg > 0) ? MINUS_ZERO_BY_SHORT : ZERO_BY_SHORT;
		} else if(isNormalized(a) && isNormalized(b)) {
			an = (fa | (1 << EXPBIT)) * (fb | (1 << EXPBIT));
			bt = BitUtils.getMsb(an);
			an = an >> (bt - EXPBIT - 1);
			bt = bt - EXPBIT * 2 - 1;
			an = an & SIGMASK;
			return _getHalf(sg, ea + eb + bt, an);
		} else if(isNormalized(a) || isNormalized(b)) {
			if(isNormalized(a)) {
				an = (fa | (1 << EXPBIT)) * fb;
			} else {
				an = fa * (fb | (1 << EXPBIT));
				ea = eb;
			}
			bt = BitUtils.getMsb(an);
			an = an >> (bt - EXPBIT - 1);
			an = an & SIGMASK;
			bt = bt - EXPBIT - 1;
			return _getHalf(sg, ea + MIN_EXPONENT + bt, an);
		} else {
			// unreachable code
			throw new RuntimeException();
			//an = fa * fb;
			//an = an >> EXPBIT;
			//return unnormalizedToF(an);
		}
	}

	public static short div(short a, short b) {
		int sg = getSignumField(a) ^ getSignumField(b);
		int ea = getExponent(a);
		int eb = getExponent(b);
		int fa = getFractionField(a);
		int fb = getFractionField(b);
		int an, bt;

		if(isNaN(a) || isNaN(b)) {
			return NaN_BY_SHORT;
		} else if(isZero(b)) {
			switch(getSignum(a) * ((getSignumField(b) > 0) ? -1 : 1)) {
			case -1:  return NEGATIVE_INFINITY_BY_SHORT;
			case 0:   return NaN_BY_SHORT;
			case 1:   return POSITIVE_INFINITY_BY_SHORT;
			default:  throw new RuntimeException();
			}
		} else if(isInfinite(b)) {
			if(isInfinite(a)) {
				return NaN_BY_SHORT;
			} else {
				return (sg > 0) ? MINUS_ZERO_BY_SHORT : ZERO_BY_SHORT;
			}
		} else if(ea - eb > MAX_EXPONENT) {
			if(sg > 0) {
				return NEGATIVE_INFINITY_BY_SHORT;
			} else {
				return POSITIVE_INFINITY_BY_SHORT;
			}
		} else if(ea - eb < MIN_EXPONENT) {
			return (sg > 0) ? MINUS_ZERO_BY_SHORT : ZERO_BY_SHORT;
		} else if(isNormalized(b)) {
			if(isNormalized(a)) {
				an = (fa | (1 << EXPBIT)) << (EXPBIT + 1);
			} else {
				an = fa << (EXPBIT + 2);
			}
			an = an / (fb | (1 << EXPBIT));
			bt = BitUtils.getMsb(an);
			if(ea - eb >= MIN_NORMALIZED_EXPONENT) {
				an = an >> (bt - EXPBIT - 1);
				an = an & SIGMASK;
				bt = bt - EXPBIT - 2;
				return _getHalf(sg, ea - eb + bt, an);
			} else {
				an = an >> ((EXPBIT + 1) - (ea - eb - MIN_EXPONENT));
				an = an & SIGMASK;
				an = unnormalizedToF(an);
				an = (sg > 0) ? (an | SGNMASK) : an;
				return (short)an;
			}
		} else if(isNormalized(a)) {
			an = ((fa | (1 << EXPBIT)) << (EXPBIT + 1)) / fb;
			bt = BitUtils.getMsb(an);
			an = an >> (bt - EXPBIT - 1);
			an = an & SIGMASK;
			bt = bt - EXPBIT - 2;
			bt = bt + ea - MIN_NORMALIZED_EXPONENT;
			return _getHalf(sg, bt, an);
		} else {
			an = fa << EXPBIT;
			an = an / fb;
			bt = BitUtils.getMsb(an);
			if(bt <= EXPBIT) {
				an = an << (EXPBIT - bt + 1);
				an = an & SIGMASK;
				return _getHalf(sg, -(EXPBIT - bt + 1), an);
			} else {
				an = an >> -(EXPBIT - bt + 1);
				an = an & SIGMASK;
				return _getHalf(sg, -(EXPBIT - bt + 1), an);
			}
		}
	}

	/**
	 * 
	 * @param a
	 * @return
	 */
	public static double toDouble(short a) {
		int as = getSignumField(a);
		int ea = getExponent(a);
		int fa = getFractionField(a);
		long l = fa;

		if(isNaN(a)) {
			return Double.NaN;
		} else if(isInfinite(a)) {
			if(as > 0) {
				return Double.NEGATIVE_INFINITY;
			} else {
				return Double.POSITIVE_INFINITY;
			}
		} else if(isZero(a)) {
			return (as > 0) ? 0.0 : -0.0;
		} else if(ea < MIN_NORMALIZED_EXPONENT) {
			l = l << (DoubleUtils.FRACTION_BITS - FRACTION_BITS);
			l = l << (BIAS - ea + 1);
			return DoubleUtils._getDouble(as, ea, l);
		} else {
			l = l << (DoubleUtils.FRACTION_BITS - FRACTION_BITS);
			return DoubleUtils._getDouble(as, ea, l);
		}
	}

	/**
	 * 
	 * @param a
	 * @return
	 */
	public static float toFloat(short a) {
		return (float)toDouble(a);
	}

	/**
	 * 
	 * @param a
	 * @return
	 */
	public static String toString(short a) {
		return Double.toString(toDouble(a));
	}

	/**
	 * 
	 * @param a
	 * @return
	 */
	public static String toHexString(short a) {
		return Double.toHexString(toDouble(a));
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static short parseHalf(String s) {
		return DoubleUtils.toHalf(Double.parseDouble(s));
	}

	/**
	 * 
	 * @param a
	 * @return
	 */
	public static short ulp(short a) {
		int e = getExponent(a);

		if(isNaN(a)) {
			return a;
		} else if(isInfinite(a)) {
			return POSITIVE_INFINITY_BY_SHORT;
		} else if(a == MAX_VALUE_BY_SHORT ||
				a == NEGATIVE_MAX_VALUE_BY_SHORT) {
			return _getHalf(0, MAX_EXPONENT - FRACTION_BITS, 0);
		} else if(isZero(a)) {
			return MIN_VALUE_BY_SHORT;
		} else if(!isNormalized(a)) {
			return MIN_VALUE_BY_SHORT;
		} else {
			return _getHalf(0, e - EXPBIT, 0);
		}
	}

	//
	private short value;

	/**
	 * 
	 * @param value
	 */
	public Half(short value) {
		this.value = value;
	}

	/**
	 * 
	 * @param value
	 */
	public Half(double value) {
		this.value = DoubleUtils.toHalf(value);
	}

	/**
	 * 
	 * @param s
	 */
	public Half(String s) {
		this.value = parseHalf(s);
	}

	/**
	 * 
	 * @return
	 */
	public short toShortValue() {
		return value;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isInfinite() {
		return isInfinite(value);
	}

	/**
	 * 
	 * @return
	 */
	public boolean isNaN() {
		return isNaN(value);
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(Half o) {
		return compare(value, o.value);
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#intValue()
	 */
	@Override
	public int intValue() {
		return (int)toDouble(value);
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#longValue()
	 */
	@Override
	public long longValue() {
		return (long)toDouble(value);
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#floatValue()
	 */
	@Override
	public float floatValue() {
		return (float)toDouble(value);
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#doubleValue()
	 */
	@Override
	public double doubleValue() {
		return toDouble(value);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof Half) {
			return value == ((Half)o).value;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return value;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return toString(value);
	}

}
