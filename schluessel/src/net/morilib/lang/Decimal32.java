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

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/30
 */
public final class Decimal32 extends Number
implements Comparable<Decimal32>, java.io.Serializable {

	//
	private static final int  SGNMASK = 0x80000000;
	private static final int  SIGMASK = 0x03f00000;
	private static final int  EXPBIT  = 20;
	private static final int  CMBMASK = 0x7c000000;
	private static final int  CMBBIT  = 26;
	//private static final int  EMAX    = 0x2ff;
	private static final int  FRCSIZE = 6;
	private static final int  EXPSIZE = 6;
	private static final int  BIAS    = 95;
	private static final int  TBIT    = 0x60000000;

	/**
	 * 
	 */
	public static int MAX_EXPONENT = 96;

	/**
	 * 
	 */
	public static int MIN_NORMALIZED_EXPONENT = -95;

	/**
	 * 
	 */
	public static int MIN_EXPONENT = -95 - 6;

	/**
	 * 
	 */
	public static int ZERO_BY_INT = 0;

	/**
	 * 
	 */
	public static int MINUS_ZERO_BY_INT = 0x80000000;

	/**
	 * 
	 */
	public static int POSITIVE_INFINITY_BY_INT = 0x78000000;

	/**
	 * 
	 */
	public static int NEGATIVE_INFINITY_BY_INT = 0xf8000000;

	/**
	 * 
	 */
	public static int NaN_BY_INT = 0x7c000000;

	//
	private int value;

	/**
	 * 
	 * @param value
	 */
	public Decimal32(int value) {
		this.value = value;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getExponentField(int x) {
		int e = (int)((x & SIGMASK) >> EXPBIT);
		int c = (int)((x & CMBMASK) >> CMBBIT);

		if(c < 0x18) {
			return e | ((c & 0x18) << (EXPSIZE - 3));
		} else {
			return e | ((c & 0x06) << (EXPSIZE - 1));
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getCombinationField(int x) {
		return (int)((x & CMBMASK) >> CMBBIT);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static BCDDecimal getFractionField(long x) {
		byte[] b = new byte[FRCSIZE + 1];
		int c = (int)((x & CMBMASK) >> CMBBIT);

		Decimal64.writedpd((int)((x) & 0x3ff), b, 0);
		Decimal64.writedpd((int)((x >> 10) & 0x3ff), b, 3);
		if((x & TBIT) == TBIT) {
			b[6] = (byte)(0x08 | (c & 1));
		} else {
			b[6] = (byte)(c & 7);
		}
		return new BCDDecimal(new BCDInteger(
				1, new BCDNaturalNumber(b)), FRCSIZE);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isZero(int x) {
		return (x & ~SGNMASK) == 0;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isInfinite(int x) {
		return getCombinationField(x) == 0x1e;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isNaN(int x) {
		return getCombinationField(x) == 0x1f;
	}

	//
	/*package*/ static boolean isNormalized(int x) {
		return (x & CMBMASK) != 0;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getSignum(int x) {
		return isZero(x) ?  0 : ((x & SGNMASK) == 0) ? 1 : -1;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getSignumExact(int x) {
		return ((x & SGNMASK) == 0) ? 1 : -1;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getExponent(int x) {
		int e = getExponentField(x);

		if(isZero(x)) {
			return Integer.MIN_VALUE;
		} else if(isNaN(x)) {
			return Integer.MAX_VALUE;
		} else if(isInfinite(x)) {
			return Integer.MAX_VALUE - 1;
		} else if(e > 0) {  // normalized
			return e - BIAS;
		} else {   // unnormalized
			return (MIN_NORMALIZED_EXPONENT +
					getFractionField(x).getMostSignificantDigit());
		}
	}

	//
	/*package*/ static int encodeDecimal(BCDDecimal d, int p) {
		int r = 0;

		r = r | (int)Decimal64.encodedec1(d, p -  6);
		r = r | (int)Decimal64.encodedec1(d, p -  3) << 10;

		if(d.getDigit(p) < 8) {
			r = r | ((int)d.getDigit(p) << CMBBIT);
			r = r | ((int)((p + BIAS) & 0x3f) << EXPBIT);
			r = r | ((int)((p + BIAS) & 0xc0) <<
					(CMBBIT - EXPSIZE + 3));
		} else {
			r = r | ((int)(d.getDigit(p) & 1) << CMBBIT);
			r = r | TBIT;
			r = r | ((int)((p + BIAS) & 0x3f) << EXPBIT);
			r = r | ((int)((p + BIAS) & 0xc0) <<
					(CMBBIT - EXPSIZE + 1));
		}
		return r;
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static int toDecimal(BCDDecimal dd) {
		int  md = dd.getMostSignificantDigit();
		int  r;

		if(dd.isZero()) {
			return ZERO_BY_INT;
		} else if(md < MIN_EXPONENT) {
			r = ZERO_BY_INT;
		} else if(md < MIN_NORMALIZED_EXPONENT) {
			dd = dd.round(-MIN_EXPONENT);
			r = encodeDecimal(dd, MIN_NORMALIZED_EXPONENT);
		} else if(md <= MAX_EXPONENT) {
			dd = dd.round(-(md - FRCSIZE));
			r = encodeDecimal(dd, md);
		} else {
			r = POSITIVE_INFINITY_BY_INT;
		}

		if(dd.signum() < 0) {
			r |= SGNMASK;
		}
		return r;
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static int parseDecimal(String s) {
		if(s == null) {
			throw new NullPointerException();
		} else if(s.equalsIgnoreCase("NaN")) {
			return NaN_BY_INT;
		} else if(s.equalsIgnoreCase("Infinity")) {
			return POSITIVE_INFINITY_BY_INT;
		} else if(s.equalsIgnoreCase("+Infinity")) {
			return POSITIVE_INFINITY_BY_INT;
		} else if(s.equalsIgnoreCase("-Infinity")) {
			return NEGATIVE_INFINITY_BY_INT;
		} else {
			return toDecimal(BCDDecimal.parseBCD(s));
		}
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static BCDDecimal toBCDDecimal(int d) {
		BCDDecimal f = getFractionField(d);
		int e = getExponentField(d) - BIAS;

		if(getSignum(d) < 0) {
			f = f.negate();
		}

		if(isNaN(d) || isInfinite(d)) {
			return null;
		} else if(e < MIN_NORMALIZED_EXPONENT) {
			return f.shift(MIN_NORMALIZED_EXPONENT);
		} else {
			return f.shift(e);
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int toDecimal(double x) {
		if(Double.isNaN(x)) {
			return NaN_BY_INT;
		} else if(x == Double.POSITIVE_INFINITY) {
			return POSITIVE_INFINITY_BY_INT;
		} else if(x == Double.NEGATIVE_INFINITY) {
			return NEGATIVE_INFINITY_BY_INT;
		} else if(Double.doubleToLongBits(x) == 0x00000000) {
			return ZERO_BY_INT;
		} else if(Double.doubleToLongBits(x) == 0x80000000) {
			return MINUS_ZERO_BY_INT;
		} else {
			return parseDecimal(Double.toString(x));
		}
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static int add(int a, int b) {
		if(isNaN(a) || isNaN(b)) {
			return NaN_BY_INT;
		} else if(isInfinite(a)) {
			if(isInfinite(b)) {
				return (getSignum(a) * getSignum(b) > 0) ?
						a : NaN_BY_INT;
			} else {
				return a;
			}
		} else if(isInfinite(b)) {
			return b;
		} else {
			return toDecimal(toBCDDecimal(a).add(toBCDDecimal(b)));
		}
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static int subtract(int a, int b) {
		if(isNaN(a) || isNaN(b)) {
			return NaN_BY_INT;
		} else if(isInfinite(a)) {
			if(isInfinite(b)) {
				return (getSignum(a) * getSignum(b) < 0) ?
						a : NaN_BY_INT;
			} else {
				return a;
			}
		} else if(isInfinite(b)) {
			return b ^ SGNMASK;
		} else {
			return toDecimal(
					toBCDDecimal(a).subtract(toBCDDecimal(b)));
		}
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static int multiply(int a, int b) {
		if(isNaN(a) || isNaN(b)) {
			return NaN_BY_INT;
		} else if(isInfinite(a) || isInfinite(b)) {
			switch(getSignum(a) * getSignum(b)) {
			case 1:   return POSITIVE_INFINITY_BY_INT;
			case 0:   return ZERO_BY_INT;
			case -1:  return NEGATIVE_INFINITY_BY_INT;
			default:  throw new RuntimeException();
			}
		} else {
			return toDecimal(
					toBCDDecimal(a).multiply(toBCDDecimal(b)));
		}
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static int divide(int a, int b) {
		BCDDecimal da, db, dc;

		if(isNaN(a) || isNaN(b)) {
			return NaN_BY_INT;
		} else if(isZero(b)) {
			if(isZero(a)) {
				return NaN_BY_INT;
			} else if(getSignumExact(a) * getSignumExact(b) > 0) {
				return POSITIVE_INFINITY_BY_INT;
			} else {
				return NEGATIVE_INFINITY_BY_INT;
			}
		} else if(isInfinite(a)) {
			if(isInfinite(b)) {
				return NaN_BY_INT;
			} else if(getSignumExact(a) * getSignumExact(b) > 0) {
				return ZERO_BY_INT;
			} else {
				return MINUS_ZERO_BY_INT;
			}
		} else if(isInfinite(b)) {
			if(getSignumExact(a) * getSignumExact(b) > 0) {
				return ZERO_BY_INT;
			} else {
				return MINUS_ZERO_BY_INT;
			}
		} else {
			da = toBCDDecimal(a).shiftExtend(FRCSIZE + 2);
			db = toBCDDecimal(b);
			dc = da.divide(db);
			return toDecimal(dc);
		}
	}

	/**
	 * 
	 * @param l
	 * @return
	 */
	public static int negate(int l) {
		return l ^ SGNMASK;
	}

	/**
	 * 
	 * @param l
	 * @return
	 */
	public static int toDecimal(long l) {
		return toDecimal(BCDDecimal.valueOf(l, 0));
	}

	/**
	 * 
	 * @param l
	 * @return
	 */
	public static BigDecimal toBigDecimal(int l) {
		BigDecimal d;
		int e;

		if(isNaN(l) || isInfinite(l)) {
			throw new IllegalArgumentException();
		} else if(isZero(l)) {
			return BigDecimal.ZERO;
		} else if(isNormalized(l)) {
			e = getExponent(l);
			d = getFractionField(l).toBigDecimal();
			return d.setScale(e + FRCSIZE, RoundingMode.HALF_EVEN);
		} else {
			d = getFractionField(l).toBigDecimal();
			return d.setScale(-MIN_EXPONENT, RoundingMode.HALF_EVEN);
		}
	}

	/**
	 * 
	 * @param l
	 * @param digit
	 * @return
	 */
	public static int round(int l, int digit) {
		BCDDecimal d;
		int e = getExponent(l);

		if(isNaN(l) || isInfinite(l) || isZero(l)) {
			return l;
		} else if(-digit > e + 1) {
			return ZERO_BY_INT;
		} else if(-digit == e + 1) {
			d = getFractionField(l);
			if(d.getDigit(0) < 5) {
				return ZERO_BY_INT;
			} else {
				d = BCDDecimal.parseBCD("1.000000").shift(-digit);
			}
		} else if(isNormalized(l)) {
			d = getFractionField(l);
			if(-digit < e - FRCSIZE) {
				return l;
			} else /*if(-digit <= e)*/ {
				d = d.round(digit + e).shift(e);
			}
		} else {
			d = getFractionField(l);
			if(-digit < MIN_EXPONENT) {
				return l;
			} else /*if(-digit <= e)*/ {
				d = d.round(digit + MIN_NORMALIZED_EXPONENT).shift(
						e + 1);
			}
		}
		return toDecimal((getSignum(l) > 0) ? d : d.negate());
	}

	/**
	 * 
	 * @param l
	 * @param digit
	 * @return
	 */
	public static int ceil(int l, int digit) {
		BCDDecimal d;
		int e = getExponent(l);

		if(isNaN(l) || isInfinite(l) || isZero(l)) {
			return l;
		} else if(-digit >= e + 1) {
			d = getFractionField(l);
			if(getSignum(l) > 0) {
				d = BCDDecimal.parseBCD("1.000000").shift(-digit);
				return toDecimal(d);
			} else {
				return ZERO_BY_INT;
			}
		} else if(isNormalized(l)) {
			d = getFractionField(l);
			d = (getSignum(l) > 0) ? d : d.negate();
			if(-digit < e - FRCSIZE) {
				return l;
			} else /*if(-digit <= e)*/ {
				d = d.ceil(digit + e).shift(e);
			}
		} else {
			d = getFractionField(l);
			d = (getSignum(l) > 0) ? d : d.negate();
			if(-digit < MIN_EXPONENT) {
				return l;
			} else /*if(-digit <= e)*/ {
				d = d.ceil(digit + MIN_NORMALIZED_EXPONENT).shift(
						e + 1);
			}
		}
		return toDecimal(d);
	}

	/**
	 * 
	 * @param l
	 * @param digit
	 * @return
	 */
	public static int floor(int l, int digit) {
		BCDDecimal d;
		int e = getExponent(l);

		if(isNaN(l) || isInfinite(l) || isZero(l)) {
			return l;
		} else if(-digit >= e + 1) {
			d = getFractionField(l);
			if(getSignum(l) > 0) {
				return ZERO_BY_INT;
			} else {
				d = BCDDecimal.parseBCD("-1.000000").shift(-digit);
				return toDecimal(d);
			}
		} else if(isNormalized(l)) {
			d = getFractionField(l);
			d = (getSignum(l) > 0) ? d : d.negate();
			if(-digit < e - FRCSIZE) {
				return l;
			} else /*if(-digit <= e)*/ {
				d = d.floor(digit + e).shift(e);
			}
		} else {
			d = getFractionField(l);
			d = (getSignum(l) > 0) ? d : d.negate();
			if(-digit < MIN_EXPONENT) {
				return l;
			} else /*if(-digit <= e)*/ {
				d = d.floor(digit + MIN_NORMALIZED_EXPONENT).shift(
						e + 1);
			}
		}
		return toDecimal(d);
	}

	/**
	 * 
	 * @param l
	 * @return
	 */
//	public static String toString(int l) {
//		StringBuilder b;
//		BCDDecimal d;
//		int e, i;
//		boolean bb = false;
//
//		if(isNaN(l)) {
//			return "NaN";
//		} else if(isInfinite(l)) {
//			return getSignum(l) > 0 ? "Infinity" : "-Infinity";
//		} else if(isZero(l)) {
//			return getSignum(l) > 0 ? "0.0" : "-0.0";
//		} else {
//			b = new StringBuilder();
//			e = getExponent(l);
//			d = getFractionField(l);
//			if(getSignum(l) < 0) {
//				b.append('-');
//			}
//
//			if(e > 4 || e < -4) {
//				b.append(d.toString());
//				b.append("e").append(e);
//			} else {
//				i = 6;
//				for(; i - e >= 0; i--) {
//					if(bb || d.getDigit(i - e) > 0) {
//						b.append((char)(d.getDigit(i - e) + '0'));
//						bb = true;
//					}
//				}
//				b.append('.');
//				if(i < 0) {
//					b.append('0');
//				} else {
//					for(; i >= 0; i--) {
//						b.append((char)(d.getDigit(i - e) + '0'));
//					}
//				}
//			}
//			return b.toString();
//		}
//	}
	public static String toString(int value) {
		if(isNaN(value)) {
			return "NaN";
		} else if(isInfinite(value)) {
			return getSignum(value) > 0 ? "Infinity" : "-Infinity";
		} else if(isZero(value)) {
			return getSignum(value) > 0 ? "0.0" : "-0.0";
		} else {
			BCDDecimal f = getFractionField(value);
			int e = getExponent(value);

			return f.shift(e).toString()
					.replaceFirst("0+((e(\\+|-)?[0-9]+)?)$", "$1");
		}
	}

	/**
	 * 
	 * @param value
	 * @param precision
	 * @return
	 */
	public static String toString(int value, int precision) {
		if(isNaN(value)) {
			return "NaN";
		} else if(isInfinite(value)) {
			return getSignum(value) > 0 ? "Infinity" : "-Infinity";
		} else if(isZero(value)) {
			return getSignum(value) > 0 ? "0.0" : "-0.0";
		} else {
			BCDDecimal f = getFractionField(value);
			int e = getExponent(value);

			return f.shift(e).toString(precision)
					.replaceFirst("0+((e(\\+|-)?[0-9]+)?)$", "$1");
		}
	}

	/**
	 * 
	 * @param l
	 * @return
	 */
	public static double doubleValue(int l) {
		if(isNaN(l)) {
			return Double.NaN;
		} else if(isInfinite(l)) {
			return getSignum(l) * Double.POSITIVE_INFINITY;
		} else if(isZero(l)) {
			return getSignum(l) * 0.0;
		} else {
			return Double.parseDouble(toString(l));
		}
	}

	/**
	 * 
	 * @param l
	 * @return
	 */
	public static float floatValue(int l) {
		if(isNaN(l)) {
			return Float.NaN;
		} else if(isInfinite(l)) {
			return getSignum(l) * Float.POSITIVE_INFINITY;
		} else if(isZero(l)) {
			return getSignum(l) * 0.0f;
		} else {
			return Float.parseFloat(toString(l));
		}
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static int compare(int a, int b) {
		BCDDecimal da, db;
		int ea, eb;

		if(isNaN(a)) {
			return isNaN(b) ? 0 : 1;
		} else if(isNaN(b)) {
			return -1;
		} else if(isInfinite(a)) {
			if(getSignum(a) > 0) {
				return (isInfinite(b) && getSignum(b) > 0) ? 0 : 1;
			} else {
				return (isInfinite(b) && getSignum(b) < 0) ? 0 : -1;
			}
		} else if(isInfinite(b)) {
			return (getSignum(b) > 0) ? -1 : 1;
		} else if(isNormalized(a)) {
			if(isNormalized(b)) {
				ea = getExponent(a);
				eb = getExponent(b);
				if(ea > eb) {
					return 1;
				} else if(ea < eb) {
					return -1;
				} else {
					da = getFractionField(a);
					db = getFractionField(b);
					return da.compareTo(db);
				}
			} else {
				return 1;
			}
		} else if(isNormalized(b)) {
			return -1;
		} else {
			da = getFractionField(a);
			db = getFractionField(b);
			return da.compareTo(db);
		}
	}

	/**
	 * 
	 * @param l
	 * @return
	 */
	public static int decimal64To32(long l) {
		return toDecimal(Decimal64.toBCDDecimal(l));
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public static long toDecimal(BigDecimal b) {
		BCDInteger bi = BCDInteger.valueOf(b.unscaledValue());
		BCDDecimal dd = new BCDDecimal(bi, 0);

		return toDecimal(dd.shift(-b.scale()));
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(Decimal32 o) {
		return compare(value, o.value);
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#intValue()
	 */
	@Override
	public int intValue() {
		return toBigDecimal(value).intValue();
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#longValue()
	 */
	@Override
	public long longValue() {
		return toBigDecimal(value).longValue();
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#floatValue()
	 */
	@Override
	public float floatValue() {
		return floatValue(value);
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#doubleValue()
	 */
	@Override
	public double doubleValue() {
		return doubleValue(value);
	}

}
