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
public final class Decimal64 extends Number
implements Comparable<Decimal64>, java.io.Serializable {

	//
	private static final long SGNMASK = 0x8000000000000000l;
	private static final long SIGMASK = 0x03fc000000000000l;
	private static final int  EXPBIT  = 50;
	private static final long CMBMASK = 0x7c00000000000000l;
	private static final int  CMBBIT  = 58;
	//private static final int  EMAX    = 0x2ff;
	private static final int  FRCSIZE = 15;
	private static final int  EXPSIZE = 8;
	private static final int  BIAS    = 383;
	private static final long TBIT    = 0x6000000000000000l;

	/**
	 * 
	 */
	public static int MAX_EXPONENT = 384;

	/**
	 * 
	 */
	public static int MIN_NORMALIZED_EXPONENT = -383;

	/**
	 * 
	 */
	public static int MIN_EXPONENT = -383 - 15;

	/**
	 * 
	 */
	public static long ZERO_BY_LONG = 0l;

	/**
	 * 
	 */
	public static long MINUS_ZERO_BY_LONG = 0x8000000000000000l;

	/**
	 * 
	 */
	public static long POSITIVE_INFINITY_BY_LONG = 0x7800000000000000l;

	/**
	 * 
	 */
	public static long NEGATIVE_INFINITY_BY_LONG = 0xf800000000000000l;

	/**
	 * 
	 */
	public static long NaN_BY_LONG = 0x7c00000000000000l;

	//
	private long value;

	/**
	 * 
	 * @param value
	 */
	public Decimal64(long value) {
		this.value = value;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getExponentField(long x) {
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
	public static int getCombinationField(long x) {
		return (int)((x & CMBMASK) >> CMBBIT);
	}

	//
	/*package*/ static void writedpd(int p, byte[] b, int o) {
		if((p & 0x008) == 0) {
			b[o + 0] = (byte)(p & 0x007);
			b[o + 1] = (byte)((p & 0x070) >> 4);
			b[o + 2] = (byte)((p & 0x380) >> 7);
		} else if((p & 0x00e) == 0x008) {
			b[o + 0] = (byte)((p & 0x001) + 8);
			b[o + 1] = (byte)((p & 0x070) >> 4);
			b[o + 2] = (byte)((p & 0x380) >> 7);
		} else if((p & 0x00e) == 0x00a) {
			b[o + 0] = (byte)(((p & 0x060) >> 4) | (p & 0x001));
			b[o + 1] = (byte)(((p & 0x010) >> 4) + 8);
			b[o + 2] = (byte)((p & 0x380) >> 7);
		} else if((p & 0x00e) == 0x00c) {
			b[o + 0] = (byte)(((p & 0x300) >> 7) | (p & 0x001));
			b[o + 1] = (byte)((p & 0x070) >> 4);
			b[o + 2] = (byte)(((p & 0x080) >> 7) + 8);
		} else if((p & 0x060) == 0x040) {
			b[o + 0] = (byte)((p & 0x001) + 8);
			b[o + 1] = (byte)(((p & 0x010) >> 4) + 8);
			b[o + 2] = (byte)((p & 0x380) >> 7);
		} else if((p & 0x060) == 0x020) {
			b[o + 0] = (byte)((p & 0x001) + 8);
			b[o + 1] = (byte)(((p & 0x300) >> 7) | ((p & 0x010) >> 4));
			b[o + 2] = (byte)(((p & 0x080) >> 7) + 8);
		} else if((p & 0x060) == 0x000) {
			b[o + 0] = (byte)(((p & 0x300) >> 7) | (p & 0x001));
			b[o + 1] = (byte)(((p & 0x010) >> 4) + 8);
			b[o + 2] = (byte)(((p & 0x080) >> 7) + 8);
		} else {
			b[o + 0] = (byte)((p & 0x001) + 8);
			b[o + 1] = (byte)(((p & 0x010) >> 4) + 8);
			b[o + 2] = (byte)(((p & 0x080) >> 7) + 8);
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static BCDDecimal getFractionField(long x) {
		byte[] b = new byte[FRCSIZE + 1];
		int c = (int)((x & CMBMASK) >> CMBBIT);

		writedpd((int)((x) & 0x3ff), b, 0);
		writedpd((int)((x >> 10) & 0x3ff), b, 3);
		writedpd((int)((x >> 20) & 0x3ff), b, 6);
		writedpd((int)((x >> 30) & 0x3ff), b, 9);
		writedpd((int)((x >> 40) & 0x3ff), b, 12);
		if((x & TBIT) == TBIT) {
			b[15] = (byte)(0x08 | (c & 1));
		} else {
			b[15] = (byte)(c & 7);
		}
		return new BCDDecimal(new BCDInteger(
				1, new BCDNaturalNumber(b)), FRCSIZE);
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
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isInfinite(long x) {
		return getCombinationField(x) == 0x1e;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static boolean isNaN(long x) {
		return getCombinationField(x) == 0x1f;
	}

	//
	/*package*/ static boolean isNormalized(long x) {
		return (x & CMBMASK) != 0;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getSignum(long x) {
		return isZero(x) ?  0 : ((x & SGNMASK) == 0) ? 1 : -1;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getSignumExact(long x) {
		return ((x & SGNMASK) == 0) ? 1 : -1;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static int getExponent(long x) {
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
	/*package*/ static long encodedec1(BCDDecimal d, int p) {
		int r = 0;

		if(d.getDigit(p + 2) < 8) {
			if(d.getDigit(p + 1) < 8) {
				if(d.getDigit(p) < 8) {
					r = r | d.getDigit(p);
					r = r | (d.getDigit(p + 1) << 4);
					r = r | (d.getDigit(p + 2) << 7);
				} else {
					r = r | d.getDigit(p);
					r = r | (d.getDigit(p + 1) << 4);
					r = r | (d.getDigit(p + 2) << 7);
				}
			} else {
				if(d.getDigit(p) < 8) {
					r = r | 0x0a;
					r = r | (d.getDigit(p) & 1);
					r = r | ((d.getDigit(p) & 6) << 4);
					r = r | ((d.getDigit(p + 1) & 1) << 4);
					r = r | (d.getDigit(p + 2) << 7);
				} else {
					r = r | 0x4e;
					r = r | (d.getDigit(p) & 1);
					r = r | ((d.getDigit(p + 1) & 1) << 4);
					r = r | (d.getDigit(p + 2) << 7);
				}
			}
		} else {
			if(d.getDigit(p + 1) < 8) {
				if(d.getDigit(p) < 8) {
					r = r | 0x0c;
					r = r | (d.getDigit(p) & 1);
					r = r | ((d.getDigit(p) & 6) << 7);
					r = r | (d.getDigit(p + 1) << 4);
					r = r | ((d.getDigit(p + 2) & 1) << 7);
				} else {
					r = r | 0x2e;
					r = r | (d.getDigit(p) & 1);
					r = r | ((d.getDigit(p + 1) & 1) << 4);
					r = r | ((d.getDigit(p + 1) & 6) << 7);
					r = r | ((d.getDigit(p + 2) & 1) << 7);
				}
			} else {
				if(d.getDigit(p) < 8) {
					r = r | 0x0e;
					r = r | (d.getDigit(p) & 1);
					r = r | ((d.getDigit(p) & 6) << 7);
					r = r | ((d.getDigit(p + 1) & 1) << 4);
					r = r | ((d.getDigit(p + 2) & 1) << 7);
				} else {
					r = r | 0x6e;
					r = r | (d.getDigit(p) & 1);
					r = r | ((d.getDigit(p + 1) & 1) << 4);
					r = r | ((d.getDigit(p + 2) & 1) << 7);
				}
			}
		}
		return (long)r;
	}

	//
	/*package*/ static long encodeDecimal(BCDDecimal d, int p) {
		long r = 0;

		r = r | encodedec1(d, p - 15);
		r = r | encodedec1(d, p - 12) << 10;
		r = r | encodedec1(d, p -  9) << 20;
		r = r | encodedec1(d, p -  6) << 30;
		r = r | encodedec1(d, p -  3) << 40;

		if(d.getDigit(p) < 8) {
			r = r | ((long)d.getDigit(p) << CMBBIT);
			r = r | ((long)((p + BIAS) & 0x0ff) << EXPBIT);
			r = r | ((long)((p + BIAS) & 0x300) <<
					(CMBBIT - EXPSIZE + 3));
		} else {
			r = r | ((long)(d.getDigit(p) & 1) << CMBBIT);
			r = r | TBIT;
			r = r | ((long)((p + BIAS) & 0x0ff) << EXPBIT);
			r = r | ((long)((p + BIAS) & 0x300) <<
					(CMBBIT - EXPSIZE + 1));
		}
		return r;
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static long toDecimal(BCDDecimal dd) {
		int  md = dd.getMostSignificantDigit();
		long r;

		if(dd.isZero()) {
			return ZERO_BY_LONG;
		} else if(md < MIN_EXPONENT) {
			r = ZERO_BY_LONG;
		} else if(md < MIN_NORMALIZED_EXPONENT) {
			dd = dd.round(-MIN_EXPONENT);
			r = encodeDecimal(dd, MIN_NORMALIZED_EXPONENT);
		} else if(md <= MAX_EXPONENT) {
			dd = dd.round(-(md - FRCSIZE));
			r = encodeDecimal(dd, md);
		} else {
			r = POSITIVE_INFINITY_BY_LONG;
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
	public static long parseDecimal(String s) {
		if(s == null) {
			throw new NullPointerException();
		} else if(s.equalsIgnoreCase("NaN")) {
			return NaN_BY_LONG;
		} else if(s.equalsIgnoreCase("Infinity")) {
			return POSITIVE_INFINITY_BY_LONG;
		} else if(s.equalsIgnoreCase("+Infinity")) {
			return POSITIVE_INFINITY_BY_LONG;
		} else if(s.equalsIgnoreCase("-Infinity")) {
			return NEGATIVE_INFINITY_BY_LONG;
		} else {
			return toDecimal(BCDDecimal.parseBCD(s));
		}
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static BCDDecimal toBCDDecimal(long d) {
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
	public static long toDecimal(double x) {
		if(Double.isNaN(x)) {
			return NaN_BY_LONG;
		} else if(x == Double.POSITIVE_INFINITY) {
			return POSITIVE_INFINITY_BY_LONG;
		} else if(x == Double.NEGATIVE_INFINITY) {
			return NEGATIVE_INFINITY_BY_LONG;
		} else if(Double.doubleToLongBits(x) == 0x0000000000000000l) {
			return ZERO_BY_LONG;
		} else if(Double.doubleToLongBits(x) == 0x8000000000000000l) {
			return MINUS_ZERO_BY_LONG;
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
	public static long add(long a, long b) {
		if(isNaN(a) || isNaN(b)) {
			return NaN_BY_LONG;
		} else if(isInfinite(a)) {
			if(isInfinite(b)) {
				return (getSignum(a) * getSignum(b) > 0) ?
						a : NaN_BY_LONG;
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
	public static long subtract(long a, long b) {
		if(isNaN(a) || isNaN(b)) {
			return NaN_BY_LONG;
		} else if(isInfinite(a)) {
			if(isInfinite(b)) {
				return (getSignum(a) * getSignum(b) < 0) ?
						a : NaN_BY_LONG;
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
	public static long multiply(long a, long b) {
		if(isNaN(a) || isNaN(b)) {
			return NaN_BY_LONG;
		} else if(isInfinite(a) || isInfinite(b)) {
			switch(getSignum(a) * getSignum(b)) {
			case 1:   return POSITIVE_INFINITY_BY_LONG;
			case 0:   return ZERO_BY_LONG;
			case -1:  return NEGATIVE_INFINITY_BY_LONG;
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
	public static long divide(long a, long b) {
		BCDDecimal da, db, dc;

		if(isNaN(a) || isNaN(b)) {
			return NaN_BY_LONG;
		} else if(isZero(b)) {
			if(isZero(a)) {
				return NaN_BY_LONG;
			} else if(getSignumExact(a) * getSignumExact(b) > 0) {
				return POSITIVE_INFINITY_BY_LONG;
			} else {
				return NEGATIVE_INFINITY_BY_LONG;
			}
		} else if(isInfinite(a)) {
			if(isInfinite(b)) {
				return NaN_BY_LONG;
			} else if(getSignumExact(a) * getSignumExact(b) > 0) {
				return ZERO_BY_LONG;
			} else {
				return MINUS_ZERO_BY_LONG;
			}
		} else if(isInfinite(b)) {
			if(getSignumExact(a) * getSignumExact(b) > 0) {
				return ZERO_BY_LONG;
			} else {
				return MINUS_ZERO_BY_LONG;
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
	public static long negate(long l) {
		return l ^ SGNMASK;
	}

	/**
	 * 
	 * @param l
	 * @return
	 */
	public static long toDecimal(long l) {
		return toDecimal(BCDDecimal.valueOf(l, 0));
	}

	/**
	 * 
	 * @param l
	 * @return
	 */
	public static BigDecimal toBigDecimal(long l) {
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
	 * @return
	 */
	public static int toInt(long l) {
		return toBigDecimal(l).intValue();
	}

	/**
	 * 
	 * @param l
	 * @return
	 */
	public static long toLong(long l) {
		return toBigDecimal(l).longValue();
	}

	/**
	 * 
	 * @param l
	 * @param digit
	 * @return
	 */
	public static long round(long l, int digit) {
		BCDDecimal d;
		int e = getExponent(l);

		if(isNaN(l) || isInfinite(l) || isZero(l)) {
			return l;
		} else if(-digit > e + 1) {
			return ZERO_BY_LONG;
		} else if(-digit == e + 1) {
			d = getFractionField(l);
			if(d.getDigit(0) < 5) {
				return ZERO_BY_LONG;
			} else {
				d = BCDDecimal.parseBCD("1.000000000000000").shift(
						-digit);
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
	public static long ceil(long l, int digit) {
		BCDDecimal d;
		int e = getExponent(l);

		if(isNaN(l) || isInfinite(l) || isZero(l)) {
			return l;
		} else if(-digit >= e + 1) {
			d = getFractionField(l);
			if(getSignum(l) > 0) {
				d = BCDDecimal.parseBCD("1.000000000000000").shift(
						-digit);
				return toDecimal(d);
			} else {
				return ZERO_BY_LONG;
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
	public static long floor(long l, int digit) {
		BCDDecimal d;
		int e = getExponent(l);

		if(isNaN(l) || isInfinite(l) || isZero(l)) {
			return l;
		} else if(-digit >= e + 1) {
			d = getFractionField(l);
			if(getSignum(l) > 0) {
				return ZERO_BY_LONG;
			} else {
				d = BCDDecimal.parseBCD("-1.000000000000000").shift(
						-digit);
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
//	public static String toString(long l) {
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
//			if(e > 10 || e < -4) {
//				b.append(d.toString());
//				b.append("e").append(e);
//			} else {
//				i = 15;
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
	public static String toString(long value) {
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
	public static String toString(long value, int precision) {
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
	public static double doubleValue(long l) {
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
	public static float floatValue(long l) {
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
	public static int compare(long a, long b) {
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
	public static long decimal32To64(int l) {
		return toDecimal(Decimal32.toBCDDecimal(l));
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
	public int compareTo(Decimal64 o) {
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
