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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/30
 */
public class BCDDecimal extends Number
implements Comparable<BCDDecimal> {

	/**
	 * 
	 */
	public static final BCDDecimal ZERO =
		new BCDDecimal(BCDInteger.ZERO, 0);

	//
	private int point;
	private BCDInteger value;

	/**
	 * 
	 * @param value
	 * @param point
	 */
	public BCDDecimal(BCDInteger value, int point) {
		this.point = point;
		this.value = value;
	}

	/**
	 * 
	 * @param value
	 * @param point
	 * @return
	 */
	public static BCDDecimal valueOf(long value, int point) {
		return new BCDDecimal(BCDInteger.valueOf(value), point);
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static BCDDecimal parseBCD(String s) {
		StringBuilder b = new StringBuilder();
		int c, pp = -1, pd, ed = 0, esgn = 0, pm = -1;
		boolean folf = true;
		BCDInteger bi;

		for(int i = 0; i < s.length(); i++) {
			c = s.charAt(i);
			if(folf && c == '+') {
				folf = false;
			} else if(folf && c == '-') {
				if(esgn == 0) {
					b.append('-');
				} else {
					esgn = -1;
				}
				folf = false;
			} else if(c >= '0' && c <= '9') {
				if(esgn == 0) {
					b.append((char)c);
				} else {
					ed = ed * 10 + (c - '0');
				}
			} else if(pp < 0 && esgn == 0 && c == '.') {
				pp = i;
			} else if(esgn == 0 && (c == 'e' || c == 'E')) {
				esgn = 1;
				pm = i;
				folf = true;
			} else {
				throw new NumberFormatException();
			}
		}

		pm = (pm < 0) ? s.length() : pm;
		if(pp < 0) {
			pd = 0;
		} else if(pp + 1 == s.length()) {  // case of #.
			throw new NumberFormatException();
		} else {
			pd = pm - (pp + 1);
		}

		bi = BCDInteger.parseBCD(b.toString());
		if(bi.isZero()) {
			return ZERO;
		} else if(esgn == 0) {
			return new BCDDecimal(bi, pd);
		} else {
			return new BCDDecimal(bi, pd - esgn * ed);
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static BCDDecimal valueOf(double x) {
		if(Double.isNaN(x) || Double.isInfinite(x)) {
			throw new IllegalArgumentException();
		}
		return parseBCD(Double.toString(x));
	}

	/**
	 * 
	 * @return
	 */
	public int getPoint() {
		return point;
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public BCDDecimal add(BCDDecimal b) {
		BCDInteger va = value, vb = b.value;
		int vp = point;

		if(point < b.point) {
			va = va.shift(b.point - point);
			vp = b.point;
		} else if(point > b.point) {
			vb = vb.shift(point - b.point);
		}
		return new BCDDecimal(va.add(vb), vp);
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public BCDDecimal subtract(BCDDecimal b) {
		BCDInteger va = value, vb = b.value;
		int vp = point;

		if(point < b.point) {
			va = va.shift(b.point - point);
			vp = b.point;
		} else if(point > b.point) {
			vb = vb.shift(point - b.point);
		}
		return new BCDDecimal(va.subtract(vb), vp);
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public BCDDecimal multiply(BCDDecimal b) {
		return new BCDDecimal(
				value.multiply(b.value), point + b.point);
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public BCDDecimal divide(BCDDecimal b) {
		return new BCDDecimal(
				value.divide(b.value), point - b.point);
	}

	/**
	 * 
	 * @return
	 */
	public BCDDecimal negate() {
		return new BCDDecimal(value.negate(), point);
	}

	/**
	 * 
	 * @param digit
	 * @return
	 */
	public BCDDecimal round(int digit) {
		int d = value.getDigit(point - digit - 1);
		BCDInteger vr = value.shift(digit - point);

		return new BCDDecimal(d < 5 ? vr : vr.succ(), digit);
	}

	/**
	 * 
	 * @param digit
	 * @return
	 */
	public BCDDecimal ceil(int digit) {
		BCDInteger vr = value.shift(digit - point);

		if(value.signum() > 0) {
			return new BCDDecimal(vr.succ(), digit);
		} else {
			return new BCDDecimal(vr, digit);
		}
	}

	/**
	 * 
	 * @param digit
	 * @return
	 */
	public BCDDecimal floor(int digit) {
		BCDInteger vr = value.shift(digit - point);

		if(value.signum() > 0) {
			return new BCDDecimal(vr, digit);
		} else {
			return new BCDDecimal(vr.prev(), digit);
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#intValue()
	 */
	@Override
	public int intValue() {
		return (int)longValue();
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#longValue()
	 */
	@Override
	public long longValue() {
		if(point > 0) {
			return round(0).value.longValue();
		} else {
			return value.shift(-point).longValue();
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#floatValue()
	 */
	@Override
	public float floatValue() {
		return (float)doubleValue();
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#doubleValue()
	 */
	@Override
	public double doubleValue() {
		return Double.parseDouble(toString());
	}

	/**
	 * 
	 * @return
	 */
	public int getMostSignificantDigit() {
		return value.digits() - point - 1;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isZero() {
		return value.isZero();
	}

	/**
	 * 
	 * @param digit
	 * @return
	 */
	public int getDigit(int digit) {
		return value.getDigit(digit + point);
	}

	/**
	 * 
	 * @return
	 */
	public int signum() {
		return value.signum();
	}

	/**
	 * 
	 * @param digit
	 * @return
	 */
	public BCDDecimal shift(int digit) {
		return new BCDDecimal(value, point - digit);
	}

	/**
	 * 
	 * @param digit
	 * @return
	 */
	public BCDDecimal shiftExtend(int digit) {
		return new BCDDecimal(value.shift(digit), point + digit);
	}

	/**
	 * 
	 * @return
	 */
	public int precision() {
		return value.digits();
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(BCDDecimal b) {
		BCDInteger va = value, vb = b.value;

		if(point < b.point) {
			va = va.shift(b.point - point);
		} else if(point > b.point) {
			vb = vb.shift(point - b.point);
		}
		return va.compareTo(vb);
	}

	/**
	 * 
	 * @return
	 */
	public BigDecimal toBigDecimal() {
		return new BigDecimal(value.toBigInteger(), point);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		if(isZero()) {
			return 0;
		} else {
			r = Hashes.A * (r + value.intValue());
			r = Hashes.A * (r + point);
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof BCDDecimal) {
			return compareTo((BCDDecimal)o) == 0;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder b = new StringBuilder();
		int l0 = value.digits() - point;

		if(point < 0 || point > 10) {
			if(value.signum() < 0) {
				b.append("-");
			}

			b.append((char)(value.getDigit(value.digits() - 1) + '0'));
			b.append(".");
			for(int i = value.digits() - 2; i >= 0; i--) {
				b.append((char)(value.getDigit(i) + '0'));
			}
			b.append("e").append(l0 - 1);
		} else if(point == 0) {
			b.append(value.toString());
		} else if(l0 <= 0) {
			if(value.signum() < 0) {
				b.append("-");
			}

			b.append("0.");
			for(int i = 0; i < -l0; i++) {
				b.append("0");
			}
			b.append(value.abs().toString());
		} else {
			if(value.signum() < 0) {
				b.append("-");
			}

			for(int i = value.digits(); i > point; i--) {
				b.append((char)(value.getDigit(i - 1) + '0'));
			}
			b.append(".");
			for(int i = point; i > 0; i--) {
				b.append((char)(value.getDigit(i - 1) + '0'));
			}
		}
		return b.toString();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString(int precision) {
		return shiftExtend(precision - precision()).toString();
	}

}
