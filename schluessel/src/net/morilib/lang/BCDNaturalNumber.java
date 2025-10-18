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

import java.math.BigInteger;
import java.util.Arrays;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/30
 */
public final class BCDNaturalNumber extends Number
implements Comparable<BCDNaturalNumber> {

	/**
	 * 
	 */
	public static final BCDNaturalNumber ZERO =
		new BCDNaturalNumber(new byte[0], false);

	/**
	 * 
	 */
	public static final BCDNaturalNumber ONE =
		new BCDNaturalNumber(new byte[] { (byte)1 }, false);

	//
	private byte[] digits;

	//
	private BCDNaturalNumber(byte[] d, boolean f) {
		this.digits = d;
	}

	/**
	 * 
	 * @param digits
	 */
	public BCDNaturalNumber(byte[] digits) {
		int s;

		for(s = digits.length - 1; s >= 0 && digits[s] == 0; s--);
		this.digits = new byte[s + 1];
		System.arraycopy(digits, 0, this.digits,
				0, this.digits.length);
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public BCDNaturalNumber add(BCDNaturalNumber b) {
		byte[] r;
		int carry = 0, t;

		r = new byte[Math.max(digits.length, b.digits.length) + 1];
		for(int i = 0; i < r.length - 1; i++) {
			if(i >= digits.length) {
				t = b.digits[i] + carry;
			} else if(i >= b.digits.length) {
				t = digits[i] + carry;
			} else {
				t = digits[i] + b.digits[i] + carry;
			}
			carry = t / 10;
			r[i] = (byte)(t % 10);
		}
		r[r.length - 1] = (byte)carry;
		return new BCDNaturalNumber(r);
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public BCDNaturalNumber subtract(BCDNaturalNumber b) {
		int cmp = compareTo(b);
		byte[] r;
		int carry = 0, t;

		if(cmp < 0) {
			throw new ArithmeticException();
		} else if(cmp == 0) {
			return ZERO;
		} else {
			r = new byte[digits.length];
			for(int i = 0; i < digits.length; i++) {
				if(i >= b.digits.length) {
					t = digits[i] - carry;
				} else {
					t = digits[i] - b.digits[i] - carry;
				}
				carry = -((t - 9) / 10);
				r[i] = (byte)((t + 10) % 10);
			}
			return new BCDNaturalNumber(r);
		}
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public BCDNaturalNumber multiply(BCDNaturalNumber b) {
		byte[] r;
		int carry, t;

		if(isZero() || b.isZero()) {
			return ZERO;
		} else {
			r = new byte[digits.length + b.digits.length];
			Arrays.fill(r, (byte)0);
			for(int i = 0; i < b.digits.length; i++) {
				carry = 0;
				for(int j = 0; j < digits.length; j++) {
					t = r[i + j] + digits[j] * b.digits[i] + carry;
					carry = t / 10;
					r[i + j] = (byte)(t % 10);
				}
				r[i + digits.length] = (byte)carry;
			}
			return new BCDNaturalNumber(r);
		}
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public BCDNaturalNumber multiply(int b) {
		byte[] r;
		int d = 1;
		long carry = 0, t;

		if(b < 0) {
			throw new ArithmeticException();
		} else if(b == 0 || isZero()) {
			return ZERO;
		} else {
			for(int z = b; z >= 10; z /= 10, d++);
			r = new byte[digits.length + d];
			Arrays.fill(r, (byte)0);
			for(int j = 0; j < r.length; j++) {
				t = ((j < digits.length) ? digits[j] * b : 0) + carry;
				carry = t / 10;
				r[j] = (byte)(t % 10);
			}
			return new BCDNaturalNumber(r);
		}
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public BCDNaturalNumber[] divideAndRemainder(BCDNaturalNumber b) {
		byte[] r, w, q;
		int carry, t, s;
		int dl = digits.length - b.digits.length, dd;

		if(b.isZero()) {
			throw new ArithmeticException();
		} else if(isZero()) {
			return new BCDNaturalNumber[] { this, this };
		} else if(compareTo(b) < 0) {
			return new BCDNaturalNumber[] { ZERO, this };
		} else {
			r = new byte[digits.length + 1];
			w = new byte[b.digits.length + 1];
			q = new byte[dl + 1];
			System.arraycopy(digits, 0, r, 0, digits.length);
			s = r[digits.length - 1] + 1;
			for(int i = dl; i >= 0; i--) {
				dd = i + b.digits.length - 1;
				s  = s / b.digits[b.digits.length - 1];

				outer: for(; s >= 0; s--) {
					// multiply b by s
					carry = 0;
					for(int j = 0; j < b.digits.length; j++) {
						t = b.digits[j] * s + carry;
						carry = t / 10;
						w[j] = (byte)(t % 10);
					}
					w[w.length - 1] = (byte)carry;
	
					// compare r to w
					for(int j = w.length - 1; j >= 0; j--) {
						if(j + i >= r.length) {
							if(w[j] == 0) {
								continue;
							} else {
								continue outer;
							}
						} else if(w[j] > r[i + j]) {
							continue outer;
						} else if(w[j] < r[i + j]) {
							break outer;
						}
					}
					break;
				}

				if(s > 0) {
					carry = 0;
					for(int j = 0; j < w.length; j++) {
						t = r[j + i] - w[j] - carry;
						carry = -((t - 9) / 10);
						r[j + i] = (byte)((t + 10) % 10);
					}
					q[i] = (byte)s;
				}

				if(i > 0) {
					s = (r[dd] > 0) ? (r[dd] + 1) * 10 : r[dd - 1];
				}
			}

			return new BCDNaturalNumber[] {
					new BCDNaturalNumber(q), new BCDNaturalNumber(r)
			};
		}
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public BCDNaturalNumber divide(BCDNaturalNumber b) {
		return divideAndRemainder(b)[0];
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public BCDNaturalNumber remainder(BCDNaturalNumber b) {
		return divideAndRemainder(b)[1];
	}

	/**
	 * 
	 * @return
	 */
	public boolean isZero() {
		return digits.length == 0;
	}

	//
	private boolean isAllNine() {
		for(int i = 0; i < digits.length; i++) {
			if(digits[i] < 9) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 
	 * @return
	 */
	public BCDNaturalNumber succ() {
		byte[] r;

		if(isZero()) {
			return ONE;
		} else if(isAllNine()) {
			r = new byte[digits.length + 1];
			r[r.length - 1] = 1;
			return new BCDNaturalNumber(r, false);
		} else {
			r = new byte[digits.length];
			System.arraycopy(digits, 0, r, 0, r.length);
			for(int i = 0; i < r.length; i++) {
				if(r[i] < 9) {
					r[i]++;
					return new BCDNaturalNumber(r, false);
				} else {
					r[i] = 0;
				}
			}
			throw new AssertionError();
		}
	}

	//
	private boolean isAllZero() {
		int i = 0;
		for(; i < digits.length - 1; i++) {
			if(digits[i] > 0) {
				return false;
			}
		}
		return digits.length > 1 && digits[i] == 1;
	}

	/**
	 * 
	 * @return
	 */
	public BCDNaturalNumber prev() {
		byte[] r;

		if(isZero()) {
			throw new ArithmeticException();
		} else if(equals(ONE)) {
			return ZERO;
		} else if(isAllZero()) {
			r = new byte[digits.length - 1];
			Arrays.fill(r, (byte)9);
			return new BCDNaturalNumber(r, false);
		} else {
			r = new byte[digits.length];
			System.arraycopy(digits, 0, r, 0, r.length);
			for(int i = 0; i < r.length; i++) {
				if(r[i] > 0) {
					r[i]--;
					return new BCDNaturalNumber(r, false);
				} else {
					r[i] = 9;
				}
			}
			throw new AssertionError();
		}
	}

	/**
	 * 
	 * @return
	 */
	public int digits() {
		return digits.length;
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(BCDNaturalNumber o) {
		if(digits.length > o.digits.length) {
			return 1;
		} else if(digits.length < o.digits.length) {
			return -1;
		} else {
			for(int j = digits.length - 1; j >= 0; j--) {
				if(digits[j] > o.digits[j]) {
					return 1;
				} else if(digits[j] < o.digits[j]) {
					return -1;
				}
			}
			return 0;
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
		long l = 0, d = 1;

		for(int i = 0; i < digits.length; i++) {
			l += digits[i] * d;
			d *= 10;
		}
		return l;
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
		double l = 0, d = 1;

		for(int i = 0; i < digits.length; i++) {
			l += digits[i] * d;
			d *= 10;
		}
		return l;
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static BCDNaturalNumber parseBCD(String s) {
		byte[] r;
		int c, i = 0;

		for(; i < s.length() && s.charAt(i) == '0'; i++);
		r = new byte[s.length() - i];

		if(i == s.length()) {
			return ZERO;
		} else {
			for(; i < s.length(); i++) {
				c = s.charAt(i);
				if(c < '0' || c > '9') {
					throw new NumberFormatException();
				}
				r[s.length() - i - 1] = (byte)(c - '0');
			}
			return new BCDNaturalNumber(r, false);
		}
	}

	/**
	 * 
	 * @param b
	 * @param offset
	 */
	public byte[] toEbcdicZoneBCD(byte[] b, int offset) {
		if(isZero()) {
			b[offset] = (byte)0xc0;
		} else {
			for(int i = 0; i < digits.length; i++) {
				b[i + offset] = digits[digits.length - i - 1];
				if(i < digits.length - 1) {
					b[i + offset] |= 0xf0;
				} else {
					b[i + offset] |= 0xc0;
				}
			}
		}
		return b;
	}

	/**
	 * 
	 * @return
	 */
	public byte[] toEbcdicZoneBCD() {
		if(isZero()) {
			return toEbcdicZoneBCD(new byte[digits.length + 1], 0);
		} else {
			return toEbcdicZoneBCD(new byte[digits.length], 0);
		}
	}

	/**
	 * 
	 * @param b
	 * @param offset
	 * @return
	 */
	public byte[] toPackedBCD(byte[] b, int offset) {
		int o = offset, l = digits.length - 1;

		if(isZero()) {
			b[o] = 0x0c;
			return b;
		} else if(digits.length % 2 == 0) {
			b[o++] = digits[l--];
		}

		for(; l > 0; l -= 2) {
			b[o++] = (byte)((digits[l] << 4) | digits[l - 1]);
		}
		b[o] = (byte)((digits[0] << 4) | 0x0c);
		return b;
	}

	/**
	 * 
	 * @return
	 */
	public byte[] toPackedBCD() {
		return toPackedBCD(new byte[digits.length / 2 + 1], 0);
	}

	/**
	 * 
	 * @param digit
	 * @return
	 */
	public BCDNaturalNumber shift(int digit) {
		byte[] r;

		if(isZero()) {
			return ZERO;
		} else if(digit == 0) {
			return this;
		} else if(digit > 0) {
			r = new byte[digits.length + digit];
			Arrays.fill(r, (byte)0);
			System.arraycopy(digits, 0, r, digit, digits.length);
		} else if(-digit < digits.length) {
			r = new byte[digits.length + digit];
			System.arraycopy(digits, -digit, r, 0, r.length);
		} else {
			return ZERO;
		}
		return new BCDNaturalNumber(r, false);
	}

	/**
	 * 
	 * @param digit
	 * @return
	 */
	public int getDigit(int digit) {
		return (digit < 0 || digit >= digits.length) ?
				0 : digits[digit];
	}

	/**
	 * 
	 * @param i
	 * @return
	 */
	public static BCDNaturalNumber valueOf(long i) {
		byte[] r;
		int d = 0, j = 0;

		if(i < 0) {
			throw new IllegalArgumentException();
		} else if(i == 0) {
			return ZERO;
		} else {
			for(long z = i; z > 0; z /= 10, d++);
			r = new byte[d];
			for(long z = i; z > 0; z /= 10, j++) {
				r[j] = (byte)(z % 10);
			}
			return new BCDNaturalNumber(r, false);
		}
	}

	/**
	 * 
	 * @return
	 */
	public BigInteger toBigInteger() {
		BigInteger r = BigInteger.ZERO;

		for(int i = 0; i < digits.length; i++) {
			r = r.multiply(BigInteger.TEN).add(BigInteger.valueOf(
					digits[i]));
		}
		return r;
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public static BCDNaturalNumber valueOf(BigInteger b) {
		int i = 0, j = 0;
		byte[] r;

		if(b.signum() < 0) {
			throw new IllegalArgumentException();
		} else if(b.signum() == 0) {
			return ZERO;
		} else {
			for(; b.signum() == 0; b = b.divide(BigInteger.TEN), i++);
			r = new byte[i];
			for(; b.signum() == 0; b = b.divide(BigInteger.TEN), j++) {
				r[j] = b.remainder(BigInteger.TEN).byteValue();
			}
			return new BCDNaturalNumber(r);
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return intValue();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof BCDNaturalNumber) {
			return compareTo((BCDNaturalNumber)o) == 0;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder b = new StringBuilder();

		if(isZero()) {
			return "0";
		} else {
			for(int i = digits.length - 1; i >= 0; i--) {
				b.append((char)('0' + digits[i]));
			}
			return b.toString();
		}
	}

}
