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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/30
 */
public class BCDInteger extends Number
implements Comparable<BCDInteger> {

	/**
	 * 
	 */
	public static final BCDInteger ZERO =
		new BCDInteger(1, BCDNaturalNumber.ZERO);

	/**
	 * 
	 */
	public static final BCDInteger ONE =
		new BCDInteger(1, BCDNaturalNumber.ONE);

	/**
	 * 
	 */
	public static final BCDInteger MINUS_ONE =
		new BCDInteger(-1, BCDNaturalNumber.ONE);

	//
	private byte signum;
	private BCDNaturalNumber value;

	/**
	 * 
	 * @param signum
	 * @param value
	 */
	public BCDInteger(int signum, BCDNaturalNumber value) {
		this.signum = (byte)(signum > 0 ? 1 : -1);
		this.value  = value;
	}

	/**
	 * 
	 * @param l
	 * @return
	 */
	public static BCDInteger valueOf(long l) {
		if(l >= 0) {
			return new BCDInteger(1, BCDNaturalNumber.valueOf(l));
		} else {
			return new BCDInteger(-1, BCDNaturalNumber.valueOf(-l));
		}
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static BCDInteger parseBCD(String s) {
		int sgn = 1;

		if(s.length() < 1) {
			throw new NumberFormatException();
		} else {
			switch(s.charAt(0)) {
			case '0': case '1': case '2': case '3': case '4':
			case '5': case '6': case '7': case '8': case '9':
				return new BCDInteger(1, BCDNaturalNumber.parseBCD(s));
			case '-':
				sgn = -1;
				/* next */
			case '+':
				if(s.length() < 2) {
					throw new NumberFormatException();
				}
				return new BCDInteger(sgn,
						BCDNaturalNumber.parseBCD(s.substring(1)));
			default:
				throw new NumberFormatException();
			}
		}
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public BCDInteger add(BCDInteger b) {
		if(signum == b.signum) {
			return new BCDInteger(signum, value.add(b.value));
		} else {
			switch(value.compareTo(b.value)) {
			case 1:
				return new BCDInteger(signum, value.subtract(b.value));
			case -1:
				return new BCDInteger(-signum, b.value.subtract(value));
			case 0:
				return ZERO;
			default:
				throw new RuntimeException();
			}
		}
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public BCDInteger subtract(BCDInteger b) {
		if(signum != b.signum) {
			return new BCDInteger(signum, value.add(b.value));
		} else {
			switch(value.compareTo(b.value)) {
			case 1:
				return new BCDInteger(
						signum, value.subtract(b.value));
			case -1:
				return new BCDInteger(
						-signum, b.value.subtract(value));
			case 0:
				return ZERO;
			default:
				throw new RuntimeException();
			}
		}
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public BCDInteger multiply(BCDInteger b) {
		if(isZero() || b.isZero()) {
			return ZERO;
		} else {
			return new BCDInteger(
					signum * b.signum, value.multiply(b.value));
		}
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public BCDInteger divide(BCDInteger b) {
		if(b.isZero()) {
			throw new ArithmeticException();
		} else if(isZero()) {
			return ZERO;
		} else {
			return new BCDInteger(
					signum * b.signum, value.divide(b.value));
		}
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public BCDInteger remainder(BCDInteger b) {
		if(b.isZero()) {
			throw new ArithmeticException();
		} else if(isZero()) {
			return ZERO;
		} else {
			return new BCDInteger(
					signum, value.remainder(b.value));
		}
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public BCDInteger[] divideAndRemainder(BCDInteger b) {
		BCDNaturalNumber n[] = value.divideAndRemainder(b.value);

		if(b.isZero()) {
			throw new ArithmeticException();
		} else {
			return new BCDInteger[] {
					new BCDInteger(signum * b.signum, n[0]),
					new BCDInteger(signum, n[1])
			};
		}
	}

	/**
	 * 
	 * @return
	 */
	public BCDInteger negate() {
		return new BCDInteger(-signum, value);
	}

	/**
	 * 
	 * @return
	 */
	public boolean isZero() {
		return value.isZero();
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#intValue()
	 */
	@Override
	public int intValue() {
		return signum * value.intValue();
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#longValue()
	 */
	@Override
	public long longValue() {
		return signum * value.longValue();
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#floatValue()
	 */
	@Override
	public float floatValue() {
		return signum * value.floatValue();
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#doubleValue()
	 */
	@Override
	public double doubleValue() {
		return signum * value.doubleValue();
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(BCDInteger o) {
		if(signum > o.signum) {
			return 1;
		} else if(signum < o.signum) {
			return -1;
		} else {
			return signum * value.compareTo(o.value);
		}
	}

	/**
	 * 
	 * @param b
	 * @param offset
	 */
	public byte[] toEbcdicZoneBCD(byte[] b, int offset) {
		value.toEbcdicZoneBCD(b, offset);
		if(signum < 0) {
			b[value.digits() + offset - 1] |= 0x10;
		}
		return b;
	}

	/**
	 * 
	 * @return
	 */
	public byte[] toEbcdicZoneBCD() {
		byte[] b = value.toEbcdicZoneBCD();

		if(signum < 0) {
			b[value.digits() - 1] |= 0x10;
		}
		return b;
	}

	/**
	 * 
	 * @param b
	 * @param offset
	 * @return
	 */
	public byte[] toPackedBCD(byte[] b, int offset) {
		value.toPackedBCD(b, offset);
		if(signum < 0) {
			b[value.digits() / 2 + offset] |= 0x01;
		}
		return b;
	}

	/**
	 * 
	 * @return
	 */
	public byte[] toPackedBCD() {
		byte[] b = value.toPackedBCD();

		if(signum < 0) {
			b[value.digits() / 2] |= 0x01;
		}
		return b;
	}

	/**
	 * 
	 * @return
	 */
	public BCDInteger succ() {
		if(equals(MINUS_ONE)) {
			return ZERO;
		} else if(isZero()) {
			return ONE;
		} else if(signum > 0) {
			return new BCDInteger(signum, value.succ());
		} else {
			return new BCDInteger(signum, value.prev());
		}
	}

	/**
	 * 
	 * @return
	 */
	public BCDInteger prev() {
		if(isZero()) {
			return MINUS_ONE;
		} else if(signum > 0) {
			return new BCDInteger(signum, value.prev());
		} else {
			return new BCDInteger(signum, value.succ());
		}
	}

	/**
	 * 
	 * @return
	 */
	public int digits() {
		return value.digits();
	}

	/**
	 * 
	 * @param digit
	 * @return
	 */
	public BCDInteger shift(int digit) {
		BCDNaturalNumber n = value.shift(digit);

		return n.isZero() ? ZERO : new BCDInteger(signum, n);
	}

	/**
	 * 
	 * @param digit
	 * @return
	 */
	public int getDigit(int digit) {
		return value.getDigit(digit);
	}

	/**
	 * 
	 * @return
	 */
	public int signum() {
		return isZero() ? 0 : signum;
	}

	/**
	 * 
	 * @return
	 */
	public BCDInteger abs() {
		return (signum > 0 || isZero()) ?
				this : new BCDInteger(1, value);
	}

	/**
	 * 
	 * @return
	 */
	public BigInteger toBigInteger() {
		BigInteger r = value.toBigInteger();

		return (signum > 0) ? r : r.negate();
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public static BCDInteger valueOf(BigInteger b) {
		return new BCDInteger(b.signum(),
				BCDNaturalNumber.valueOf(b.abs()));
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
		if(o instanceof BCDInteger) {
			BCDInteger b = (BCDInteger)o;

			return ((isZero() && b.isZero()) ||
					(signum == b.signum && value.equals(b.value)));
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return (signum < 0) ?
				"-" + value.toString() : value.toString();
	}

}
