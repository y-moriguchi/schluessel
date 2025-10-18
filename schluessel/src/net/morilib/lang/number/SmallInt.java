/*
 * Copyright 2009 Yuichiro Moriguchi
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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

import net.morilib.lang.algebra.QuotientAndRemainder;
import net.morilib.util.IntMath;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2010
 */
public final class SmallInt extends Integer2
implements java.io.Serializable {

	//
	private static final long serialVersionUID = -4912159628063856328L;

	//
	private int value;

	//
	/*package*/ SmallInt(int value) {
		this.value = value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#add(java.lang.Object)
	 */
	public Integer2 add(Integer2 x) {
		if(x instanceof SmallInt) {
			long vr1 = value;
			long vr2 = x.toLong();

			return Integer2.valueOf(vr1 + vr2);
		} else {
			BigInteger vb = BigInteger.valueOf(value);

			return Integer2.valueOf(vb.add(x.toBigInteger()));
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Dividable#divide(java.lang.Object)
	 */
	public Integer2 divide(Integer2 x) {
		if(x instanceof SmallInt) {
			return Integer2.valueOf(value / x.toInt());
		} else {
			BigInteger vb = BigInteger.valueOf(value);

			return Integer2.valueOf(vb.divide(x.toBigInteger()));
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#multiply(java.lang.Object)
	 */
	public Integer2 multiply(Integer2 x) {
		if(x instanceof SmallInt) {
			long vr1 = value;
			long vr2 = x.toLong();

			return Integer2.valueOf(vr1 * vr2);
		} else {
			BigInteger vb = BigInteger.valueOf(value);

			return Integer2.valueOf(vb.multiply(x.toBigInteger()));
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Subtractable#subtract(java.lang.Object)
	 */
	public Integer2 subtract(Integer2 x) {
		if(x instanceof SmallInt) {
			long vr1 = value;
			long vr2 = x.toLong();

			return Integer2.valueOf(vr1 - vr2);
		} else {
			BigInteger vb = BigInteger.valueOf(value);

			return Integer2.valueOf(vb.subtract(x.toBigInteger()));
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Negatable#negate()
	 */
	public Integer2 negate() {
		return new SmallInt(-value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.AlgebricInteger#signum()
	 */
	public int signum() {
		return IntMath.signum(value);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public boolean isEqualTo(Integer2 x) {
		if(x instanceof SmallInt) {
			return value == x.toInt();
		} else {
			BigInteger vb = BigInteger.valueOf(value);

			return vb.equals(x.toBigInteger());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.Integer2#toBigInteger()
	 */
	public BigInteger toBigInteger() {
		return BigInteger.valueOf(value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.Integer2#toInt()
	 */
	public int toInt() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.Integer2#toLong()
	 */
	public long toLong() {
		return value;
	}

	/**
	 * 
	 * @return
	 */
	public BigDecimal getBigDecimal() {
		return new BigDecimal(value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Remainderable#remainder(java.lang.Object)
	 */
	public Integer2 remainder(Integer2 x) {
		if(x instanceof SmallInt) {
			return Integer2.valueOf(value % x.toInt());
		} else {
			BigInteger vb = BigInteger.valueOf(value);

			return Integer2.valueOf(vb.remainder(x.toBigInteger()));
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Remainderable#divideAndRemainder(java.lang.Object)
	 */
	public QuotientAndRemainder<Integer2> divideAndRemainder(
			Integer2 x) {
		if(x instanceof SmallInt) {
			return new QuotientAndRemainder<Integer2>(
					Integer2.valueOf(value / x.toInt()),
					Integer2.valueOf(value % x.toInt()));
		} else {
			BigInteger vb = BigInteger.valueOf(value);
			BigInteger[] dr = vb.divideAndRemainder(x.toBigInteger());

			return new QuotientAndRemainder<Integer2>(
					Integer2.valueOf(dr[0]),
					Integer2.valueOf(dr[1]));
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.RingElement#isZero()
	 */
	public boolean isZero() {
		return value == 0;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public Integer2 multiply(int n) {
		return Integer2.valueOf(value * n);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#power(int)
	 */
	public Integer2 power(int n) {
		BigInteger vb = BigInteger.valueOf(value);

		return Integer2.valueOf(vb.pow(n));
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(Integer2 o) {
		if(o instanceof SmallInt) {
			int z = o.toInt();

			return (value > z) ? 1 : (value < z) ? -1 : 0;
		} else {
			BigInteger vb = BigInteger.valueOf(value);

			return vb.compareTo(o.toBigInteger());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#doubleValue()
	 */
	public double doubleValue() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#floatValue()
	 */
	public float floatValue() {
		return value;
	}

	/**
	 * 
	 * @param mc
	 * @return
	 */
	public BigDecimal getBigDecimal(MathContext mc) {
		return new BigDecimal(value);
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Integer2#isByteValue()
	 */
	@Override
	public boolean inByteRange() {
		return value >= Byte.MIN_VALUE && value <= Byte.MAX_VALUE;
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Integer2#isIntValue()
	 */
	@Override
	public boolean inIntRange() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Integer2#isLongValue()
	 */
	@Override
	public boolean inLongRange() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Integer2#isShortValue()
	 */
	@Override
	public boolean inShortRange() {
		return value >= Short.MIN_VALUE && value <= Short.MAX_VALUE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRingElement#isUnit()
	 */
	public boolean isUnit() {
		return value == 1;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object x) {
		if(x instanceof SmallInt) {
			return value == ((SmallInt)x).value;
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int l = 17;

		l = 37 * l + value;
		return l;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return Integer.toString(value);
	}

}
