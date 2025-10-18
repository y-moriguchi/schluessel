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

import java.math.BigInteger;

import net.morilib.lang.algebra.QuotientAndRemainder;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2010
 */
public final class BigInt extends Integer2
implements java.io.Serializable {

	//
	private static final long serialVersionUID = 3591491043374883031L;

	//
	private BigInteger value;

	//
	/*package*/ BigInt(BigInteger value) {
		if(value == null) {
			throw new NullPointerException();
		}
		this.value = value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#add(java.lang.Object)
	 */
	public Integer2 add(Integer2 x) {
		return Integer2.valueOf(value.add(x.toBigInteger()));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Dividable#divide(java.lang.Object)
	 */
	public Integer2 divide(Integer2 x) {
		return Integer2.valueOf(value.divide(x.toBigInteger()));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#multiply(java.lang.Object)
	 */
	public Integer2 multiply(Integer2 x) {
		return Integer2.valueOf(value.multiply(x.toBigInteger()));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Subtractable#subtract(java.lang.Object)
	 */
	public Integer2 subtract(Integer2 x) {
		return Integer2.valueOf(value.subtract(x.toBigInteger()));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Negatable#negate()
	 */
	public Integer2 negate() {
		return new BigInt(value.negate());
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public boolean isEqualTo(Integer2 x) {
		return value.equals(x.toBigInteger());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.AlgebricInteger#signum()
	 */
	public int signum() {
		return value.signum();
	}

	/**
	 * 
	 * @return
	 */
	public String print() {
		return value.toString();
	}

	/**
	 * 
	 * @return
	 */
	public String getResult() {
		return value.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.Integer2#toBigInteger()
	 */
	public BigInteger toBigInteger() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.Integer2#toInt()
	 */
	public int toInt() {
		return value.intValue();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.Integer2#toLong()
	 */
	public long toLong() {
		return value.longValue();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#floatValue()
	 */
	public float floatValue() {
		return value.floatValue();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#doubleValue()
	 */
	public double doubleValue() {
		return value.doubleValue();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Remainderable#remainder(java.lang.Object)
	 */
	public Integer2 remainder(Integer2 x) {
		return Integer2.valueOf(value.remainder(x.toBigInteger()));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Remainderable#divideAndRemainder(java.lang.Object)
	 */
	public QuotientAndRemainder<Integer2> divideAndRemainder(
			Integer2 x) {
		BigInteger[] dr  = value.divideAndRemainder(x.toBigInteger());

		return new QuotientAndRemainder<Integer2>(
				Integer2.valueOf(dr[0]),
				Integer2.valueOf(dr[1]));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.RingElement#isZero()
	 */
	public boolean isZero() {
		return value.equals(BigInteger.ZERO);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public Integer2 multiply(int n) {
		return Integer2.valueOf(value.multiply(BigInteger.valueOf(n)));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#power(int)
	 */
	public Integer2 power(int n) {
		return Integer2.valueOf(value.pow(n));
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(Integer2 o) {
		return value.compareTo(o.toBigInteger());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRingElement#isUnit()
	 */
	public boolean isUnit() {
		return value.equals(BigInteger.ONE);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object x) {
		if(x instanceof BigInt) {
			return value.equals(((BigInt)x).value);
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int l = 17;

		l = 37 * l + value.hashCode();
		return l;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return value.toString();
	}

}
