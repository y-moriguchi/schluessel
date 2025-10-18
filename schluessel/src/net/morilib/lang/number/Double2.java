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

import net.morilib.lang.algebra.FieldElement;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2010
 */
public class Double2 extends AbstractNumerical<Double2>
implements NumericalFieldElement<Double2>, FieldElement<Double2> {

	//
//	private static final long serialVersionUID = -4601644469309680393L;

	/**
	 * 
	 */
	public static final Double2 ZERO = new Double2(0.0);

	/**
	 * 
	 */
	public static final Double2 ONE  = new Double2(1.0);

	/**
	 * 
	 */
	public static final Double2 NaN  = new Double2(Double.NaN);

	/**
	 * 
	 */
	public static final Double2 POSITIVE_INFINITY =
		new Double2(Double.POSITIVE_INFINITY);

	/**
	 * 
	 */
	public static final Double2 NEGATIVE_INFINITY =
		new Double2(Double.NEGATIVE_INFINITY);

	//
	/*package*/ double value;

	//
	private Double2(double d) {
		this.value = d;
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static Double2 valueOf(double d) {
		if(d == 0.0) {
			return ZERO;
		} else if(d == 1.0) {
			return ONE;
		} else {
			return new Double2(d);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRingElement#isUnit()
	 */
	public boolean isUnit() {
		return value == 1.0;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.RingElement#isZero()
	 */
	public boolean isZero() {
		return value == 0.0;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Negatable#negate()
	 */
	public Double2 negate() {
		return valueOf(value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Subtractable#subtract(java.lang.Object)
	 */
	public Double2 subtract(Double2 x) {
		return valueOf(value - x.value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#add(java.lang.Object)
	 */
	public Double2 add(Double2 x) {
		return valueOf(value + x.value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public Double2 multiply(int n) {
		return valueOf(value * n);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#multiply(java.lang.Object)
	 */
	public Double2 multiply(Double2 x) {
		return valueOf(value * x.value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#power(int)
	 */
	public Double2 power(int n) {
		return valueOf(Math.pow(value, n));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Calculatable#invert()
	 */
	public Double2 invert() {
		return valueOf(1 / value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.Dividable#divide(java.lang.Object)
	 */
	public Double2 divide(Double2 x) {
		return valueOf(value / x.value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#floatValue()
	 */
	public float floatValue() {
		return (float)value;
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
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(Double2 o) {
		return (value > o.value) ? 1 : ((value < o.value) ? -1 : 0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castInt()
	 */
	public int castInt() {
		return (int)value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castLong()
	 */
	public long castLong() {
		return (long)value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castInteger2()
	 */
	public Integer2 castInteger2() {
		Rational r = Rational.valueOf(value);

		return r.castInteger2();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#intFloor()
	 */
	public int intFloor() {
		return (int)longFloor();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#longFloor()
	 */
	public long longFloor() {
		return (long)Math.floor(value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Floor()
	 */
	public Integer2 getInteger2Floor() {
		Rational r = Rational.valueOf(value);

		return r.getInteger2Floor();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#intCeil()
	 */
	public int intCeil() {
		return (int)longCeil();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#longCeil()
	 */
	public long longCeil() {
		return (long)Math.ceil(value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Ceil()
	 */
	public Integer2 getInteger2Ceil() {
		Rational r = Rational.valueOf(value);

		return r.getInteger2Ceil();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#isInteger()
	 */
	public boolean isInteger() {
		Rational r = Rational.valueOf(value);

		return r.isInteger();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getUniverse()
	 */
	public NumericalField<Double2> getUniverse() {
		return Double2Field.getInstance();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getRational()
	 */
	public Rational getRational() {
		return Rational.valueOf(value);
	}

}
