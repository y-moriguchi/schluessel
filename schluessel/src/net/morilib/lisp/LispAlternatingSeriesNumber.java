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
package net.morilib.lisp;

import net.morilib.lang.number.Integer2;
import net.morilib.lang.number.Rational;
import net.morilib.math.constants.AlternatingSeriesNumber;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/26
 */
public abstract class LispAlternatingSeriesNumber
extends LispIrrational {

	/**
	 * 
	 */
	protected AlternatingSeriesNumber value;

	/**
	 * 
	 * @param scale
	 */
	protected LispAlternatingSeriesNumber(
			AlternatingSeriesNumber value) {
		this.value = value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castInt()
	 */
	public int castInt() {
		return value.intValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castLong()
	 */
	public long castLong() {
		return value.longValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castInteger2()
	 */
	public Integer2 castInteger2() {
		return value.toInteger2();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#intFloor()
	 */
	public int intFloor() {
		return castInt() - ((value.signum() > 0) ? 0 : 1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#longFloor()
	 */
	public long longFloor() {
		return castLong() - ((value.signum() > 0) ? 0 : 1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Floor()
	 */
	public Integer2 getInteger2Floor() {
		return castInteger2().add(Integer2.valueOf(
				((value.signum() > 0) ? 0 : -1)));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#intCeil()
	 */
	public int intCeil() {
		return castInt() + ((value.signum() > 0) ? 1 : 0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#longCeil()
	 */
	public long longCeil() {
		return castLong() + ((value.signum() > 0) ? 1 : 0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Ceil()
	 */
	public Integer2 getInteger2Ceil() {
		return castInteger2().add(Integer2.valueOf(
				((value.signum() > 0) ? 1 : 0)));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getRational()
	 */
	public Rational getRational() {
		return Rational.valueOf(value.doubleValue());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#floatValue()
	 */
	public float floatValue() {
		return value.floatValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#doubleValue()
	 */
	public double doubleValue() {
		return value.doubleValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Calculatable#invert()
	 */
	public LispReal invert() {
		return new LispDouble(1 / value.doubleValue());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispExactReal#toRational()
	 */
	@Override
	public Rational toRational() {
		return getRational();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isLessThan(net.morilib.lisp.LispReal)
	 */
	@Override
	public boolean isLessThan(LispReal x) {
		LispAlternatingSeriesNumber w;
		LispExactReal a;

		if(getClass().equals(x.getClass())) {
			w = (LispAlternatingSeriesNumber)x;
			a = prototype(
					value.getScale().subtract(w.value.getScale()),
					value.getShift().subtract(w.value.getShift()));
			return a.signum() < 0;
		} else if(x instanceof LispAlternatingSeriesNumber) {
			return value.compareTo(
					((LispAlternatingSeriesNumber)x).value) < 0;
		} else if(x.isRational()) {
			return value.compareTo(x.getRational()) < 0;
		} else {
			// approximately
			return value.doubleValue() < x.doubleValue();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isMoreThan(net.morilib.lisp.LispReal)
	 */
	@Override
	public boolean isMoreThan(LispReal x) {
		LispAlternatingSeriesNumber w;
		LispExactReal a;

		if(getClass().equals(x.getClass())) {
			w = (LispAlternatingSeriesNumber)x;
			a = prototype(
					value.getScale().subtract(w.value.getScale()),
					value.getShift().subtract(w.value.getShift()));
			return a.signum() > 0;
		} else if(x instanceof LispAlternatingSeriesNumber) {
			return value.compareTo(
					((LispAlternatingSeriesNumber)x).value) > 0;
		} else if(x.isRational()) {
			return value.compareTo(x.getRational()) > 0;
		} else {
			// approximately
			return value.doubleValue() > x.doubleValue();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispQuaternion#isEqualTo(net.morilib.lisp.LispNumber)
	 */
	@Override
	public boolean isEqualTo(LispNumber x) {
		if(getClass().equals(x.getClass())) {
			return value.getScale().compareTo(
					((LispAlternatingSeriesNumber)x)
					.value.getScale()) == 0;
		} else if(x instanceof LispAlternatingSeriesNumber) {
			// assume to be algebraically independent
			return false;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#signum()
	 */
	@Override
	public int signum() {
		return value.signum();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#toInexact()
	 */
	@Override
	public LispReal toInexact() {
		return new LispDouble(value.doubleValue());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#getResult()
	 */
	@Override
	public String getResult() {
		return value.toString();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#floor()
	 */
	@Override
	public LispReal floor() {
		return LispInteger.valueOf(getInteger2Floor().toBigInteger());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#ceil()
	 */
	@Override
	public LispReal ceil() {
		return LispInteger.valueOf(getInteger2Ceil().toBigInteger());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#hashCode()
	 */
	@Override
	public int hashCode() {
		return value.hashCode();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		return ((o instanceof LispAlternatingSeriesNumber) &&
				value.equals(((LispAlternatingSeriesNumber)o).value));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispQuaternion#getRealDouble()
	 */
	@Override
	public double getRealDouble() {
		return value.doubleValue();
	}

	/**
	 * 
	 * @param scale
	 * @return
	 */
	public abstract LispExactReal prototype(Rational scale,
			Rational shift);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispExactReal#add(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber add(LispNumber x) {
		Rational v, s;

		if(getClass().equals(x.getClass())) {
			v = value.getScale().add(
					((LispAlternatingSeriesNumber)x).value.getScale());
			s = value.getShift().add(
					((LispAlternatingSeriesNumber)x).value.getShift());
			return prototype(v, s);
		} else if(x.isRational() && x.isExact()) {
			v = value.getShift().add(((LispExactReal)x).toRational());
			return prototype(value.getScale(), v);
		} else if(x instanceof LispReal) {
			return new LispDouble(
					value.doubleValue() + x.getRealDouble());
		} else {
			return super.add(x);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispExactReal#sub(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber sub(LispNumber x) {
		Rational v, s;

		if(getClass().equals(x.getClass())) {
			v = value.getScale().subtract(
					((LispAlternatingSeriesNumber)x).value.getScale());
			s = value.getShift().subtract(
					((LispAlternatingSeriesNumber)x).value.getShift());
			return prototype(v, s);
		} else if(x.isRational() && x.isExact()) {
			v = value.getShift().subtract(
					((LispExactReal)x).toRational());
			return prototype(value.getScale(), v);
		} else if(x instanceof LispReal) {
			return new LispDouble(
					value.doubleValue() - x.getRealDouble());
		} else {
			return super.sub(x);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispExactReal#mul(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber mul(LispNumber x) {
		Rational v, s;

		if(x.isRational() && x.isExact()) {
			v = value.getScale().multiply(
					((LispExactReal)x).getRational());
			s = value.getShift().multiply(
					((LispExactReal)x).getRational());
			return prototype(v, s);
		} else if(x instanceof LispReal) {
			return new LispDouble(
					value.doubleValue() * x.getRealDouble());
		} else {
			return super.mul(x);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispExactReal#div(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber div(LispNumber x) {
		Rational v, s;

		if(x.isRational() && x.isExact()) {
			v = value.getScale().divide(
					((LispExactReal)x).getRational());
			s = value.getShift().divide(
					((LispExactReal)x).getRational());
			return prototype(v, s);
		} else if(x instanceof LispReal) {
			return new LispDouble(
					value.doubleValue() / x.getRealDouble());
		} else {
			return super.mul(x);
		}
	}

}
