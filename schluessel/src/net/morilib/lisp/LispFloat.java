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
package net.morilib.lisp;

import java.math.BigDecimal;
import java.math.BigInteger;

import net.morilib.lang.Decimal32;
import net.morilib.lang.Decimal64;
import net.morilib.lang.number.AbstractNumericalField;
import net.morilib.lang.number.Double2;
import net.morilib.lang.number.Integer2;
import net.morilib.lang.number.NumericalField;
import net.morilib.lang.number.Rational;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class LispFloat extends LispInexactReal
implements JavaObjective, java.io.Serializable {

	/**
	 * 
	 */
	public static final NumericalField<LispReal>
	FIELD = new AbstractNumericalField<LispReal>() {

		public LispReal valueOf(int v) {
			return valueOf((float)v);
		}

		public LispReal valueOf(long v) {
			return valueOf((float)v);
		}

		public LispReal valueOf(Integer2 v) {
			return valueOf(v.floatValue());
		}

		public LispReal getUnit() {
			return LispFloat.ONE;
		}

		public LispReal getZero() {
			return LispFloat.ZERO;
		}

		public LispReal valueOf(float v) {
			return new LispFloat(v);
		}

		public LispReal valueOf(double v) {
			return valueOf((float)v);
		}

		public LispReal valueOf(BigDecimal v) {
			return valueOf(v.floatValue());
		}

		public LispReal valueOf(Rational v) {
			return valueOf(v.floatValue());
		}

	};

	/**
	 * 
	 */
	public static final LispFloat ZERO = new LispFloat(0.0f);

	/**
	 * 
	 */
	public static final LispFloat ONE = new LispFloat(1.0f);

	/**
	 * 
	 */
	public static final LispFloat POSITIVE_INFINITY =
		new LispFloat(Float.POSITIVE_INFINITY);

	/**
	 * 
	 */
	public static final LispFloat NEGATIVE_INFINITY =
		new LispFloat(Float.NEGATIVE_INFINITY);

	/**
	 * 
	 */
	public static final LispFloat NaN = new LispFloat(Float.NaN);

	//
	private float number;

	/**
	 * 
	 * @param x
	 */
	public LispFloat(float x) {
		this.number = x;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getNumerator()
	 */
	public BigInteger getNumerator() {
		LispReal r = LispDouble.toExact(number);

		return r.getNumerator();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getDenominator()
	 */
	public BigInteger getDenominator() {
		LispReal r = LispDouble.toExact(number);

		return r.getDenominator();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#add(net.morilib.lisp.LispNumber)
	 */
	public LispNumber add(LispNumber x) {
		if(x instanceof LispRangedDouble) {
			return new LispRangedDouble(number + x.getRealDouble(),
					((LispRangedDouble)x).error);
		} else if(x instanceof LispHalf) {
			return new LispFloat(number + x.getRealFloat());
		} else if(x instanceof LispDouble) {
			return new LispDouble(number + x.getRealDouble());
		} else if(x instanceof LispDecimal32) {
			return new LispDecimal32(Decimal32.add(
					Decimal32.toDecimal(number),
					x.getRealDecimal32()));
		} else if(x instanceof LispDecimal64) {
			return new LispDecimal64(Decimal64.add(
					Decimal64.toDecimal(number),
					x.getRealDecimal64()));
		} else if(x instanceof LispReal) {
			return new LispFloat(number + x.getRealFloat());
		} else if(x instanceof LispComplex) {
			LispComplex c = (LispComplex)x;

			if(x == LispComplex.INFINITY)  return LispComplex.INFINITY;
			return LispComplex.newComplex(
					add(c.getReal()), c.getImag());
		} else if(x instanceof LispQuaternion) {
			return LispQuaternion.add(this, (LispQuaternion)x);
		} else if(x instanceof LispOctonion) {
			return LispOctonion.add(this, (LispOctonion)x);
		}
		throw new IllegalArgumentException(x.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#sub(net.morilib.lisp.LispNumber)
	 */
	public LispNumber sub(LispNumber x) {
		if(x instanceof LispRangedDouble) {
			return new LispRangedDouble(number - x.getRealDouble(),
					((LispRangedDouble)x).error);
		} else if(x instanceof LispHalf) {
			return new LispFloat(number - x.getRealFloat());
		} else if(x instanceof LispDouble) {
			return new LispDouble(number - x.getRealDouble());
		} else if(x instanceof LispDecimal32) {
			return new LispDecimal32(Decimal32.subtract(
					Decimal32.toDecimal(number),
					x.getRealDecimal32()));
		} else if(x instanceof LispDecimal64) {
			return new LispDecimal64(Decimal64.subtract(
					Decimal64.toDecimal(number),
					x.getRealDecimal64()));
		} else if(x instanceof LispReal) {
			return new LispFloat(number - x.getRealFloat());
		} else if(x instanceof LispComplex) {
			LispComplex c = (LispComplex)x;

			if(x == LispComplex.INFINITY)  return LispComplex.INFINITY;
			return LispComplex.newComplex(
					subtract(c.getReal()), c.getImag().uminus());
		} else if(x instanceof LispQuaternion) {
			return LispQuaternion.sub(this, (LispQuaternion)x);
		} else if(x instanceof LispOctonion) {
			return LispOctonion.sub(this, (LispOctonion)x);
		}
		throw new IllegalArgumentException(x.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#mul(net.morilib.lisp.LispNumber)
	 */
	public LispNumber mul(LispNumber x) {
		if(x instanceof LispRangedDouble) {
			return LispRangedDouble.valueOf(
					number * x.getRealDouble(),
					number * ((LispRangedDouble)x).error);
		} else if(x instanceof LispHalf) {
			return new LispFloat(number * x.getRealFloat());
		} else if(x instanceof LispDouble) {
			return new LispDouble(number * x.getRealDouble());
		} else if(x instanceof LispDecimal32) {
			return new LispDecimal32(Decimal32.multiply(
					Decimal32.toDecimal(number),
					x.getRealDecimal32()));
		} else if(x instanceof LispDecimal64) {
			return new LispDecimal64(Decimal64.multiply(
					Decimal64.toDecimal(number),
					x.getRealDecimal64()));
		} else if(x instanceof LispReal) {
			return new LispFloat(number * x.getRealFloat());
		} else if(x instanceof LispComplex) {
			LispComplex c = (LispComplex)x;

			if(x == LispComplex.INFINITY)  return LispComplex.INFINITY;
			return LispComplex.newComplex(
					multiply(c.getReal()), multiply(c.getImag()));
		} else if(x instanceof LispQuaternion) {
			return LispQuaternion.mul(this, (LispQuaternion)x);
		} else if(x instanceof LispOctonion) {
			return LispOctonion.mul(this, (LispOctonion)x);
		}
		throw new IllegalArgumentException(x.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#div(net.morilib.lisp.LispNumber)
	 */
	public LispNumber div(LispNumber x) {
		double y1, y2;
		LispRangedDouble x0;

		if(x instanceof LispRangedDouble) {
			x0 = (LispRangedDouble)x;
			if(x0.isZeroIncluded()) {
				return LispDouble.NaN;
			}
			y1 = number / x0.getRealDouble() + number * x0.error;
			y2 = number / x0.getRealDouble() - number * x0.error;
			return LispRangedDouble.valueOf(
					(y1 + y2) / 2.0, Math.abs(y1 - y2) / 2.0);
		} else if(x instanceof LispRangedDouble) {
			return new LispRangedDouble(number + x.getRealDouble(),
					((LispRangedDouble)x).error);
		} else if(x instanceof LispHalf) {
			return new LispFloat(number / x.getRealFloat());
		} else if(x instanceof LispDouble) {
			return new LispDouble(number / x.getRealDouble());
		} else if(x instanceof LispDecimal32) {
			return new LispDecimal32(Decimal32.divide(
					Decimal32.toDecimal(number),
					x.getRealDecimal32()));
		} else if(x instanceof LispDecimal64) {
			return new LispDecimal64(Decimal64.divide(
					Decimal64.toDecimal(number),
					x.getRealDecimal64()));
		} else if(x instanceof LispReal) {
			return new LispFloat(number / x.getRealFloat());
		} else if(x instanceof LispComplex) {
			LispReal xr = x.getReal();
			LispReal xi = x.getImag();
			LispReal xn = xr.multiply(xr).add(xi.multiply(xi));

			if(x == LispComplex.INFINITY) {
				return isExact() ? LispInteger.ZERO : LispDouble.ZERO;
			}
			return LispComplex.newComplex(
					multiply(xr).divide(xn),
					multiply(xi).uminus().divide(xn));
		} else if(x instanceof LispQuaternion) {
			return LispQuaternion.div(this, (LispQuaternion)x);
		} else if(x instanceof LispOctonion) {
			return LispOctonion.div(this, (LispOctonion)x);
		}
		throw new IllegalArgumentException(x.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#uminus()
	 */
	public LispFloat uminus() {
		return new LispFloat(-number);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isEqualTo(net.morilib.lisp.LispNumber)
	 */
	public boolean isEqualTo(LispNumber x) {
		if(x instanceof LispHalf) {
			return number == x.getRealFloat();
		} else if(x instanceof LispFloat) {
			return number == x.getRealFloat();
		} else if(x instanceof LispDouble) {
			return number == x.getRealDouble();
		} else if(x instanceof LispDecimal32) {
			return number == x.getRealFloat();
		} else if(x instanceof LispDecimal64) {
			return number == x.getRealFloat();
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isLessThan(net.morilib.lisp.LispReal)
	 */
	public boolean isLessThan(LispReal x) {
		return number < x.getRealFloat();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isMoreThan(net.morilib.lisp.LispReal)
	 */
	public boolean isMoreThan(LispReal x) {
		return number > x.getRealFloat();
	}

	/**
	 * 
	 * @return
	 */
	public BigInteger bigIntegerValue() {
		BigDecimal dec = new BigDecimal(number);
		return dec.toBigInteger();
	}

	/**
	 * 
	 * @return
	 */
	public double doubleValue() {
		return number;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#signum()
	 */
	public int signum() {
		return (number > 0) ? 1 : ((number < 0) ? -1 : 0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#toExact()
	 */
	public LispExactReal toExact() {
		return LispDouble.toExact(number);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#print()
	 */
	public String print() {
		return disp(number);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#getResult()
	 */
	public String getResult() {
		return disp(number);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isInteger()
	 */
	@Override
	public boolean isInteger() {
		return LispUtils.toIntegerExact(number) != null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toLispString(int)
	 */
	public LispString toLispString(int radix) {
		if(radix < 2 || radix > 36) {
			throw new IndexOutOfBoundsException("radix is out of range");
		} else if(radix != 10) {
			throw new IllegalArgumentException(
					"radix except 10 is not supported");
		}

		if(Float.isNaN(number)) {
			return new LispString("+nan.0");
		} else if(number == Float.POSITIVE_INFINITY) {
			return new LispString("+inf.0");
		} else if(number == Float.NEGATIVE_INFINITY) {
			return new LispString("-inf.0");
		} else {
			return new LispString(Float.toString(number));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toLispString(int, int)
	 */
	@Override
	public LispString toLispString(int radix, int precision) {
		if(radix < 2 || radix > 36) {
			throw new IndexOutOfBoundsException("radix is out of range");
		} else if(radix != 10) {
			throw new IllegalArgumentException(
					"radix except 10 is not supported");
		} else if(precision < 0) {
			throw new IllegalArgumentException(
					"precision must not be negative");
		}

		if(Double.isNaN(number)) {
			return new LispString("+nan.0");
		} else if(number == Double.POSITIVE_INFINITY) {
			return new LispString("+inf.0");
		} else if(number == Double.NEGATIVE_INFINITY) {
			return new LispString("-inf.0");
		} else {
			return new LispString(String.format(
					"%" + precision + "d", number));
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isNaN()
	 */
	public boolean isNaN() {
		return Double.isNaN(number);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isOne()
	 */
	public boolean isOne() {
		return number == 1.0;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getBigDecimal()
	 */
	public BigDecimal getBigDecimal() {
		if(Double.isInfinite(number) || Double.isNaN(number)) {
			throw new NumberFormatException(
					"Infinities or NaNs is not supported");
		}

		return BigDecimal.valueOf(number);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getRealDouble()
	 */
	@Override
	public double getRealDouble() {
		return number;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getRealDecimal64()
	 */
	@Override
	public long getRealDecimal64() {
		return Decimal64.toDecimal(number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getRealDecimal32()
	 */
	public int getRealDecimal32() {
		return Decimal32.toDecimal(number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isInfinity()
	 */
	@Override
	public boolean isInfinity() {
		return Double.isInfinite(number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Calculatable#invert()
	 */
	public LispReal invert() {
		return new LispFloat(1 / number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRingElement#isUnit()
	 */
	public boolean isUnit() {
		return number == 1.0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public LispReal multiply(int n) {
		return new LispFloat(number * n);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#power(int)
	 */
	public LispReal power(int n) {
		return new LispFloat((float)Math.pow(number, n));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalFieldElement#getUniverse()
	 */
	public NumericalField<LispReal> getUniverse() {
		return FIELD;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castInt()
	 */
	public int castInt() {
		return (int)number;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castLong()
	 */
	public long castLong() {
		return (long)number;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castInteger2()
	 */
	public Integer2 castInteger2() {
		return Double2.valueOf(number).castInteger2();
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
		return (long)Math.floor(number);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Floor()
	 */
	public Integer2 getInteger2Floor() {
		Rational r = Rational.valueOf(number);
		
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
		return (long)Math.ceil(number);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Ceil()
	 */
	public Integer2 getInteger2Ceil() {
		Rational r = Rational.valueOf(number);
		
		return r.getInteger2Ceil();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getRational()
	 */
	public Rational getRational() {
		return LispDouble.toExact(number).getRational();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#floatValue()
	 */
	public float floatValue() {
		return (float)number;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.JavaObjective#toObject()
	 */
	public Object toObject() {
		return Double.valueOf(number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#floor()
	 */
	@Override
	public LispReal floor() {
		return new LispFloat((float)Math.floor(number));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#ceil()
	 */
	@Override
	public LispReal ceil() {
		return new LispFloat((float)Math.ceil(number));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isFinite()
	 */
	@Override
	public boolean isFinite() {
		return !Float.isInfinite(number);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object x) {
		if(x instanceof LispFloat) {
//			return number == ((LispFloat)x).number;
			return Float.compare(number, ((LispFloat)x).number) == 0;
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return Float.floatToIntBits(number);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return Double.toString(number);
	}

}
