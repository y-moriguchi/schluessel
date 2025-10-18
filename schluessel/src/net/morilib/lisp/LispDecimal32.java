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

import java.math.BigDecimal;

import net.morilib.lang.Decimal32;
import net.morilib.lang.Decimal64;
import net.morilib.lang.number.AbstractNumericalField;
import net.morilib.lang.number.Integer2;
import net.morilib.lang.number.NumericalField;
import net.morilib.lang.number.Rational;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/11/06
 */
public class LispDecimal32 extends LispInexactReal
implements JavaObjective, java.io.Serializable {

	/**
	 * 
	 */
	public static final LispDecimal32 ZERO =
		new LispDecimal32(Decimal32.ZERO_BY_INT);

	/**
	 * 
	 */
	public static final LispDecimal32 ONE =
		new LispDecimal32(Decimal32.parseDecimal("1"));

	/**
	 * 
	 */
	public static final NumericalField<LispReal>
	FIELD = new AbstractNumericalField<LispReal>() {

		public LispReal valueOf(int v) {
			return valueOf((long)v);
		}

		public LispReal valueOf(long v) {
			return new LispDecimal32(Decimal32.toDecimal(v));
		}

		public LispReal valueOf(Integer2 v) {
			return valueOf(v.doubleValue());
		}

		public LispReal getUnit() {
			return LispFloat.ONE;
		}

		public LispReal getZero() {
			return LispFloat.ZERO;
		}

		public LispReal valueOf(float v) {
			return valueOf((double)v);
		}

		public LispReal valueOf(double v) {
			return new LispDecimal32(Decimal32.toDecimal(v));
		}

		public LispReal valueOf(BigDecimal v) {
			return valueOf(v.floatValue());
		}

		public LispReal valueOf(Rational v) {
			return valueOf(v.floatValue());
		}

	};

	//
	private int number;

	/**
	 * @param add
	 */
	public LispDecimal32(int number) {
		this.number = number;
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
		return Decimal32.toBigDecimal(number).intValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castLong()
	 */
	public long castLong() {
		return Decimal32.toBigDecimal(number).longValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castInteger2()
	 */
	public Integer2 castInteger2() {
		return Integer2.valueOf(
				Decimal32.toBigDecimal(number).toBigInteger());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#intFloor()
	 */
	public int intFloor() {
		return Decimal32.toBigDecimal(
				Decimal32.floor(number, 0)).intValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#longFloor()
	 */
	public long longFloor() {
		return Decimal32.toBigDecimal(
				Decimal32.floor(number, 0)).longValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Floor()
	 */
	public Integer2 getInteger2Floor() {
		return Integer2.valueOf(Decimal32.toBigDecimal(
				Decimal32.floor(number, 0)).toBigInteger());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#intCeil()
	 */
	public int intCeil() {
		return Decimal32.toBigDecimal(
				Decimal32.ceil(number, 0)).intValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#longCeil()
	 */
	public long longCeil() {
		return Decimal32.toBigDecimal(
				Decimal32.ceil(number, 0)).longValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Ceil()
	 */
	public Integer2 getInteger2Ceil() {
		return Integer2.valueOf(Decimal32.toBigDecimal(
				Decimal32.ceil(number, 0)).toBigInteger());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getRational()
	 */
	public Rational getRational() {
		return Rational.valueOf(Decimal32.toBigDecimal(number));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#floatValue()
	 */
	public float floatValue() {
		return Decimal32.floatValue(number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#doubleValue()
	 */
	public double doubleValue() {
		return Decimal32.doubleValue(number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRingElement#isUnit()
	 */
	public boolean isUnit() {
		return doubleValue() == 1.0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public LispReal multiply(int n) {
		return new LispDecimal32(Decimal32.multiply(
				number, Decimal32.toDecimal(n)));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#power(int)
	 */
	public LispReal power(int n) {
		int x = number;

		if(n < 0) {
			throw new IllegalArgumentException();
		} else if(n == 0) {
			return ZERO;
		} else {
			for(int i = 1; i < n; i++) {
				x = Decimal32.multiply(x, number);
			}
			return new LispDecimal32(x);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Calculatable#invert()
	 */
	public LispReal invert() {
		return new LispDecimal32(Decimal32.divide(
				Decimal32.toDecimal(1), number));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isLessThan(net.morilib.lisp.LispReal)
	 */
	@Override
	public boolean isLessThan(LispReal x) {
		return Decimal32.compare(number, x.getRealDecimal32()) < 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isMoreThan(net.morilib.lisp.LispReal)
	 */
	@Override
	public boolean isMoreThan(LispReal x) {
		return Decimal32.compare(number, x.getRealDecimal32()) > 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#signum()
	 */
	@Override
	public int signum() {
		return Decimal32.getSignum(number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#toExact()
	 */
	@Override
	public LispExactReal toExact() {
		return LispUtils.bigDecimalToRational(
				Decimal32.toBigDecimal(number));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isInfinity()
	 */
	@Override
	public boolean isInfinity() {
		return Decimal32.isInfinite(number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#uminus()
	 */
	@Override
	public LispReal uminus() {
		return new LispDecimal32(Decimal32.negate(number));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispQuaternion#getRealDouble()
	 */
	@Override
	public double getRealDouble() {
		return Decimal32.doubleValue(number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#add(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber add(LispNumber x) {
		if(x instanceof LispRangedDouble) {
			return new LispRangedDouble(
					Decimal32.doubleValue(number) + x.getRealDouble(),
					((LispRangedDouble)x).error);
		} else if(x instanceof LispHalf) {
			return new LispDecimal32(Decimal32.add(
					number, x.getRealDecimal32()));
		} else if(x instanceof LispFloat) {
			return new LispDecimal32(Decimal32.add(
					number, x.getRealDecimal32()));
		} else if(x instanceof LispDouble) {
			return new LispDouble(
					Decimal32.doubleValue(number) + x.getRealDouble());
		} else if(x instanceof LispDecimal64) {
			return new LispDecimal64(Decimal64.add(
					Decimal64.decimal32To64(number),
					x.getRealDecimal64()));
		} else if(x instanceof LispReal) {
			return new LispDecimal32(Decimal32.add(
					number, x.getRealDecimal32()));
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

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#sub(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber sub(LispNumber x) {
		if(x instanceof LispRangedDouble) {
			return new LispRangedDouble(
					Decimal32.doubleValue(number) - x.getRealDouble(),
					((LispRangedDouble)x).error);
		} else if(x instanceof LispHalf) {
			return new LispDecimal32(Decimal32.subtract(
					number, x.getRealDecimal32()));
		} else if(x instanceof LispFloat) {
			return new LispDecimal32(Decimal32.subtract(
					number, x.getRealDecimal32()));
		} else if(x instanceof LispDouble) {
			return new LispDouble(
					Decimal32.doubleValue(number) - x.getRealDouble());
		} else if(x instanceof LispDecimal64) {
			return new LispDecimal64(Decimal64.subtract(
					Decimal64.decimal32To64(number),
					x.getRealDecimal64()));
		} else if(x instanceof LispReal) {
			return new LispDecimal32(Decimal32.subtract(
					number, x.getRealDecimal32()));
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

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#mul(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber mul(LispNumber x) {
		if(x instanceof LispRangedDouble) {
			return LispRangedDouble.valueOf(
					Decimal32.doubleValue(number) * x.getRealDouble(),
					Decimal32.doubleValue(number) *
					((LispRangedDouble)x).error);
		} else if(x instanceof LispHalf) {
			return new LispDecimal32(Decimal32.multiply(
					number, x.getRealDecimal32()));
		} else if(x instanceof LispFloat) {
			return new LispDecimal32(Decimal32.multiply(
					number, x.getRealDecimal32()));
		} else if(x instanceof LispDouble) {
			return new LispDouble(
					Decimal32.doubleValue(number) * x.getRealDouble());
		} else if(x instanceof LispDecimal64) {
			return new LispDecimal64(Decimal64.multiply(
					Decimal64.decimal32To64(number),
					x.getRealDecimal64()));
		} else if(x instanceof LispReal) {
			return new LispDecimal32(Decimal32.multiply(
					number, x.getRealDecimal32()));
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

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#div(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber div(LispNumber x) {
		double y1, y2;
		LispRangedDouble x0;

		if(x instanceof LispRangedDouble) {
			x0 = (LispRangedDouble)x;
			if(x0.isZeroIncluded()) {
				return LispDouble.NaN;
			}
			y1 = (doubleValue() / x0.getRealDouble() +
					doubleValue() * x0.error);
			y2 = (doubleValue() / x0.getRealDouble() -
					doubleValue() * x0.error);
			return LispRangedDouble.valueOf(
					(y1 + y2) / 2.0, Math.abs(y1 - y2) / 2.0);
		} else if(x instanceof LispHalf) {
			return new LispDecimal32(Decimal32.divide(
					number, x.getRealDecimal32()));
		} else if(x instanceof LispFloat) {
			return new LispDecimal32(Decimal32.divide(
					number, x.getRealDecimal32()));
		} else if(x instanceof LispDouble) {
			return new LispDouble(
					Decimal32.doubleValue(number) / x.getRealDouble());
		} else if(x instanceof LispDecimal64) {
			return new LispDecimal64(Decimal64.divide(
					Decimal64.decimal32To64(number),
					x.getRealDecimal64()));
		} else if(x instanceof LispReal) {
			return new LispDecimal32(Decimal32.divide(
					number, x.getRealDecimal32()));
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

	/* (non-Javadoc)
	 * @see net.morilib.lisp.JavaObjective#toObject()
	 */
	public Object toObject() {
		return new Decimal32(number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getRealDecimal64()
	 */
	@Override
	public long getRealDecimal64() {
		return Decimal64.decimal32To64(number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getRealDecimal64()
	 */
	@Override
	public int getRealDecimal32() {
		return number;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isNaN()
	 */
	@Override
	public boolean isNaN() {
		return Decimal32.isNaN(number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isOne()
	 */
	@Override
	public boolean isOne() {
		return equals(ONE);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#getResult()
	 */
	@Override
	public String getResult() {
		return Decimal32.toString(number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#floor()
	 */
	@Override
	public LispReal floor() {
		return new LispDecimal32(Decimal32.floor(number, 0));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#ceil()
	 */
	@Override
	public LispReal ceil() {
		return new LispDecimal32(Decimal32.ceil(number, 0));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isFinite()
	 */
	@Override
	public boolean isFinite() {
		return !Decimal32.isInfinite(number);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#toLispString(int)
	 */
	@Override
	public LispString toLispString(int radix) {
		if(radix < 2 || radix > 36) {
			throw new IndexOutOfBoundsException(
					"radix is out of range");
		} else if(radix != 10) {
			throw new IllegalArgumentException(
					"radix except 10 is not supported");
		}
		return new LispString(Decimal32.toString(number));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toLispString(int, int)
	 */
	@Override
	public LispString toLispString(int radix, int precision) {
		if(radix < 2 || radix > 36) {
			throw new IndexOutOfBoundsException(
					"radix is out of range");
		} else if(radix != 10) {
			throw new IllegalArgumentException(
					"radix except 10 is not supported");
		} else if(precision < 0) {
			throw new IllegalArgumentException(
					"precision must not be negative");
		}
		return new LispString(Decimal32.toString(number, precision));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#hashCode()
	 */
	@Override
	public int hashCode() {
		return number;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		if(o instanceof LispDecimal32) {
			return Decimal32.compare(number,
					((LispDecimal32)o).number) == 0;
		}
		return false;
	}

}
