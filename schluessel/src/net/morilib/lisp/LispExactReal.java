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

import net.morilib.lang.Decimal32;
import net.morilib.lang.Decimal64;
import net.morilib.lang.DoubleUtils;
import net.morilib.lang.number.AbstractNumericalField;
import net.morilib.lang.number.Integer2;
import net.morilib.lang.number.NumericalField;
import net.morilib.lang.number.Rational;
import net.morilib.lisp.sos.LispType;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public abstract class LispExactReal extends LispReal {

	//
	private static final NumericalField<LispReal>
	_FIELDR = new AbstractNumericalField<LispReal>() {

		public LispReal valueOf(float v) {
			return LispDouble.toExact(v);
		}

		public LispReal valueOf(double v) {
			return LispDouble.toExact(v);
		}

		public LispReal valueOf(BigDecimal v) {
			return LispUtils.bigDecimalToRational(v);
		}

		public LispReal valueOf(Rational v) {
			return LispRational.valueOf(v);
		}

		public LispReal valueOf(int v) {
			return LispInteger.valueOf(v);
		}

		public LispReal valueOf(long v) {
			return LispInteger.valueOf(v);
		}

		public LispReal valueOf(Integer2 v) {
			return LispInteger.valueOf(v.toBigInteger());
		}

		public LispReal getUnit() {
			return LispInteger.ONE;
		}

		public LispReal getZero() {
			return LispInteger.ZERO;
		}

	};

	/**
	 * 
	 * @return
	 */
	public abstract Rational toRational();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#uminus()
	 */
	public abstract LispExactReal uminus();

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#toExact()
	 */
	public LispExactReal toExact() {
		return this;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isNaN()
	 */
	public boolean isNaN() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isInfinity()
	 */
	@Override
	public boolean isInfinity() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	@Override
	public LispType getType() {
		return LispType.REAL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public LispReal multiply(int n) {
		return multiply(new LispSmallInt(n));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isExact()
	 */
	@Override
	public boolean isExact() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#power(int)
	 */
	public LispReal power(int n) {
		if(n < 0) {
			return power(-n).invert();
		} else if(n == 0) {
			return LispInteger.ONE;
		} else if(n == 1) {
			return this;
		} else {
			LispReal r = this;

			for(int i = 1; i < n; i++) {
				r = r.multiply(this);
			}
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getUniverse()
	 */
	public NumericalField<LispReal> getUniverse() {
		return _FIELDR;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#add(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber add(LispNumber x) {
		if(x instanceof LispRangedDouble) {
			return new LispRangedDouble(
					doubleValue() + x.getRealDouble(),
					((LispRangedDouble)x).error);
		} else if(x instanceof LispHalf) {
			return new LispHalf(DoubleUtils.toHalf(
					doubleValue() + x.getRealDouble()));
		} else if(x instanceof LispFloat) {
			return new LispFloat(floatValue() + x.getRealFloat());
		} else if(x instanceof LispDouble) {
			return new LispDouble(doubleValue() + x.getRealDouble());
		} else if(x instanceof LispDecimal32) {
			return new LispDecimal32(Decimal32.add(
					getRealDecimal32(), x.getRealDecimal32()));
		} else if(x instanceof LispDecimal64) {
			return new LispDecimal64(Decimal64.add(
					getRealDecimal64(), x.getRealDecimal64()));
		} else if(x instanceof LispAlternatingSeriesNumber) {
			return x.add(this);
		} else if(x instanceof LispIrrational) {
			return new LispDouble(doubleValue() + x.getRealDouble());
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
					doubleValue() - x.getRealDouble(),
					((LispRangedDouble)x).error);
		} else if(x instanceof LispHalf) {
			return new LispHalf(DoubleUtils.toHalf(
					doubleValue() - x.getRealDouble()));
		} else if(x instanceof LispFloat) {
			return new LispFloat(floatValue() - x.getRealFloat());
		} else if(x instanceof LispDouble) {
			return new LispDouble(doubleValue() - x.getRealDouble());
		} else if(x instanceof LispDecimal32) {
			return new LispDecimal32(Decimal32.subtract(
					getRealDecimal32(), x.getRealDecimal32()));
		} else if(x instanceof LispDecimal64) {
			return new LispDecimal64(Decimal64.subtract(
					getRealDecimal64(), x.getRealDecimal64()));
		} else if(x instanceof LispAlternatingSeriesNumber) {
			return ((LispAlternatingSeriesNumber)x).negate().add(this);
		} else if(x instanceof LispIrrational) {
			return new LispDouble(doubleValue() - x.getRealDouble());
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
					doubleValue() * x.getRealDouble(),
					doubleValue() * ((LispRangedDouble)x).error);
		} else if(x instanceof LispHalf) {
			return new LispHalf(DoubleUtils.toHalf(
					doubleValue() * x.getRealDouble()));
		} else if(x instanceof LispFloat) {
			return new LispFloat(floatValue() * x.getRealFloat());
		} else if(x instanceof LispDouble) {
			return new LispDouble(doubleValue() * x.getRealDouble());
		} else if(x instanceof LispDecimal32) {
			return new LispDecimal32(Decimal32.multiply(
					getRealDecimal32(), x.getRealDecimal32()));
		} else if(x instanceof LispDecimal64) {
			return new LispDecimal64(Decimal64.multiply(
					getRealDecimal64(), x.getRealDecimal64()));
		} else if(x instanceof LispAlternatingSeriesNumber) {
			return x.mul(this);
		} else if(x instanceof LispIrrational) {
			return new LispDouble(doubleValue() * x.getRealDouble());
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
			return new LispHalf(DoubleUtils.toHalf(
					doubleValue() / x.getRealDouble()));
		} else if(x instanceof LispFloat) {
			return new LispFloat(floatValue() / x.getRealFloat());
		} else if(x instanceof LispDouble) {
			return new LispDouble(doubleValue() / x.getRealDouble());
		} else if(x instanceof LispDecimal32) {
			return new LispDecimal32(Decimal32.divide(
					getRealDecimal32(), x.getRealDecimal32()));
		} else if(x instanceof LispDecimal64) {
			return new LispDecimal64(Decimal64.divide(
					getRealDecimal64(), x.getRealDecimal64()));
		} else if(x instanceof LispIrrational) {
			return new LispDouble(doubleValue() / x.getRealDouble());
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
	 * @see net.morilib.lisp.LispReal#remainder(net.morilib.lisp.LispReal)
	 */
	public LispReal remainder(LispReal r) {
		LispReal b;
		double x;

		if(!r.isExact()) {
			x = Math.IEEEremainder(doubleValue(), r.doubleValue());
			return new LispDouble(x);
		} else if(r.signum() > 0) {
			for(b = r; compareTo(r) > 0; b = b.subtract(r));
			return b;
		} else {
			for(b = r; compareTo(r) < 0; b = b.add(r));
			return b;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toLispString(int, int)
	 */
	@Override
	public LispString toLispString(int radix, int precision) {
		return toLispString(radix);
	}

}
