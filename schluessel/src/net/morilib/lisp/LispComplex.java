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

import net.morilib.lisp.math.constants.LispPi;
import net.morilib.lisp.sos.LispType;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public abstract class LispComplex extends LispQuaternion {

	/**
	 * 
	 */
	public static final LispComplex INFINITY = LispComplexInfinity.INF;

	/**
	 * 
	 * @param r
	 * @param i
	 * @return
	 */
	public static LispComplex newComplex(LispReal r, LispReal i) {
		if(r.isNaN() || i.isNaN()) {
			return LispDouble.NaN;
//		} else if(i.signum() == 0) {
		} else if(i.isExact() && i.signum() == 0) {
			return r;
		} else if(r.isExact() && i.isExact()) {
			return new LispComplexImpl(r, i);
		} else {
			return new LispComplexImpl(r.toInexact(), i.toInexact());
		}
	}

	/**
	 * 
	 * @param r
	 * @param i
	 * @return
	 */
	public static LispComplex newComplex(double r, double i) {
		if(Double.isNaN(r) || Double.isNaN(i)) {
			return LispDouble.NaN;
//		} else if(i == 0.0) {
//			return new LispDouble(r);
		} else {
			return new LispComplexImpl(
					new LispDouble(r), new LispDouble(i));
		}
	}

	/**
	 * 
	 * @param r
	 * @param i
	 * @return
	 */
	public static LispComplex newComplex(float r, float i) {
		if(Float.isNaN(r) || Float.isNaN(i)) {
			return LispFloat.NaN;
//		} else if(i == 0.0) {
//			return new LispFloat(r);
		} else {
			return new LispComplexImpl(
					new LispFloat(r), new LispFloat(i));
		}
	}

	/**
	 * 
	 * @param r
	 * @param i
	 * @return
	 */
	public static LispComplex newPolar(LispReal r, LispReal a) {
		final LispReal PI2 = LispPi.PI.multiply(2);
		LispReal b;
		double x;

		if(r.signum() < 0) {
			r = r.abs();
			a = a.add(LispPi.PI);
		} else if(r.signum() > 0) {
			// do nothing
		} else if(r.isExact() && a.isExact()) {
			return LispInteger.ZERO;
		} else {
			return LispDouble.ZERO;
		}

		if(r.isNaN() || a.isNaN() || a.isInfinity()) {
			return LispDouble.NaN;
		} else if(r.isInfinity()) {
			return INFINITY;
		} else if(a.isZero()) {
			return r;
		} else if(isAngleRegular(a)) {
			return new LispComplexPolar(r, a);
		} else if(!a.isExact()) {
			x = Math.IEEEremainder(a.doubleValue(), 2 * Math.PI);
			x = (x > Math.PI) ? x - 2 * Math.PI : x;
			return new LispComplexPolar(r, new LispDouble(x));
		} else if(a.signum() > 0) {
			b = a;
			for(; !isAngleRegular(b); b = b.subtract(PI2));
			b = b.compareTo(LispPi.PI) > 0 ? b.subtract(PI2) : b;
			return new LispComplexPolar(r, b);
		} else {
			b = a;
			for(; !isAngleRegular(b); b = b.add(PI2));
			b = b.compareTo(LispPi.PI) > 0 ? b.subtract(PI2) : b;
			return new LispComplexPolar(r, b);
		}
	}

	/**
	 * 
	 * @param r
	 * @param i
	 * @return
	 */
	public static LispComplex newPolar(double r, double a) {
		double x;

		if(Double.isNaN(r) ||
				Double.isNaN(a) || Double.isInfinite(a)) {
			return LispDouble.NaN;
		} else if(r == 0.0) {
			return LispDouble.ZERO;
		} else {
			x = Math.IEEEremainder(a, 2 * Math.PI);
			x = (x > Math.PI) ? x - 2 * Math.PI : x;
			return new LispComplexPolar(
					new LispDouble(r), new LispDouble(x));
		}
	}

	/**
	 * 
	 * @param r
	 * @param i
	 * @return
	 */
	public static LispComplex newPolar(float r, float a) {
		double x;

		if(Float.isNaN(r) || Float.isNaN(a) || Float.isInfinite(a)) {
			return LispFloat.NaN;
		} else if(r == 0.0) {
			return LispFloat.ZERO;
		} else {
			x = Math.IEEEremainder(a, 2 * Math.PI);
			x = (x > Math.PI) ? x - 2 * Math.PI : x;
			return new LispComplexPolar(
					new LispFloat(r), new LispFloat((float)x));
		}
	}

	//
	private static boolean isAngleRegular(LispReal a) {
		return (LispPi.PI.compareTo(a) > 0 &&
				LispPi.PI.uminus().compareTo(a) < 0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isInteger()
	 */
	@Override
	public boolean isInteger() {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isRational()
	 */
	@Override
	public boolean isRational() {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isReal()
	 */
	@Override
	public boolean isReal() {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isNaN()
	 */
	public boolean isNaN() {
		return getReal().isNaN() || getImag().isNaN();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isOne()
	 */
	public boolean isOne() {
		return getReal().isOne() && getImag().isZero();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getBigInteger()
	 */
	@Override
	public BigInteger getBigInteger() {
		throw new UnsupportedOperationException();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getDenominator()
	 */
	@Override
	public BigInteger getDenominator() {
		throw new UnsupportedOperationException();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getInt()
	 */
	@Override
	public int getInt() {
		throw new UnsupportedOperationException();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getLong()
	 */
	@Override
	public long getLong() {
		throw new UnsupportedOperationException();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getBigDecimal()
	 */
	public BigDecimal getBigDecimal() {
		throw new UnsupportedOperationException();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getNumerator()
	 */
	@Override
	public BigInteger getNumerator() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispQuaternion#uminus()
	 */
	public abstract LispComplex uminus();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toExact()
	 */
	public abstract LispComplex toExact();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toInexact()
	 */
	public abstract LispComplex toInexact();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#conjugate()
	 */
	public abstract LispComplex conjugate();

	/**
	 * 
	 * @return
	 */
	public abstract LispReal angle();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImags()
	 */
	@Override
	public LispReal[] getImags() {
		return new LispReal[] { getImag() };
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImagsDouble()
	 */
	@Override
	public double[] getImagsDouble() {
		return new double[] { getImagDouble() };
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImagsAsQuaternion()
	 */
	@Override
	public LispReal[] getImagsAsQuaternion() {
		LispReal im = getImag();
		LispReal o  = isExact() ?
				LispInteger.ZERO : new LispDouble(0.0);

		return new LispReal[] { im, o, o };
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImagsDoubleAsQuaternion()
	 */
	@Override
	public double[] getImagsDoubleAsQuaternion() {
		double im = getImagDouble();

		return new double[] {
				im, 0.0, 0.0
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImagsAsOctonion()
	 */
	@Override
	public LispReal[] getImagsAsOctonion() {
		LispReal im = getImag();
		LispReal o  = isExact() ?
				LispInteger.ZERO : new LispDouble(0.0);

		return new LispReal[] {
				im, o, o, o, o, o, o
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImagsDoubleAsOctonion()
	 */
	@Override
	public double[] getImagsDoubleAsOctonion() {
		double im = getImagDouble();

		return new double[] {
				im, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
		};
	}

	//
	@Override
	/*package*/ LispComplex getComplexPairA() {
		return this;
	}

	//
	@Override
	/*package*/ LispComplex getComplexPairB() {
		return LispInteger.ZERO;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toLispString(int)
	 */
	public LispString toLispString(int radix) {
		if(radix < 2 || radix > 36) {
			throw new IndexOutOfBoundsException(
					"radix is out of range");
		} else if(radix != 10) {
			throw new IllegalArgumentException(
					"radix except 10 is not supported");
		}

		if(getImag().signum() < 0 || getImag().isInfinity()) {
			return new LispString(
					LispUtils.getResult(getReal()) +
					LispUtils.getResult(getImag()) + "i");
		} else {
			return new LispString(
					LispUtils.getResult(getReal()) + "+" +
					LispUtils.getResult(getImag()) + "i");
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toLispString(int, int)
	 */
	@Override
	public LispString toLispString(int radix, int precision) {
		String rs, is;

		if(radix < 2 || radix > 36) {
			throw new IndexOutOfBoundsException(
					"radix is out of range");
		} else if(radix != 10) {
			throw new IllegalArgumentException(
					"radix except 10 is not supported");
		}

		rs = getReal().toLispString(radix, precision).getString();
		is = getImag().toLispString(radix, precision).getString();
		if(getImag().signum() < 0 && !getImag().isInfinity()) {
			return new LispString(rs + is + "i");
		} else {
			return new LispString(rs + "+" + is + "i");
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	@Override
	public LispType getType() {
		return LispType.COMPLEX;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isEqualTo(net.morilib.lisp.LispNumber)
	 */
	@Override
	public boolean isEqualTo(LispNumber x) {
		if(x instanceof LispComplex) {
			LispComplex c = (LispComplex)x;

			return ((getReal().isEqualTo(c.getReal())) &&
					(getImag().isEqualTo(c.getImag())));
		} else if(x instanceof LispReal) {
			return false;
		} else {
			return super.isEqualTo(x);
		}
		//throw new IllegalArgumentException(x.toString());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#add(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber add(LispNumber x) {
		if(x instanceof LispComplex) {
			LispComplex c = (LispComplex)x;

			if(x == INFINITY)  return INFINITY;
			return LispComplex.newComplex(
					getReal().add(c.getReal()),
					getImag().add(c.getImag()));
		} else if(x instanceof LispReal) {
			return newComplex(getReal().add(x.getReal()), getImag());
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
		if(x instanceof LispComplex) {
			LispComplex c = (LispComplex)x;

			if(x == INFINITY)  return INFINITY;
			return LispComplex.newComplex(
					getReal().subtract(c.getReal()),
					getImag().subtract(c.getImag()));
		} else if(x instanceof LispReal) {
			return newComplex(
					getReal().subtract(x.getReal()), getImag());
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
		if(x instanceof LispComplex) {
			LispReal xr = ((LispComplex)x).getReal();
			LispReal xi = ((LispComplex)x).getImag();

			if(x == INFINITY) {
				return INFINITY;
			} else if(xr.signum() == 0) {
				return newComplex(
						getImag().uminus().multiply(xi),
						getReal().multiply(xi));
			} else {
				return newComplex(
						getReal().multiply(xr)
						.subtract(getImag().multiply(xi)),
						getImag().multiply(xr)
						.add(getReal().multiply(xi)));
			}
		} else if(x instanceof LispReal) {
			LispReal r = x.getReal();

			return newComplex(
					getReal().multiply(r), getImag().multiply(r));
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
		if(x instanceof LispComplex) {
			LispReal xr = ((LispComplex)x).getReal();
			LispReal xi = ((LispComplex)x).getImag();

			if(x == INFINITY) {
				return isExact() ? LispInteger.ZERO : LispDouble.ZERO;
			} else if(xr.signum() == 0) {
				return newComplex(
						getImag().divide(xi),
						getReal().uminus().divide(xi));
			} else {
				LispReal nr = xr.multiply(xr).add(xi.multiply(xi));
				LispReal x1 =
					getReal().multiply(xr).add(getImag().multiply(xi));
				LispReal x2 =
					getImag().multiply(xr)
					.subtract(getReal().multiply(xi));

				return newComplex(x1.divide(nr), x2.divide(nr));
			}
		} else if(x instanceof LispReal) {
			LispReal r = x.getReal();

			return newComplex(
					getReal().divide(r), getImag().divide(r));
		} else if(x instanceof LispQuaternion) {
			return LispQuaternion.div(this, (LispQuaternion)x);
		} else if(x instanceof LispOctonion) {
			return LispOctonion.div(this, (LispOctonion)x);
		}
		throw new IllegalArgumentException(x.toString());
	}

}
