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

import net.morilib.lang.Decimal64;
import net.morilib.lang.number.Integer2;
import net.morilib.lang.number.Rational;
import net.morilib.lisp.sos.LispType;
import net.morilib.util.IntMath;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class LispRational extends LispExactReal
implements java.io.Serializable {

	//
	private static final long serialVersionUID = 5519828952722765386L;

	//
	private BigInteger numer;
	private BigInteger denom;

	//
	private LispRational(BigInteger num, BigInteger den) {
		numer = num;
		denom = den;
	}

	/**
	 * 
	 * @param r
	 */
	public static final LispExactReal valueOf(Rational r) {
		return newRational(
				r.getNumerator().toBigInteger(),
				r.getDenominator().toBigInteger());
	}

	/**
	 * 
	 * @param num
	 * @param den
	 * @return
	 */
	public static final LispExactReal newRational(
			BigInteger num, BigInteger den) {
		BigInteger n = num;
		BigInteger d = den;
		int nsig = num.signum();
		int dsig = den.signum();

		if(dsig == 0) {
			throw new LispArithmeticException("err.divbyzero");
		} else if(nsig == 0) {
			return LispInteger.ZERO;
		} else if(dsig < 0) {
			n = n.negate();
			d = d.negate();
		}

		BigInteger gcd = n.gcd(d);
		n = n.divide(gcd);
		d = d.divide(gcd);

		if(d.equals(BigInteger.ONE)) {
			return LispInteger.valueOf(n);
		} else {
			return new LispRational(n, d);
		}
	}

	/**
	 * 
	 * @param num
	 * @param den
	 * @return
	 */
	public static final LispExactReal newRational(int num, int den) {
		int n1   = num;
		int d1   = den;

		if(d1 == 0) {
			throw new LispArithmeticException("err.divbyzero");
		} else if(n1 == 0) {
			return LispInteger.ZERO;
		} else if(d1 < 0) {
			n1 = -n1;
			d1 = -d1;
		}

		int gcd = IntMath.gcd(n1, d1);
		n1  = n1 / gcd;
		d1  = d1 / gcd;

		if(d1 == 1) {
			return LispInteger.valueOf(n1);
		} else {
			return new LispRational(
					BigInteger.valueOf(n1), BigInteger.valueOf(d1));
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getNumerator()
	 */
	public BigInteger getNumerator() {
		return numer;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getDenominator()
	 */
	public BigInteger getDenominator() {
		return denom;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#add(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber add(LispNumber x) {
		if(x instanceof LispRational) {
			LispRational n = (LispRational)x;
			BigInteger nd = denom.multiply(n.denom);
			BigInteger nn = numer.multiply(n.denom).add(
					n.numer.multiply(denom));

			return LispRational.newRational(nn, nd);
		} else if(x instanceof LispInteger) {
			BigInteger n  = x.getBigInteger();
			BigInteger nd = denom;
			BigInteger nn = numer.add(n.multiply(denom));

			return LispRational.newRational(nn, nd);
		} else {
			return super.add(x);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#div(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber div(LispNumber x) {
		if(x instanceof LispRational) {
			LispRational n = (LispRational)x;
			BigInteger nd = denom.multiply(n.numer);
			BigInteger nn = numer.multiply(n.denom);

			return LispRational.newRational(nn, nd);
		} else if(x instanceof LispInteger) {
			BigInteger n  = x.getBigInteger();
			BigInteger nd = n.multiply(denom);
			BigInteger nn = numer;

			return LispRational.newRational(nn, nd);
		} else {
			return super.div(x);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isEqualTo(net.morilib.lisp.LispNumber)
	 */
	@Override
	public boolean isEqualTo(LispNumber x) {
		if(x instanceof LispRational) {
			LispRational n = (LispRational)x;

			return (numer.equals(n.numer) && denom.equals(n.denom));
		} else if(x instanceof LispInteger) {
			return false;
		} else if(x instanceof LispDouble) {
			double d1 = (numer.doubleValue() / denom.doubleValue());

			return d1 == ((LispDouble)x).doubleValue();
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isLessThan(net.morilib.lisp.LispReal)
	 */
	@Override
	public boolean isLessThan(LispReal x) {
		if(x instanceof LispRational) {
			LispRational n = (LispRational)x;
			BigInteger n1 = numer.multiply(n.denom);
			BigInteger n2 = n.numer.multiply(denom);

			return n1.compareTo(n2) < 0;
		} else if(x instanceof LispInteger) {
			BigInteger n  = x.getBigInteger();
			BigInteger n2 = n.multiply(denom);

			return numer.compareTo(n2) < 0;
		} else if(x instanceof LispDouble) {
			double d1 = (numer.doubleValue() / denom.doubleValue());

			return d1 < x.getRealDouble();
		} else if(x instanceof LispAlternatingSeriesNumber) {
			return ((LispAlternatingSeriesNumber)x).value.compareTo(
					getRational()) > 0;
		}
		throw new IllegalArgumentException(x.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isMoreThan(net.morilib.lisp.LispReal)
	 */
	@Override
	public boolean isMoreThan(LispReal x) {
		if(x instanceof LispRational) {
			LispRational n = (LispRational)x;
			BigInteger n1 = numer.multiply(n.denom);
			BigInteger n2 = n.numer.multiply(denom);

			return n1.compareTo(n2) > 0;
		} else if(x instanceof LispInteger) {
			BigInteger n  = x.getBigInteger();
			BigInteger n2 = n.multiply(denom);

			return numer.compareTo(n2) > 0;
		} else if(x instanceof LispDouble) {
			double d1 = (numer.doubleValue() / denom.doubleValue());

			return d1 > x.getRealDouble();
		} else if(x instanceof LispAlternatingSeriesNumber) {
			return ((LispAlternatingSeriesNumber)x).value.compareTo(
					getRational()) < 0;
		}
		throw new IllegalArgumentException(x.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#mul(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber mul(LispNumber x) {
		if(x instanceof LispRational) {
			LispRational n = (LispRational)x;
			BigInteger nd = denom.multiply(n.denom);
			BigInteger nn = numer.multiply(n.numer);

			return LispRational.newRational(nn, nd);
		} else if(x instanceof LispInteger) {
			BigInteger n  = x.getBigInteger();
			BigInteger nd = denom;
			BigInteger nn = n.multiply(numer);

			return LispRational.newRational(nn, nd);
		} else {
			return super.mul(x);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#sub(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber sub(LispNumber x) {
		if(x instanceof LispRational) {
			LispRational n = (LispRational)x;
			BigInteger nd = denom.multiply(n.denom);
			BigInteger nn = numer.multiply(n.denom).subtract(
					n.numer.multiply(denom));

			return LispRational.newRational(nn, nd);
		} else if(x instanceof LispInteger) {
			BigInteger n  = x.getBigInteger();
			BigInteger nd = denom;
			BigInteger nn = numer.subtract(n.multiply(denom));

			return LispRational.newRational(nn, nd);
		} else {
			return super.sub(x);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#uminus()
	 */
	@Override
	public LispRational uminus() {
		return new LispRational(numer.negate(), denom);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#signum()
	 */
	public int signum() {
		return numer.signum();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#toInexact()
	 */
	public LispReal toInexact() {
		return new LispDouble(
				numer.doubleValue() / denom.doubleValue());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#getResult()
	 */
	@Override
	public String getResult() {
		return numer.toString() + "/" + denom.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#print()
	 */
	@Override
	public String print() {
		return numer.toString() + "/" + denom.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isRational()
	 */
	@Override
	public boolean isRational() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isReal()
	 */
	@Override
	public boolean isReal() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isExact()
	 */
	public boolean isExact() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toLispString(int)
	 */
	public LispString toLispString(int radix) {
		if(radix < 2 || radix > 36) {
			throw new IndexOutOfBoundsException(
					"radix is out of range");
		}

		return new LispString(
				numer.toString(radix) + "/" + denom.toString(radix));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isOne()
	 */
	public boolean isOne() {
		return numer.equals(denom);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getBigInteger()
	 */
	@Override
	public BigInteger getBigInteger() {
		return numer.divide(denom);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getInt()
	 */
	@Override
	public int getInt() {
		return getBigInteger().intValue();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getLong()
	 */
	public long getLong() {
		return getBigInteger().longValue();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getBigDecimal()
	 */
	public BigDecimal getBigDecimal() {
		BigDecimal n, d;

		n = new BigDecimal(numer);
		d = new BigDecimal(denom);
		return n.divide(d);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getRealDouble()
	 */
	@Override
	public double getRealDouble() {
		return numer.doubleValue() / denom.doubleValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getRealDecimal64()
	 */
	@Override
	public long getRealDecimal64() {
		return Decimal64.toDecimal(getBigDecimal());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	@Override
	public LispType getType() {
		return LispType.RATIONAL;
	}

	/**
	 * 
	 * @return
	 */
	public Rational toRational() {
		return Rational.valueOf(
				Integer2.valueOf(numer), Integer2.valueOf(denom));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRingElement#isUnit()
	 */
	public boolean isUnit() {
		return (numer.equals(BigInteger.ONE) &&
				denom.equals(BigInteger.ONE));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Calculatable#invert()
	 */
	public LispReal invert() {
		return new LispRational(denom, numer);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.algebra.RingElement#isZero()
	 */
	public boolean isZero() {
		return numer.equals(BigInteger.ZERO);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#doubleValue()
	 */
	public double doubleValue() {
		return numer.doubleValue() / denom.doubleValue();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#floatValue()
	 */
	public float floatValue() {
		return numer.floatValue() / denom.floatValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castInt()
	 */
	public int castInt() {
		return numer.divide(denom).intValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castLong()
	 */
	public long castLong() {
		return numer.divide(denom).longValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castInteger2()
	 */
	public Integer2 castInteger2() {
		return Integer2.valueOf(numer.divide(denom));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Floor()
	 */
	public Integer2 getInteger2Floor() {
		BigInteger[] r0 = numer.divideAndRemainder(denom);

		if(r0[1].equals(Integer2.ZERO)) {
			return Integer2.valueOf(r0[0]);
		} else if((numer.signum() > 0) ^ (denom.signum() > 0)) {
			return Integer2.valueOf(r0[0]);
		} else {
			return Integer2.valueOf(r0[0]).subtract(Integer2.ONE);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#intFloor()
	 */
	public int intFloor() {
		return getInteger2Floor().toInt();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#longFloor()
	 */
	public long longFloor() {
		return getInteger2Floor().toLong();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Ceil()
	 */
	public Integer2 getInteger2Ceil() {
		BigInteger[] r0 = numer.divideAndRemainder(denom);

		if(r0[1].equals(Integer2.ZERO)) {
			return Integer2.valueOf(r0[0]);
		} else if((numer.signum() > 0) ^ (denom.signum() > 0)) {
			return Integer2.valueOf(r0[0]).add(Integer2.ONE);
		} else {
			return Integer2.valueOf(r0[0]);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#intCeil()
	 */
	public int intCeil() {
		return getInteger2Ceil().toInt();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#longCeil()
	 */
	public long longCeil() {
		return getInteger2Ceil().toLong();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#isInteger()
	 */
	public boolean isInteger() {
		return denom.equals(Integer2.ONE);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getRational()
	 */
	public Rational getRational() {
		return toRational();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object x) {
		if(x instanceof LispRational) {
			LispRational n = (LispRational)x;

			return (numer.equals(n.numer) && denom.equals(n.denom));
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#floor()
	 */
	@Override
	public LispReal floor() {
		if(numer.signum() > 0) {
			return LispInteger.valueOf(numer.divide(denom));
		} else {
			return ceil().subtract(LispInteger.ONE);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#ceil()
	 */
	@Override
	public LispReal ceil() {
		if(numer.signum() > 0) {
			return floor().add(LispInteger.ONE);
		} else {
			return LispInteger.valueOf(numer.divide(denom));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#remainder(net.morilib.lisp.LispReal)
	 */
	@Override
	public LispReal remainder(LispReal r) {
		BigInteger n, d;

		if(!r.isExact()) {
			return new LispDouble(Math.IEEEremainder(
					doubleValue(), r.doubleValue()));
		} else if(r.isRational()) {
			d = denom.multiply(r.getDenominator());
			n = numer.multiply(r.getDenominator());
			n = n.remainder(r.getNumerator().multiply(denom));
			return LispRational.newRational(n, d);
		} else {
			return new LispDouble(Math.IEEEremainder(
					doubleValue(), r.doubleValue()));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isFinite()
	 */
	@Override
	public boolean isFinite() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int l = 17;

		l = 37 * l + numer.hashCode();
		l = 37 * l + denom.hashCode();
		return l;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return numer.toString() + "/" + denom.toString();
	}

}
