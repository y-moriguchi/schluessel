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
import net.morilib.lisp.math.InvalidOrdinalNumberException;
import net.morilib.lisp.math.LispOrdinalNumber;
import net.morilib.util.Inclementor;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class LispBigInt extends LispInteger
implements JavaObjective, java.io.Serializable {

	//
	private static final long serialVersionUID = 3485888605431065348L;

	//
	private BigInteger value;

	//
	/*package*/ LispBigInt(BigInteger value) {
		if(value == null) {
			throw new NullPointerException();
		}
		this.value = value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#add(net.morilib.lisp.LispNumber)
	 */
	public LispNumber add(LispNumber x) {
		if(x instanceof LispInteger) {
			return LispInteger.valueOf(value.add(x.getBigInteger()));
		} else if(x instanceof LispRational) {
			LispRational r = (LispRational)x;
			BigInteger nd = r.getDenominator();
			BigInteger nn = value.multiply(
					r.getDenominator()).add(r.getNumerator());

			return LispRational.newRational(nn, nd);
		} else {
			return super.add(x);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#div(net.morilib.lisp.LispNumber)
	 */
	public LispNumber div(LispNumber x) {
		if(x instanceof LispInteger) {
			return LispRational.newRational(value, x.getBigInteger());
		} else if(x instanceof LispRational) {
			LispRational r = (LispRational)x;
			BigInteger nd = r.getNumerator();
			BigInteger nn = value.multiply(r.getDenominator());

			return LispRational.newRational(nn, nd);
		} else {
			return super.div(x);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#mul(net.morilib.lisp.LispNumber)
	 */
	public LispNumber mul(LispNumber x) {
		if(x instanceof LispInteger) {
			return LispInteger.valueOf(
					value.multiply(x.getBigInteger()));
		} else if(x instanceof LispRational) {
			LispRational r = (LispRational)x;
			BigInteger nd = r.getDenominator();
			BigInteger nn = value.multiply(r.getNumerator());

			return LispRational.newRational(nn, nd);
		} else if(x instanceof LispIrrational) {
			return x.mul(this);
		} else {
			return super.mul(x);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#sub(net.morilib.lisp.LispNumber)
	 */
	public LispNumber sub(LispNumber x) {
		if(x instanceof LispInteger) {
			return LispInteger.valueOf(
					value.subtract(x.getBigInteger()));
		} else if(x instanceof LispRational) {
			LispRational r = (LispRational)x;
			BigInteger nd = r.getDenominator();
			BigInteger nn = value.multiply(
					r.getDenominator()).subtract(r.getNumerator());

			return LispRational.newRational(nn, nd);
		} else {
			return super.sub(x);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#uminus()
	 */
	public LispInteger uminus() {
		return new LispBigInt(value.negate());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isEqualTo(net.morilib.lisp.LispNumber)
	 */
	public boolean isEqualTo(LispNumber x) {
		if(x instanceof LispSmallInt) {
			return false;
		} else if(x instanceof LispBigInt) {
			return value.equals(x.getBigInteger());
		} else if(x instanceof LispRational) {
			return false;
		} else if(x instanceof LispDouble) {
			return value.doubleValue() == ((LispDouble)x).doubleValue();
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#isLessThan(net.morilib.lisp.LispReal)
	 */
	public boolean isLessThan(LispReal x) {
		if(x instanceof LispInteger) {
			return value.compareTo(x.getBigInteger()) < 0;
		} else if(x instanceof LispRational) {
			LispRational r = (LispRational)x;
			BigInteger n1 = value.multiply(r.getDenominator());
			BigInteger n2 = r.getNumerator();

			return n1.compareTo(n2) < 0;
		} else if(x instanceof LispDouble) {
			return value.doubleValue() < x.getRealDouble();
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
	public boolean isMoreThan(LispReal x) {
		if(x instanceof LispInteger) {
			return value.compareTo(x.getBigInteger()) > 0;
		} else if(x instanceof LispRational) {
			LispRational r = (LispRational)x;
			BigInteger n1 = value.multiply(r.getDenominator());
			BigInteger n2 = r.getNumerator();

			return n1.compareTo(n2) > 0;
		} else if(x instanceof LispDouble) {
			return value.doubleValue() > x.getRealDouble();
		} else if(x instanceof LispAlternatingSeriesNumber) {
			return ((LispAlternatingSeriesNumber)x).value.compareTo(
					getRational()) < 0;
		}
		throw new IllegalArgumentException(x.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#signum()
	 */
	public int signum() {
		return value.signum();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#toInexact()
	 */
	public LispReal toInexact() {
		return new LispDouble(value.doubleValue());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#print()
	 */
	public String print() {
		return value.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#getResult()
	 */
	public String getResult() {
		return value.toString();
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

		return new LispString(value.toString(radix));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isOne()
	 */
	public boolean isOne() {
		return value.equals(BigInteger.ONE);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getBigInteger()
	 */
	@Override
	public BigInteger getBigInteger() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getInt()
	 */
	@Override
	public int getInt() {
		return value.intValue();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getLong()
	 */
	public long getLong() {
		return value.longValue();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getBigDecimal()
	 */
	public BigDecimal getBigDecimal() {
		return new BigDecimal(value);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getRealDouble()
	 */
	@Override
	public double getRealDouble() {
		return value.doubleValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getRealDecimal64()
	 */
	@Override
	public long getRealDecimal64() {
		return Decimal64.toDecimal(new BigDecimal(value));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#equalIncliment(net.morilib.util.Inclementor)
	 */
	public boolean equalIncliment(Inclementor<?> i) {
		if(i instanceof LispBigInt) {
			return value.equals(((LispBigInt)i).value);
		} else {
			return i.equalInt(value);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#equalInt(int)
	 */
	public boolean equalInt(int i) {
		return value.equals(BigInteger.valueOf(i));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#equalInt(java.math.BigInteger)
	 */
	public boolean equalInt(BigInteger i) {
		return value.equals(i);
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#suc()
	 */
	public Inclementor<LispInteger> suc() {
		return LispInteger.valueOf(value.add(BigInteger.ONE));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#suc(int)
	 */
	public Inclementor<LispInteger> suc(int step) {
		return LispInteger.valueOf(
				value.add(BigInteger.valueOf(step)));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.util.Inclementor#toInt()
	 */
	public int toInt() {
		return value.intValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispExactReal#toRational()
	 */
	@Override
	public Rational toRational() {
		return Rational.valueOf(Integer2.valueOf(value), Integer2.ONE);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.UnitaryRingElement#isUnit()
	 */
	public boolean isUnit() {
		return value.equals(BigInteger.ONE);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Calculatable#invert()
	 */
	public LispReal invert() {
		return LispRational.newRational(BigInteger.ONE, value);
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
		return Integer2.valueOf(value);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#intFloor()
	 */
	public int intFloor() {
		return value.intValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#longFloor()
	 */
	public long longFloor() {
		return value.longValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Floor()
	 */
	public Integer2 getInteger2Floor() {
		return Integer2.valueOf(value);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#intCeil()
	 */
	public int intCeil() {
		return value.intValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#longCeil()
	 */
	public long longCeil() {
		return value.longValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getInteger2Ceil()
	 */
	public Integer2 getInteger2Ceil() {
		return Integer2.valueOf(value);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#getRational()
	 */
	public Rational getRational() {
		return toRational();
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
	 * @see net.morilib.lisp.Atom#toObject()
	 */
	public Object toObject() {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.LispOrdinalNumber#inclement()
	 */
	public LispOrdinalNumber inclement() {
		if(value.signum() < 0) {
			throw new InvalidOrdinalNumberException();
		}
		return LispInteger.valueOf(value.add(BigInteger.ONE));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#remainder(net.morilib.lisp.LispReal)
	 */
	@Override
	public LispReal remainder(LispReal r) {
		BigInteger n, d;

		if(!r.isExact()) {
			return new LispDouble(Math.IEEEremainder(
					value.doubleValue(), r.doubleValue()));
		} else if(r instanceof LispInteger) {
			n = value.remainder(r.getBigInteger());
			return LispInteger.valueOf(n);
		} else if(r instanceof LispRational) {
			d = r.getDenominator();
			n = value.remainder(r.getNumerator());
			return LispRational.newRational(n, d);
		} else {
			return new LispDouble(Math.IEEEremainder(
					value.doubleValue(), r.doubleValue()));
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
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object x) {
		if(x instanceof LispBigInt) {
			return value.equals(((LispBigInt)x).value);
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
