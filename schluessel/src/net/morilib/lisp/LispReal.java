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

import java.math.BigInteger;

import net.morilib.lang.number.Integer2;
import net.morilib.lang.number.NumericalFieldElement;
import net.morilib.lang.number.Rational;
import net.morilib.lisp.math.ILispQuantity;
import net.morilib.lisp.math.ILispQuantityFactory;
import net.morilib.lisp.math.constants.LispPi;
import net.morilib.lisp.sos.LispType;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public abstract class LispReal extends LispComplex
implements Comparable<LispReal>, NumericalFieldElement<LispReal>,
ILispQuantity {

	/**
	 * 
	 */
	public static final ILispQuantityFactory
	FACTORY = new ILispQuantityFactory() {

		public ILispQuantity getInstance(LispReal r) {
			return r;
		}

	};

	//
	private static final Integer2 _I_MAX =
		Integer2.valueOf(Integer.MAX_VALUE);
	private static final Integer2 _I_MIN =
		Integer2.valueOf(Integer.MIN_VALUE);
	private static final Integer2 _S_MAX =
		Integer2.valueOf(Short.MAX_VALUE);
	private static final Integer2 _S_MIN =
		Integer2.valueOf(Short.MIN_VALUE);
	private static final Integer2 _B_MAX =
		Integer2.valueOf(Byte.MAX_VALUE);
	private static final Integer2 _B_MIN =
		Integer2.valueOf(Byte.MIN_VALUE);
	private static final Integer2 _L_MAX =
		Integer2.valueOf(Long.MAX_VALUE);
	private static final Integer2 _L_MIN =
		Integer2.valueOf(Long.MIN_VALUE);

	private static final Integer2 _I_MAXU =
		Integer2.valueOf(0xffffffffl);
	private static final Integer2 _S_MAXU =
		Integer2.valueOf(0xffff);
	private static final Integer2 _B_MAXU =
		Integer2.valueOf(0xff);
	private static final Integer2 _L_MAXU =
		Integer2.valueOf(new BigInteger("ffffffffffffffff", 16));

	/**
	 * 
	 * @param r1
	 * @param r2
	 * @return
	 */
	public static LispReal rationalize(LispReal r1, LispReal r2) {
		LispReal res;

		if(r1.isNaN() || r2.isNaN()) {
			return LispDouble.NaN;
		} else if(r1.isInfinity()) {
			return r2.isInfinity() ? LispDouble.NaN : r1;
		} else if(r2.isInfinity()) {
			return r1.isInfinity() ?
					LispDouble.NaN : LispDouble.ZERO;
		} else {
			res = LispRational.valueOf(Rational.rationalize(
					r1.toExact().toRational(),
					r2.toExact().toRational()));
			if(r1.isExact() && r2.isExact()) {
				return res;
			} else {
				return res.toInexact();
			}
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getReal()
	 */
	public LispReal getReal() {
		return this;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getImagDouble()
	 */
	public double getImagDouble() {
		return 0.0;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getImag()
	 */
	public LispReal getImag() {
		if(isNaN()) {
			return LispDouble.NaN;
		} else if(isExact()) {
			return LispInteger.ZERO;
		} else {
			return LispDouble.ZERO;
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(LispReal o) {
		return isLessThan(o) ? -1 : (isMoreThan(o) ? 1 : 0);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public abstract boolean isLessThan(LispReal x);

	/**
	 * 
	 * @param x
	 * @return
	 */
	public abstract boolean isMoreThan(LispReal x);

	/**
	 * 
	 * @return
	 */
	public abstract int signum();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toExact()
	 */
	@Override
	public abstract LispExactReal toExact();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toInexact()
	 */
	@Override
	public abstract LispReal toInexact();

	/**
	 * 
	 * @return
	 */
	public abstract boolean isInfinity();

	/**
	 * 
	 * @return
	 */
	public LispReal abs() {
		return (signum() >= 0) ? this : negate(); 
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	@Override
	public LispType getType() {
		return LispType.REAL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Negatable#negate()
	 */
	public LispReal negate() {
		return (LispReal)this.uminus();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Dividable#divide(java.lang.Object)
	 */
	public LispReal divide(LispReal x) {
		return (LispReal)div(x);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#multiply(java.lang.Object)
	 */
	public LispReal multiply(LispReal x) {
		return (LispReal)mul(x);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Subtractable#subtract(java.lang.Object)
	 */
	public LispReal subtract(LispReal x) {
		return (LispReal)sub(x);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#add(java.lang.Object)
	 */
	public LispReal add(LispReal x) {
		return (LispReal)add((LispNumber)x);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castByte()
	 */
	public byte castByte() {
		return (byte)castInt();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.number.NumericalRingElement#castShort()
	 */
	public short castShort() {
		return (short)castInt();
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#byteFloor()
	 */
	public byte byteFloor() {
		return (byte)intFloor();
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#shortFloor()
	 */
	public short shortFloor() {
		return (short)intFloor();
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#byteCeil()
	 */
	public byte byteCeil() {
		return (byte)intCeil();
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#shortCeil()
	 */
	public short shortCeil() {
		return (short)intCeil();
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#isByteValue()
	 */
	public boolean inByteRange() {
		Integer2 f = getInteger2Floor();
		Integer2 c = getInteger2Ceil();

		return c.compareTo(_B_MAX) <= 0 && f.compareTo(_B_MIN) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#isShortValue()
	 */
	public boolean inShortRange() {
		Integer2 f = getInteger2Floor();
		Integer2 c = getInteger2Ceil();

		return c.compareTo(_S_MAX) <= 0 && f.compareTo(_S_MIN) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#isIntValue()
	 */
	public boolean inIntRange() {
		Integer2 f = getInteger2Floor();
		Integer2 c = getInteger2Ceil();

		return c.compareTo(_I_MAX) <= 0 && f.compareTo(_I_MIN) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#isLongValue()
	 */
	public boolean inLongRange() {
		Integer2 f = getInteger2Floor();
		Integer2 c = getInteger2Ceil();

		return c.compareTo(_L_MAX) <= 0 && f.compareTo(_L_MIN) >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#isByteValue()
	 */
	public boolean inUnsignedByteRange() {
		Integer2 f = getInteger2Floor();
		Integer2 c = getInteger2Ceil();

		return c.compareTo(_B_MAXU) <= 0 && f.signum() >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#isShortValue()
	 */
	public boolean inUnsignedShortRange() {
		Integer2 f = getInteger2Floor();
		Integer2 c = getInteger2Ceil();

		return c.compareTo(_S_MAXU) <= 0 && f.signum() >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#isIntValue()
	 */
	public boolean inUnsignedIntRange() {
		Integer2 f = getInteger2Floor();
		Integer2 c = getInteger2Ceil();

		return c.compareTo(_I_MAXU) <= 0 && f.signum() >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.number.Numeric#isLongValue()
	 */
	public boolean inUnsignedLongRange() {
		Integer2 f = getInteger2Floor();
		Integer2 c = getInteger2Ceil();

		return c.compareTo(_L_MAXU) <= 0 && f.signum() >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#isNaN()
	 */
	public abstract boolean isNaN();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isOne()
	 */
	public abstract boolean isOne();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#uminus()
	 */
	public abstract LispReal uminus();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#getResult()
	 */
	public abstract String getResult();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#norm2()
	 */
	@Override
	public LispReal norm() {
		return (signum() < 0) ? uminus() : this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#norm2()
	 */
	@Override
	public LispReal normSquared() {
		return norm().multiply(norm());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#conjugate()
	 */
	@Override
	public LispReal conjugate() {
		return this;
	}

	/**
	 * 
	 * @return
	 */
	public abstract LispReal floor();

	/**
	 * 
	 * @return
	 */
	public abstract LispReal ceil();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.ILispQuantity#factory()
	 */
	public ILispQuantityFactory factory() {
		return FACTORY;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#angle()
	 */
	@Override
	public final LispReal angle() {
		if(getReal().signum() < 0) {
			return getReal().isExact() ?
					LispPi.PI : new LispDouble(Math.PI);
		} else {
			return getReal().isExact() ?
					LispInteger.ZERO : LispDouble.ZERO;
		}
	}

	/**
	 * 
	 * @param r
	 * @return
	 */
	public abstract LispReal remainder(LispReal r);

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public abstract int hashCode();

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public abstract boolean equals(Object o);

}
