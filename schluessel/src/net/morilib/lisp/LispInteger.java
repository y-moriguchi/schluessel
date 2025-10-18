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
import java.util.NoSuchElementException;

import net.morilib.lisp.accessor.ILispRef;
import net.morilib.lisp.iterator.ILispIterator;
import net.morilib.lisp.math.LispOrdinalNumber;
import net.morilib.lisp.sos.LispType;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Inclementor;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public abstract class LispInteger extends LispExactReal
implements Inclementor<LispInteger>, LispOrdinalNumber,
ILispIterator, ILispRef {

	/**
	 * 
	 */
	public static final LispInteger FIXNUM_MAX =
			LispInteger.valueOf(Integer.MAX_VALUE);

	/**
	 * 
	 */
	public static final LispInteger FIXNUM_MIN =
			LispInteger.valueOf(Integer.MIN_VALUE);

	/**
	 * 
	 */
	public static final long FIXNUM_MAX_LONG = 2147483647;

	/**
	 * 
	 */
	public static final long FIXNUM_MIN_LONG = -2147483648;

	/**
	 * 
	 */
	public static final long FIXNUM_MASK = 0xffffffffl;

	/**
	 * 
	 */
	public static final int FIXNUM_WIDTH = 32;

	//
	private static final BigInteger INT_MAX =
			BigInteger.valueOf(Integer.MAX_VALUE);
	private static final BigInteger INT_MIN =
			BigInteger.valueOf(Integer.MIN_VALUE);
	private static final int FWSIZE = 1000;

	/**
	 * 
	 */
	public static final LispInteger ZERO = LispInteger.valueOf(0);

	/**
	 * 
	 */
	public static final LispInteger ONE  = LispInteger.valueOf(1);

	//
	private static LispInteger[] flyweightp = new LispInteger[FWSIZE];
	private static LispInteger[] flyweightn = new LispInteger[FWSIZE];

	/**
	 * 
	 * @param val
	 * @return
	 */
	public static LispInteger valueOf(BigInteger val) {
		if(val.compareTo(INT_MAX) > 0 || val.compareTo(INT_MIN) < 0) {
			return new LispBigInt(val);
		} else {
			return valueOf(val.intValue());
		}
	}

	/**
	 * 
	 * @param val
	 * @return
	 */
	public static LispInteger valueOf(int val) {
		if(flyweightp == null || flyweightn == null ||
				val >= FWSIZE || val <= -FWSIZE) {
			return new LispSmallInt(val);
		} else if(val > 0) {
			if(flyweightp[val] == null) {
				flyweightp[val] = new LispSmallInt(val);
			}
			return flyweightp[val];
		} else if(val < 0) {
			if(flyweightn[-val] == null) {
				flyweightn[-val] = new LispSmallInt(val);
			}
			return flyweightn[-val];
		} else {
			return LispInteger.ZERO;
		}
	}

	/**
	 * 
	 * @param val
	 * @return
	 */
	public static LispInteger valueOf(long val) {
		if(val > Integer.MAX_VALUE || val < Integer.MIN_VALUE) {
			return new LispBigInt(BigInteger.valueOf(val));
		} else {
			return valueOf((int)val);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#negate()
	 */
	@Override
	public abstract LispInteger uminus();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispInteger#abs()
	 */
	@Override
	public LispInteger abs() {
		return (signum() >= 0) ? this : negate(); 
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Negatable#negate()
	 */
	public LispInteger negate() {
		return uminus();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getNumerator()
	 */
	public BigInteger getNumerator() {
		return getBigInteger();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getDenominator()
	 */
	public BigInteger getDenominator() {
		return BigInteger.ONE;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isInteger()
	 */
	@Override
	public boolean isInteger() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isRational()
	 */
	@Override
	public boolean isRational() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isReal()
	 */
	@Override
	public boolean isReal() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isExact()
	 */
	public boolean isExact() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Inclimentor#getObject()
	 */
	public LispInteger getObject() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Inclimentor#getZero()
	 */
	public Inclementor<LispInteger> getZero() {
		return LispInteger.ZERO;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.Inclimentor#hasNext()
	 */
	public boolean hasNext() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#floor()
	 */
	@Override
	public LispReal floor() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#ceil()
	 */
	@Override
	public LispReal ceil() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	@Override
	public LispType getType() {
		return LispType.INTEGER;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.accessor.ILispRef#ref(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum ref(Datum arg, LispMessage mesg) {
		LispInteger k = SubrUtils.getInteger(arg, mesg);
		LispInteger s = abs();

		if(k.signum() < 0 || k.compareTo(s) >= 0) {
			throw mesg.getError("err.accessor.ref.outofrange", k);
		}
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#isTerminated()
	 */
	@Override
	public boolean isTerminated() {
		return signum() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#next()
	 */
	@Override
	public ILispIterator next() {
		if(signum() > 0) {
			return (LispInteger)subtract(ONE);
		} else if(signum() < 0) {
			return (LispInteger)add(ONE);
		} else {
			throw new NoSuchElementException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#getCurrentDatum()
	 */
	@Override
	public Datum getCurrentDatum() {
		return this;
	}

}
