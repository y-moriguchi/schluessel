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
package net.morilib.lisp.arith;

import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMath;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.math.constants.LispSqrt;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/05
 */
public class LispRatio extends Datum2 implements ILispRatio {

	/**
	 * 
	 */
	public static final LispRatio GOLDEN_RATIO = new LispRatio(
			LispInteger.ONE, LispSqrt.GOLDEN_NUMBER);

	/**
	 * 
	 */
	public static final LispRatio SILVER_RATIO = new LispRatio(
			LispInteger.ONE,
			(LispReal)LispMath.sqrt(LispInteger.valueOf(2)));

	//
	LispReal val1, val2;

	/**
	 * 
	 * @param val1
	 * @param val2
	 */
	public LispRatio(LispReal val1, LispReal val2) {
		if(val1.isZero() || val2.isZero()) {
			throw new ArithmeticException();
		}
		this.val1 = val1;
		this.val2 = val2;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.arith.ILispRatio#getVal1()
	 */
	public LispReal getVal1() {
		return val1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.arith.ILispRatio#getVal2()
	 */
	public LispReal getVal2() {
		return val2;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.arith.ILispRatio#getRatioValue()
	 */
	public LispReal getRatioValue() {
		return val1.divide(val2);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.arith.ILispRatio#toValue(net.morilib.lisp.LispReal)
	 */
	public LispReal toValue(LispReal r) {
		return val2.divide(val1).multiply(r);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.arith.ILispRatio#invertedValue(net.morilib.lisp.LispReal)
	 */
	public LispReal invertedValue(LispReal r) {
		return val1.divide(val2).multiply(r);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append(val1).append(":").append(val2);
	}

}
