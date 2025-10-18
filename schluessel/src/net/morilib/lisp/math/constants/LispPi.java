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
package net.morilib.lisp.math.constants;

import net.morilib.lang.number.Rational;
import net.morilib.lisp.LispAlternatingSeriesNumber;
import net.morilib.lisp.LispExactReal;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispRational;
import net.morilib.lisp.LispReal;
import net.morilib.math.constants.Pi;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/26
 */
public final class LispPi extends LispAlternatingSeriesNumber {

	/**
	 * 
	 */
	public static final LispPi PI = new LispPi(Pi.PI);

	/**
	 * 
	 */
	public static final LispPi ONE_DEGREE = new LispPi(Pi.ONE_DEGREE);

	//
	private LispPi(Pi pi) {
		super(pi);
	}

	//
	private LispPi(Rational scale, Rational shift) {
		super(new Pi(scale, shift));
	}

	/**
	 * 
	 * @param scale
	 * @return
	 */
	public static LispReal scaled(LispReal scale, LispReal shift) {
		return scale.isZero() ?
				LispInteger.ZERO :
					new LispPi(scale.getRational(),
							shift.getRational());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispExactReal#uminus()
	 */
	@Override
	public LispPi uminus() {
		return new LispPi(value.getScale().negate(),
				value.getShift().negate());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispAlternatingSeriesNumber#prototype(net.morilib.lang.number.Rational)
	 */
	@Override
	public LispExactReal prototype(Rational scale, Rational shift) {
		return scale.isZero() ?
				LispRational.valueOf(shift) : new LispPi(scale, shift);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isFinite()
	 */
	@Override
	public boolean isFinite() {
		return true;
	}

}
