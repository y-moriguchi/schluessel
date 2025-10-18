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
import net.morilib.lisp.LispInexactReal;
import net.morilib.lisp.LispInteger;
import net.morilib.math.constants.AlternatingSeriesNumber;
import net.morilib.math.constants.ChampernowneConstant;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/02
 */
public final class LispChampernowneConstant
extends LispAlternatingSeriesNumber {

	/**
	 * 
	 */
	public static final LispChampernowneConstant CONSTANT =
		new LispChampernowneConstant(ChampernowneConstant.INSTANCE);

	//
	private LispChampernowneConstant(AlternatingSeriesNumber e) {
		super(e);
	}

	//
	private LispChampernowneConstant(Rational scale, Rational shift) {
		super(new ChampernowneConstant(scale, shift));
	}

	/**
	 * 
	 * @param scale
	 * @return
	 */
	public static LispExactReal scaled(Rational scale,
			Rational shift) {
		return scale.isZero() ?
				LispInteger.ZERO :
					new LispChampernowneConstant(scale, shift);
	}

	/**
	 * 
	 * @param scale
	 * @return
	 */
	public static LispExactReal scaled(LispInexactReal scale,
			LispInexactReal shift) {
		return scale.isZero() ?
				LispInteger.ZERO :
					new LispChampernowneConstant(scale.getRational(),
							shift.getRational());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispExactReal#uminus()
	 */
	@Override
	public LispChampernowneConstant uminus() {
		return new LispChampernowneConstant(
				value.getScale().negate(), value.getShift().negate());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispAlternatingSeriesNumber#prototype(net.morilib.lang.number.Rational)
	 */
	@Override
	public LispExactReal prototype(Rational scale, Rational shift) {
		return scale.isZero() ?
				LispInteger.ZERO :
					new LispChampernowneConstant(scale, shift);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isFinite()
	 */
	@Override
	public boolean isFinite() {
		return true;
	}

}
