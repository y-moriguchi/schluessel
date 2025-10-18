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
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispRational;
import net.morilib.math.constants.ExpNumber;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/04
 */
public class LispExp extends LispAlternatingSeriesNumber {

	/**
	 * 
	 */
	public static final LispExp NAPIER =
		new LispExp(Rational.ONE, Rational.ONE, Rational.ZERO);

	//
	private Rational val, _scale, _shift;

	//
	LispExp(Rational x, Rational scale, Rational shift) {
		super(new ExpNumber(x, scale, shift));
		val = x;
		_scale = scale;
		_shift = shift;
	}

	/**
	 * 
	 * @param x
	 * @param scale
	 * @return
	 */
	public static LispExactReal valueOf(Rational x, Rational scale,
			Rational shift) {
		if(scale.isZero()) {
			return LispRational.valueOf(shift);
		} else if(x.isZero()) {
			return LispRational.valueOf(shift.add(Rational.ONE));
		} else {
			return new LispExp(x, scale, shift);
		}
	}

	/**
	 * @return the x
	 */
	public Rational getX() {
		return val;
	}

	/**
	 * @return the scale
	 */
	public Rational getScale() {
		return _scale;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispAlternatingSeriesNumber#prototype(net.morilib.lang.number.Rational)
	 */
	@Override
	public LispExactReal prototype(Rational scale, Rational shift) {
		return valueOf(val, scale, shift);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispExactReal#uminus()
	 */
	@Override
	public LispExactReal uminus() {
		return new LispExp(val, _scale.negate(), _shift.negate());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispAlternatingSeriesNumber#add(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber add(LispNumber x) {
		LispExp e;

		if(!(x instanceof LispExp)) {
			return super.add(x);
		} else if(val.equals((e = (LispExp)x).val)) {
			return prototype(_scale.add(e._scale),
					_shift.add(e._shift));
		} else {
			return toInexact().add(e.toInexact());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispAlternatingSeriesNumber#sub(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber sub(LispNumber x) {
		LispExp e;

		if(!(x instanceof LispExp)) {
			return super.add(x);
		} else if(val.equals((e = (LispExp)x).val)) {
			return prototype(_scale.subtract(e._scale),
					_shift.subtract(e._shift));
		} else {
			return toInexact().sub(e.toInexact());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispAlternatingSeriesNumber#mul(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber mul(LispNumber x) {
		LispExp e;

		if(!(x instanceof LispExp)) {
			return super.add(x);
		} else if(_shift.isZero() &&
				(e = (LispExp)x)._shift.isZero()) {
			return valueOf(val.add(e.val), _scale.multiply(e._scale),
					_shift);
		} else {
			return toInexact().sub(x.toInexact());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispAlternatingSeriesNumber#div(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber div(LispNumber x) {
		LispExp e;

		if(!(x instanceof LispExp)) {
			return super.add(x);
		} else if(_shift.isZero() &&
				(e = (LispExp)x)._shift.isZero()) {
			return valueOf(val.subtract(e.val),
					_scale.divide(e._scale), _shift);
		} else {
			return toInexact().sub(x.toInexact());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isFinite()
	 */
	@Override
	public boolean isFinite() {
		return true;
	}

}
