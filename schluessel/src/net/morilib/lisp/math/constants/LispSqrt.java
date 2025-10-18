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

import java.math.BigInteger;

import net.morilib.lang.number.Integer2;
import net.morilib.lang.number.Rational;
import net.morilib.lisp.LispAlternatingSeriesNumber;
import net.morilib.lisp.LispComplex;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispExactReal;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispRational;
import net.morilib.math.constants.SqrtNumber;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/04
 */
public class LispSqrt extends LispAlternatingSeriesNumber {

	/**
	 * 
	 */
	public static final LispSqrt GOLDEN_NUMBER =
		new LispSqrt(Rational.valueOf(5),
				Rational.valueOf(1, 2), Rational.valueOf(1, 2));

	/**
	 * 
	 */
	public static final LispSqrt SILVER_NUMBER =
		new LispSqrt(Rational.valueOf(2), Rational.ONE, Rational.ONE);

	//
	private Rational val, _scale, _shift;

	//
	LispSqrt(Rational x, Rational scale, Rational shift) {
		super(new SqrtNumber(x, scale, shift));
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
	public static LispComplex valueOf(Rational x, Rational scale,
			Rational shift) {
		if(scale.isZero() || x.isZero()) {
			return LispRational.valueOf(shift);
		} else if(x.signum() < 0) {
			return LispComplex.newComplex(LispInteger.ZERO,
					new LispDouble(Math.sqrt(-x.doubleValue())));
		} else {
			return new LispSqrt(x, scale, shift);
		}
	}

	/**
	 * @param b
	 * @param one
	 * @param zero
	 * @return
	 */
	public static LispComplex valueOf(BigInteger b, Rational scale,
			Rational shift) {
		return valueOf(
				Rational.valueOf(Integer2.valueOf(b), Integer2.ONE),
				scale, shift);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispAlternatingSeriesNumber#prototype(net.morilib.lang.number.Rational, net.morilib.lang.number.Rational)
	 */
	@Override
	public LispExactReal prototype(Rational scale, Rational shift) {
		return (LispExactReal)valueOf(val, scale, shift);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispExactReal#uminus()
	 */
	@Override
	public LispExactReal uminus() {
		return new LispSqrt(val, _scale.negate(), _shift.negate());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispAlternatingSeriesNumber#add(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber add(LispNumber x) {
		LispSqrt q;

		if(!(x instanceof LispSqrt)) {
			return super.add(x);
		} else if(val.equals((q = (LispSqrt)x).val)) {
			return valueOf(val, _scale.add(q._scale),
					_shift.add(_shift));
		} else {
			return new LispDouble(doubleValue() + q.doubleValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispAlternatingSeriesNumber#sub(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber sub(LispNumber x) {
		LispSqrt q;

		if(!(x instanceof LispSqrt)) {
			return super.sub(x);
		} else if(val.equals((q = (LispSqrt)x).val)) {
			return valueOf(val, _scale.subtract(q._scale),
					_shift.subtract(_shift));
		} else {
			return new LispDouble(doubleValue() - q.doubleValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispAlternatingSeriesNumber#mul(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber mul(LispNumber x) {
		LispSqrt q;

		if(!(x instanceof LispSqrt)) {
			return super.mul(x);
		} else if(val.equals((q = (LispSqrt)x).val)) {
			return valueOf(val,
					_scale.multiply(q._shift).add(
							_shift.multiply(q._scale)),
					_scale.multiply(q._scale).multiply(
							val).add(_shift.multiply(q._shift)));
		} else {
			return new LispDouble(doubleValue() * q.doubleValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispAlternatingSeriesNumber#div(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber div(LispNumber x) {
		LispSqrt q;
		Rational d, p;

		if(!(x instanceof LispSqrt)) {
			return super.div(x);
		} else if(val.equals((q = (LispSqrt)x).val)) {
			d = q._scale.multiply(q._scale).multiply(val).subtract(
					q._shift.multiply(q._shift));
			p = q._shift.negate();
			return valueOf(val,
					_scale.multiply(p).add(
							_shift.multiply(q._scale)).divide(d),
					_scale.multiply(q._scale).multiply(
							val).add(_shift.multiply(p)).divide(d));
		} else {
			return new LispDouble(doubleValue() / q.doubleValue());
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
