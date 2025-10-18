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
package net.morilib.lisp;

import net.morilib.lisp.math.constants.LispPi;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/19
 */
public final class LispComplexPolar extends LispComplex {

	//
	private LispReal radius, angle;

	//
	/*package*/ LispComplexPolar(LispReal r, LispReal a) {
		this.radius = r;
		this.angle  = a;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getRealDouble()
	 */
	public double getRealDouble() {
		return (radius.getRealDouble() *
				Math.cos(angle.getRealDouble()));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getReal()
	 */
	public LispReal getReal() {
		return new LispDouble(getRealDouble());
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getImagDouble()
	 */
	public double getImagDouble() {
		return (radius.getRealDouble() *
				Math.sin(angle.getRealDouble()));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getImag()
	 */
	public LispReal getImag() {
		return new LispDouble(getImagDouble());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#toExact()
	 */
	@Override
	public LispComplex toExact() {
		return new LispComplexPolar(
				radius.toExact(), angle.toExact());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#toInexact()
	 */
	@Override
	public LispComplex toInexact() {
		return new LispComplexPolar(
				radius.toInexact(), angle.toInexact());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#conjugate()
	 */
	@Override
	public LispComplex conjugate() {
		return new LispComplexPolar(radius, angle.uminus());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#uminus()
	 */
	@Override
	public LispComplex uminus() {
		return new LispComplexPolar(
				getReal().uminus(), LispPi.PI.subtract(angle));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#add(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber add(LispNumber x) {
		LispComplexPolar p;

		if(!(x instanceof LispComplexPolar)) {
			return super.add(x);
		} else if((p = (LispComplexPolar)x).angle.isEqualTo(angle)) {
			return newPolar(radius.add(p.radius), angle);
		} else if(angle.subtract(p.angle).abs().equals(LispPi.PI)) {
			return newPolar(radius.subtract(p.radius), angle);
		} else {
			return super.add(x);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#add(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber sub(LispNumber x) {
		LispComplexPolar p;

		if(!(x instanceof LispComplexPolar)) {
			return super.add(x);
		} else if((p = (LispComplexPolar)x).angle.isEqualTo(angle)) {
			return newPolar(radius.subtract(p.radius), angle);
		} else if(angle.subtract(p.angle).abs().equals(LispPi.PI)) {
			return newPolar(radius.add(p.radius), angle);
		} else {
			return super.add(x);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#mul(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber mul(LispNumber x) {
		LispComplexPolar p;

		if(!(x instanceof LispComplexPolar)) {
			return super.mul(x);
		} else if((p = (LispComplexPolar)x).isNaN()) {
			return LispDouble.NaN;
		} else if(p.radius.isZero()) {
			return isExact() && x.isExact() ?
					LispInteger.ZERO : LispDouble.ZERO;
		} else {
			return newPolar(radius.multiply(p.radius),
					angle.add(p.angle));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#mul(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber div(LispNumber x) {
		LispComplexPolar p;

		if(!(x instanceof LispComplexPolar)) {
			return super.mul(x);
		} else if((p = (LispComplexPolar)x).isNaN()) {
			return LispDouble.NaN;
		} else if(p.radius.isZero()) {
			// return LispComplex.INFINITY;
			return LispDouble.NaN;
		} else {
			return newPolar(radius.divide(p.radius),
					angle.subtract(p.angle));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isExact()
	 */
	@Override
	public boolean isExact() {
		return radius.isExact() && angle.isExact();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#norm()
	 */
	@Override
	public LispReal norm() {
		return radius;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#norm()
	 */
	@Override
	public LispReal normSquared() {
		return radius.multiply(radius);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#angle()
	 */
	@Override
	public LispReal angle() {
		return angle;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isFinite()
	 */
	@Override
	public boolean isFinite() {
		return radius.isFinite() && angle.isFinite();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object x) {
		if(x instanceof LispComplexPolar) {
			LispComplexPolar c = (LispComplexPolar)x;

			return radius.equals(c.radius) && angle.equals(c.angle);
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int l = 17;

		l = 37 * (l + radius.hashCode());
		l = 37 * (l + angle.hashCode());
		return l;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#getResult()
	 */
	@Override
	public String getResult() {
		return (LispUtils.getResult(radius) + "@" +
				LispUtils.getResult(angle));
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#print()
	 */
	@Override
	public String print() {
		return getResult();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toString()
	 */
	public String toString() {
		return getResult();
	}

}
