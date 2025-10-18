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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/09
 */
public final class LispComplexInfinity extends LispComplex {

	//
	private LispComplexInfinity() {}

	//
	static final LispComplexInfinity INF = new LispComplexInfinity();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispNorm#normSquared()
	 */
	@Override
	public LispNumber normSquared() {
		return LispDouble.POSITIVE_INFINITY;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#uminus()
	 */
	@Override
	public LispComplex uminus() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#toExact()
	 */
	@Override
	public LispComplex toExact() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#toInexact()
	 */
	@Override
	public LispComplex toInexact() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#conjugate()
	 */
	@Override
	public LispComplex conjugate() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#angle()
	 */
	@Override
	public LispReal angle() {
		return LispDouble.NaN;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispQuaternion#getReal()
	 */
	@Override
	public LispReal getReal() {
		return LispDouble.NaN;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispQuaternion#getRealDouble()
	 */
	@Override
	public double getRealDouble() {
		return Double.NaN;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getImag()
	 */
	@Override
	public LispReal getImag() {
		return LispDouble.NaN;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getImagDouble()
	 */
	@Override
	public double getImagDouble() {
		return Double.NaN;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isExact()
	 */
	@Override
	public boolean isExact() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#norm()
	 */
	@Override
	public LispReal norm() {
		return LispDouble.POSITIVE_INFINITY;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#isNaN()
	 */
	@Override
	public boolean isNaN() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#isOne()
	 */
	@Override
	public boolean isOne() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#isEqualTo(net.morilib.lisp.LispNumber)
	 */
	@Override
	public boolean isEqualTo(LispNumber x) {
		return x == INF;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#add(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber add(LispNumber x) {
		if(!(x instanceof LispComplex)) {
			return LispDouble.NaN;
		} else if(x == INF) {
			return LispDouble.NaN;
		} else {
			return this;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#sub(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber sub(LispNumber x) {
		if(!(x instanceof LispComplex)) {
			return LispDouble.NaN;
		} else if(x == INF) {
			return LispDouble.NaN;
		} else {
			return this;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#mul(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber mul(LispNumber x) {
		if(!(x instanceof LispComplex)) {
			return LispDouble.NaN;
		} else if(x == INF) {
			return LispDouble.NaN;
		} else {
			return this;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#div(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber div(LispNumber x) {
		if(!(x instanceof LispComplex)) {
			return LispDouble.NaN;
		} else if(x == INF) {
			return LispDouble.NaN;
		} else {
			return this;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#pow(int)
	 */
	@Override
	public LispNumber pow(int n) {
		return LispDouble.NaN;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#inv()
	 */
	@Override
	public LispNumber inv() {
		return LispInteger.ZERO;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isFinite()
	 */
	@Override
	public boolean isFinite() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#getResult()
	 */
	@Override
	public String getResult() {
		return "+inf.0@";
	}

}
