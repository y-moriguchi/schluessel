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
 * @author MORIGUCHI, Yuichiro 2011/10/19
 */
public final class LispComplexImpl extends LispComplex {

	//
	private LispReal real, imag;

	//
	/*package*/ LispComplexImpl(LispReal r, LispReal i) {
		this.real = r;
		this.imag = i;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getRealDouble()
	 */
	public double getRealDouble() {
		return real.getRealDouble();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getReal()
	 */
	public LispReal getReal() {
		return real;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getImagDouble()
	 */
	public double getImagDouble() {
		return imag.getRealDouble();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getImag()
	 */
	public LispReal getImag() {
		return imag;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#toExact()
	 */
	@Override
	public LispComplex toExact() {
		return newComplex(getReal().toExact(), getImag().toExact());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#toInexact()
	 */
	@Override
	public LispComplex toInexact() {
		return newComplex(
				getReal().toInexact(), getImag().toInexact());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#conjugate()
	 */
	@Override
	public LispComplex conjugate() {
		return new LispComplexImpl(getReal(), getImag().uminus());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#uminus()
	 */
	@Override
	public LispComplex uminus() {
		return new LispComplexImpl(
				getReal().uminus(), getImag().uminus());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isExact()
	 */
	@Override
	public boolean isExact() {
		return getReal().isExact();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#norm()
	 */
	@Override
	public LispReal norm() {
		return (LispReal)LispMath.sqrt(
				(LispReal)getReal().mul(getReal())
				.add(getImag().mul(getImag())));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#norm()
	 */
	@Override
	public LispReal normSquared() {
		return ((LispReal)getReal().mul(getReal())
				.add(getImag().mul(getImag())));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispComplex#angle()
	 */
	@Override
	public LispReal angle() {
		return new LispDouble(Math.atan2(
				getImagDouble(), getRealDouble()));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isFinite()
	 */
	@Override
	public boolean isFinite() {
		return real.isFinite() && imag.isFinite();
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object x) {
		if(x instanceof LispComplexImpl) {
			LispComplexImpl c = (LispComplexImpl)x;

			return real.equals(c.real) && imag.equals(c.imag);
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int l = 17;

		l = 37 * (l + real.hashCode());
		l = 37 * (l + imag.hashCode());
		return l;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#getResult()
	 */
	@Override
	public String getResult() {
		if(getImag().signum() < 0 || getImag().isInfinity()) {
			return (LispUtils.getResult(getReal()) +
					LispUtils.getResult(getImag()) + "i");
		} else {
			return (LispUtils.getResult(getReal()) + "+" +
					LispUtils.getResult(getImag()) + "i");
		}
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
