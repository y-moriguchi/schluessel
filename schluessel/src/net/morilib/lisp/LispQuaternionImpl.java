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

import net.morilib.lang.Hashes;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/20
 */
public class LispQuaternionImpl extends LispQuaternion
implements java.io.Serializable {

	//
	private LispReal re, ii, ij, ik;

	//
	/*package*/ LispQuaternionImpl(LispReal r, LispReal i, LispReal j,
			LispReal k) {
		this.re = r;
		this.ii = i;
		this.ij = j;
		this.ik = k;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispQuaternion#getComplexPairA()
	 */
	@Override
	/*package*/ LispComplex getComplexPairA() {
		return LispComplexImpl.newComplex(re, ii);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispQuaternion#getComplexPairB()
	 */
	@Override
	/*package*/ LispComplex getComplexPairB() {
		return LispComplexImpl.newComplex(ij, ik);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#add(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber add(LispNumber x) {
		if(x instanceof LispReal) {
			return LispQuaternion.newQuaternion(re.add(x.getReal()),
					ii, ij, ik);
		} else if(x instanceof LispQuaternion) {
			return LispQuaternion.add(this, (LispQuaternion)x);
		} else if(x instanceof LispOctonion) {
			return LispOctonion.add(this, (LispOctonion)x);
		}
		throw new IllegalArgumentException(x.toString());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#sub(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber sub(LispNumber x) {
		if(x instanceof LispReal) {
			return LispQuaternion.newQuaternion(
					re.subtract(x.getReal()), ii, ij, ik);
		} else if(x instanceof LispQuaternion) {
			return LispQuaternion.sub(this, (LispQuaternion)x);
		} else if(x instanceof LispOctonion) {
			return LispOctonion.sub(this, (LispOctonion)x);
		}
		throw new IllegalArgumentException(x.toString());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#mul(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber mul(LispNumber x) {
		if(x instanceof LispReal) {
			LispReal a = x.getReal();

			return LispQuaternion.newQuaternion(
					re.multiply(a), ii.multiply(a),
					ij.multiply(a), ik.multiply(a));
		} else if(x instanceof LispQuaternion) {
			return LispQuaternion.mul(this, (LispQuaternion)x);
		} else if(x instanceof LispOctonion) {
			return LispOctonion.mul(this, (LispOctonion)x);
		}
		throw new IllegalArgumentException(x.toString());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#div(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber div(LispNumber x) {
		if(x instanceof LispReal) {
			LispReal a = x.getReal();

			return LispQuaternion.newQuaternion(
					re.divide(a), ii.divide(a),
					ij.divide(a), ik.divide(a));
		} else if(x instanceof LispQuaternion) {
			return LispQuaternion.div(this, (LispQuaternion)x);
		} else if(x instanceof LispOctonion) {
			return LispOctonion.div(this, (LispOctonion)x);
		}
		throw new IllegalArgumentException(x.toString());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#uminus()
	 */
	@Override
	public LispQuaternion uminus() {
		return new LispQuaternionImpl(re.uminus(), ii.uminus(),
				ij.uminus(), ik.uminus());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isExact()
	 */
	@Override
	public boolean isExact() {
		return re.isExact();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toExact()
	 */
	@Override
	public LispQuaternion toExact() {
		return newQuaternion(re.toExact(), ii.toExact(),
				ij.toExact(), ik.toExact());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toInexact()
	 */
	@Override
	public LispQuaternion toInexact() {
		return newQuaternion(re.toInexact(), ii.toInexact(),
				ij.toInexact(), ik.toInexact());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#norm()
	 */
	@Override
	public LispReal norm() {
		return (LispReal)LispMath.sqrt(
				re.multiply(re).add(ii.multiply(ii))
				.add(ij.multiply(ij)).add(ik.multiply(ik)));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#norm()
	 */
	@Override
	public LispReal normSquared() {
		return (re.multiply(re).add(ii.multiply(ii))
				.add(ij.multiply(ij)).add(ik.multiply(ik)));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#conjugate()
	 */
	@Override
	public LispQuaternion conjugate() {
		return new LispQuaternionImpl(re, ii.uminus(), ij.uminus(),
				ik.uminus());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispQuaternion#getReal()
	 */
	@Override
	public LispReal getReal() {
		return re;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispQuaternion#getRealDouble()
	 */
	@Override
	public double getRealDouble() {
		return re.getRealDouble();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImags()
	 */
	@Override
	public LispReal[] getImags() {
		return new LispReal[] { ii, ij, ik };
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImagsDouble()
	 */
	@Override
	public double[] getImagsDouble() {
		return new double[] {
				ii.getRealDouble(),
				ij.getRealDouble(),
				ik.getRealDouble()
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isFinite()
	 */
	@Override
	public boolean isFinite() {
		return (re.isFinite() && ii.isFinite() &&
				ij.isFinite() && ik.isFinite());
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		r = Hashes.A * (r + re.hashCode());
		r = Hashes.A * (r + ii.hashCode());
		r = Hashes.A * (r + ij.hashCode());
		r = Hashes.A * (r + ik.hashCode());
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof LispQuaternionImpl) {
			LispQuaternionImpl q = (LispQuaternionImpl)o;

			return (re.equals(q.re) && ii.equals(q.ii) &&
					ij.equals(q.ij) && ik.equals(q.ik));
		}
		return false;
	}

}
