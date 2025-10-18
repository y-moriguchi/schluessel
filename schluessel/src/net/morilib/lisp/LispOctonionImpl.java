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
public class LispOctonionImpl extends LispOctonion
implements java.io.Serializable {

	//
	private LispQuaternion pa, pb;

	//
	/*package*/ LispOctonionImpl(LispQuaternion pa,
			LispQuaternion pb) {
		this.pa = pa;
		this.pb = pb;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispOctonion#getQuaternionPairA()
	 */
	@Override
	/*package*/ LispQuaternion getQuaternionPairA() {
		return pa;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispOctonion#getQuaternionPairB()
	 */
	@Override
	/*package*/ LispQuaternion getQuaternionPairB() {
		return pb;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#add(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber add(LispNumber x) {
		if(x instanceof LispReal) {
			return LispOctonion.newOctonion(
					(LispQuaternion)pa.add(x), pb);
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
			return LispOctonion.newOctonion(
					(LispQuaternion)pa.sub(x), pb);
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

			return LispOctonion.newOctonion(
					(LispQuaternion)pa.mul(a),
					(LispQuaternion)pb.mul(a));
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

			return LispOctonion.newOctonion(
					(LispQuaternion)pa.div(a),
					(LispQuaternion)pb.div(a));
		} else if(x instanceof LispOctonion) {
			return LispOctonion.div(this, (LispOctonion)x);
		}
		throw new IllegalArgumentException(x.toString());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#uminus()
	 */
	@Override
	public LispOctonion uminus() {
		return new LispOctonionImpl(pa.uminus(), pb.uminus());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isExact()
	 */
	@Override
	public boolean isExact() {
		return pa.isExact();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toExact()
	 */
	@Override
	public LispOctonion toExact() {
		return newOctonion(pa.toExact(), pb.toExact());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toInexact()
	 */
	@Override
	public LispOctonion toInexact() {
		return newOctonion(pa.toInexact(), pb.toInexact());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#norm()
	 */
	@Override
	public LispReal norm() {
		LispReal   re = getReal();
		LispReal[] im = getImags();

		return (LispReal)LispMath.sqrt(
				re.multiply(re)
				.add(im[0].multiply(im[0]))
				.add(im[1].multiply(im[1]))
				.add(im[2].multiply(im[2]))
				.add(im[3].multiply(im[3]))
				.add(im[4].multiply(im[4]))
				.add(im[5].multiply(im[5]))
				.add(im[6].multiply(im[6])));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#norm()
	 */
	@Override
	public LispReal normSquared() {
		LispReal   re = getReal();
		LispReal[] im = getImags();

		return (re.multiply(re)
				.add(im[0].multiply(im[0]))
				.add(im[1].multiply(im[1]))
				.add(im[2].multiply(im[2]))
				.add(im[3].multiply(im[3]))
				.add(im[4].multiply(im[4]))
				.add(im[5].multiply(im[5]))
				.add(im[6].multiply(im[6])));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#conjugate()
	 */
	@Override
	public LispNumber conjugate() {
		return new LispOctonionImpl(pa.conjugate(), pb.uminus());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isFinite()
	 */
	@Override
	public boolean isFinite() {
		return pa.isFinite() && pb.isFinite();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		r = Hashes.A * (r + pa.hashCode());
		r = Hashes.A * (r + pb.hashCode());
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof LispOctonionImpl) {
			LispOctonionImpl q = (LispOctonionImpl)o;

			return (pa.equals(q.pa) && pb.equals(q.pb));
		}
		return false;
	}

}
