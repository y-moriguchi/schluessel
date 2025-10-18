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
 * @author MORIGUCHI, Yuichiro 2011/10/16
 */
public abstract class LispQuaternion extends LispOctonion {

	/**
	 * 
	 * @param r
	 * @param i
	 * @return
	 */
	public static LispQuaternion newQuaternion(
			LispReal r, LispReal i, LispReal j, LispReal k) {
		if(r.isNaN() || i.isNaN() || j.isNaN() || k.isNaN()) {
			return LispDouble.NaN;
		} else if(j.isZero() && k.isZero()) {
			return LispComplex.newComplex(r, i);
		} else if(r.isNaN() || i.isNaN() || j.isNaN() || k.isNaN()) {
			return LispDouble.NaN;
		} else if(r.isExact() && i.isExact() &&
				j.isExact() && k.isExact()) {
			return new LispQuaternionImpl(r, i, j, k);
		} else {
			return new LispQuaternionImpl(
					r.toInexact(), i.toInexact(),
					j.toInexact(), k.toInexact());
		}
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static LispQuaternion add(LispQuaternion a,
			LispQuaternion b) {
		LispReal[] aa, bb;
		
		aa = a.getImagsAsQuaternion();
		bb = b.getImagsAsQuaternion();
		return LispQuaternion.newQuaternion(
				a.getReal().add(b.getReal()),
				aa[0].add(bb[0]),
				aa[1].add(bb[1]),
				aa[2].add(bb[2]));
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static LispQuaternion sub(LispQuaternion a,
			LispQuaternion b) {
		LispReal[] aa, bb;
		
		aa = a.getImagsAsQuaternion();
		bb = b.getImagsAsQuaternion();
		return LispQuaternion.newQuaternion(
				a.getReal().subtract(b.getReal()),
				aa[0].subtract(bb[0]),
				aa[1].subtract(bb[1]),
				aa[2].subtract(bb[2]));
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static LispQuaternion mul(LispQuaternion a,
			LispQuaternion b) {
		LispReal[] aa, bb;
		LispReal   a1, b1, c1, d1, a2, b2, c2, d2, r, i, j, k;

		aa = a.getImagsAsQuaternion();
		bb = b.getImagsAsQuaternion();
		a1 = a.getReal();
		b1 = aa[0];
		c1 = aa[1];
		d1 = aa[2];
		a2 = b.getReal();
		b2 = bb[0];
		c2 = bb[1];
		d2 = bb[2];
		r  = a1.multiply(a2).subtract(b1.multiply(b2)).subtract(
				c1.multiply(c2)).subtract(d1.multiply(d2));
		i  = a1.multiply(b2).add(b1.multiply(a2)).add(
				c1.multiply(d2)).subtract(d1.multiply(c2));
		j  = a1.multiply(c2).subtract(b1.multiply(d2)).add(
				c1.multiply(a2)).add(d1.multiply(b2));
		k  = a1.multiply(d2).add(b1.multiply(c2)).subtract(
				c1.multiply(b2)).add(d1.multiply(a2));
		return LispQuaternion.newQuaternion(r, i, j, k);
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static LispQuaternion div(LispQuaternion a,
			LispQuaternion b) {
		LispReal[] aa, bb;
		LispReal   r0, r1, r2, r3, q0, q1, q2, q3, rs;
		LispReal   r, i, j, k;

		aa = a.getImagsAsQuaternion();
		bb = b.getImagsAsQuaternion();
		q0 = a.getReal();
		q1 = aa[0];
		q2 = aa[1];
		q3 = aa[2];
		r0 = b.getReal();
		r1 = bb[0];
		r2 = bb[1];
		r3 = bb[2];
		rs = r0.multiply(r0).add(r1.multiply(r1)).add(
				r2.multiply(r2)).add(r3.multiply(r3));
		r  = r0.multiply(q0).add(r1.multiply(q1)).add(
				r2.multiply(q2)).add(r3.multiply(q3));
		i  = r0.multiply(q1).subtract(r1.multiply(q0)).subtract(
				r2.multiply(q3)).add(r3.multiply(q2));
		j  = r0.multiply(q2).add(r1.multiply(q3)).subtract(
				r2.multiply(q0)).subtract(r3.multiply(q1));
		k  = r0.multiply(q3).subtract(r1.multiply(q2)).add(
				r2.multiply(q1)).subtract(r3.multiply(q0));
		r  = r.divide(rs);
		i  = i.divide(rs);
		j  = j.divide(rs);
		k  = k.divide(rs);
		return LispQuaternion.newQuaternion(r, i, j, k);
	}

	/**
	 * 
	 * @return
	 */
	public LispComplex[] getComplexPair() {
		return new LispComplex[] {
				getComplexPairA(), getComplexPairB()
		};
	}

	//
	/*package*/ abstract LispComplex getComplexPairA();

	//
	/*package*/ abstract LispComplex getComplexPairB();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#uminus()
	 */
	public abstract LispQuaternion uminus();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#conjugate()
	 */
	public abstract LispQuaternion conjugate();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isEqualTo(net.morilib.lisp.LispNumber)
	 */
	@Override
	public boolean isEqualTo(LispNumber x) {
		if(x instanceof LispQuaternion) {
			LispQuaternion q = (LispQuaternion)x;

			return (getComplexPairA().isEqualTo(q.getComplexPairA()) &&
					getComplexPairB().isEqualTo(q.getComplexPairB()));
		} else {
			return super.isEqualTo(x);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getReal()
	 */
	public abstract LispReal getReal();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispOctonion#getRealDouble()
	 */
	public abstract double getRealDouble();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImagsAsQuaternion()
	 */
	@Override
	public LispReal[] getImagsAsQuaternion() {
		return getImags();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImagsDoubleAsQuaternion()
	 */
	@Override
	public double[] getImagsDoubleAsQuaternion() {
		return getImagsDouble();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImagsAsOctonion()
	 */
	@Override
	public LispReal[] getImagsAsOctonion() {
		LispReal[] im = getImags();
		LispReal   o  = isExact() ?
				LispInteger.ZERO : new LispDouble(0.0);

		return new LispReal[] {
				im[0], im[1], im[2], o, o, o, o
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImagsDoubleAsOctonion()
	 */
	@Override
	public double[] getImagsDoubleAsOctonion() {
		double[] im = getImagsDouble();

		return new double[] {
				im[0], im[1], im[2], 0.0, 0.0, 0.0, 0.0
		};
	}

	//
	@Override
	/*package*/ LispQuaternion getQuaternionPairA() {
		return this;
	}

	//
	@Override
	/*package*/ LispQuaternion getQuaternionPairB() {
		return LispInteger.ZERO;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toLispString(int)
	 */
	@Override
	public LispString toLispString(int radix) {
		String si, sj, sk;
		LispReal   re;
		LispReal[] ims;
		StringBuilder b = new StringBuilder();

		if(radix < 2 || radix > 36) {
			throw new IndexOutOfBoundsException("radix is out of range");
		} else if(radix != 10) {
			throw new IllegalArgumentException(
					"radix except 10 is not supported");
		}

		re  = getReal();
		ims = getImags();
		si  = (ims[0].signum() > 0 && !ims[0].isInfinity()) ? "+" : "";
		sj  = (ims[1].signum() > 0 && !ims[1].isInfinity()) ? "+" : "";
		sk  = (ims[2].signum() > 0 && !ims[2].isInfinity()) ? "+" : "";

		b.append(LispUtils.getResult(re));
		if(!ims[0].isZero()) {
			b.append(si).append(LispUtils.getResult(ims[0]))
			.append("i");
		}
		if(!ims[1].isZero()) {
			b.append(sj).append(LispUtils.getResult(ims[1]))
			.append("j");
		}
		if(!ims[2].isZero()) {
			b.append(sk).append(LispUtils.getResult(ims[2]))
			.append("k");
		}
		return new LispString(b.toString());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toLispString(int, int)
	 */
	@Override
	public LispString toLispString(int radix, int precision) {
		String si, sj, sk;
		LispReal   re;
		LispReal[] ims;
		StringBuilder b = new StringBuilder();

		if(radix < 2 || radix > 36) {
			throw new IndexOutOfBoundsException("radix is out of range");
		} else if(radix != 10) {
			throw new IllegalArgumentException(
					"radix except 10 is not supported");
		} else if(precision < 0) {
			throw new IllegalArgumentException(
					"precision must not be negative");
		}

		re  = getReal();
		ims = getImags();
		si  = (ims[0].signum() > 0 && !ims[0].isInfinity()) ? "+" : "";
		sj  = (ims[1].signum() > 0 && !ims[1].isInfinity()) ? "+" : "";
		sk  = (ims[2].signum() > 0 && !ims[2].isInfinity()) ? "+" : "";

		b.append(LispUtils.getResult(re));
		if(!ims[0].isZero()) {
			b.append(si)
			.append(ims[0].toLispString(radix, precision).getString())
			.append("i");
		}
		if(!ims[1].isZero()) {
			b.append(sj)
			.append(ims[1].toLispString(radix, precision).getString())
			.append("j");
		}
		if(!ims[2].isZero()) {
			b.append(sk)
			.append(ims[2].toLispString(radix, precision).getString())
			.append("k");
		}
		return new LispString(b.toString());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#print()
	 */
	@Override
	public String print() {
		return getResult();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#getResult()
	 */
	@Override
	public String getResult() {
		return toLispString(10).getString();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toExact()
	 */
	public abstract LispQuaternion toExact();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toExact()
	 */
	public abstract LispQuaternion toInexact();

}
