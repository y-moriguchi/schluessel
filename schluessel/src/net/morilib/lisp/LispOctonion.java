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

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/19
 */
public abstract class LispOctonion extends LispNumber {

	/**
	 * 
	 * @param qa
	 * @param qb
	 * @return
	 */
	public static LispOctonion newOctonion(LispQuaternion qa,
			LispQuaternion qb) {
		if(qa.isNaN() || qb.isNaN()) {
			return LispDouble.NaN;
		} else if(qb.isZero()) {
			return qa;
		} else if(qa.isExact() && qb.isExact()) {
			return new LispOctonionImpl(qa, qb);
		} else {
			return new LispOctonionImpl(
					qa.toInexact(), qb.toInexact());
		}
	}

	/**
	 * 
	 * @param r
	 * @param i
	 * @param j
	 * @param k
	 * @param l
	 * @param il
	 * @param jl
	 * @param kl
	 * @return
	 */
	public static LispOctonion newOctonion(
			LispReal r,  LispReal i,  LispReal j,  LispReal k,  
			LispReal l,  LispReal il, LispReal jl, LispReal kl) {
		LispQuaternion rp, ip;

		rp = LispQuaternion.newQuaternion(r, i, j, k);
		ip = LispQuaternion.newQuaternion(l, il, jl, kl);
		return newOctonion(rp, ip);
	}

	/**
	 * 
	 * @param o1
	 * @param o2
	 * @return
	 */
	public static LispOctonion add(LispOctonion o1, LispOctonion o2) {
		LispQuaternion o1a, o1b, o2a, o2b;

		o1a = o1.getQuaternionPairA();
		o1b = o1.getQuaternionPairB();
		o2a = o2.getQuaternionPairA();
		o2b = o2.getQuaternionPairB();
		return newOctonion(
				LispQuaternion.add(o1a, o2a),
				LispQuaternion.add(o1b, o2b));
	}

	/**
	 * 
	 * @param o1
	 * @param o2
	 * @return
	 */
	public static LispOctonion sub(LispOctonion o1, LispOctonion o2) {
		LispQuaternion o1a, o1b, o2a, o2b;

		o1a = o1.getQuaternionPairA();
		o1b = o1.getQuaternionPairB();
		o2a = o2.getQuaternionPairA();
		o2b = o2.getQuaternionPairB();
		return newOctonion(
				LispQuaternion.sub(o1a, o2a),
				LispQuaternion.sub(o1b, o2b));
	}

	/**
	 * 
	 * @param o1
	 * @param o2
	 * @return
	 */
	public static LispOctonion mul(LispOctonion o1, LispOctonion o2) {
		LispQuaternion p, q, r, s;

		p = o1.getQuaternionPairA();
		q = o1.getQuaternionPairB();
		r = o2.getQuaternionPairA();
		s = o2.getQuaternionPairB();
		return newOctonion(
				(LispQuaternion)p.mul(r).sub(s.conjugate().mul(q)),
				(LispQuaternion)s.mul(p).add(q.mul(r.conjugate())));
	}

	/**
	 * 
	 * @param o1
	 * @param o2
	 * @return
	 */
	public static LispOctonion div(LispOctonion o1, LispOctonion o2) {
		LispReal   nr;
		LispReal[] im;

		im = o2.getImagsAsOctonion();
		nr = o2.getReal().multiply(o2.getReal());
		nr = nr.add(im[0].multiply(im[0]));
		nr = nr.add(im[1].multiply(im[1]));
		nr = nr.add(im[2].multiply(im[2]));
		nr = nr.add(im[3].multiply(im[3]));
		nr = nr.add(im[4].multiply(im[4]));
		nr = nr.add(im[5].multiply(im[5]));
		nr = nr.add(im[6].multiply(im[6]));
		return (LispOctonion)o1.mul(o2.conjugate().div(nr));
	}

	/**
	 * 
	 * @return
	 */
	public LispQuaternion[] getQuaternionPair() {
		return new LispQuaternion[] {
				getQuaternionPairA(), getQuaternionPairB()
		};
	}

	//
	/*package*/ abstract LispQuaternion getQuaternionPairA();

	//
	/*package*/ abstract LispQuaternion getQuaternionPairB();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isEqualTo(net.morilib.lisp.LispNumber)
	 */
	@Override
	public boolean isEqualTo(LispNumber x) {
		if(x instanceof LispOctonion) {
			LispOctonion q = (LispOctonion)x;

			return (getQuaternionPairA()
					.isEqualTo(q.getQuaternionPairA()) &&
					getQuaternionPairB()
					.isEqualTo(q.getQuaternionPairB()));
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isInteger()
	 */
	@Override
	public boolean isInteger() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isRational()
	 */
	@Override
	public boolean isRational() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isReal()
	 */
	@Override
	public boolean isReal() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isNaN()
	 */
	@Override
	public boolean isNaN() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#isOne()
	 */
	@Override
	public boolean isOne() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getInt()
	 */
	@Override
	public int getInt() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getLong()
	 */
	@Override
	public long getLong() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getBigInteger()
	 */
	@Override
	public BigInteger getBigInteger() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getBigDecimal()
	 */
	@Override
	public BigDecimal getBigDecimal() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getReal()
	 */
	public LispReal getReal() {
		return getQuaternionPairA().getReal();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getRealDouble()
	 */
	@Override
	public double getRealDouble() {
		return getQuaternionPairA().getRealDouble();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getRealDecimal64()
	 */
	@Override
	public long getRealDecimal64() {
		return getReal().getRealDecimal64();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getNumerator()
	 */
	@Override
	public BigInteger getNumerator() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getDenominator()
	 */
	@Override
	public BigInteger getDenominator() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImags()
	 */
	@Override
	public LispReal[] getImags() {
		LispReal[] pa = getQuaternionPairA().getImagsAsQuaternion();
		LispReal[] pb = getQuaternionPairB().getImagsAsQuaternion();

		return new LispReal[] {
				pa[0], pa[1], pa[2],
				getQuaternionPairB().getReal(), pb[0], pb[1], pb[2]
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImagsDouble()
	 */
	@Override
	public double[] getImagsDouble() {
		double[] pa =
			getQuaternionPairA().getImagsDoubleAsQuaternion();
		double[] pb =
			getQuaternionPairB().getImagsDoubleAsQuaternion();

		return new double[] {
				pa[0], pa[1], pa[2],
				getQuaternionPairB().getRealDouble(),
				pb[0], pb[1], pb[2]
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImagsAsQuaternion()
	 */
	@Override
	public LispReal[] getImagsAsQuaternion() {
		throw new RuntimeException("not quaternion");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImagsDoubleAsQuaternion()
	 */
	@Override
	public double[] getImagsDoubleAsQuaternion() {
		throw new RuntimeException("not quaternion");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImagsAsOctonion()
	 */
	@Override
	public LispReal[] getImagsAsOctonion() {
		return getImags();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#getImagsDoubleAsOctonion()
	 */
	@Override
	public double[] getImagsDoubleAsOctonion() {
		return getImagsDouble();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toLispString(int)
	 */
	@Override
	public LispString toLispString(int radix) {
		String si, sj, sk, sl, sil, sjl, skl;
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
		sl  = (ims[3].signum() > 0 && !ims[3].isInfinity()) ? "+" : "";
		sil = (ims[4].signum() > 0 && !ims[4].isInfinity()) ? "+" : "";
		sjl = (ims[5].signum() > 0 && !ims[5].isInfinity()) ? "+" : "";
		skl = (ims[6].signum() > 0 && !ims[6].isInfinity()) ? "+" : "";

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
		if(!ims[3].isZero()) {
			b.append(sl).append(LispUtils.getResult(ims[3]))
			.append("o");
		}
		if(!ims[4].isZero()) {
			b.append(sil).append(LispUtils.getResult(ims[4]))
			.append("io");
		}
		if(!ims[5].isZero()) {
			b.append(sjl).append(LispUtils.getResult(ims[5]))
			.append("jo");
		}
		if(!ims[6].isZero()) {
			b.append(skl).append(LispUtils.getResult(ims[6]))
			.append("ko");
		}
		return new LispString(b.toString());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toLispString(int)
	 */
	@Override
	public LispString toLispString(int radix, int precision) {
		String si, sj, sk, sl, sil, sjl, skl;
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
		sl  = (ims[3].signum() > 0 && !ims[3].isInfinity()) ? "+" : "";
		sil = (ims[4].signum() > 0 && !ims[4].isInfinity()) ? "+" : "";
		sjl = (ims[5].signum() > 0 && !ims[5].isInfinity()) ? "+" : "";
		skl = (ims[6].signum() > 0 && !ims[6].isInfinity()) ? "+" : "";

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
		if(!ims[3].isZero()) {
			b.append(sl)
			.append(ims[3].toLispString(radix, precision).getString())
			.append("o");
		}
		if(!ims[4].isZero()) {
			b.append(sil)
			.append(ims[4].toLispString(radix, precision).getString())
			.append("io");
		}
		if(!ims[5].isZero()) {
			b.append(sjl)
			.append(ims[5].toLispString(radix, precision).getString())
			.append("jo");
		}
		if(!ims[6].isZero()) {
			b.append(skl)
			.append(ims[6].toLispString(radix, precision).getString())
			.append("ko");
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
	public abstract LispOctonion toExact();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#toExact()
	 */
	public abstract LispOctonion toInexact();

}
