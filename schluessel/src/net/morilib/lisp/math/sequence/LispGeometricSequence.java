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
package net.morilib.lisp.math.sequence;

import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispReal;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/11
 */
public class LispGeometricSequence extends AbstractLispRealSequence {

	/**
	 * 
	 */
	public static final int INFINITE = -1;

	//
	private LispReal a1, r;
	private int size;

	/**
	 * @param a12
	 * @param r2
	 * @param i
	 */
	public LispGeometricSequence(LispReal a1, LispReal r,
			int size) {
		if(a1 == null || r == null) {
			throw new NullPointerException();
		} else if(a1.signum() == 0) {
			throw new IllegalArgumentException();
		}
		this.a1   = a1;
		this.r    = r;
		this.size = size;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#get(int)
	 */
	public LispReal get(int i) {
		if(i < 1)  throw new IndexOutOfBoundsException();
		return a1.multiply(r.power(i - 1));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#limit()
	 */
	public LispReal limit() {
		int c;

		if(size >= 1) {
			return LispInteger.ZERO;
		} else if(r.abs().compareTo(LispInteger.ONE) < 0) {
			return LispInteger.ZERO;
		} else if((c = r.compareTo(LispInteger.ONE)) == 0) {
			return r;
		} else if(c < 0) {
			return LispDouble.NaN;
		} else if(a1.signum() > 0) {
			return LispDouble.POSITIVE_INFINITY;
		} else {
			return LispDouble.NEGATIVE_INFINITY;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispSequence#isFinite()
	 */
	public boolean isFinite() {
		return size >= 1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispSequence#size()
	 */
	public int size() {
		return size;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.AbstractLispRealSequence#difference()
	 */
	@Override
	public ILispRealSequence difference() {
		if(size > 1) {
			return new LispGeometricSequence(
					a1.multiply(r.subtract(LispInteger.ONE)),
					r, size - 1);
		} else if(size < 0) {
			return new LispGeometricSequence(
					a1.multiply(r.subtract(LispInteger.ONE)),
					r, size);
		} else {
			return new LispRealConstantSequence(LispInteger.ZERO, -1);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.AbstractLispRealSequence#sum(int, int)
	 */
	@Override
	public LispReal sum(int start, int end) {
		LispReal x;

		if(start < 1)    throw new IllegalArgumentException();
		if(end   < 1)    throw new IllegalArgumentException();
		if(start > end)  throw new IllegalArgumentException();
		x = a1.multiply(r.power(end - 1).subtract(r.power(start)));
		x = x.divide(LispInteger.ONE.subtract(r));
		return x;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.AbstractLispRealSequence#sum()
	 */
	@Override
	public LispReal sum() {
		if(isFinite()) {
			return sum(1, size());
		} else if(r.abs().compareTo(LispInteger.ONE) < 0) {
			return r.divide(LispInteger.ONE.subtract(r));
		} else if(r.compareTo(LispInteger.ONE) < 0) {
			return LispDouble.NaN;
		} else if(a1.signum() > 0) {
			return LispDouble.POSITIVE_INFINITY;
		} else {
			return LispDouble.NEGATIVE_INFINITY;
		}
	}

}
