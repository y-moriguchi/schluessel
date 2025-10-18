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

import java.math.BigInteger;

import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispReal;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/11
 */
public class LispRatio2GeometricSequence
extends AbstractLispRealSequence {

	//
	private BigInteger a1;
	private int size;

	//
	private LispRatio2GeometricSequence(BigInteger a1, int size) {
		this.a1   = a1;
		this.size = size;
	}

	/**
	 * 
	 * @param a1
	 * @param size
	 */
	public LispRatio2GeometricSequence(LispInteger a1, int size) {
		this(a1.getBigInteger(), size);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#get(int)
	 */
	public LispReal get(int i) {
		if(i < 1)  throw new IndexOutOfBoundsException();
		return (i <= size) ?
				LispInteger.valueOf(a1.shiftLeft(i - 1)) :
					LispInteger.ZERO;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#limit()
	 */
	public LispReal limit() {
		return (size < 0) ?
				LispDouble.POSITIVE_INFINITY : LispInteger.ZERO;
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
			return new LispRatio2GeometricSequence(a1, size - 1);
		} else if(size < 0) {
			return new LispRatio2GeometricSequence(a1, size);
		} else {
			return new LispRealConstantSequence(LispInteger.ZERO, -1);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.AbstractLispRealSequence#sum(int, int)
	 */
	@Override
	public LispReal sum(int start, int end) {
		BigInteger x;

		if(start < 1)    throw new IllegalArgumentException();
		if(end   < 1)    throw new IllegalArgumentException();
		if(start > end)  throw new IllegalArgumentException();
		x = a1.multiply(BigInteger.ONE.pow(end).subtract(
				BigInteger.ONE.pow(start + 1)));
		x = x.negate();
		return LispInteger.valueOf(x);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.AbstractLispRealSequence#sum()
	 */
	@Override
	public LispReal sum() {
		if(isFinite()) {
			return sum(1, size());
		} else if(a1.signum() > 0) {
			return LispDouble.POSITIVE_INFINITY;
		} else {
			return LispDouble.NEGATIVE_INFINITY;
		}
	}

}
