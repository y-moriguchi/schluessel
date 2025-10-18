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
public class LispArithmeticSequence extends AbstractLispRealSequence {

	/**
	 * 
	 */
	public static final int INFINITE = -1;

	//
	private LispReal a1, d;
	private int size;

	/**
	 * 
	 * @param value
	 * @param size
	 */
	public LispArithmeticSequence(LispReal a1, LispReal d,
			int size) {
		if(a1 == null || d == null) {
			throw new NullPointerException();
		} else if(d.signum() == 0) {
			throw new IllegalArgumentException();
		} else if(size == 0) {
			throw new IllegalArgumentException();
		}
		this.a1   = a1;
		this.d    = d;
		this.size = size;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#get(int)
	 */
	public LispReal get(int i) {
		if(i < 1)  throw new IndexOutOfBoundsException();
		return a1.add(d.multiply(i - 1));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#limit()
	 */
	public LispReal limit() {
		if(size >= 1) {
			return LispInteger.ZERO;
		} else if(d.signum() > 0) {
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
		return new LispRealConstantSequence(d,
				(size >= 2) ? size - 1 : size);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.AbstractLispRealSequence#inv()
	 */
	@Override
	public ILispSequence inv() {
		return new LispHarmonicSequence(
				a1.invert(), d.divide(a1), size);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.AbstractLispRealSequence#sum(int, int)
	 */
	@Override
	public LispReal sum(int start, int end) {
		if(start < 1)    throw new IllegalArgumentException();
		if(end   < 1)    throw new IllegalArgumentException();
		if(start > end)  throw new IllegalArgumentException();
		return a1.multiply(2).add(d.multiply(end - start)).multiply(
				end - start + 1).divide(LispInteger.valueOf(2));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.AbstractLispRealSequence#sum()
	 */
	@Override
	public LispReal sum() {
		if(size >= 1) {
			return sum(1, size());
		} else if(d.signum() > 0) {
			return LispDouble.POSITIVE_INFINITY;
		} else {
			return LispDouble.NEGATIVE_INFINITY;
		}
	}

}
