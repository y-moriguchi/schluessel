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
public class LispRealConstantSequence
extends AbstractLispRealSequence {

	/**
	 * 
	 */
	public static final int INFINITE = -1;

	//
	private LispReal value;
	private int size;

	/**
	 * 
	 * @param value
	 * @param size
	 */
	public LispRealConstantSequence(LispReal value, int size) {
		if(value == null) {
			throw new NullPointerException();
		} else if(size == 0) {
			throw new IllegalArgumentException();
		}
		this.value = value;
		this.size  = size;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#get(int)
	 */
	public LispReal get(int i) {
		if(i < 1)  throw new IndexOutOfBoundsException();
		return (i <= size) ? value : LispInteger.ZERO;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#limit()
	 */
	public LispReal limit() {
		return (size < 0) ? value : LispInteger.ZERO;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispSequence#isFinite()
	 */
	public boolean isFinite() {
		return size > 0;
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
		if(size >= 2) {
			return new LispRealConstantSequence(
					LispInteger.ZERO, size - 1);
		} else {
			return this;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.AbstractLispRealSequence#sum(int, int)
	 */
	@Override
	public LispReal sum(int start, int end) {
		if(start < 1)    throw new IllegalArgumentException();
		if(end   < 1)    throw new IllegalArgumentException();
		if(start > end)  throw new IllegalArgumentException();
		return value.multiply(end - start + 1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.AbstractLispRealSequence#sum()
	 */
	@Override
	public LispReal sum() {
		if(value.signum() == 0) {
			return value;
		} else if(size >= 1) {
			return value.multiply(size);
		} else if(value.signum() > 0) {
			return LispDouble.POSITIVE_INFINITY;
		} else {
			return LispDouble.NEGATIVE_INFINITY;
		}
	}

}
