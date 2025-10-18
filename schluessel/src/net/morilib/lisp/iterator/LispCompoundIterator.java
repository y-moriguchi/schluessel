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
package net.morilib.lisp.iterator;

import java.util.NoSuchElementException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/29
 */
public class LispCompoundIterator extends Datum2 implements ILispIterator {

	//
	private ILispIterator[] iterators;
	private int ptr;

	//
	LispCompoundIterator(boolean dummy, ILispIterator[] iterators) {
		this.iterators = iterators;
	}

	/**
	 * 
	 * @param iterators
	 */
	public LispCompoundIterator(ILispIterator... iterators) {
		this(false, _c(iterators));
	}

	//
	private static ILispIterator[] _c(ILispIterator[] iterators) {
		ILispIterator[] r = new ILispIterator[iterators.length];

		System.arraycopy(iterators, 0, r, 0, r.length);
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#isTerminated()
	 */
	public boolean isTerminated() {
		return ptr >= iterators.length;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#next()
	 */
	public ILispIterator next() {
		if(isTerminated()) {
			throw new NoSuchElementException();
		} else if((iterators[ptr] = iterators[ptr].next(
				)).isTerminated()) {
			ptr++;
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#getCurrentDatum()
	 */
	public Datum getCurrentDatum() {
		if(isTerminated()) {
			throw new NoSuchElementException();
		}
		return iterators[ptr].getCurrentDatum();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<compound-iterator>");
	}

}
