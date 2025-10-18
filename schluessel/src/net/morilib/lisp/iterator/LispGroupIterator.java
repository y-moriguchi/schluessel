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

import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/29
 */
public class LispGroupIterator extends Datum2 implements ILispIterator {

	//
	private ILispIterator[] iterators;

	//
	LispGroupIterator(boolean dummy, ILispIterator[] iterators) {
		this.iterators = iterators;
	}

	/**
	 * 
	 * @param iterators
	 */
	public LispGroupIterator(ILispIterator... iterators) {
		this(false, _c(iterators));
	}

	/**
	 * 
	 * @param iterators
	 */
	public LispGroupIterator(ILispIterable... data) {
		this(false, _c(data));
	}

	//
	private static ILispIterator[] _c(ILispIterator[] iterators) {
		ILispIterator[] r = new ILispIterator[iterators.length];

		System.arraycopy(iterators, 0, r, 0, r.length);
		return r;
	}

	//
	private static ILispIterator[] _c(ILispIterable[] data) {
		ILispIterator[] r = new ILispIterator[data.length];

		for(int i = 0; i < r.length; i++) {
			r[i] = data[i].lispIterator();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#isTerminated()
	 */
	public boolean isTerminated() {
		for(ILispIterator itr : iterators) {
			if(itr.isTerminated()) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#next()
	 */
	public ILispIterator next() {
		for(int i = 0; i < iterators.length; i++) {
			if(iterators[i].isTerminated()) {
				throw new NoSuchElementException();
			} else {
				iterators[i] = iterators[i].next();
			}
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#getCurrentDatum()
	 */
	public Datum getCurrentDatum() {
		ConsListBuilder b = new ConsListBuilder();

		for(ILispIterator itr : iterators) {
			if(itr.isTerminated()) {
				throw new NoSuchElementException();
			} else {
				b.append(itr.getCurrentDatum());
			}
		}
		return b.get();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<group-iterator>");
	}

}
