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
import net.morilib.lisp.LispCharacter;
import net.morilib.util.string.StringIterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/29
 */
public class LispStringGroupIterator extends Datum2
implements ILispIterator {

	//
	private StringIterator[] iterators;

	//
	LispStringGroupIterator(StringIterator[] iterators) {
		this.iterators = iterators;
	}

	/**
	 * 
	 * @param iterators
	 */
	public LispStringGroupIterator(String... strings) {
		this(_c(strings));
	}

	//
	private static StringIterator[] _c(String[] strings) {
		StringIterator[] r = new StringIterator[strings.length];

		for(int i = 0; i < r.length; i++) {
			r[i] = new StringIterator(strings[i]);
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#isTerminated()
	 */
	public boolean isTerminated() {
		for(StringIterator itr : iterators) {
			if(!itr.hasNext()) {
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
			if(!iterators[i].hasNext()) {
				throw new NoSuchElementException();
			} else {
				iterators[i].next();
			}
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#getCurrentDatum()
	 */
	public Datum getCurrentDatum() {
		ConsListBuilder b = new ConsListBuilder();

		for(StringIterator itr : iterators) {
			if(!itr.hasNext()) {
				throw new NoSuchElementException();
			} else {
				b.append(LispCharacter.valueOf(itr.peek()));
			}
		}
		return b.get();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<string-group-iterator>");
	}

}
