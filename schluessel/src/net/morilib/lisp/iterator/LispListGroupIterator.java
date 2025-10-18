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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/29
 */
public class LispListGroupIterator extends Datum2 implements ILispIterator {

	//
	private Datum[] lists;
	private List<Set<Datum>> memos;

	//
	LispListGroupIterator(boolean dummy, Datum[] lists) {
		this.lists = lists;
		this.memos = new ArrayList<Set<Datum>>();
		for(int i = 0; i < lists.length; i++) {
			memos.add(new HashSet<Datum>());
		}
	}

	/**
	 * 
	 * @param iterators
	 */
	public LispListGroupIterator(Datum... lists) {
		this(false, _c(lists));
	}

	//
	private static Datum[] _c(Datum[] lists) {
		Datum[] r = new Datum[lists.length];

		System.arraycopy(lists, 0, r, 0, r.length);
		return r;
	}

	//
	private boolean isAllCirculated() {
		for(Set<Datum> d : memos) {
			if(d != null) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#isTerminated()
	 */
	public boolean isTerminated() {
//		for(Datum itr : lists) {
//			if(!itr.isNil()) {
//				return false;
//			}
//		}
//		return true;
		if(lists.length == 0)  return true;
		for(Datum itr : lists) {
			if(itr.isNil()) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#next()
	 */
	public ILispIterator next() {
		for(int i = 0; i < lists.length; i++) {
			if(lists[i].isNil()) {
				throw new NoSuchElementException();
			} else if(!(lists[i] instanceof Cons)) {
				// not a proper list
				throw new NoSuchElementException();
			} else if(memos.get(i) != null &&
					memos.get(i).contains(lists[i])) {
				// a curculated list
				memos.set(i, null);
			}

			if(isAllCirculated()) {
				throw new NoSuchElementException();
			} else if(memos.get(i) != null) {
				memos.get(i).add(lists[i]);
			}
			lists[i] = ((Cons)lists[i]).getCdr();
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#getCurrentDatum()
	 */
	public Datum getCurrentDatum() {
		ConsListBuilder b = new ConsListBuilder();

		for(Datum d : lists) {
			if(d.isNil()) {
				throw new NoSuchElementException();
			} else if(!(d instanceof Cons)) {
				// not a proper list
				throw new NoSuchElementException();
			} else {
				b.append(((Cons)d).getCar());
			}
		}
		return b.get();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<list-group-iterator>");
	}

}
