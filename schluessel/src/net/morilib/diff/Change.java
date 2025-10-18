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
package net.morilib.diff;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/10
 */
public final class Change<T> {

	/**
	 * 
	 */
	public static final int CHANGED = 1;

	/**
	 * 
	 */
	public static final int INSERTED = 2;

	/**
	 * 
	 */
	public static final int DELETED = 3;

	//
	int beginA, endA, beginB, endB, operation;
	List<T> before, after;

	//
	Change() {
		before = new ArrayList<T>();
		after  = new ArrayList<T>();
	}

	/**
	 * 
	 * @return
	 */
	public int getBeginIndexA() {
		return beginA;
	}

	/**
	 * 
	 * @return
	 */
	public int getEndIndexA() {
		return endA;
	}

	/**
	 * 
	 * @return
	 */
	public int getBeginIndexB() {
		return beginB;
	}

	/**
	 * 
	 * @return
	 */
	public int getEndIndexB() {
		return endB;
	}

	/**
	 * 
	 * @return
	 */
	public int getOperation() {
		return operation;
	}

	/**
	 * 
	 * @return
	 */
	public List<T> getBefore() {
		return before;
	}

	/**
	 * 
	 * @return
	 */
	public List<T> getAfter() {
		return after;
	}

	//
	private Change<T> confirm(int op) {
		operation = op;
		before = Collections.unmodifiableList(before);
		after  = Collections.unmodifiableList(after);
		return this;
	}

	//
	private static<E> E next(Iterator<E> itr) {
		return itr.hasNext() ? itr.next() : null;
	}

	//
	static<T> List<Change<T>> makechange(List<EditScript<T>> scripts) {
		Iterator<EditScript<T>> itr = scripts.iterator();
		List<Change<T>> r = new ArrayList<Change<T>>();
		int stat = 100, al = 1, bl = 1;
		Change<T> ch = null;

		for(EditScript<T> e = itr.next(); true;) {
			switch(stat) {
			case 100:  // initial
				if(e == null) {
					return r;
				} else {
					ch = new Change<T>();
					bl = bl + (e.getIndex() - al);
					al = e.getIndex();
					ch.beginA = al;
					ch.beginB = bl;
					if(e.isDelete()) {
						ch.before.add(e.getObject());
						stat = 101;
					} else {
						ch.after.add(e.getObject());
						stat = 102;
					}
					e  = next(itr);
					break;
				}
			case 101:  // delete
				if(e == null) {
					ch.endA = al;
					r.add(ch.confirm(DELETED));
					stat = 100;
				} else if(e.isDelete()) {
					if(++al != e.getIndex()) {
						ch.endA = al - 1;
						r.add(ch.confirm(DELETED));
						stat = 100;
					} else {
						ch.before.add(e.getObject());
						e = next(itr);
					}
				} else {
					if(al != e.getIndex()) {
						ch.endA = al;
						r.add(ch.confirm(DELETED));
						stat = 100;
					} else {
						bl++;
						ch.after.add(e.getObject());
						ch.endA = al;
						stat = 103;
						e = next(itr);
					}
				}
				break;
			case 102:  // insert
				if(e == null || e.isDelete() || al != e.getIndex()) {
					r.add(ch.confirm(INSERTED));
					stat = 100;
				} else {
					bl++;
					ch.after.add(e.getObject());
					e = next(itr);
				}
				break;
			case 103:  // change
				if(e == null || e.isDelete() || al != e.getIndex()) {
					ch.endB = bl - 1;
					r.add(ch.confirm(CHANGED));
					stat = 100;
				} else {
					bl++;
					ch.after.add(e.getObject());
					e = next(itr);
				}
				break;
			}
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder b = new StringBuilder();

		switch(operation) {
		case DELETED:
			return beginA + "," + endA + "d";
		case INSERTED:
			return beginA + "a";
		case CHANGED:
			return beginA + "," + endA + "c" + beginB + "," + endB;
		}
		return b.toString();
	}

}
