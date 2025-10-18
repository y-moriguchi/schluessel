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
package net.morilib.lisp.collection;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.IsEqual;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/18
 */
public class LispArrayList extends Datum2
implements LispFlexibleSequence {

	//
	private static final Procedure EQUAL   = new IsEqual();
	private static final Symbol ARRAY_LIST =
		Symbol.getSymbol("array-list");

	//
	private List<Datum> list;

	/**
	 * 
	 */
	public LispArrayList() {
		this(new ArrayList<Datum>());
	}

	/**
	 * 
	 * @param list
	 */
	public LispArrayList(List<Datum> list) {
		this.list = new ArrayList<Datum>(list);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#get(int)
	 */
	public Datum get(int index) {
		return list.get(index);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#first()
	 */
	public Datum first() {
		return list.get(0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#last()
	 */
	public Datum last() {
		return list.get(list.size() - 1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#copySet(int, net.morilib.lisp.Datum)
	 */
	public LispArrayList copySet(int index, Datum d) {
		return new LispArrayList(list).set(index, d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#set(int, net.morilib.lisp.Datum)
	 */
	public LispArrayList set(int index, Datum d) {
		list.set(index, d);
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#replace(net.morilib.lisp.collection.LispSequence, int, int, int)
	 */
	public LispArrayList replace(LispSequence src, int srcPos,
			int destPos, int len) {
		return new LispArrayList(list).arraycopy(src, srcPos, destPos,
				len);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#arraycopy(net.morilib.lisp.collection.LispSequence, int, int, int)
	 */
	public LispArrayList arraycopy(LispSequence src, int srcPos,
			int destPos, int len) {
		for(int i = 0; i < len; i++) {
			list.set(destPos + i, src.get(srcPos + i));
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#copy(int, int)
	 */
	public Datum copy(int b, int e) {
		return new LispArrayList(list.subList(b, e));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#equivalence()
	 */
	public Procedure equivalence() {
		return EQUAL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#equivalence(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public boolean equivalence(Datum a, Datum b) {
		return LispUtils.equals(a, b);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyAdd(net.morilib.lisp.Datum)
	 */
	public LispArrayList copyAdd(Datum d) {
		return new LispArrayList(list).add(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#add(net.morilib.lisp.Datum)
	 */
	public LispArrayList add(Datum d) {
		list.add(d);
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDelete(net.morilib.lisp.Datum)
	 */
	public LispArrayList copyDelete(Datum d) {
		return new LispArrayList(list).delete(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#delete(net.morilib.lisp.Datum)
	 */
	public LispArrayList delete(Datum d) {
		list.remove(d);
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteAll(net.morilib.lisp.Datum)
	 */
	public LispArrayList copyDeleteAll(Datum d) {
		return new LispArrayList(list).deleteAll(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteAll(net.morilib.lisp.Datum)
	 */
	public LispArrayList deleteAll(Datum d) {
		while(list.remove(d));
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyAddFrom(net.morilib.lisp.collection.LispBag)
	 */
	public LispArrayList copyAddFrom(LispBag d) {
		return new LispArrayList(list).addFrom(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#addFrom(net.morilib.lisp.collection.LispBag)
	 */
	public LispArrayList addFrom(LispBag d) {
		if(d instanceof LispArrayList) {
			list.addAll(((LispArrayList)d).list);
		} else {
			for(Datum x : d) {
				list.add(x);
			}
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteFrom(net.morilib.lisp.collection.LispBag)
	 */
	public LispArrayList copyDeleteFrom(LispBag d) {
		return new LispArrayList(list).deleteFrom(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteFrom(net.morilib.lisp.collection.LispBag)
	 */
	public LispArrayList deleteFrom(LispBag d) {
		for(Datum x : d) {
			list.remove(x);
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteAllFrom(net.morilib.lisp.collection.LispBag)
	 */
	public LispArrayList copyDeleteAllFrom(LispBag d) {
		return new LispArrayList(list).deleteAllFrom(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteAllFrom(net.morilib.lisp.collection.LispBag)
	 */
	public LispArrayList deleteAllFrom(LispBag d) {
		Iterator<Datum> itr = list.iterator();

		while(itr.hasNext()) {
			if(d.contains(itr.next())) {
				itr.remove();
			}
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#getCollectionName()
	 */
	public Symbol getCollectionName() {
		return ARRAY_LIST;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#toList()
	 */
	public Datum toList() {
		return LispUtils.toCons(list);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#count(net.morilib.lisp.Datum)
	 */
	public int count(Datum c2a) {
		int c = 0;

		for(Datum o : this) {
			if(equivalence(o, c2a)) {
				c++;
			}
		}
		return c;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#size()
	 */
	public int size() {
		return list.size();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#prototype()
	 */
	public Datum prototype() {
		return new LispArrayList();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#clear()
	 */
	public LispArrayList clear() {
		list.clear();
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection)
	 */
	public boolean equalTo(LispCollection col) {
		if(list.size() != col.size()) {
			return false;
		} else {
			Iterator<Datum> j = col.iterator();

			for(int i = 0; i < list.size(); i++) {
				if(!LispUtils.equals(list.get(i), j.next())) {
					return false;
				}
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection, net.morilib.lisp.Procedure, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public boolean equalTo(LispCollection col, Procedure p,
			Environment env, LispMessage mesg) {
		if(list.size() != col.size()) {
			return false;
		} else {
			Iterator<Datum> j = col.iterator();

			for(int i = 0; i < list.size(); i++) {
				if(!Scheme.callva(p, env, mesg, list.get(i),
						j.next()).isTrue()) {
					return false;
				}
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#duplicate()
	 */
	public Datum duplicate() {
		return new LispArrayList(list);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#contains(net.morilib.lisp.Datum)
	 */
	public boolean contains(Datum d) {
		return list.contains(d);
	}

	/* (non-Javadoc)
	 * @see java.lang.Iterable#iterator()
	 */
	public Iterator<Datum> iterator() {
		return list.iterator();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#insertFirst(net.morilib.lisp.Datum)
	 */
	public LispArrayList insertFirst(Datum d) {
		list.add(0, d);
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#insertLast(net.morilib.lisp.Datum)
	 */
	public LispArrayList insertLast(Datum d) {
		list.add(d);
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#copyInsertFirst(net.morilib.lisp.Datum)
	 */
	public LispArrayList copyInsertFirst(Datum d) {
		return new LispArrayList(list).insertFirst(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#copyInsertLast(net.morilib.lisp.Datum)
	 */
	public LispArrayList copyInsertLast(Datum d) {
		return new LispArrayList(list).insertLast(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#removeFirst()
	 */
	public Datum[] removeFirst() {
		return new Datum[] { this, list.remove(0) };
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#removeLast()
	 */
	public Datum[] removeLast() {
		return new Datum[] { this, list.remove(list.size() - 1) };
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#copyWithoutFirst()
	 */
	public Datum[] copyWithoutFirst() {
		LispArrayList r = new LispArrayList(list);

		return new Datum[] { r, r.list.remove(0) };
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#copyWithoutLast()
	 */
	public Datum[] copyWithoutLast() {
		LispArrayList r = new LispArrayList(list);

		return new Datum[] { r, r.list.remove(r.list.size() - 1) };
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispFlexibleSequence#copyInsert(int, net.morilib.lisp.Datum)
	 */
	public LispArrayList copyInsert(int index, Datum d) {
		return new LispArrayList(list).insert(index, d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispFlexibleSequence#insert(int, net.morilib.lisp.Datum)
	 */
	public LispArrayList insert(int index, Datum d) {
		list.add(index, d);
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispFlexibleSequence#copyDelete(int)
	 */
	public Datum copyDelete(int index) {
		return new LispArrayList(list).delete(index);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispFlexibleSequence#delete(int)
	 */
	public Datum delete(int index) {
		list.remove(index);
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("(array-list");
		for(Datum x : list) {
			buf.append(" ").append(LispUtils.print(x));
		}
		buf.append(")");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.accessor.ILispRef#ref(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum ref(Datum arg, LispMessage mesg) {
		int k = SubrUtils.getSmallInt(arg, mesg);

		if(k < 0 || k >= list.size()) {
			throw mesg.getError("err.accessor.ref.outofrange", arg);
		}
		return list.get(k);
	}

}
