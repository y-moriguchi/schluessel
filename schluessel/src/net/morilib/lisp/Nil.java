/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp;

import java.util.Comparator;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.NoSuchElementException;

import net.morilib.lisp.accessor.ILispRef;
import net.morilib.lisp.collection.LispBag;
import net.morilib.lisp.collection.LispCollection;
import net.morilib.lisp.collection.LispDictionary;
import net.morilib.lisp.collection.LispMap;
import net.morilib.lisp.collection.LispSequence;
import net.morilib.lisp.iterator.ILispIterable;
import net.morilib.lisp.iterator.ILispIterator;
import net.morilib.lisp.sos.LispType;
import net.morilib.lisp.subr.IsEqual;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class Nil extends SExpressionDatum
implements ConsOrNil, ILispIterable, ILispIterator, ILispRef,
java.io.Serializable {

	/**
	 * 
	 */
	public static final Nil NIL = new Nil();

	//
	private Nil() {
		// do nothing
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toString()
	 */
	public String toString() {
		return "()";
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#isNil()
	 */
	public boolean isNil() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	@Override
	public LispType getType() {
		return LispType.NULL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.JavaObjective#toObject()
	 */
	public Object toObject() {
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#get(int)
	 */
	public Datum get(int index) {
		throw new IndexOutOfBoundsException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#copySet(int, net.morilib.lisp.Datum)
	 */
	public Datum copySet(int index, Datum d) {
		throw new IndexOutOfBoundsException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#set(int, net.morilib.lisp.Datum)
	 */
	public Datum set(int index, Datum d) {
		throw new IndexOutOfBoundsException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#replace(net.morilib.lisp.collection.LispSequence, int, int, int)
	 */
	public Datum replace(LispSequence src, int srcPos, int destPos,
			int len) {
		throw new IndexOutOfBoundsException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#arraycopy(net.morilib.lisp.collection.LispSequence, int, int, int)
	 */
	public Datum arraycopy(LispSequence src, int srcPos, int destPos,
			int len) {
		throw new IndexOutOfBoundsException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#copy(int, int)
	 */
	public Datum copy(int b, int e) {
		if(b == e) {
			return Nil.NIL;
		} else {
			throw new IndexOutOfBoundsException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#equivalence()
	 */
	public Procedure equivalence() {
		return new IsEqual();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyAdd(net.morilib.lisp.Datum)
	 */
	public Datum copyAdd(Datum d) {
		return new Cons(d, Nil.NIL);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#add(net.morilib.lisp.Datum)
	 */
	public Datum add(Datum d) {
		return copyAdd(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDelete(net.morilib.lisp.Datum)
	 */
	public Datum copyDelete(Datum d) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#delete(net.morilib.lisp.Datum)
	 */
	public Datum delete(Datum d) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteAll(net.morilib.lisp.Datum)
	 */
	public Datum copyDeleteAll(Datum d) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteAll(net.morilib.lisp.Datum)
	 */
	public Datum deleteAll(Datum d) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyAddFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyAddFrom(LispBag d) {
		return LispUtils.copy(d.iterator(), Nil.NIL);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#addFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum addFrom(LispBag d) {
		return copyAddFrom(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteFrom(LispBag d) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteFrom(LispBag d) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteAllFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteAllFrom(LispBag d) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteAllFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteAllFrom(LispBag d) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#getCollectionName()
	 */
	public Symbol getCollectionName() {
		return Symbol.getSymbol("list");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#size()
	 */
	public int size() {
		return 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equivalence(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public boolean equivalence(Datum a, Datum b) {
		return a.equals(b);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#prototype()
	 */
	public Datum prototype() {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#clear()
	 */
	public Datum clear() {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection)
	 */
	public boolean equalTo(LispCollection col) {
		return ((Datum)col).isNil();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection, net.morilib.lisp.Procedure, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public boolean equalTo(LispCollection col, Procedure proc,
			Environment env, LispMessage mesg) {
		return ((Datum)col).isNil();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#duplicate()
	 */
	public Datum duplicate() {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#contains(net.morilib.lisp.Datum)
	 */
	public boolean contains(Datum d) {
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Iterable#iterator()
	 */
	public Iterator<Datum> iterator() {
		return Iterators.emptyIterator();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#first()
	 */
	public Datum first() {
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#last()
	 */
	public Datum last() {
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#insertFirst(net.morilib.lisp.Datum)
	 */
	public Datum insertFirst(Datum d) {
		return new Cons(d, Nil.NIL);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#insertLast(net.morilib.lisp.Datum)
	 */
	public Datum insertLast(Datum d) {
		return new Cons(d, Nil.NIL);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#copyInsertFirst(net.morilib.lisp.Datum)
	 */
	public Datum copyInsertFirst(Datum d) {
		return new Cons(d, Nil.NIL);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#copyInsertLast(net.morilib.lisp.Datum)
	 */
	public Datum copyInsertLast(Datum d) {
		return new Cons(d, Nil.NIL);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#removeFirst()
	 */
	public Datum[] removeFirst() {
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#removeLast()
	 */
	public Datum[] removeLast() {
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#copyWithoutFirst()
	 */
	public Datum[] copyWithoutFirst() {
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#copyWithoutLast()
	 */
	public Datum[] copyWithoutLast() {
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispFlexibleSequence#copyInsert(int, net.morilib.lisp.Datum)
	 */
	public Datum copyInsert(int index, Datum d) {
		if(index != 0) {
			throw new IndexOutOfBoundsException();
		}
		return new Cons(d, Nil.NIL);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispFlexibleSequence#insert(int, net.morilib.lisp.Datum)
	 */
	public Datum insert(int index, Datum d) {
		if(index != 0) {
			throw new IndexOutOfBoundsException();
		}
		return new Cons(d, Nil.NIL);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispFlexibleSequence#copyDelete(int)
	 */
	public Datum copyDelete(int index) {
		throw new IndexOutOfBoundsException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispFlexibleSequence#delete(int)
	 */
	public Datum delete(int index) {
		throw new IndexOutOfBoundsException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispEntryEnumeration#entryIterator()
	 */
	public Iterator<Entry<Datum, Datum>> entryIterator() {
		return Iterators.emptyIterator();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#keyEquivalence()
	 */
	public Procedure keyEquivalence() {
		return new IsEqual();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#valueEquivalence()
	 */
	public Procedure valueEquivalence() {
		return new IsEqual();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#containsKey(net.morilib.lisp.Datum)
	 */
	public boolean containsKey(Datum k) {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#keysToList()
	 */
	public SExpressionDatum keysToList() {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#get(net.morilib.lisp.Datum)
	 */
	public Datum get(Datum k) {
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#copyPut(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public Datum[] copyPut(Datum k, Datum v) {
		Datum[] r = new Datum[2];

		r[0] = new Cons(new Cons(k, v), Nil.NIL);
		r[1] = null;
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#put(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public Datum[] put(Datum k, Datum v) {
		return copyPut(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#copyAddFrom(net.morilib.lisp.collection.LispMap)
	 */
	public Datum copyAddFrom(LispMap m) {
		return LispUtils.toCons(m);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#addFrom(net.morilib.lisp.collection.LispMap)
	 */
	public Datum addFrom(LispMap m) {
		return copyAddFrom(m);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#copyDeleteFromMap(net.morilib.lisp.Datum)
	 */
	public Datum copyDeleteKey(Datum k) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#deleteFromMap(net.morilib.lisp.Datum)
	 */
	public Datum deleteKey(Datum k) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#count(net.morilib.lisp.Datum)
	 */
	public int count(Datum c2a) {
		return 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#equivalenceKey(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public boolean equivalenceKey(Datum a, Datum b) {
		return LispUtils.equals(a, b);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#equivalenceValue(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public boolean equivalenceValue(Datum a, Datum b) {
		return LispUtils.equals(a, b);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#copyDeleteFromKey(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteFromKey(LispBag k) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#deleteFromKey(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteFromKey(LispBag k) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SExpression#getAllAsList(net.morilib.lisp.Datum)
	 */
	public SExpressionDatum getAllAsList(Datum k) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SExpression#copyUpdateAll(net.morilib.lisp.Datum, net.morilib.lisp.Procedure, net.morilib.lisp.Procedure, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public Datum copyUpdateAll(Datum k, Procedure f, Procedure th,
			Environment env, LispMessage mesg) {
		Datum d;

		if(th == null) {
			d = new Cons(k, LispBoolean.FALSE);
		} else {
			d = new Cons(k, Scheme.callva(th, env, mesg));
		}
		return LispUtils.list(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SExpression#updateAll(net.morilib.lisp.Datum, net.morilib.lisp.Procedure, net.morilib.lisp.Procedure, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public Datum updateAll(Datum k, Procedure f, Procedure th,
			Environment env, LispMessage mesg) {
		return copyUpdateAll(k, f, th, env, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SExpression#copyDeleteAllKey(net.morilib.lisp.Datum)
	 */
	public Datum copyDeleteAllKey(Datum k) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SExpression#deleteAllKey(net.morilib.lisp.Datum)
	 */
	public Datum deleteAllKey(Datum k) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SExpression#copyDeleteAllFromKey(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteAllFromKey(LispBag c2a) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SExpression#deleteAllFromKey(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteAllFromKey(LispBag c2a) {
		return Nil.NIL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SExpression#copyAddFrom(net.morilib.lisp.collection.LispDictionary)
	 */
	public Datum copyAddFrom(LispDictionary d) {
		return LispUtils.toAlist(d.entryIterator());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SExpression#addFrom(net.morilib.lisp.collection.LispDictionary)
	 */
	public Datum addFrom(LispDictionary d) {
		return LispUtils.toAlist(d.entryIterator());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SExpression#copyReplaceAll(net.morilib.lisp.Datum, net.morilib.lisp.Datum[])
	 */
	public Datum copyReplaceAll(Datum k, Datum[] array) {
		ConsListBuilder b = new ConsListBuilder();

		for(Datum x : array) {
			b.append(new Cons(k, x));
		}
		return b.get();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SExpression#replaceAll(net.morilib.lisp.Datum, net.morilib.lisp.Datum[])
	 */
	public Datum replaceAll(Datum k, Datum[] array) {
		return copyReplaceAll(k, array);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#toList()
	 */
	public Datum toList() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("()");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#countValue(net.morilib.lisp.Datum)
	 */
	public int countValue(Datum d) {
		return 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#countKey(net.morilib.lisp.Datum)
	 */
	public int countKey(Datum d) {
		return 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#equalToMap(net.morilib.lisp.collection.LispMap, net.morilib.lisp.Procedure, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public boolean equalToMap(LispMap e, Procedure p, Environment env,
			LispMessage mesg) {
		return e.size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#equalToDictionary(net.morilib.lisp.collection.LispDictionary, net.morilib.lisp.Procedure, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public boolean equalToDictionary(LispDictionary e, Procedure p,
			Environment env, LispMessage mesg) {
		return e.size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispAddable#add(net.morilib.lisp.math.algebra.ILispAddable)
	 */
	public SExpression add(SExpression y) {
		return y;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispMultipliable#mul(net.morilib.lisp.math.algebra.ILispMultipliable)
	 */
	public SExpression mul(SExpression y) {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.ConsOrNil#getCar()
	 */
	public Datum getCar() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.ConsOrNil#getCdr()
	 */
	public Datum getCdr() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#isSorted(java.util.Comparator)
	 */
	public boolean isSorted(Comparator<Datum> cmp) {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#merge(net.morilib.lisp.sort.SRFI95Sequence, java.util.Comparator)
	 */
	public ConsOrNil merge(ConsOrNil m, Comparator<Datum> cmp) {
		return m;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#mergeS(net.morilib.lisp.sort.SRFI95Sequence, java.util.Comparator)
	 */
	public ConsOrNil mergeS(ConsOrNil m, Comparator<Datum> cmp) {
		return m;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#sort(java.util.Comparator)
	 */
	public ConsOrNil sort(Comparator<Datum> cmp) {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#sortS(java.util.Comparator)
	 */
	public void sortS(Comparator<Datum> cmp) {
		// do nothing
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#isTerminated()
	 */
	public boolean isTerminated() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#next()
	 */
	public ILispIterator next() {
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#getCurrentDatum()
	 */
	public Datum getCurrentDatum() {
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterable#lispIterator()
	 */
	public ILispIterator lispIterator() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.accessor.ILispRef#ref(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum ref(Datum arg, LispMessage mesg) {
		SubrUtils.getSmallInt(arg, mesg);
		throw mesg.getError("err.list.outofrange", arg);
	}

}
