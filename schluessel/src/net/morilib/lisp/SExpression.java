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
package net.morilib.lisp;

import java.util.Iterator;
import java.util.Map.Entry;

import net.morilib.lisp.collection.ImmutableException;
import net.morilib.lisp.collection.LispBag;
import net.morilib.lisp.collection.LispCollection;
import net.morilib.lisp.collection.LispDictionary;
import net.morilib.lisp.collection.LispFlexibleSequence;
import net.morilib.lisp.collection.LispMap;
import net.morilib.lisp.collection.LispSequence;
import net.morilib.lisp.math.algebra.ILispAddable;
import net.morilib.lisp.math.algebra.ILispMultipliable;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/14
 */
public interface SExpression
extends LispFlexibleSequence, LispMap, LispDictionary,
ILispAddable<SExpression>, ILispMultipliable<SExpression> {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#get(int)
	 */
	public Datum get(int index);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#copySet(int, net.morilib.lisp.Datum)
	 */
	public Datum copySet(int index, Datum d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#set(int, net.morilib.lisp.Datum)
	 */
	public Datum set(int index, Datum d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#replace(net.morilib.lisp.collection.LispSequence, int, int, int)
	 */
	public Datum replace(LispSequence src, int srcPos, int destPos,
			int len);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#arraycopy(net.morilib.lisp.collection.LispSequence, int, int, int)
	 */
	public Datum arraycopy(LispSequence src, int srcPos, int destPos,
			int len);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#copy(int, int)
	 */
	public Datum copy(int b, int e);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#equivalence()
	 */
	public Procedure equivalence();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyAdd(net.morilib.lisp.Datum)
	 */
	public Datum copyAdd(Datum d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#add(net.morilib.lisp.Datum)
	 */
	public Datum add(Datum d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDelete(net.morilib.lisp.Datum)
	 */
	public Datum copyDelete(Datum d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#delete(net.morilib.lisp.Datum)
	 */
	public Datum delete(Datum d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteAll(net.morilib.lisp.Datum)
	 */
	public Datum copyDeleteAll(Datum d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteAll(net.morilib.lisp.Datum)
	 */
	public Datum deleteAll(Datum d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyAddFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyAddFrom(LispBag d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#addFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum addFrom(LispBag d) throws ImmutableException;

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteFrom(LispBag d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteFrom(LispBag d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteAllFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteAllFrom(LispBag d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteAllFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteAllFrom(LispBag d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#getCollectionName()
	 */
	public Symbol getCollectionName();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#size()
	 */
	public int size();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equivalence(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public boolean equivalence(Datum a, Datum b);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#prototype()
	 */
	public Datum prototype();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#clear()
	 */
	public Datum clear();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection)
	 */
	public boolean equalTo(LispCollection col);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#duplicate()
	 */
	public Datum duplicate();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#contains(net.morilib.lisp.Datum)
	 */
	public boolean contains(Datum d);

	/* (non-Javadoc)
	 * @see java.lang.Iterable#iterator()
	 */
	public Iterator<Datum> iterator();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#first()
	 */
	public Datum first();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#last()
	 */
	public Datum last();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#insertFirst(net.morilib.lisp.Datum)
	 */
	public Datum insertFirst(Datum d) throws ImmutableException;

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#insertLast(net.morilib.lisp.Datum)
	 */
	public Datum insertLast(Datum d) throws ImmutableException;

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#copyInsertFirst(net.morilib.lisp.Datum)
	 */
	public Datum copyInsertFirst(Datum d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#copyInsertLast(net.morilib.lisp.Datum)
	 */
	public Datum copyInsertLast(Datum d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#removeFirst()
	 */
	public Datum[] removeFirst();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#removeLast()
	 */
	public Datum[] removeLast();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#copyWithoutFirst()
	 */
	public Datum[] copyWithoutFirst();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#copyWithoutLast()
	 */
	public Datum[] copyWithoutLast();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispFlexibleSequence#copyInsert(int, net.morilib.lisp.Datum)
	 */
	public Datum copyInsert(int index, Datum d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispFlexibleSequence#insert(int, net.morilib.lisp.Datum)
	 */
	public Datum insert(int index, Datum d);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispFlexibleSequence#copyDelete(int)
	 */
	public Datum copyDelete(int index);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispFlexibleSequence#delete(int)
	 */
	public Datum delete(int index);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispEntryEnumeration#entryIterator()
	 */
	public Iterator<Entry<Datum, Datum>> entryIterator();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#keyEquivalence()
	 */
	public Procedure keyEquivalence();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#valueEquivalence()
	 */
	public Procedure valueEquivalence();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#containsKey(net.morilib.lisp.Datum)
	 */
	public boolean containsKey(Datum k);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#keysToList()
	 */
	public SExpressionDatum keysToList();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#get(net.morilib.lisp.Datum)
	 */
	public Datum get(Datum k);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#copyPut(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public Datum[] copyPut(Datum k, Datum v);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#put(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public Datum[] put(Datum k, Datum v);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#copyAddFrom(net.morilib.lisp.collection.LispMap)
	 */
	public Datum copyAddFrom(LispMap m);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#addFrom(net.morilib.lisp.collection.LispMap)
	 */
	public Datum addFrom(LispMap m);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#copyDeleteFromMap(net.morilib.lisp.Datum)
	 */
	public Datum copyDeleteKey(Datum k);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#deleteFromMap(net.morilib.lisp.Datum)
	 */
	public Datum deleteKey(Datum k);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#count(net.morilib.lisp.Datum)
	 */
	public int count(Datum c2a);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#equivalenceKey(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public boolean equivalenceKey(Datum a, Datum b);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#equivalenceValue(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public boolean equivalenceValue(Datum a, Datum b);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#getAllAsList(net.morilib.lisp.Datum)
	 */
	public SExpressionDatum getAllAsList(Datum k);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#copyUpdateAll(net.morilib.lisp.Datum, net.morilib.lisp.Procedure, net.morilib.lisp.Procedure)
	 */
	public Datum copyUpdateAll(Datum k, Procedure f, Procedure th,
			Environment env, LispMessage mesg);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#updateAll(net.morilib.lisp.Datum, net.morilib.lisp.Procedure, net.morilib.lisp.Procedure)
	 */
	public Datum updateAll(Datum k, Procedure f, Procedure th,
			Environment env, LispMessage mesg);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#copyDeleteAllDictionary(net.morilib.lisp.Datum)
	 */
	public Datum copyDeleteAllKey(Datum k);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#deleteAllDictionary(net.morilib.lisp.Datum)
	 */
	public Datum deleteAllKey(Datum k);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#copyDeleteFromDictionary(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteFromKey(LispBag c2a);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#deleteFromDictionary(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteFromKey(LispBag c2a);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#copyDeleteAllFromDictionary(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteAllFromKey(LispBag c2a);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#deleteAllFromDictionary(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteAllFromKey(LispBag c2a);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#copyAddFromDictionary(net.morilib.lisp.collection.LispDictionary)
	 */
	public Datum copyAddFrom(LispDictionary c2a);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#addFromDictionary(net.morilib.lisp.collection.LispDictionary)
	 */
	public Datum addFrom(LispDictionary c2a);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#copyReplaceAll(net.morilib.lisp.Datum, net.morilib.lisp.Datum[])
	 */
	public Datum copyReplaceAll(Datum k, Datum[] array);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#replaceAll(net.morilib.lisp.Datum, net.morilib.lisp.Datum[])
	 */
	public Datum replaceAll(Datum k, Datum[] array);

}
