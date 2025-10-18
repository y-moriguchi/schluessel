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

import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.SExpressionDatum;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;
import net.morilib.util.Sets;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/08
 */
public final class LispCollections {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/14
	 */
	public abstract static class BindBase extends TernaryArgs {

		/**
		 * @param env
		 * @param c
		 * @param f
		 * @param e
		 */
		protected abstract void bind(Environment env, Class<?> c,
				String f, String e);

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
				Environment env, LispMessage mesg) {
			try {
				Class<?> c = Class.forName(SubrUtils.getSymbolName(
						c1a, mesg));
				String   f = SubrUtils.getSymbolName(c2a, mesg);
				String   e = SubrUtils.getSymbolName(c3a, mesg);

				bind(env, c, f, e);
				return Undef.UNDEF;
			} catch (ClassNotFoundException e) {
				throw mesg.getError("err.java.class.notfound", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/14
	 */
	public static class BindEnumeration extends BindBase {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollections.BindBase#bind(net.morilib.lisp.Environment, java.lang.Class, java.lang.String, java.lang.String)
		 */
		@Override
		protected void bind(Environment env, Class<?> c, String f,
				String e) {
			bindEnumeration(env, c, f, e);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/14
	 */
	public static class BindEntryEnumeration extends BindBase {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollections.BindBase#bind(net.morilib.lisp.Environment, java.lang.Class, java.lang.String, java.lang.String)
		 */
		@Override
		protected void bind(Environment env, Class<?> c, String f,
				String e) {
			bindEntryEnumeration(env, c, f, e);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/14
	 */
	public static class BindCollection extends BindBase {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollections.BindBase#bind(net.morilib.lisp.Environment, java.lang.Class, java.lang.String, java.lang.String)
		 */
		@Override
		protected void bind(Environment env, Class<?> c, String f,
				String e) {
			bindCollection(env, c, f, e);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/14
	 */
	public static class BindOrderedCollection extends BindBase {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollections.BindBase#bind(net.morilib.lisp.Environment, java.lang.Class, java.lang.String, java.lang.String)
		 */
		@Override
		protected void bind(Environment env, Class<?> c, String f,
				String e) {
			bindOrderedCollection(env, c, f, e);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/14
	 */
	public static class BindDirectionalCollection extends BindBase {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollections.BindBase#bind(net.morilib.lisp.Environment, java.lang.Class, java.lang.String, java.lang.String)
		 */
		@Override
		protected void bind(Environment env, Class<?> c, String f,
				String e) {
			bindDirectionalCollection(env, c, f, e);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/14
	 */
	public static class BindBag extends BindBase {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollections.BindBase#bind(net.morilib.lisp.Environment, java.lang.Class, java.lang.String, java.lang.String)
		 */
		@Override
		protected void bind(Environment env, Class<?> c, String f,
				String e) {
			bindBag(env, c, f, e);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/14
	 */
	public static class BindSet extends BindBase {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollections.BindBase#bind(net.morilib.lisp.Environment, java.lang.Class, java.lang.String, java.lang.String)
		 */
		@Override
		protected void bind(Environment env, Class<?> c, String f,
				String e) {
			bindSet(env, c, f, e);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/14
	 */
	public static class BindSequence extends BindBase {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollections.BindBase#bind(net.morilib.lisp.Environment, java.lang.Class, java.lang.String, java.lang.String)
		 */
		@Override
		protected void bind(Environment env, Class<?> c, String f,
				String e) {
			bindSequence(env, c, f, e);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/14
	 */
	public static class BindFlexibleSequence extends BindBase {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollections.BindBase#bind(net.morilib.lisp.Environment, java.lang.Class, java.lang.String, java.lang.String)
		 */
		@Override
		protected void bind(Environment env, Class<?> c, String f,
				String e) {
			bindFlexibleSequence(env, c, f, e);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/14
	 */
	public static class BindMap extends BindBase {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollections.BindBase#bind(net.morilib.lisp.Environment, java.lang.Class, java.lang.String, java.lang.String)
		 */
		@Override
		protected void bind(Environment env, Class<?> c, String f,
				String e) {
			bindMap(env, c, f, e);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/14
	 */
	public static class BindDictionary extends BindBase {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollections.BindBase#bind(net.morilib.lisp.Environment, java.lang.Class, java.lang.String, java.lang.String)
		 */
		@Override
		protected void bind(Environment env, Class<?> c, String f,
				String e) {
			bindDictionary(env, c, f, e);
		}

	}

	//
	/*package*/ static void bindEnumeration(
			Environment env, Class<?> cls, String f, String err) {
		String err0 = "err.srfi44.require.enumeration";

		env.bindDatum(Symbol.getSymbol("collection-fold-left"),
				new LispEnumeration.EnumerationFoldLeft(
						LispEnumeration.class, err0));
		env.bindDatum(Symbol.getSymbol(f + "-fold-left"),
				new LispEnumeration.EnumerationFoldLeft(cls, err));
		env.bindDatum(Symbol.getSymbol("collection-fold-right"),
				new LispEnumeration.EnumerationFoldRight(
						LispEnumeration.class, err0));
		env.bindDatum(Symbol.getSymbol(f + "-fold-right"),
				new LispEnumeration.EnumerationFoldRight(cls, err));
	}

	//
	/*package*/ static void bindEntryEnumeration(
			Environment env, Class<?> cls, String f, String err) {
		String err0 = "err.srfi44.require.entryenumeration";

//		env.bindDatum(Symbol.getSymbol("collection-fold-left"),
//				new LispEntryEnumeration
//				.EntryEnumerationFoldLeft(cls, err0));
//		env.bindDatum(Symbol.getSymbol(f + "-fold-left"),
//				new LispEntryEnumeration
//				.EntryEnumerationFoldLeft(cls, err));
//		env.bindDatum(Symbol.getSymbol("collection-fold-right"),
//				new LispEntryEnumeration
//				.EntryEnumerationFoldRight(cls, err0));
//		env.bindDatum(Symbol.getSymbol(f + "-fold-right"),
//				new LispEntryEnumeration
//				.EntryEnumerationFoldRight(cls, err));
		env.bindDatum(Symbol.getSymbol("collection-fold-keys-left"),
				new LispEntryEnumeration
				.EntryEnumerationFoldKeysLeft(
						LispEntryEnumeration.class, err0));
		env.bindDatum(Symbol.getSymbol(f + "-fold-keys-left"),
				new LispEntryEnumeration
				.EntryEnumerationFoldKeysLeft(cls, err));
		env.bindDatum(Symbol.getSymbol("collection-fold-keys-right"),
				new LispEntryEnumeration
				.EntryEnumerationFoldKeysRight(
						LispEntryEnumeration.class, err0));
		env.bindDatum(Symbol.getSymbol(f + "-fold-keys-right"),
				new LispEntryEnumeration
				.EntryEnumerationFoldKeysRight(cls, err));
	}

	//
	/*package*/ static void bindCollection(
			Environment env, Class<?> cls, String f, String err) {
		bindEnumeration(env, cls, f, err);
		env.bindDatum(Symbol.getSymbol(f + "-size"),
				new LispCollection.CollectionSize(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-count"),
				new LispCollection.CollectionCount(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-get-any"),
				new LispCollection.CollectionGetAny(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-empty?"),
				new LispCollection.IsCollectionEmpty(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "->list"),
				new LispCollection.CollectionToList(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-clear"),
				new LispCollection.CollectionClear(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-clear!"),
				new LispCollection.CollectionClearS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "="),
				new LispCollection.CollectionEq(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-copy"),
				new LispCollection.CollectionCopy(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-contains?"),
				new LispCollection.IsCollectionContains(cls, err));
	}

	//
	/*package*/ static void bindOrderedCollection(
			Environment env, Class<?> cls, String f, String err) {
		bindCollection(env, cls, f, err);
		env.bindDatum(Symbol.getSymbol(f + "-ordering-function"),
				new LispOrderedCollection
				.OrderedCollectionOrderingFunction(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-get-left"),
				new LispOrderedCollection
				.OrderedCollectionGetLeft(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-get-right"),
				new LispOrderedCollection
				.OrderedCollectionGetRight(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-left"),
				new LispOrderedCollection
				.OrderedCollectionDeleteLeft(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-left!"),
				new LispOrderedCollection
				.OrderedCollectionDeleteLeftS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-right"),
				new LispOrderedCollection
				.OrderedCollectionDeleteRight(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-right!"),
				new LispOrderedCollection
				.OrderedCollectionDeleteRightS(cls, err));
	}

	//
	/*package*/ static void bindDirectionalCollection(
			Environment env, Class<?> cls, String f, String err) {
		bindCollection(env, cls, f, err);
		env.bindDatum(Symbol.getSymbol(f + "-get-left"),
				new LispDirectionalCollection
				.DirectedCollectionGetLeft(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-get-right"),
				new LispDirectionalCollection
				.DirectedCollectionGetRight(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-insert-left"),
				new LispDirectionalCollection
				.DirectedCollectionInsertLeft(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-insert-left!"),
				new LispDirectionalCollection
				.DirectedCollectionInsertLeftS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-insert-right"),
				new LispDirectionalCollection
				.DirectedCollectionInsertRight(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-insert-right!"),
				new LispDirectionalCollection
				.DirectedCollectionInsertRightS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-left"),
				new LispDirectionalCollection
				.DirectedCollectionDeleteLeft(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-left!"),
				new LispDirectionalCollection
				.DirectedCollectionDeleteLeftS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-right"),
				new LispDirectionalCollection
				.DirectedCollectionDeleteRight(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-right!"),
				new LispDirectionalCollection
				.DirectedCollectionDeleteRightS(cls, err));
	}

	//
	/*package*/ static void bindBag(
			Environment env, Class<?> cls, String f, String err) {
		bindCollection(env, cls, f, err);
		env.bindDatum(Symbol.getSymbol(f + "-equivalence-function"),
				new LispBag.BagEquivalenceFunction(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-add"),
				new LispBag.BagAdd(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-add!"),
				new LispBag.BagAddS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete"),
				new LispBag.BagDelete(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete!"),
				new LispBag.BagDeleteS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-all"),
				new LispBag.BagDeleteAll(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-all!"),
				new LispBag.BagDeleteAllS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-add-from"),
				new LispBag.BagAddFrom(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-add-from!"),
				new LispBag.BagAddFromS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-from"),
				new LispBag.BagDeleteFrom(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-from!"),
				new LispBag.BagDeleteFromS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-all-from"),
				new LispBag.BagDeleteAllFrom(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-all-from!"),
				new LispBag.BagDeleteAllFromS(cls, err));
	}

	//
	/*package*/ static void bindSet(
			Environment env, Class<?> cls, String f, String err) {
		bindCollection(env, cls, f, err);
		env.bindDatum(Symbol.getSymbol(f + "-equivalence-function"),
				new LispSet.SetEquivalenceFunction(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-subset?"),
				new LispSet.IsSubset(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-add"),
				new LispSet.SetAdd(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-add!"),
				new LispSet.SetAddS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete"),
				new LispSet.SetDelete(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete!"),
				new LispSet.SetDeleteS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-union"),
				new LispSet.SetUnion(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-union!"),
				new LispSet.SetUnionS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-intersection"),
				new LispSet.SetIntersection(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-intersection!"),
				new LispSet.SetIntersectionS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-difference"),
				new LispSet.SetDifference(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-difference!"),
				new LispSet.SetDifferenceS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-symmetric-difference"),
				new LispSet.SetSymmetricDifference(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-symmetric-difference!"),
				new LispSet.SetSymmetricDifferenceS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-add-from"),
				new LispSet.SetAddFrom(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-add-from!"),
				new LispSet.SetAddFromS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-from"),
				new LispSet.SetDeleteFrom(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-from!"),
				new LispSet.SetDeleteFromS(cls, err));
	}

	//
	/*package*/ static void bindSequence(
			Environment env, Class<?> cls, String f, String err) {
		bindBag(env, cls, f, err);
		env.bindDatum(Symbol.getSymbol(f + "-ref"),
				new LispSequence.SequenceRef(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-get-left"),
				new LispSequence.SequenceGetLeft(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-get-right"),
				new LispSequence.SequenceGetRight(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-set"),
				new LispSequence.SequenceSet(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-set!"),
				new LispSequence.SequenceSetS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-replace-from"),
				new LispSequence.SequenceReplaceFrom(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-replace-from!"),
				new LispSequence.SequenceReplaceFromS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-copy"),
				new LispSequence.SequenceCopy(cls, err));
	}

	//
	/*package*/ static void bindFlexibleSequence(
			Environment env, Class<?> cls, String f, String err) {
		bindDirectionalCollection(env, cls, f, err);
		bindSequence(env, cls, f, err);
		env.bindDatum(Symbol.getSymbol(f + "-insert"),
				new LispFlexibleSequence
				.FlexibleSequenceInsert(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-insert!"),
				new LispFlexibleSequence
				.FlexibleSequenceInsertS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-at"),
				new LispFlexibleSequence
				.FlexibleSequenceDeleteAt(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-at!"),
				new LispFlexibleSequence
				.FlexibleSequenceDeleteAtS(cls, err));
	}

	//
	/*package*/ static void bindMap(
			Environment env, Class<?> cls, String f, String err) {
		bindCollection(env, cls, f, err);
		bindEntryEnumeration(env, cls, f, err);
		env.bindDatum(Symbol.getSymbol(f + "-equivalence-function"),
				new LispMap.MapEquivalenceFunction(cls, err));
		env.bindDatum(
				Symbol.getSymbol(f + "-key-equivalence-function"),
				new LispMap.MapKeyEquivalenceFunction(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-count"),
				new LispMap.MapCount(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-contains-key?"),
				new LispMap.IsMapContainsKey(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-keys->list"),
				new LispMap.MapKeysToList(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "="),
				new LispMap.MapEq(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-get"),
				new LispMap.MapGet(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-put"),
				new LispMap.MapPut(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-put!"),
				new LispMap.MapPutS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-update"),
				new LispMap.MapUpdate(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-update!"),
				new LispMap.MapUpdateS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete"),
				new LispMap.MapDelete(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete!"),
				new LispMap.MapDeleteS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-from"),
				new LispMap.MapDeleteFrom(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-from!"),
				new LispMap.MapDeleteFromS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-add-from"),
				new LispMap.MapAddFrom(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-add-from!"),
				new LispMap.MapAddFromS(cls, err));
	}

	//
	/*package*/ static void bindDictionary(
			Environment env, Class<?> cls, String f, String err) {
		bindCollection(env, cls, f, err);
		bindEntryEnumeration(env, cls, f, err);
		env.bindDatum(Symbol.getSymbol(f + "-equivalence-function"),
				new LispDictionary
				.DictionaryEquivalenceFunction(cls, err));
		env.bindDatum(
				Symbol.getSymbol(f + "-key-equivalence-function"),
				new LispDictionary
				.DictionaryKeyEquivalenceFunction(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-count"),
				new LispDictionary.DictionaryCount(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-key-count"),
				new LispDictionary.DictionaryKeyCount(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-contains-key?"),
				new LispDictionary.IsDictionaryContainsKey(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-keys->list"),
				new LispDictionary.DictionaryKeysToList(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "="),
				new LispDictionary.DictionaryEq(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-get"),
				new LispDictionary.DictionaryGet(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-get-all"),
				new LispDictionary.DictionaryGetAll(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-put"),
				new LispDictionary.DictionaryPut(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-put!"),
				new LispDictionary.DictionaryPutS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-replace-all"),
				new LispDictionary.DictionaryReplaceAll(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-replace-all!"),
				new LispDictionary.DictionaryReplaceAllS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-update"),
				new LispDictionary.DictionaryUpdate(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-update!"),
				new LispDictionary.DictionaryUpdateS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-update-all"),
				new LispDictionary.DictionaryUpdateAll(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-update-all!"),
				new LispDictionary.DictionaryUpdateAllS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete"),
				new LispDictionary.DictionaryDelete(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete!"),
				new LispDictionary.DictionaryDeleteS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-all"),
				new LispDictionary.DictionaryDeleteAll(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-all!"),
				new LispDictionary.DictionaryDeleteAllS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-from"),
				new LispDictionary.DictionaryDeleteFrom(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-from!"),
				new LispDictionary.DictionaryDeleteFromS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-all-from"),
				new LispDictionary.DictionaryDeleteAllFrom(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-delete-all-from!"),
				new LispDictionary.DictionaryDeleteAllFromS(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-add-from"),
				new LispDictionary.DictionaryAddFrom(cls, err));
		env.bindDatum(Symbol.getSymbol(f + "-add-from!"),
				new LispDictionary.DictionaryAddFromS(cls, err));
	}

	/**
	 * 
	 * @param m1
	 * @param m2
	 * @param proc
	 * @param env
	 * @param mesg
	 * @return
	 */
	public static boolean contains(LispMap m1, LispMap m2,
			Procedure proc, Environment env,LispMessage mesg) {
		Iterator<Map.Entry<Datum, Datum>> itr;

		itr = m2.entryIterator();
		while(itr.hasNext()) {
			Map.Entry<Datum, Datum> e = itr.next();
			Datum p = m1.get(e.getKey());

			if(p == null) {
				return false;
			} else if(!Scheme.callva(proc, env, mesg, p,
					e.getValue()).isTrue()) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 
	 * @param m1
	 * @param m2
	 * @param proc
	 * @param env
	 * @param mesg
	 * @return
	 */
	public static boolean contains(LispDictionary m1,
			LispDictionary m2, Procedure proc, Environment env,
			LispMessage mesg) {
		for(Datum x : m2.keysToList()) {
			SExpressionDatum p = m2.getAllAsList(x);
			SExpressionDatum q = m1.getAllAsList(x);

			if(q.isNil()) {
				return false;
			} else if(!LispUtils.containsAsSet(p, q, proc, env,
					mesg)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static boolean equalsMapEntry(LispCollection a,
			LispCollection b) {
		for(Datum x : b) {
			if(x instanceof Cons) {
				if(!a.contains(x)) {
					return false;
				}
			} else {
				return false;
			}
		}

		for(Datum e : a) {
			if(!b.contains(e)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static boolean equals(LispCollection a,
			LispCollection b) {
		for(Datum x : b) {
			if(!a.contains(x)) {
				return false;
			}
		}

		for(Datum e : a) {
			if(!b.contains(e)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 
	 * @param m
	 * @param a
	 * @param b
	 * @return
	 */
	public static boolean equivalence(LispMap m, Datum a, Datum b) {
		if(a instanceof Cons && b instanceof Cons) {
			Cons x = (Cons)a;
			Cons y = (Cons)b;

			return (m.equivalenceKey  (x.getCar(), y.getCar()) &&
					m.equivalenceValue(x.getCdr(), y.getCdr()));
		} else {
			return m.equivalenceKey(a, b);
		}
	}

	/**
	 * 
	 * @param s
	 * @param col
	 * @param p
	 * @param env
	 * @param mesg
	 * @return
	 */
	public static boolean equalTo(LispCollection s, LispCollection col,
			Procedure p, Environment env, LispMessage mesg) {
		if(col instanceof LispSet) {
			if(s.size() != col.size()) {
				return false;
			} else {
				outer: for(Datum e : s) {
					for(Datum f : col) {
						if(Scheme.callva(
								p, env, mesg, e, f).isTrue()) {
							continue outer;
						}
					}
					return false;
				}
				return true;
			}
		} else {
			Set<Datum> a, b;

			a = Sets.toSet(s.iterator());
			b = Sets.toSet(col.iterator());
			return a.equals(b);
		}
	}

}
