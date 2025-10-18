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

import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import net.morilib.lang.Hashes;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.collection.hash.Hash;
import net.morilib.lisp.compare.SRFI67;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/20
 */
public class LispTreeSet extends LispMutableSortedSetDatum {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/08/20
	 */
	public static class MakeTreeSet extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, final Environment env,
				final LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum op = Iterators.nextIf(itr);
			Procedure dop = new BinaryArgs() {

				@Override
				protected Datum execute(Datum x1, Datum x2,
						Environment env2, LispMessage mesg2) {
					Datum d =
						SRFI67.callCompareDefault(x1, x2, env, mesg);

					return LispBoolean.getInstance(d.getInt() < 0);
				}

			};

			if(op == null) {
				return new LispTreeSet(
						new TreeSet<Datum>(comparator(dop, env, mesg)),
						dop, env, mesg);
			} else if(op instanceof Procedure) {
				return new LispTreeSet(
						new TreeSet<Datum>(comparator(dop, env, mesg)),
						(Procedure)op, env, mesg);
			} else {
				throw mesg.getError("err.require.procedure", op);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/08/20
	 */
	public static class SubrTreeSet extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, final Environment env,
				final LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			SortedSet<Datum> st;
			Datum op = Iterators.nextIf(itr);
			Procedure op2;
			Procedure dop = new BinaryArgs() {

				@Override
				protected Datum execute(Datum x1, Datum x2,
						Environment env2, LispMessage mesg2) {
					Datum d =
						SRFI67.callCompareDefault(x1, x2, env, mesg);

					return LispBoolean.getInstance(d.getInt() < 0);
				}

			};

			if(op == null) {
				op2 = dop;
				st  = new TreeSet<Datum>(comparator(op2, env, mesg));
			} else if(op instanceof Procedure) {
				op2 = (Procedure)op;
				st  = new TreeSet<Datum>(comparator(op2, env, mesg));
				while(itr.hasNext()) {
					st.add(itr.next());
				}
			} else {
				op2 = dop;
				st  = new TreeSet<Datum>(comparator(op2, env, mesg));
				st.add(op);
				while(itr.hasNext()) {
					st.add(itr.next());
				}
			}
			return new LispTreeSet(st, op2, env, mesg);
		}
	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class IsTreeSet extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(c1a instanceof LispTreeSet);
		}

	}

	//
	private SortedSet<Datum> treeSet;
	private Procedure ordering;
	private Procedure eqproc;
	private Environment activeenv;
	private LispMessage message;

	//
	private LispTreeSet(SortedSet<Datum> set, final Procedure ordering,
			final Environment activeenv, final LispMessage message) {
		this.treeSet = set;
		this.ordering = ordering;
		this.eqproc = new BinaryArgs() {

			@Override
			protected Datum execute(Datum a, Datum b,
					Environment env, LispMessage mesg) {
				boolean r;

				r = !Scheme.callva(
						ordering, activeenv, message, a, b).isTrue();
				r = r && !Scheme.callva(
						ordering, activeenv, message, b, a).isTrue();
				return LispBoolean.getInstance(r);
			}

		};
		this.activeenv = activeenv;
		this.message = message;
	}

	//
	private static Comparator<Datum> comparator(final Procedure op,
			final Environment env, final LispMessage meg) {
		return new Comparator<Datum>() {

			public int compare(Datum a, Datum b) {
				if(Scheme.callva(op, env, meg, a, b).isTrue()) {
					return -1;
				} else if(Scheme.callva(op, env, meg, b, a).isTrue()) {
					return 1;
				} else {
					return 0;
				}
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispPurelyMutableCollection#clear()
	 */
	public Datum clear() {
		treeSet.clear();
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#getCollectionName()
	 */
	public Symbol getCollectionName() {
		return Symbol.getSymbol("tree-set");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#size()
	 */
	public int size() {
		return treeSet.size();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection)
	 */
	public boolean equalTo(LispCollection col) {
		return equalTo(col, eqproc, activeenv, message);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#contains(net.morilib.lisp.Datum)
	 */
	public boolean contains(Datum d) {
		return treeSet.contains(d);
	}

	/* (non-Javadoc)
	 * @see java.lang.Iterable#iterator()
	 */
	public Iterator<Datum> iterator() {
		return Iterators.unmodifiable(treeSet.iterator());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispOrderedCollection#ordering()
	 */
	public Procedure ordering() {
		return ordering;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispOrderedCollection#first()
	 */
	public Datum first() throws NoBoundsException {
		return treeSet.first();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispOrderedCollection#last()
	 */
	public Datum last() throws NoBoundsException {
		return treeSet.last();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#equivalence()
	 */
	public Procedure equivalence() {
		return eqproc;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#equivalence(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public boolean equivalence(Datum a, Datum b) {
		return Scheme.callva(
				eqproc, activeenv, message, a, b).isTrue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMutableSortedSetDatum#prototype()
	 */
	@Override
	public LispTreeSet prototype() {
		return new LispTreeSet(new TreeSet<Datum>(), ordering,
				activeenv, message);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMutableSortedSetDatum#duplicate()
	 */
	@Override
	public LispTreeSet duplicate() {
		return new LispTreeSet(new TreeSet<Datum>(treeSet),
				ordering, activeenv, message);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMutableSortedSetDatum#add(net.morilib.lisp.Datum)
	 */
	@Override
	public Datum add(Datum s) {
		treeSet.add(s);
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMutableSortedSetDatum#delete(net.morilib.lisp.Datum)
	 */
	@Override
	public Datum delete(Datum s) {
		treeSet.remove(s);
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#intersection(net.morilib.lisp.collection.LispSet)
	 */
	public LispSetDatum intersection(LispSet s) {
		return duplicate().retainAll(s);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#retainAll(net.morilib.lisp.collection.LispSet)
	 */
	public LispSetDatum retainAll(LispSet s) {
		Iterator<Datum> itr = treeSet.iterator();

		while(itr.hasNext()) {
			if(!s.contains(itr.next())) {
				itr.remove();
			}
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum3#getResult(java.lang.StringBuilder)
	 */
	@Override
	public void getResult(StringBuilder buf) {
		String dlm = "";

		buf.append("#<tree-set ");
		for(Datum d : this) {
			buf.append(dlm);
			buf.append(LispUtils.getResult(d));
			dlm = " ";
		}
		buf.append(">");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum3#getResult(java.lang.StringBuilder)
	 */
	@Override
	public void print(StringBuilder buf) {
		String dlm = "";

		buf.append("#<tree-set ");
		for(Datum d : this) {
			buf.append(dlm);
			buf.append(LispUtils.print(d));
			dlm = " ";
		}
		buf.append(">");
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		int r = Hashes.INIT;

		for(Datum d : this) {
			r = Hashes.A * r + Hash.hash(d);
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof LispCollection) {
			return equalTo((LispCollection)obj);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispEnumeratableTopology#getJavaSet()
	 */
	public Set<Datum> getJavaSet() {
		return Collections.unmodifiableSet(treeSet);
	}

}
