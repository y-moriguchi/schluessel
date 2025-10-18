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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import net.morilib.lisp.accessor.ILispRef;
import net.morilib.lisp.array.ILispArray;
import net.morilib.lisp.array.LispArrayPrototype;
import net.morilib.lisp.array.LispArrayShape;
import net.morilib.lisp.array.LispDefaultArray;
import net.morilib.lisp.collection.LispBag;
import net.morilib.lisp.collection.LispCollection;
import net.morilib.lisp.collection.LispPurelyMutableCollection;
import net.morilib.lisp.collection.LispSequence;
import net.morilib.lisp.iterator.ILispIterable;
import net.morilib.lisp.iterator.ILispIterator;
import net.morilib.lisp.math.algebra.ILispAddable;
import net.morilib.lisp.sort.SRFI95Sequence;
import net.morilib.lisp.sos.LispType;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.IsEqual;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;
import net.morilib.util.Lists;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class LispVector extends Datum
implements LispSequence, LispPurelyMutableCollection, ILispArray,
LispArrayPrototype, ILispVector, ILispAddable<LispVector>,
SRFI95Sequence<LispVector>, ILispIterable, ILispRef,
java.io.Serializable {

	//
	private static final long serialVersionUID = 3364203058083838114L;

	/**
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/01/09
	 */
	public static class VectorSortS extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a,
				final Environment env, final LispMessage mesg) {
			final Procedure p = SubrUtils.getProcedure(c1a, mesg);
			Comparator<Datum> cmp = new Comparator<Datum>() {

				public int compare(Datum o1, Datum o2) {
					if(Scheme.callva(p, env, mesg, o1, o2).isTrue()) {
						return -1;
					} else if(Scheme.callva(p, env, mesg,
							o2, o1).isTrue()) {
						return 1;
					} else {
						return 0;
					}
				}

			};

			if(c2a instanceof LispVector) {
				Collections.sort(((LispVector)c2a).vec, cmp);
				return Undef.UNDEF;
			} else {
				throw mesg.getError("err.require.vector", c2a);
			}
		}

	}

	//
	private List<Datum> vec;

	//
	private static final List<Datum> _EMPTY = Collections.emptyList();

	/**
	 * 
	 */
	public  static final LispVector  EMPTY  = new LispVector(_EMPTY);

	/**
	 * 
	 * @param vec
	 */
	public LispVector(Collection<Datum> vec) {
		if(vec == null) {
			throw new NullPointerException();
		}
		this.vec = new ArrayList<Datum>(vec);
	}

	/**
	 * 
	 * @param data
	 */
	public LispVector(Datum... data) {
		this(Arrays.asList(data));
	}

	/**
	 * 
	 */
	public LispVector() {
		this(new ArrayList<Datum>());
	}

	/**
	 * 
	 * @param vec
	 */
	public LispVector(LispVector vec) {
		if(vec == null) {
			throw new NullPointerException();
		}
		this.vec = new ArrayList<Datum>(vec.vec);
	}

	/**
	 * 
	 */
	public LispVector(int size) {
		this(new ArrayList<Datum>(size));
	}

	/**
	 * 
	 * @param index
	 * @return
	 */
	public Datum get(int index) {
		return vec.get(index);
	}

	//
	/*package*/ void setS(int index, Datum d) {
		vec.set(index, d);
	}

	/**
	 * 
	 * @return
	 */
	public int size() {
		return vec.size();
	}

	/**
	 * 
	 * @return
	 */
	public Datum toConsList() {
		return LispUtils.listToCons(vec);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toString()
	 */
	public String toString() {
		return LispUtils.getResult(this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#isTypeVector()
	 */
	public boolean isTypeVector() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getList()
	 */
	public List<Datum> getList() {
		return Collections.unmodifiableList(vec);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	@Override
	public LispType getType() {
		return LispType.VECTOR;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#copySet(int, net.morilib.lisp.Datum)
	 */
	public Datum copySet(int index, Datum d) {
		return duplicate().set(index, d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#set(int, net.morilib.lisp.Datum)
	 */
	public Datum set(int index, Datum d) {
		setS(index, d);
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#replace(net.morilib.lisp.collection.LispSequence, int, int, int)
	 */
	public Datum replace(LispSequence src, int srcPos, int destPos,
			int len) {
		return duplicate().arraycopy(src, srcPos, destPos, len);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#arraycopy(net.morilib.lisp.collection.LispSequence, int, int, int)
	 */
	public Datum arraycopy(LispSequence src, int srcPos, int destPos,
			int len) {
		for(int i = 0; i < len; i++) {
			setS(i + destPos, src.get(i + srcPos));
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#copy(int, int)
	 */
	public Datum copy(int b, int e) {
		LispVector r = new LispVector();

		if(b < 0 || b >= size()) {
			throw new IndexOutOfBoundsException("" + b);
		}
		for(int i = b; i < e; i++) {
			r.vec.add(get(i));
		}
		return r;
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
		return duplicate().add(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#add(net.morilib.lisp.Datum)
	 */
	public Datum add(Datum d) {
		vec.add(d);
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDelete(net.morilib.lisp.Datum)
	 */
	public Datum copyDelete(Datum d) {
		return duplicate().delete(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#delete(net.morilib.lisp.Datum)
	 */
	public Datum delete(Datum d) {
		for(int i = 0; i < vec.size(); i++) {
			vec.remove(i);
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteAll(net.morilib.lisp.Datum)
	 */
	public Datum copyDeleteAll(Datum d) {
		return duplicate().deleteAll(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteAll(net.morilib.lisp.Datum)
	 */
	public Datum deleteAll(Datum d) {
		vec.remove(d);
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyAddFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyAddFrom(LispBag d) {
		return duplicate().addFrom(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#addFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum addFrom(LispBag d) {
		for(Datum x : d) {
			vec.add(x);
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteFrom(LispBag d) {
		return duplicate().deleteFrom(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteFrom(LispBag d) {
		for(Datum x : d) {
			int i = vec.indexOf(x);

			if(i >= 0) {
				vec.remove(i);
			}
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteAllFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteAllFrom(LispBag d) {
		return duplicate().deleteAllFrom(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteAllFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteAllFrom(LispBag d) {
		for(Datum x : d) {
			vec.remove(x);
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#getCollectionName()
	 */
	public Symbol getCollectionName() {
		return Symbol.getSymbol("vector");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equivalence(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public boolean equivalence(Datum a, Datum b) {
		return LispUtils.equals(a, b);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#prototype()
	 */
	public Datum prototype() {
		return new LispVector();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#clear()
	 */
	public Datum clear() {
		vec.clear();
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection)
	 */
	public boolean equalTo(LispCollection col) {
		Iterator<Datum> itr = col.iterator();
		int i = 0;

		for(; i < vec.size() && itr.hasNext(); i++) {
			if(!LispUtils.equals(itr.next(), vec.get(i))) {
				return false;
			}
		}
		return i >= vec.size() && !itr.hasNext();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection, net.morilib.lisp.Procedure, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public boolean equalTo(LispCollection col, Procedure proc,
			Environment env, LispMessage mesg) {
		Iterator<Datum> itr = col.iterator();
		int i = 0;

		for(; i < vec.size() && itr.hasNext(); i++) {
			if(!Scheme.callva(proc, env, mesg, vec.get(i),
					itr.next()).isTrue()) {
				return false;
			}
		}
		return i >= vec.size() && !itr.hasNext();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#duplicate()
	 */
	public LispVector duplicate() {
		return new LispVector(this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#contains(net.morilib.lisp.Datum)
	 */
	public boolean contains(Datum d) {
		return vec.contains(d);
	}

	/* (non-Javadoc)
	 * @see java.lang.Iterable#iterator()
	 */
	public Iterator<Datum> iterator() {
		return Iterators.unmodifiable(vec.iterator());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#count(net.morilib.lisp.Datum)
	 */
	public int count(Datum c2a) {
		int r = 0;

		for(int i = 0; i < vec.size(); i++) {
			if(LispUtils.equals(vec.get(i), c2a)) {
				r++;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#toList()
	 */
	public Datum toList() {
		return LispUtils.toCons(vec);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#first()
	 */
	public Datum first() {
		if(vec.isEmpty()) {
			throw new NoSuchElementException();
		}
		return vec.get(0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#last()
	 */
	public Datum last() {
		if(vec.isEmpty()) {
			throw new NoSuchElementException();
		}
		return vec.get(vec.size() - 1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#rank()
	 */
	public int rank() {
		return 1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#startIndex(int)
	 */
	public int startIndex(int dim) {
		if(dim != 0) {
			throw new IndexOutOfBoundsException();
		}
		return 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#endIndex(int)
	 */
	public int endIndex(int dim) {
		if(dim != 0) {
			throw new IndexOutOfBoundsException();
		}
		return size();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#toVector()
	 */
	public LispVector toVector() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getFromArray(int[])
	 */
	public Datum getFromArray(int... is) {
		if(is.length != 1) {
			throw new IndexOutOfBoundsException();
		}
		return vec.get(is[0]);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#setToArray(net.morilib.lisp.Datum, int[])
	 */
	public void setToArray(Datum d, int... is) {
		if(is.length != 1) {
			throw new IndexOutOfBoundsException();
		}
		vec.set(is[0], d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#isIndexEqualTo(net.morilib.lisp.array.LispArray)
	 */
	public boolean isIndexEqualTo(ILispArray a) {
		return (a.rank() == 1 &&
				a.startIndex(0) == 0 &&
				a.endIndex(0) == size());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#isEqualTo(net.morilib.lisp.array.LispArray)
	 */
	public boolean isEqualTo(ILispArray a) {
		if(!isIndexEqualTo(a)) {
			return false;
		} else {
			for(int i = 0; i < a.endIndex(0); i++) {
				if(!LispUtils.equals(vec.get(i), a.getFromArray(i))) {
					return false;
				}
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getTypeSpecifier()
	 */
	public String getTypeSpecifier() {
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArrayPrototype#makeArray(int[])
	 */
	public ILispArray makeArray(int... is) {
		int[] bi = new int[is.length];
		int[] ei = new int[is.length];

		Arrays.fill(bi, 0);
		System.arraycopy(is, 0, ei, 0, is.length);
		return LispDefaultArray.malloc(bi, ei,
				vec.isEmpty() ? Undef.UNDEF : vec.get(0));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#fill(java.util.Iterator)
	 */
	public void fill(Iterator<Datum> itr) {
		for(int i = 0; i < vec.size() && itr.hasNext(); i++) {
			vec.set(i, itr.next());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getShape()
	 */
	public LispArrayShape getShape() {
		return new LispArrayShape(new int[] { vec.size() });
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispAddable#add(net.morilib.lisp.math.algebra.ILispAddable)
	 */
	public LispVector add(LispVector y) {
		List<Datum> x = new ArrayList<Datum>(vec);

		x.addAll(y.vec);
		return new LispVector(x);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#isSorted(java.util.Comparator)
	 */
	public boolean isSorted(Comparator<Datum> cmp) {
		return Lists.isSorted(vec, cmp);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#merge(net.morilib.lisp.sort.SRFI95Sequence, java.util.Comparator)
	 */
	public LispVector merge(LispVector m, Comparator<Datum> cmp) {
		LispVector r = new LispVector(this);

		r.mergeS(m, cmp);
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#mergeS(net.morilib.lisp.sort.SRFI95Sequence, java.util.Comparator)
	 */
	public LispVector mergeS(LispVector m, Comparator<Datum> cmp) {
		Lists.merge(vec, m.vec, cmp);
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#sort(java.util.Comparator)
	 */
	public LispVector sort(Comparator<Datum> cmp) {
		LispVector r = new LispVector(this);

		r.sortS(cmp);
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#sortS(java.util.Comparator)
	 */
	public void sortS(Comparator<Datum> cmp) {
		Collections.sort(vec, cmp);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterable#lispIterator()
	 */
	public ILispIterator lispIterator() {
		final int ptr[] = new int[1];

		ptr[0] = 0;
		return new ILispIterator() {

			public boolean isTerminated() {
				return vec.size() <= ptr[0];
			}

			public ILispIterator next() {
				if(ptr[0]++ >= vec.size()) {
					throw new NoSuchElementException();
				}
				return this;
			}

			public Datum getCurrentDatum() {
				if(isTerminated()) {
					throw new NoSuchElementException();
				}
				return vec.get(ptr[0]);
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.accessor.ILispRef#ref(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum ref(Datum arg, LispMessage mesg) {
		int k = SubrUtils.getSmallInt(arg, mesg);

		if(k < 0 || k >= vec.size()) {
			throw mesg.getError("err.vector.outofrange", arg);
		}
		return vec.get(k);
	}

}
