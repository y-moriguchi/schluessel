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
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import net.morilib.lisp.accessor.ILispRef;
import net.morilib.lisp.collection.ImmutableException;
import net.morilib.lisp.collection.LispBag;
import net.morilib.lisp.collection.LispCollection;
import net.morilib.lisp.collection.LispCollections;
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
public final class Cons extends SExpressionDatum
implements ConsOrNil, ILispIterable, ILispIterator, ILispRef,
java.io.Serializable {

	//
	private Datum car;
	private Datum cdr;

	/**
	 * 
	 */
	public Cons() {
		this.car = Nil.NIL;
		this.cdr = Nil.NIL;
	}

	/**
	 * 
	 * @param car
	 * @param cdr
	 */
	public Cons(Datum car, Datum cdr) {
		if(car == null) {
			throw new NullPointerException("car is null");
		}
		if(cdr == null) {
			throw new NullPointerException("cdr is null");
		}

		this.car = car;
		this.cdr = cdr;
	}

	/**
	 * @return the car
	 */
	public Datum getCar() {
		return car;
	}

	/**
	 * @param car the car to set
	 */
	/*package*/ void setCar(Datum car) {
		this.car = car;
	}

	/**
	 * @return the cdr
	 */
	public Datum getCdr() {
		return cdr;
	}

	/**
	 * @param cdr the cdr to set
	 */
	/*package*/ void setCdr(Datum cdr) {
		this.cdr = cdr;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#isTypeList()
	 */
	public boolean isTypeList() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#isDottedList()
	 */
	public boolean isDottedList() {
		return !getDottedDatum().isNil();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getList()
	 */
	public List<Datum> getList() {
		List<Datum> res = new ArrayList<Datum>();
		Datum p = cdr;

		res.add(car);
		while(true) {
			if(p instanceof Cons) {
				Cons p2 = (Cons)p;

				res.add(p2.car);
				p = p2.cdr;
			} else {
				return Collections.unmodifiableList(res);
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getDottedDatum()
	 */
	public Datum getDottedDatum() {
		Datum p = cdr;

		while(p instanceof Cons) {
			p = ((Cons)p).cdr;
		}
		return p;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toString()
	 */
	public String toString() {
		return LispUtils.getResult(this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	@Override
	public LispType getType() {
		return LispType.PAIR;
	}

	/**
	 * 
	 * @return
	 */
	public Datum copy(DatumPredicate p) {
		Datum q = this;
		Cons  r = null, r0 = null;
		Map<Cons, Datum> h = new HashMap<Cons, Datum>();

		while(q instanceof Cons) {
			Cons c = (Cons)q;

			if(p.test(c.car)) {
				if(r == null) {
					r0 = r = new Cons();
				} else {
					r.cdr = new Cons();
					r = (Cons)r.cdr;
				}
				r.car = c.car;
			}

			h.put(c, c.cdr);
			if(h.containsKey(c.cdr)) {
				// circular list
				if(r == null) {
					return Nil.NIL;
				} else {
					r.cdr = h.get(c.cdr);
					return r0;
				}
			}
			q = c.cdr;
		}
		r.cdr = q;
		return (r0 == null) ? Nil.NIL : r0;
	}

	/**
	 * 
	 * @return
	 */
	public Cons copy() {
		Datum d = this;
		Cons  r = new Cons(), r0 = r;
		Map<Cons, Datum> h = new HashMap<Cons, Datum>();
		Map<Cons, Datum> g = new HashMap<Cons, Datum>();

		while(d instanceof Cons) {
			Cons c = (Cons)d;

			r.car = c.car;
			if(h.containsKey(c.cdr)) {
				// circular list
				r.cdr = g.get(c.cdr);
				return r0;
			}

			h.put(c, c.cdr);
			if(c.cdr instanceof Cons) {
				r.cdr = new Cons();
				g.put(c, r.cdr);
				r = (Cons)r.cdr;
			} else {
				r.cdr = c.cdr;
			}
			d = c.cdr;
		}
		return r0;
	}

	/**
	 * 
	 * @return
	 */
	public Datum delete(DatumPredicate p) {
		Datum d = this;
		Cons  r = null, r0 = null;
		Map<Cons, Datum> h = new HashMap<Cons, Datum>();

		while(d instanceof Cons) {
			Cons c = (Cons)d;

			if(p.test(c.car)) {
				if(r == null) {
					r0 = c;
				}
				r = c;
				r.car = c.car;
			} else {
				if(r != null) {
					r.cdr = c.cdr;
				}
			}

			h.put(c, c.cdr);
			if(h.containsKey(c.cdr)) {
				// circular list
				if(r == null) {
					return Nil.NIL;
				} else {
					r.cdr = h.get(c.cdr);
					return r0;
				}
			} else {
				d = c.cdr;
			}
		}
		return (r0 == null) ? Nil.NIL : r0;
	}

	//
	private Cons _get(int index) {
		Cons r = this;

		if(index < 0) {
			throw new IndexOutOfBoundsException("" + index);
		}

		for(int i = 0; i < index; i++) {
			if(r.cdr instanceof Cons) {
				r = (Cons)r.cdr;
			} else {
				throw new IndexOutOfBoundsException("" + i);
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#get(int)
	 */
	public Datum get(int index) {
		return _get(index).car;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#copySet(int, net.morilib.lisp.Datum)
	 */
	public Datum copySet(int index, Datum d) {
		return copy().set(index, d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#set(int, net.morilib.lisp.Datum)
	 */
	public Datum set(int index, Datum d) {
		_get(index).car = d;
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#replace(net.morilib.lisp.collection.LispSequence, int, int, int)
	 */
	public Datum replace(LispSequence src, int srcPos, int destPos,
			int len) {
		return copy().arraycopy(src, srcPos, destPos, len);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#arraycopy(net.morilib.lisp.collection.LispSequence, int, int, int)
	 */
	public Datum arraycopy(LispSequence src, int srcPos, int destPos,
			int len) {
		Datum d1 = _get(destPos);

		for(int i = srcPos; i < srcPos + len; i++) {
			if(d1 instanceof Cons) {
				((Cons)d1).car = src.get(i);
				d1 = ((Cons)d1).cdr;
			} else {
				throw new IndexOutOfBoundsException("" + i);
			}
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#copy(int, int)
	 */
	public Datum copy(int b, int e) {
		Cons  r = new Cons(), r0;
		Datum c = _get(b);

		if(b == e) {
			return Nil.NIL;
		}

		r0 = r;
		for(int i = b; i < e; i++) {
			if(c instanceof Cons) {
				r.car = ((Cons)c).car;
				if(i < e - 1 && ((Cons)c).cdr instanceof Cons) {
					r.cdr = new Cons();
					r = (Cons)r.cdr;
				}
				c = ((Cons)c).cdr;
			} else {
				throw new IndexOutOfBoundsException("" + i);
			}
		}
		return r0;
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
		Cons r = new Cons();

		r.car = d;
		r.cdr = copy();
		return r;
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
		final Datum x = d;
		final boolean[] b = new boolean[1];

		b[0] = false;
		return copy(new DatumPredicate() {

			public boolean test(Datum d) {
				return b[0] || !(b[0] = LispUtils.equals(x, d));
			}

		});
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#delete(net.morilib.lisp.Datum)
	 */
	public Datum delete(Datum d) {
		final Datum x = d;
		final boolean[] b = new boolean[1];

		b[0] = false;
		return delete(new DatumPredicate() {

			public boolean test(Datum d) {
				return b[0] || !(b[0] = LispUtils.equals(x, d));
			}

		});
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteAll(net.morilib.lisp.Datum)
	 */
	public Datum copyDeleteAll(Datum d) {
		final Datum x = d;

		return copy(new DatumPredicate() {

			public boolean test(Datum d) {
				return !LispUtils.equals(x, d);
			}

		});
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteAll(net.morilib.lisp.Datum)
	 */
	public Datum deleteAll(Datum d) {
		final Datum x = d;

		return delete(new DatumPredicate() {

			public boolean test(Datum d) {
				return !LispUtils.equals(x, d);
			}

		});
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyAddFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyAddFrom(LispBag d) {
		if(d.size() == 0) {
			return copy();
		} else {
			return LispUtils.copy(d.iterator(), copy());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#addFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum addFrom(LispBag d) throws ImmutableException {
		if(d.size() == 0) {
			return this;
		} else {
			return LispUtils.nconc(this, LispUtils.toCons(d));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteFrom(LispBag d) {
		return copy().deleteFrom(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteFrom(LispBag d) {
		SExpressionDatum r = this;

		for(Datum x : d) {
			r = (SExpressionDatum)r.delete(x);
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteAllFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteAllFrom(LispBag d) {
		final LispBag e = d;

		return copy(new DatumPredicate() {

			public boolean test(Datum x) {
				return !e.contains(x);
			}

		});
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteAllFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteAllFrom(LispBag d) {
		final LispBag e = d;

		return delete(new DatumPredicate() {

			public boolean test(Datum x) {
				return !e.contains(x);
			}

		});
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
		Datum p = this;
		Set<Datum> s = new HashSet<Datum>();
		int r = 0;

		while(p instanceof Cons) {
			if(s.contains(p)) {
				return -1;
			}
			r++;
			s.add(p);
			p = ((Cons)p).cdr;
		}
		return r;
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
		Cons p = this;

		for(Datum x : col) {
			if(!(p instanceof Cons)) {
				return p.getCdr().isNil();
			} else if(!LispUtils.equals(x, p.car)) {
				return false;
			}
			p = (Cons)p.cdr;
		}
		return p.isNil();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection, net.morilib.lisp.Procedure, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public boolean equalTo(LispCollection col, Procedure proc,
			Environment env, LispMessage mesg) {
		Datum p = this;

		for(Datum x : col) {
			if(!(p instanceof Cons)) {
				return ((Cons)p).getCdr().isNil();
			} else if(!Scheme.callva(proc, env, mesg, ((Cons)p).car,
					x).isTrue()) {
				return false;
			}
			p = ((Cons)p).cdr;
		}
		return p.isNil();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#duplicate()
	 */
	public Datum duplicate() {
		return copy();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#contains(net.morilib.lisp.Datum)
	 */
	public boolean contains(Datum d) {
		for(Datum p = this; p instanceof Cons; p = ((Cons)p).cdr) {
			if(LispUtils.equals(d, ((Cons)p).car)) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Iterable#iterator()
	 */
	public Iterator<Datum> iterator() {
		return new ConsIterator(this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#first()
	 */
	public Datum first() {
		return car;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#last()
	 */
	public Datum last() {
		Cons p = this;

		for(; p.cdr instanceof Cons; p = (Cons)p.cdr);
		return p.car;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#insertFirst(net.morilib.lisp.Datum)
	 */
	public Datum insertFirst(Datum d) throws ImmutableException {
		return new Cons(d, this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#insertLast(net.morilib.lisp.Datum)
	 */
	public Datum insertLast(Datum d) throws ImmutableException {
		Cons p = this;

		for(; p.cdr instanceof Cons; p = (Cons)p.cdr);
		p.cdr = new Cons(d, p.cdr);
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#copyInsertFirst(net.morilib.lisp.Datum)
	 */
	public Datum copyInsertFirst(Datum d) {
		return new Cons(d, copy());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#copyInsertLast(net.morilib.lisp.Datum)
	 */
	public Datum copyInsertLast(Datum d) {
		Cons p = copy(), q = p;

		for(; p.cdr instanceof Cons; p = (Cons)p.cdr);
		p.cdr = new Cons(d, p.cdr);
		return q;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#removeFirst()
	 */
	public Datum[] removeFirst() {
		return new Datum[] { cdr, car };
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#removeLast()
	 */
	public Datum[] removeLast() {
		Cons p = this, p0 = null;

		for(; p.cdr instanceof Cons; p = (Cons)p.cdr) {
			p0 = (p0 == null) ? this : p;
		}

		if(p0 == null) {
			return new Datum[] { Nil.NIL, p.car };
		} else {
			p0.cdr = Nil.NIL;
			return new Datum[] { this, p.car };
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#copyWithoutFirst()
	 */
	public Datum[] copyWithoutFirst() {
		if(cdr instanceof Cons) {
			return new Datum[] { ((Cons)cdr).copy(), car };
		} else {
			return new Datum[] { Nil.NIL, car };
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDirectionalCollection#copyWithoutLast()
	 */
	public Datum[] copyWithoutLast() {
		return copy().removeLast();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispFlexibleSequence#copyInsert(int, net.morilib.lisp.Datum)
	 */
	public Datum copyInsert(int index, Datum d) {
		return copy().insert(index, d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispFlexibleSequence#insert(int, net.morilib.lisp.Datum)
	 */
	public Datum insert(int index, Datum d) {
		if(index < 0) {
			throw new IndexOutOfBoundsException("" + index);
		} else if(index == 0) {
			return new Cons(d, this);
		} else {
			Datum p = this;
			Cons  q = this;

			for(int i = 0; i < index; i++) {
				if(p instanceof Cons) {
					q = (Cons)p;
					p = ((Cons)p).cdr;
				} else {
					throw new IndexOutOfBoundsException("" + i);
				}
			}
			q.cdr = new Cons(d, p);
			return this;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispFlexibleSequence#copyDelete(int)
	 */
	public Datum copyDelete(int index) {
		return copy().delete(index);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispFlexibleSequence#delete(int)
	 */
	public Datum delete(int index) {
		Cons p = this, q = null;

		if(index < 0) {
			throw new IndexOutOfBoundsException("" + index);
		}

		for(int i = 0; i < index; i++) {
			if(p.cdr instanceof Cons) {
				q = p;
				p = (Cons)p.cdr;
			} else {
				throw new IndexOutOfBoundsException();
			}
		}

		if(q == null) {
			return p.cdr;
		} else {
			q.cdr = p.cdr;
			return this;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispEntryEnumeration#entryIterator()
	 */
	public Iterator<Entry<Datum, Datum>> entryIterator() {
		final Iterator<Datum> itr = iterator();

		return new Iterator<Entry<Datum, Datum>>() {

			public boolean hasNext() {
				return itr.hasNext();
			}

			public Entry<Datum, Datum> next() {
				final Datum d = itr.next();

				if(d instanceof Cons) {
					final Cons c = (Cons)d;

					return new Entry<Datum, Datum>() {

						public Datum getKey() {
							return c.getCar();
						}

						public Datum getValue() {
							return c.getCdr();
						}

						public Datum setValue(Datum value) {
							throw new UnsupportedOperationException();
						}

					};
				} else {
					return new Entry<Datum, Datum>() {

						public Datum getKey() {
							return d;
						}

						public Datum getValue() {
							return Undef.UNDEF;
						}

						public Datum setValue(Datum value) {
							throw new UnsupportedOperationException();
						}

					};
				}
			}

			public void remove() {
				itr.remove();
			}

		};
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
		for(Datum x = this; x instanceof Cons; x = ((Cons)x).cdr) {
			Cons c = (Cons)x;

			if(c.car instanceof Cons) {
				if(LispUtils.equals(k, ((Cons)c.car).car)) {
					return true;
				}
			} else if(LispUtils.equals(k, c.car)) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#keysToList()
	 */
	public SExpressionDatum keysToList() {
		ConsListBuilder b = new ConsListBuilder();

		for(Datum x = this; x instanceof Cons; x = ((Cons)x).cdr) {
			Cons c = (Cons)x;

			if(c.car instanceof Cons) {
				b.append(((Cons)c.car).car);
			} else {
				b.append(c.car);
			}
		}
		return b.get();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#get(net.morilib.lisp.Datum)
	 */
	public Datum get(Datum k) {
		for(Datum x = this; x instanceof Cons; x = ((Cons)x).cdr) {
			Cons c = (Cons)x;

			if(c.car instanceof Cons) {
				if(LispUtils.equals(k, ((Cons)c.car).car)) {
					return ((Cons)c.car).cdr;
				}
			} else if(LispUtils.equals(k, c.car)) {
				return Undef.UNDEF;
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#copyPut(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public Datum[] copyPut(Datum k, Datum v) {
		return copy().put(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#put(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public Datum[] put(Datum k, Datum v) {
		Datum[] r = new Datum[2];

		for(Datum x = this; x instanceof Cons; x = ((Cons)x).cdr) {
			Cons c = (Cons)x;

			if(c.car instanceof Cons) {
				if(LispUtils.equals(k, ((Cons)c.car).car)) {
					r[0] = this;
					r[1] = ((Cons)c.car).cdr;
					((Cons)c.car).cdr = v;
					return r;
				}
			} else if(LispUtils.equals(k, c.car)) {
				r[0] = this;
				r[1] = Undef.UNDEF;
				c.car = new Cons(k, v);
				return r;
			}
		}

		r[0] = new Cons(new Cons(k, v), this);
		r[1] = null;
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#copyAddFrom(net.morilib.lisp.collection.LispMap)
	 */
	public Datum copyAddFrom(LispMap m) {
		return copy().addFrom(m);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#addFrom(net.morilib.lisp.collection.LispMap)
	 */
	public Datum addFrom(LispMap m) {
		Iterator<Map.Entry<Datum, Datum>> itr = m.entryIterator();
		Datum r = this;

		while(itr.hasNext()) {
			Map.Entry<Datum, Datum> e = itr.next();

			r = put(e.getKey(), e.getValue())[0];
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#copyDeleteFromMap(net.morilib.lisp.Datum)
	 */
	public Datum copyDeleteKey(Datum k) {
		return copy().deleteKey(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#deleteFromMap(net.morilib.lisp.Datum)
	 */
	public Datum deleteKey(Datum k) {
		Datum x = this, s = null;
		Cons  r = null;

		while(x instanceof Cons) {
			Cons c = (Cons)x;

			if(c.car instanceof Cons &&
					LispUtils.equals(k, ((Cons)c.car).car)) {
				if(s != null) {
					r.cdr = c.cdr;
				} else {
					s = c.cdr;
				}
				break;
			} else if(LispUtils.equals(k, c.car)) {
				if(s != null) {
					r.cdr = c.cdr;
				} else {
					s = c.cdr;
				}
				break;
			} else {
				if(s == null) {
					s = c;
				}
				r = c;
			}
			x = c.cdr;
		}
		return (s == null) ? Nil.NIL : s;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#count(net.morilib.lisp.Datum)
	 */
	public int count(Datum c2a) {
		int r = 0;

		for(Datum x : this) {
			if(equivalence(x, c2a)) {
				r++;
			}
		}
		return r;
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
	 * @see net.morilib.lisp.collection.LispDictionary#getAllAsList(net.morilib.lisp.Datum)
	 */
	public SExpressionDatum getAllAsList(Datum k) {
		ConsListBuilder b = new ConsListBuilder();

		for(Datum d : this) {
			if(d instanceof Cons) {
				Cons c = (Cons)d;

				if(equivalenceKey(c.getCar(), k)) {
					b.append(c.getCdr());
				}
			} else if(equivalenceKey(d, k)) {
				b.append(Undef.UNDEF);
			}
		}
		return b.get();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#copyUpdateAll(net.morilib.lisp.Datum, net.morilib.lisp.Procedure, net.morilib.lisp.Procedure)
	 */
	public Datum copyUpdateAll(Datum k, Procedure f, Procedure th,
			Environment env, LispMessage mesg) {
		return copy().updateAll(k, f, th, env, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#updateAll(net.morilib.lisp.Datum, net.morilib.lisp.Procedure, net.morilib.lisp.Procedure)
	 */
	public Datum updateAll(Datum k, Procedure f, Procedure th,
			Environment env, LispMessage mesg) {
		boolean chg = false;

		for(Datum p = this; p instanceof Cons;) {
			Cons q = (Cons)p;

			if(q.getCar() instanceof Cons) {
				Cons c = (Cons)q.getCar();

				if(equivalenceKey(c.car, k)) {
					c.cdr = Scheme.callva(f, env, mesg, c.cdr);
					chg = true;
				}
			} else if(equivalenceKey(q, k)) {
				q.car = new Cons(k, Scheme.callva(f, env, mesg, q));
				chg = true;
			}
			p = q.cdr;
		}

		if(chg) {
			return this;
		} else if(th != null) {
			return new Cons(new Cons(k, Scheme.callva(th, env, mesg)),
					this);
		} else {
			return new Cons(new Cons(k, LispBoolean.FALSE), this);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#copyDeleteAllDictionary(net.morilib.lisp.Datum)
	 */
	public Datum copyDeleteAllKey(Datum k) {
		return copy().deleteAllKey(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#deleteAllDictionary(net.morilib.lisp.Datum)
	 */
	public Datum deleteAllKey(Datum k) {
		Datum x = this;
		Cons  r = null, s = null;

		while(x instanceof Cons) {
			Cons c = (Cons)x;

			if(c.car instanceof Cons &&
					LispUtils.equals(k, ((Cons)c.car).car)) {
				if(s != null) {
					r.cdr = c.cdr;
				}
			} else if(LispUtils.equals(k, c.car)) {
				if(s != null) {
					r.cdr = c.cdr;
				}
			} else {
				if(s == null) {
					s = c;
				}
				r = c;
			}
			x = c.cdr;
		}
		return (s == null) ? Nil.NIL : s;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#copyDeleteFromDictionary(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteFromKey(LispBag d) {
		return copy().deleteFromKey(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#deleteFromDictionary(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteFromKey(LispBag d) {
		SExpressionDatum r = this;

		for(Datum x : d) {
			r = (SExpressionDatum)r.deleteKey(x);
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#copyDeleteAllFromDictionary(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteAllFromKey(LispBag d) {
		final LispBag e = d;

		return copy(new DatumPredicate() {

			public boolean test(Datum x) {
				if(x instanceof Cons) {
					Cons c = (Cons)x;

					return !e.contains(c.car);
				} else {
					return !e.contains(x);
				}
			}

		});
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#deleteAllFromDictionary(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteAllFromKey(LispBag d) {
		final LispBag e = d;

		return delete(new DatumPredicate() {

			public boolean test(Datum x) {
				if(x instanceof Cons) {
					Cons c = (Cons)x;

					return !e.contains(c.car);
				} else {
					return !e.contains(x);
				}
			}

		});
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#copyAddFromDictionary(net.morilib.lisp.collection.LispDictionary)
	 */
	public Datum copyAddFrom(LispDictionary d) {
		return LispUtils.nconc(
				copy(), LispUtils.toAlist(d.entryIterator()));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#addFromDictionary(net.morilib.lisp.collection.LispDictionary)
	 */
	public Datum addFrom(LispDictionary d) {
		if(d instanceof SExpressionDatum) {
			return LispUtils.nconc(this, (Datum)d);
		} else {
			return LispUtils.nconc(
					this, LispUtils.toAlist(d.entryIterator()));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#copyReplaceAll(net.morilib.lisp.Datum, net.morilib.lisp.Datum[])
	 */
	public Datum copyReplaceAll(Datum k, Datum[] array) {
		return copy().replaceAll(k, array);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#replaceAll(net.morilib.lisp.Datum, net.morilib.lisp.Datum[])
	 */
	public Datum replaceAll(Datum k, Datum[] array) {
		SExpressionDatum d = (SExpressionDatum)deleteAllKey(k);

		for(Datum x : array) {
			Cons l = new Cons(new Cons(k, x), Nil.NIL);

			if(d.isNil()) {
				d = l;
			} else if(d instanceof Cons) {
				d = LispUtils.nconc((Cons)d, l);
			} else {
				throw new RuntimeException();
			}
		}
		return d;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#toList()
	 */
	public Datum toList() {
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#countValue(net.morilib.lisp.Datum)
	 */
	public int countValue(Datum d) {
		Iterator<Map.Entry<Datum, Datum>> itr = entryIterator();
		int r = 0;

		while(itr.hasNext()) {
			if(equivalence(itr.next().getValue(), d)) {
				r++;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#countKey(net.morilib.lisp.Datum)
	 */
	public int countKey(Datum d) {
		Iterator<Map.Entry<Datum, Datum>> itr = entryIterator();
		int r = 0;

		while(itr.hasNext()) {
			if(equivalence(itr.next().getKey(), d)) {
				r++;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#equalToMap(net.morilib.lisp.collection.LispMap, net.morilib.lisp.Procedure, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public boolean equalToMap(LispMap e, Procedure p, Environment env,
			LispMessage mesg) {
		return (LispCollections.contains(this, e, p, env, mesg) &&
				LispCollections.contains(e, this, p, env, mesg));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispDictionary#equalToDictionary(net.morilib.lisp.collection.LispMap, net.morilib.lisp.Procedure, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public boolean equalToDictionary(LispDictionary e, Procedure p,
			Environment env, LispMessage mesg) {
		return (LispCollections.contains(this, e, p, env, mesg) &&
				LispCollections.contains(e, this, p, env, mesg));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispAddable#add(net.morilib.lisp.math.algebra.ILispAddable)
	 */
	public SExpression add(SExpression y) {
		ConsListBuilder b = new ConsListBuilder();
		ConsIterator itr = new ConsIterator(this);

		while(itr.hasNext()) {
			b.append(itr.next());
		}
		return (Cons)b.get((Datum)y);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispMultipliable#mul(net.morilib.lisp.math.algebra.ILispMultipliable)
	 */
	public SExpression mul(SExpression y) {
		ConsListBuilder b = new ConsListBuilder();
		ConsIterator itr = new ConsIterator(this);
		ConsIterator ity;
		Datum x;

		if(((Datum)y).isNil()) {
			return Nil.NIL;
		} else {
			while(itr.hasNext()) {
				x   = itr.next();
				ity = new ConsIterator((Datum)y);
				while(ity.hasNext()) {
					b.append(new Cons(x, ity.next()));
				}
			}
			return b.get();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#isSorted(java.util.Comparator)
	 */
	public boolean isSorted(Comparator<Datum> cmp) {
		return Iterators.isSorted(new ConsIterator(this), cmp);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#merge(net.morilib.lisp.sort.SRFI95Sequence, java.util.Comparator)
	 */
	public ConsOrNil merge(ConsOrNil m, Comparator<Datum> cmp) {
		return ((ConsOrNil)duplicate()).mergeS(m, cmp);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#mergeS(net.morilib.lisp.sort.SRFI95Sequence, java.util.Comparator)
	 */
	public ConsOrNil mergeS(ConsOrNil m, Comparator<Datum> cmp) {
		Cons c1 = this;
		ConsOrNil c2 = m;
		Datum d;
		int c;

		while(!c2.isNil()) {
			if((c = cmp.compare(c1.getCar(), c2.getCar())) > 0) {
				d = c1.getCar();
				c1.setCar(c2.getCar());
				c1.setCdr(new Cons(d, c1.getCdr()));
				c1 = (Cons)c1.getCdr();
				c2 = (ConsOrNil)c2.getCdr();
			} else if(!(c1.getCdr() instanceof Cons)) {
				c1.setCdr(new Cons(c2.getCar(), c1.getCdr()));
				c1 = (Cons)c1.getCdr();
				c2 = (ConsOrNil)c2.getCdr();
			} else if(c < 0) {
				c1 = (Cons)c1.getCdr();
			} else {
				c1 = (Cons)c1.getCdr();
				c2 = (ConsOrNil)c2.getCdr();
			}
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#sort(java.util.Comparator)
	 */
	public ConsOrNil sort(Comparator<Datum> cmp) {
		List<Datum> d = LispUtils.consToListIgnoreDot(this);

		Collections.sort(d, cmp);
		return (ConsOrNil)LispUtils.listToCons(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#sortS(java.util.Comparator)
	 */
	public void sortS(Comparator<Datum> cmp) {
		List<Datum> d = LispUtils.consToListIgnoreDot(this);
		Cons c = this;

		Collections.sort(d, cmp);
		for(int i = 0; i < d.size() - 1; i++) {
			c.setCar(d.get(i));
			if(c.getCdr() instanceof Cons) {
				c = (Cons)c.getCdr();
			} else {
				c = new Cons(Undef.UNDEF, c.getCdr());
			}
		}
		c.setCar(d.get(d.size() - 1));
		c.setCdr(Nil.NIL);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#isTerminated()
	 */
	public boolean isTerminated() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#next()
	 */
	public ILispIterator next() {
		if(cdr instanceof ConsOrNil) {
			return (ILispIterator)cdr;
		} else {
			return Nil.NIL;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterator#getCurrentDatum()
	 */
	public Datum getCurrentDatum() {
		return car;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterable#lispIterator()
	 */
	public ILispIterator lispIterator() {
		return this;
	}

	//
	private Datum itrnext(ConsIterator itr, Datum arg,
			LispMessage mesg) {
		if(!itr.hasNext()) {
			throw mesg.getError("err.list.outofrange", arg);
		} else {
			return itr.next();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.accessor.ILispRef#ref(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum ref(Datum arg, LispMessage mesg) {
		int k = SubrUtils.getNonnegativeSmallInt(arg, mesg);
		ConsIterator itr = new ConsIterator(this);

		for(int i = 0; i < k; i++)  itrnext(itr, arg, mesg);
		return itrnext(itr, arg, mesg);
	}

}
