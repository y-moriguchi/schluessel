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
package net.morilib.lisp.collection.enums;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import net.morilib.lang.Hashes;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.ISubr;
import net.morilib.lisp.Keyword;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.collection.LispBag;
import net.morilib.lisp.collection.LispCollection;
import net.morilib.lisp.collection.LispCollections;
import net.morilib.lisp.collection.LispSet;
import net.morilib.lisp.collection.hash.LispHash;
import net.morilib.lisp.r6rs.enums.ILispR6RSEnum;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.lisp.topology.AbstractLispFiniteTopology;
import net.morilib.lisp.topology.ILispTopology;
import net.morilib.lisp.topology.LispCardinality;
import net.morilib.util.bit.BitSet2;
import net.morilib.util.bit.BitSet2Class;
import net.morilib.util.mapset.HashOneToOneSet;
import net.morilib.util.mapset.OneToOneSet;
import net.morilib.util.primitive.iterator.IntegerIterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/13
 */
public class LispEnumSetClass extends Datum2
implements ISubr, ILispR6RSEnum {

	//
	private static class Tplg {

		//
		private BitSet2 tplg, tplc, mask;

		//
		private int containso(BitSet2 b) {
			if(b.containsAllAnd(tplg, mask)) {
				return tplg.join(mask.complement()).meet(b).size();
			} else {
				return -1;
			}
		}

		//
		private boolean eqvo(BitSet2 b) {
			return tplg.equalsAnd(b, mask);
		}

		//
		private int containsc(BitSet2 b) {
			if(tplc.containsAllAnd(b, mask)) {
				return tplc.join(b).size();
			} else {
				return Integer.MAX_VALUE;
			}
		}

		//
		private boolean eqvc(BitSet2 b) {
			return tplc.equalsAnd(b, mask);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class IsEnumSet extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(c1a instanceof LispEnumSet);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/10/15
	 */
	public static class LispEnumSet
	extends AbstractLispFiniteTopology
	implements LispSet, ILispR6RSEnum {

		//
		private BitSet2 bit;
		private LispEnumSetClass kls;

		//
		private LispEnumSet(LispEnumSetClass kls, BitSet2 bit) {
			this.kls = kls;
			this.bit = bit;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.topology.ILispTopology#isNeighborOf(net.morilib.lisp.Datum)
		 */
		public boolean isNeighborhoodOf(Datum d) {
			if(kls.map.containsValue(d)) {
				return bit.contains(kls.map.getKey(d));
			} else {
				return false;
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.topology.ILispTopology#isContained(net.morilib.lisp.topology.ILispTopology)
		 */
		public boolean isContained(ILispTopology t) {
			IntegerIterator itr = bit.iterator();

			while(itr.hasNext()) {
				if(!t.isNeighborhoodOf(kls.map.getValue(itr.next()))) {
					return false;
				}
			}
			return true;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.topology.ILispTopology#isIndependent(net.morilib.lisp.topology.ILispTopology)
		 */
		public boolean isIndependent(ILispTopology t) {
			IntegerIterator itr = bit.iterator();

			while(itr.hasNext()) {
				if(t.isNeighborhoodOf(kls.map.getValue(itr.next()))) {
					return false;
				}
			}
			return true;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.topology.ILispTopology#unionTopology(net.morilib.lisp.topology.ILispTopology)
		 */
		public ILispTopology unionTopology(ILispTopology t) {
			if(t instanceof LispEnumSet &&
					kls.equals(((LispEnumSet)t).kls)) {
				return new LispEnumSet(kls,
						bit.join(((LispEnumSet)t).bit));
			} else {
				return super.unionTopology(t);
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.topology.ILispTopology#intersectTopology(net.morilib.lisp.topology.ILispTopology)
		 */
		public ILispTopology intersectionTopology(ILispTopology t) {
			if(t instanceof LispEnumSet &&
					kls.equals(((LispEnumSet)t).kls)) {
				return new LispEnumSet(kls,
						bit.meet(((LispEnumSet)t).bit));
			} else {
				return super.intersectionTopology(t);
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.topology.ILispTopology#interior()
		 */
		public ILispTopology interior() {
			int  m = -1, p;
			Tplg r = null;

			if(kls.topology == null) {
				return this;
			} else if(bit.size() == 0 ||
					bit.size() == kls.map.size()) {
				return this;
			} else {
				for(Tplg t : kls.topology) {
					p = t.containso(bit);
					if(p > m) {
						m = p;
						r = t;
					}
				}
	
				if(r == null) {
					return new LispEnumSet(kls,
							kls.bscls.newInstance());
				} else {
					return new LispEnumSet(kls, bit.meet(
							r.tplg.join(r.mask.complement())));
				}
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.topology.ILispTopology#closure()
		 */
		public ILispTopology closure() {
			int  m = Integer.MAX_VALUE, p;
			Tplg r = null;

			if(kls.topology == null) {
				return this;
			} else if(bit.size() == 0 ||
					bit.size() == kls.map.size()) {
				return this;
			} else {
				for(Tplg t : kls.topology) {
					p = t.containsc(bit);
					if(p < m) {
						m = p;
						r = t;
					}
				}
	
				if(r == null) {
					return new LispEnumSet(kls,
							kls.bscls.newInstance().complement());
				} else {
					return new LispEnumSet(kls, bit.join(r.tplc));
				}
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.topology.ILispTopology#isOpen()
		 */
		public boolean isOpen() {
			if(kls.topology == null) {
				return true;
			} else if(bit.size() == 0 ||
					bit.size() == kls.map.size()) {
				return true;
			} else {
				for(Tplg t : kls.topology) {
					if(t.eqvo(bit)) {
						return true;
					}
				}
				return false;
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.topology.ILispTopology#isClosed()
		 */
		public boolean isClosed() {
			if(kls.topology == null) {
				return true;
			} else if(bit.size() == 0 ||
					bit.size() == kls.map.size()) {
				return true;
			} else {
				for(Tplg t : kls.topology) {
					if(t.eqvc(bit)) {
						return true;
					}
				}
				return false;
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.topology.ILispTopology#cardinality()
		 */
		public LispCardinality cardinality() {
			return LispCardinality.finiteValueOf(bit.size());
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.topology.ILispTopology#isEmpty()
		 */
		public boolean isEmpty() {
			return bit.isEmpty();
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.topology.ILispTopology#isUniverse()
		 */
		public boolean isUniverse() {
			return false;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.topology.ILispEnumerableTopology#getJavaSet()
		 */
		public Set<Datum> getJavaSet() {
			IntegerIterator itr = bit.iterator();
			Set<Datum> r = new HashSet<Datum>();

			while(itr.hasNext()) {
				r.add(kls.map.getValue(itr.next()));
			}
			return r;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#getCollectionName()
		 */
		public Symbol getCollectionName() {
			return Symbol.getSymbol("enum-set2");
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#toList()
		 */
		public Datum toList() {
			ConsListBuilder b = new ConsListBuilder();
			IntegerIterator i = bit.iterator();

			while(i.hasNext()) {
				b.append(kls.map.getValue(i.next()));
			}
			return b.get();
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#count(net.morilib.lisp.Datum)
		 */
		public int count(Datum c2a) {
			IntegerIterator i = bit.iterator();
			int c = 0;

			while(i.hasNext()) {
				if(kls.map.getValue(i.next()).equals(c2a)) {
					c++;
				}
			}
			return c;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#size()
		 */
		public int size() {
			return bit.size();
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#prototype()
		 */
		public LispEnumSet prototype() {
			return new LispEnumSet(kls, kls.bscls.newInstance());
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#clear()
		 */
		public Datum clear() {
			bit.clear();
			return this;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection)
		 */
		public boolean equalTo(LispCollection col) {
			return LispCollections.equals(this, col);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection, net.morilib.lisp.Procedure, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		public boolean equalTo(LispCollection col, Procedure p,
				Environment env, LispMessage mesg) {
			return LispCollections.equalTo(this, col, p, env, mesg);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#duplicate()
		 */
		public LispEnumSet duplicate() {
			return new LispEnumSet(kls, new BitSet2(bit));
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#contains(net.morilib.lisp.Datum)
		 */
		public boolean contains(Datum d) {
			return isNeighborhoodOf(d);
		}

		/* (non-Javadoc)
		 * @see java.lang.Iterable#iterator()
		 */
		public Iterator<Datum> iterator() {
			final IntegerIterator i = bit.iterator();

			return new Iterator<Datum>() {

				public boolean hasNext() {
					return i.hasNext();
				}

				public Datum next() {
					return kls.map.getValue(i.next());
				}

				public void remove() {
					throw new UnsupportedOperationException();
				}

			};
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.math.algebra.ILispAddable#add(net.morilib.lisp.math.algebra.ILispAddable)
		 */
		public LispSet add(LispSet y) {
			return union(y);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.math.algebra.ILispSubtractable#sub(net.morilib.lisp.math.algebra.ILispSubtractable)
		 */
		public LispSet sub(LispSet y) {
			return difference(y);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.math.algebra.ILispNegatable#uminus()
		 */
		public LispSet uminus() {
			return prototype();
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.math.algebra.ILispMultipliable#mul(net.morilib.lisp.math.algebra.ILispMultipliable)
		 */
		public LispSet mul(LispSet y) {
			return intersection(y);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#equivalence()
		 */
		public Procedure equivalence() {
			return LispHash.EQV;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#equivalence(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
		 */
		public boolean equivalence(Datum a, Datum b) {
			return a.equals(b);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#subset(net.morilib.lisp.collection.LispSet)
		 */
		public boolean subset(LispSet set) {
			for(Datum d : this) {
				if(!set.contains(d)) {
					return false;
				}
			}
			return true;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#copyAdd(net.morilib.lisp.Datum)
		 */
		public Datum copyAdd(Datum d) {
			return duplicate().add(d);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#add(net.morilib.lisp.Datum)
		 */
		public Datum add(Datum d) {
			Integer p = kls.map.getKey(d);

			if(p != null) {
				bit.add(p);
			}
			return this;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#copyDelete(net.morilib.lisp.Datum)
		 */
		public Datum copyDelete(Datum d) {
			return duplicate().delete(d);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#delete(net.morilib.lisp.Datum)
		 */
		public Datum delete(Datum d) {
			Integer p = kls.map.getKey(d);

			if(p != null) {
				bit.remove(p);
			}
			return this;
		}

		/* (non-Javadoc)
		 */
		public LispEnumSet union(LispSet s) {
			return duplicate().addAll(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#addAll(net.morilib.lisp.collection.LispSet)
		 */
		public LispEnumSet addAll(LispSet s) {
			for(Datum d : s) {
				add(d);
			}
			return this;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#intersection(net.morilib.lisp.collection.LispSet)
		 */
		public LispEnumSet intersection(LispSet s) {
			return duplicate().retainAll(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#retainAll(net.morilib.lisp.collection.LispSet)
		 */
		public LispEnumSet retainAll(LispSet s) {
			for(Datum d : duplicate()) {
				if(!s.contains(d)) {
					delete(d);
				}
			}
			return this;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#difference(net.morilib.lisp.collection.LispSet)
		 */
		public LispEnumSet difference(LispSet s) {
			return duplicate().removeAll(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#removeAll(net.morilib.lisp.collection.LispSet)
		 */
		public LispEnumSet removeAll(LispSet s) {
			for(Datum d : s) {
				if(contains(d)) {
					delete(d);
				}
			}
			return this;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#symmetricDifference(net.morilib.lisp.collection.LispSet)
		 */
		public LispEnumSet symmetricDifference(LispSet s) {
			return duplicate().symmetricDifferenceModify(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#symmetricDifferenceModify(net.morilib.lisp.collection.LispSet)
		 */
		public LispEnumSet symmetricDifferenceModify(LispSet s) {
			for(Datum d : s) {
				if(contains(d)) {
					delete(d);
				} else {
					add(d);
				}
			}
			return this;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#copyAddFrom(net.morilib.lisp.collection.LispBag)
		 */
		public Datum copyAddFrom(LispBag b) {
			return duplicate().addFrom(b);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#addFrom(net.morilib.lisp.collection.LispBag)
		 */
		public Datum addFrom(LispBag b) {
			for(Datum d : b) {
				add(d);
			}
			return this;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#copyDeleteFrom(net.morilib.lisp.collection.LispBag)
		 */
		public Datum copyDeleteFrom(LispBag b) {
			return duplicate().deleteFrom(b);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispSet#deleteFrom(net.morilib.lisp.collection.LispBag)
		 */
		public Datum deleteFrom(LispBag b) {
			for(Datum d : b) {
				if(contains(d)) {
					delete(d);
				}
			}
			return this;
		}

		//
		LispEnumSetClass getNewClass() {
			return new LispEnumSetClass(
					LispUtils.consToListIgnoreDot(toList()));
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet#getUniverse()
		 */
		@Override
		public Datum getUniverse() {
			return kls.getUniverse();
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet#getIndexer()
		 */
		@Override
		public Procedure getIndexer() {
			return kls.getIndexer();
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet#getConstructor()
		 */
		@Override
		public Procedure getConstructor() {
			return getNewClass().getConstructor();
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet#getUniverseEnumSet()
		 */
		@Override
		public ILispR6RSEnum getUniverseEnumSet() {
			return kls.getUniverseEnumSet();
		}

		/**
		 * 
		 * @return
		 */
		public LispEnumSetClass getEnumSetClass() {
			return kls;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet#isIn(net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet)
		 */
		@Override
		public boolean isIn(ILispR6RSEnum s) {
			ConsIterator itr = new ConsIterator(toList());

			if(kls.isIn(s.getUniverseEnumSet())) {
				while(itr.hasNext()) {
					if(!s.contains(itr.next())) {
						return false;
					}
				}
				return true;
			} else {
				return false;
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet#isEqualTo(net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet)
		 */
		@Override
		public boolean isEqualTo(ILispR6RSEnum s) {
			return isIn(s) && s.isIn(this);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.r6rs.enums.ILispR6RSEnum#complement()
		 */
		public LispEnumSet complement() {
			return new LispEnumSet(kls, bit.complement());
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			IntegerIterator itr = bit.iterator();
			String dlm = "";
			int i;

			buf.append("#<enum-set (");
			while(itr.hasNext()) {
				i = itr.next();
				buf.append(dlm);
				buf.append(LispUtils.print(kls.map.getValue(i)));
				dlm = " ";
			}
			buf.append(")>");
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#hashCode()
		 */
		public int hashCode() {
			int r = Hashes.INIT;

			r = Hashes.A * (r + kls.hashCode());
			r = Hashes.A * (r + bit.hashCode());
			return r;
		}

		/* (non-Javadoc)
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		public boolean equals(Object o) {
			if(o instanceof LispEnumSet) {
				LispEnumSet s = (LispEnumSet)o;

				return kls.equals(s.kls) && bit.equals(s.bit);
			}
			return false;
		}

	}

	//
	private OneToOneSet<Integer, Datum> map;
	private Collection<Tplg> topology;
	private BitSet2Class bscls;

	//
	/*package*/ LispEnumSetClass(Collection<Datum> topology,
			Collection<Datum> data,
			LispMessage mesg) throws EnumTopologyException {
		setelems(data);
		bscls = new BitSet2Class(0, data.size());
		if(topology == null) {
			topology = null;
		} else {
			settops(topology, mesg);
		}
	}

	/**
	 * 
	 * @param data
	 */
	public LispEnumSetClass(Collection<Datum> data) {
		setelems(data);
		bscls = new BitSet2Class(0, data.size());
		topology = null;
	}

	//
	private BitSet2 tobit(ConsIterator t, Datum b, LispMessage mesg) {
		BitSet2 bs = bscls.newInstance();
		Datum d;

		while(t.hasNext()) {
			d = t.next();
			if(map.containsValue(d)) {
				bs.add(map.getKey(d));
			}
		}
		SubrUtils.checkProper(t, b, mesg);
		return bs;
	}

	//
	private void setelems(Collection<Datum> t) {
		int i = 0;

		map = new HashOneToOneSet<Integer, Datum>();
		for(Datum d : t) {
			map.put(i++, d);
		}
	}

	//
	//private static final Keyword UNION = Keyword.getKeyword("union");
	private static final Keyword POWER = Keyword.getKeyword("power");

	//
	private Tplg settops1(Datum d,
			LispMessage mesg) throws EnumTopologyException {
		ConsIterator itr;
		Tplg tp = new Tplg(), sp;
		Datum e, f;

		itr = new ConsIterator(d);
		if(!itr.hasNext()) {
			f = itr.getTerminal();
			tp.tplg = bscls.newInstance();
			tp.mask = bscls.newInstance();
			if(map.containsValue(f)) {
				tp.tplg.add(map.getKey(f));
			} else {
				throw new EnumTopologyException(
						"err.collection.enumset.notelement", f);
			}
		} else {
			e = itr.next();
			if(e.equals(POWER)) {
				tp.tplg = bscls.newInstance();
				tp.mask = tobit(itr, d, mesg);
			} else {
				tp = settops1(e, mesg);
				while(itr.hasNext()) {
					sp = settops1(itr.next(), mesg);
					tp.tplg = tp.tplg.join(sp.tplg);
					tp.mask = tp.mask.join(sp.mask);
				}
				SubrUtils.checkProper(itr, d, mesg);
			}
		}
		return tp;
	}

	//
	private void settops(Collection<Datum> t,
			LispMessage mesg) throws EnumTopologyException {
		topology = new ArrayList<Tplg>();
		for(Datum d : t) {
			Tplg s = settops1(d, mesg);

			s.mask = s.mask.complement();
			s.tplc = s.tplg.complement().meet(s.mask);
			topology.add(s);
		}

		outer: for(Tplg x : topology) {
			for(Tplg y : topology) {
				BitSet2 x2 = x.tplg.join(x.mask.complement());
				BitSet2 y2 = y.tplg.join(y.mask.complement());

				if(x2.containsAll(y.tplg) || y2.containsAll(x.tplg)) {
					continue outer;
				}
				throw new EnumTopologyException(
						"err.collection.enumset.notelement", null);
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.ISubr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		BitSet2 bit = bscls.newInstance();
		Datum d;

		while(itr.hasNext()) {
			d = itr.next();
			if(map.containsValue(d)) {
				bit.add(map.getKey(d));
			}
		}
		SubrUtils.checkTerminated(itr, body, mesg);
		return new LispEnumSet(this, bit);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet#getUniverse()
	 */
	@Override
	public Datum getUniverse() {
		ConsListBuilder b = new ConsListBuilder();

		for(int i = 0; i < map.size(); i++) {
			b.append(map.getValue(i));
		}
		return b.get();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet#getIndexer()
	 */
	@Override
	public Procedure getIndexer() {
		return new UnaryArgs("#<indexer-of-enum-set>") {

			@Override
			protected Datum execute(Datum c1a, Environment env,
					LispMessage mesg) {
				Integer i = map.getKey(c1a);

				return (i != null) ?
						LispInteger.valueOf(i) : LispBoolean.FALSE;
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet#getConstructor()
	 */
	@Override
	public Procedure getConstructor() {
		return new UnaryArgs("#<constructor-of-enum-set>") {

			@Override
			protected Datum execute(Datum c1a, Environment env,
					LispMessage mesg) {
				ConsIterator itr = new ConsIterator(c1a);
				BitSet2 bit = bscls.newInstance();
				Datum d;

				while(itr.hasNext()) {
					d = itr.next();
					if(map.containsValue(d)) {
						bit.add(map.getKey(d));
					} else {
						throw mesg.getError(
								"err.r6rs.require.enum.member", d);
					}
				}
				SubrUtils.checkProper(itr, c1a, mesg);
				return new LispEnumSet(LispEnumSetClass.this,
						bit);
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet#toList()
	 */
	@Override
	public Datum toList() {
		return getUniverse();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet#contains(net.morilib.lisp.Datum)
	 */
	@Override
	public boolean contains(Datum d) {
		return map.containsValue(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet#getUniverseEnumSet()
	 */
	@Override
	public LispEnumSet getUniverseEnumSet() {
		return new LispEnumSet(this, bscls.get1());
	}

	/**
	 * 
	 * @return
	 */
	public LispEnumSet getEmptyEnumSet() {
		return new LispEnumSet(this, bscls.get0());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet#containsSet(net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet)
	 */
	@Override
	public boolean isIn(ILispR6RSEnum s) {
		for(Datum d : map.valueSet()) {
			if(!s.contains(d)) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet#isEqualTo(net.morilib.lisp.r6rs.enums.ILispR6RSEnumSet)
	 */
	@Override
	public boolean isEqualTo(ILispR6RSEnum s) {
		return isIn(s) && s.isIn(this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.enums.ILispR6RSEnum#complement()
	 */
	@Override
	public LispEnumSet complement() {
		return getEmptyEnumSet();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<enum-set-class>");
	}

}
