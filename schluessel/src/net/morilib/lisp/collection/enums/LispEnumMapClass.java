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

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.ISubr;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.SExpressionDatum;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;
import net.morilib.lisp.collection.LispBag;
import net.morilib.lisp.collection.LispCollection;
import net.morilib.lisp.collection.LispCollections;
import net.morilib.lisp.collection.LispMap;
import net.morilib.lisp.collection.hash.LispHash;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.mapset.HashOneToOneSet;
import net.morilib.util.mapset.OneToOneSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/15
 */
public class LispEnumMapClass extends Datum2 implements ISubr {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/10/15
	 */
	public static class LispEnumMap extends Datum2
	implements LispMap {

		//
		private Datum[] data;
		private LispEnumMapClass cls;

		//
		private LispEnumMap(LispEnumMapClass cls) {
			this.cls = cls;
		}

		//
		private LispEnumMap(LispEnumMapClass cls, Datum... data) {
			this.cls  = cls;
			this.data = new Datum[data.length];
			System.arraycopy(data, 0, this.data, 0, data.length);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#getCollectionName()
		 */
		public Symbol getCollectionName() {
			return Symbol.getSymbol("enum-map");
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#toList()
		 */
		public Datum toList() {
			ConsListBuilder b = new ConsListBuilder();

			for(int i = 0; i < data.length; i++) {
				if(data[i] != null) {
					b.append(new Cons(cls.map.getValue(i), data[i]));
				}
			}
			return b.get();
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#count(net.morilib.lisp.Datum)
		 */
		public int count(Datum c2a) {
			int c = 0;

			for(Datum d : data) {
				if(d != null && c2a.equals(d)) {
					c++;
				}
			}
			return c;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#size()
		 */
		public int size() {
			int c = 0;

			for(Datum d : data) {
				if(d != null) {
					c++;
				}
			}
			return c;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#prototype()
		 */
		public Datum prototype() {
			return new LispEnumMap(cls);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#clear()
		 */
		public Datum clear() {
			for(int i = 0; i < data.length; i++) {
				data[i] = null;
			}
			return this;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection)
		 */
		public boolean equalTo(LispCollection col) {
			return LispCollections.equalsMapEntry(this, col);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection, net.morilib.lisp.Procedure, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		public boolean equalTo(LispCollection col, Procedure p,
				Environment env, LispMessage mesg) {
			return LispCollections.equalsMapEntry(this, col);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#duplicate()
		 */
		public LispEnumMap duplicate() {
			return new LispEnumMap(cls, data);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispCollection#contains(net.morilib.lisp.Datum)
		 */
		public boolean contains(Datum d) {
			if(d instanceof Cons) {
				return LispCollections.equivalence(this,
						((Cons)d).getCar(),
						((Cons)d).getCdr());
			} else {
				return false;
			}
		}

		/* (non-Javadoc)
		 * @see java.lang.Iterable#iterator()
		 */
		public Iterator<Datum> iterator() {
			// p[0]: pointer, p[1]: removed if the value is -1
			final int[] p = new int[2];

			p[0] = p[1] = 0;
			while(p[0] < data.length && data[p[0]] == null) {
				p[0]++;
			}

			return new Iterator<Datum>() {

				private void seeknext() {
					p[0]++;
					while(p[0] < data.length && data[p[0]] == null) {
						p[0]++;
					}
					p[1] = 0;
				}

				public boolean hasNext() {
					return p[0] < data.length;
				}

				public Datum next() {
					int i = p[0];

					seeknext();
					return new Cons(cls.map.getValue(i), data[i]);
				}

				public void remove() {
					throw new UnsupportedOperationException();
				}

			};
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispEntryEnumeration#entryIterator()
		 */
		public Iterator<Map.Entry<Datum, Datum>> entryIterator() {
			// p[0]: pointer, p[1]: removed if the value is -1
			final int[] p = new int[2];

			p[0] = p[1] = 0;
			while(p[0] < data.length && data[p[0]] == null) {
				p[0]++;
			}

			return new Iterator<Map.Entry<Datum, Datum>>() {

				private void seeknext() {
					p[0]++;
					while(p[0] < data.length && data[p[0]] == null) {
						p[0]++;
					}
					p[1] = 0;
				}

				public boolean hasNext() {
					return p[0] < data.length;
				}

				public Map.Entry<Datum, Datum> next() {
					final int i = p[0];

					seeknext();
					return new Map.Entry<Datum, Datum>() {

						public Datum getKey() {
							return cls.map.getValue(i);
						}

						public Datum getValue() {
							return data[i];
						}

						public Datum setValue(Datum value) {
							Datum r = data[i];

							data[i] = value;
							return r;
						}
					};
				}

				public void remove() {
					throw new UnsupportedOperationException();
				}

			};
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispMap#keyEquivalence()
		 */
		public Procedure keyEquivalence() {
			return LispHash.EQV_HASH;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispMap#equivalenceKey(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
		 */
		public boolean equivalenceKey(Datum a, Datum b) {
			return a.equals(b);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispMap#valueEquivalence()
		 */
		public Procedure valueEquivalence() {
			return LispHash.EQV_HASH;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispMap#equivalenceValue(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
		 */
		public boolean equivalenceValue(Datum a, Datum b) {
			return a.equals(b);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispMap#containsKey(net.morilib.lisp.Datum)
		 */
		public boolean containsKey(Datum k) {
			Integer p = cls.map.getKey(k);

			return (p == null) ? false : data[p] != null;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispMap#keysToList()
		 */
		public SExpressionDatum keysToList() {
			ConsListBuilder b = new ConsListBuilder();

			for(int i = 0; i < data.length; i++) {
				if(data[i] != null) {
					b.append(cls.map.getValue(i));
				}
			}
			return b.get();
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispMap#get(net.morilib.lisp.Datum)
		 */
		public Datum get(Datum k) {
			Integer p = cls.map.getKey(k);

			return (p == null) ? null : data[p];
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispMap#copyPut(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
		 */
		public Datum[] copyPut(Datum k, Datum v) {
			return duplicate().put(k, v);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispMap#put(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
		 */
		public Datum[] put(Datum k, Datum v) {
			Integer p = cls.map.getKey(k);
			Datum r;

			if(p != null) {
				r = data[p];
				data[p] = v;
				return new Datum[] { this, r };
			} else {
				return new Datum[] { this, null };
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispMap#copyDeleteKey(net.morilib.lisp.Datum)
		 */
		public Datum copyDeleteKey(Datum k) {
			return duplicate().deleteKey(k);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispMap#deleteKey(net.morilib.lisp.Datum)
		 */
		public Datum deleteKey(Datum k) {
			Integer p = cls.map.getKey(k);

			if(p != null) {
				data[p] = null;
			}
			return this;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispMap#copyDeleteFromKey(net.morilib.lisp.collection.LispBag)
		 */
		public Datum copyDeleteFromKey(LispBag k) {
			return duplicate().deleteFromKey(k);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispMap#deleteFromKey(net.morilib.lisp.collection.LispBag)
		 */
		public Datum deleteFromKey(LispBag k) {
			Iterator<Datum> i = k.iterator();

			while(i.hasNext()) {
				deleteKey(i.next());
			}
			return this;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispMap#copyAddFrom(net.morilib.lisp.collection.LispMap)
		 */
		public Datum copyAddFrom(LispMap m) {
			return duplicate().addFrom(m);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispMap#addFrom(net.morilib.lisp.collection.LispMap)
		 */
		public Datum addFrom(LispMap m) {
			Iterator<Map.Entry<Datum, Datum>> i;
			Map.Entry<Datum, Datum> e;
			Integer p;

			i = m.entryIterator();
			while(i.hasNext()) {
				e = i.next();
				p = cls.map.getKey(e.getKey());
				if(p != null) {
					data[p] = e.getValue();
				} else {
					// do nothing
				}
			}
			return this;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.collection.LispMap#countValue(net.morilib.lisp.Datum)
		 */
		public int countValue(Datum d) {
			int c = 0;

			for(Datum x : data) {
				if(x != null && d.equals(x)) {
					c++;
				}
			}
			return c;
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
		 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			Iterator<Map.Entry<Datum, Datum>> i;
			Map.Entry<Datum, Datum> e;
			String dlm = "";

			buf.append("#<enum-map (");
			i = entryIterator();
			while(i.hasNext()) {
				e = i.next();
				buf.append(dlm);
				buf.append("(").append(LispUtils.print(e.getKey()));
				buf.append(" . ");
				buf.append(LispUtils.print(e.getValue())).append(")");
				dlm = " ";
			}
			buf.append(")>");
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.accessor.ILispRef#ref(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum ref(Datum arg, LispMessage mesg) {
			Datum r = get(arg);

			return (r != null) ? r : Undef.UNDEF;
		}

	}

	//
	private OneToOneSet<Integer, Datum> map;

	/**
	 * 
	 * @param data
	 */
	public LispEnumMapClass(Collection<Datum> data) {
		int i = 0;

		map = new HashOneToOneSet<Integer, Datum>();
		for(Datum d : data) {
			map.put(i++, d);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.ISubr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body), jtr;
		Datum[] a = new Datum[map.size()];
		Integer i;
		Datum d, x, y;
		Datum c1a = SubrUtils.nextIf(itr, mesg, body);

		SubrUtils.checkTerminated(itr, body, mesg);
		jtr = new ConsIterator(c1a);
		while(jtr.hasNext()) {
			d = jtr.next();
			if(d instanceof Cons) {
				x = ((Cons)d).getCar();
				y = ((Cons)d).getCdr();
			} else {
				x = d;
				y = Undef.UNDEF;
			}

			if((i = map.getKey(x)) != null) {
				a[i] = y;
			}
		}
		return new LispEnumMap(this, a);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<enum-map-class>");
	}

}
