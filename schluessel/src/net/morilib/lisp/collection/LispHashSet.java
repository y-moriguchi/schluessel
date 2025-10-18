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

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

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
import net.morilib.lisp.collection.hash.CharCiHash;
import net.morilib.lisp.collection.hash.CharHash;
import net.morilib.lisp.collection.hash.Hash;
import net.morilib.lisp.collection.hash.HashByIdentity;
import net.morilib.lisp.collection.hash.Hasv;
import net.morilib.lisp.collection.hash.StringCiHash;
import net.morilib.lisp.collection.hash.StringHash;
import net.morilib.lisp.subr.CharCIEqual;
import net.morilib.lisp.subr.CharEqual;
import net.morilib.lisp.subr.IsEq;
import net.morilib.lisp.subr.IsEqual;
import net.morilib.lisp.subr.IsEqv;
import net.morilib.lisp.subr.StringCIEqual;
import net.morilib.lisp.subr.StringEqual;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.Iterators;
import net.morilib.util.set.IdentityHashSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/20
 */
public class LispHashSet extends LispMutableSetDatum {

	//
	private class HashD {

		//
		private Datum datum;

		//
		private HashD(Datum k) {
			this.datum = k;
		}

		//
		public boolean equals(Object o) {
			if(o instanceof HashD) {
				if(eqproc.equals(EQ)) {
					return datum == ((HashD)o).datum;
				} else if(eqproc.equals(EQV)) {
					return datum.equals(((HashD)o).datum);
				} else {
					return Scheme.callva(
							eqproc, activeenv, message,
							datum, ((HashD)o).datum).isTrue();
				}
			}
			return false;
		}

		//
		public int hashCode() {
			return Scheme.callva(
					hashproc, activeenv, message, datum).getInt();
		}

		//
		public String toString() {
			return datum.toString();
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/08/20
	 */
	public static class MakeHashSet extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum ep = Iterators.nextIf(itr);
			Datum hp = Iterators.nextIf(itr);

			if(ep == null) {
				return new LispHashSet(new HashSet<Object>(),
						EQV, EQUAL_H, env, mesg);
			} else if(hp == null) {
				if(!(ep instanceof Procedure)) {
					throw mesg.getError("err.require.procedure", ep);
				}
				return new LispHashSet(new HashSet<Object>(),
						(Procedure)ep, EQUAL_H, env, mesg);
			} else if(hp instanceof Procedure) {
				if(hp.equals(EQ)) {
					return new LispHashSet(
							new IdentityHashSet<Object>(),
							(Procedure)ep, (Procedure)hp, env, mesg);
				} else {
					return new LispHashSet(new HashSet<Object>(),
							(Procedure)ep, (Procedure)hp, env, mesg);
				}
			} else {
				throw mesg.getError("err.require.procedure", ep);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/08/20
	 */
	public static class SubrHashSet extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Set<Object> st = new HashSet<Object>();
			Datum ep = Iterators.nextIf(itr);
			Datum hp = Iterators.nextIf(itr);
			Procedure ep2, hp2;
			LispHashSet rs;

			if(ep == null) {
				ep2 = EQV;  hp2 = EQUAL_H;
			} else if(hp == null) {
				if(ep instanceof Procedure) {
					ep2 = (Procedure)ep;
					hp2 = EQUAL_H;
				} else {
					ep2 = EQV;  hp2 = EQUAL_H;
					st.add(ep);
				}
			} else if(hp instanceof Procedure) {
				if(ep instanceof Procedure) {
					ep2 = (Procedure)ep;
					hp2 = (Procedure)hp;
					rs  = new LispHashSet(st, ep2, hp2, env, mesg);
					while(itr.hasNext()) {
						st.add(rs.new HashD(itr.next()));
					}
					return rs;
				} else {
					ep2 = EQV;   hp2 = EQUAL_H;
					st.add(ep);  st.add(hp);
					while(itr.hasNext()) {
						st.add(itr.next());
					}
				}
			} else {
				if(ep instanceof Procedure) {
					ep2 = (Procedure)ep;
					hp2 = EQUAL_H;
					rs  = new LispHashSet(st, ep2, hp2, env, mesg);
					st.add(rs.new HashD(hp));
					while(itr.hasNext()) {
						st.add(rs.new HashD(itr.next()));
					}
					return rs;
				} else {
					ep2 = EQV;   hp2 = EQUAL_H;
					st.add(ep);  st.add(hp);
					while(itr.hasNext()) {
						st.add(itr.next());
					}
				}
			}
			return new LispHashSet(st, ep2, hp2, env, mesg);
		}
	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class IsHashSet extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(c1a instanceof LispHashSet);
		}

	}

	//
	private static final Map<Datum, Datum> DEFHASH;
	private static final Procedure EQ      = new IsEq();
	private static final Procedure EQV     = new IsEqv();
	private static final Procedure EQUAL   = new IsEqual();
	private static final Procedure EQ_H    = new HashByIdentity();
	private static final Procedure EQV_H   = new Hasv();
	private static final Procedure EQUAL_H = new Hash();

	static {
		DEFHASH = new HashMap<Datum, Datum>();
		DEFHASH.put(new StringEqual(),   new StringHash());
		DEFHASH.put(new StringCIEqual(), new StringCiHash());
		DEFHASH.put(new CharEqual(),     new CharHash());
		DEFHASH.put(new CharCIEqual(),   new CharCiHash());
		DEFHASH.put((Datum)EQ,           (Datum)EQ_H);
		DEFHASH.put((Datum)EQV,          (Datum)EQV_H);
		DEFHASH.put((Datum)EQUAL,        (Datum)EQUAL_H);
	}

	//
	private Set<Object> hashSet;
	private Procedure eqproc;
	private Procedure hashproc;
	private Environment activeenv;
	private LispMessage message;

	//
	private LispHashSet(Set<Object> hashSet,
			Procedure eqproc, Procedure hashproc,
			Environment activeenv, LispMessage message) {
		this.hashSet   = hashSet;
		this.eqproc    = eqproc;
		this.hashproc  = hashproc;
		this.activeenv = activeenv;
		this.message   = message;
	}

	/**
	 * 
	 */
	public LispHashSet() {
		this.hashSet   = new HashSet<Object>();
		this.eqproc    = EQV;
		this.hashproc  = EQUAL_H;
		this.activeenv = new Environment();
		this.message   = LispMessage.getInstance();
	}

	/**
	 * 
	 * @param col
	 */
	public LispHashSet(Collection<Datum> col) {
		this.hashSet   = new HashSet<Object>(col);
		this.eqproc    = EQV;
		this.hashproc  = EQUAL_H;
		this.activeenv = new Environment();
		this.message   = LispMessage.getInstance();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#getCollectionName()
	 */
	public Symbol getCollectionName() {
		return Symbol.getSymbol("hash-set");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#size()
	 */
	public int size() {
		return hashSet.size();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#prototype()
	 */
	public LispHashSet prototype() {
		return new LispHashSet(new HashSet<Object>(), eqproc,
				hashproc, activeenv, message);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#clear()
	 */
	public Datum clear() {
		hashSet.clear();
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection)
	 */
	public boolean equalTo(LispCollection col) {
		return equalTo(col, eqproc, activeenv, message);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#duplicate()
	 */
	public LispHashSet duplicate() {
		return new LispHashSet(new HashSet<Object>(hashSet), eqproc,
				hashproc, activeenv, message);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#contains(net.morilib.lisp.Datum)
	 */
	public boolean contains(Datum d) {
		if(eqproc.equals(EQ) || eqproc.equals(EQV)) {
			return hashSet.contains(d);
		} else {
			return hashSet.contains(new HashD(d));
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Iterable#iterator()
	 */
	public Iterator<Datum> iterator() {
		final Iterator<Object> itr = hashSet.iterator();

		return new Iterator<Datum>() {

			public boolean hasNext() {
				return itr.hasNext();
			}

			public Datum next() {
				Object o = itr.next();

				if(o instanceof HashD) {
					return ((HashD)o).datum;
				} else {
					return (Datum)o;
				}
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}

		};
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
		if(eqproc.equals(EQ)) {
			return a == b;
		} else if(eqproc.equals(EQV)) {
			return a.equals(b);
		} else {
			return Scheme.callva(
					eqproc, activeenv, message, a, b).isTrue();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#add(net.morilib.lisp.Datum)
	 */
	public Datum add(Datum d) {
		if(eqproc.equals(EQ) || eqproc.equals(EQV)) {
			hashSet.add(d);
		} else {
			hashSet.add(new HashD(d));
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSet#delete(net.morilib.lisp.Datum)
	 */
	public Datum delete(Datum d) {
		if(eqproc.equals(EQ) || eqproc.equals(EQV)) {
			hashSet.remove(d);
		} else {
			hashSet.remove(new HashD(d));
		}
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
		Iterator<Object> itr = hashSet.iterator();

		while(itr.hasNext()) {
			if(eqproc.equals(EQ) || eqproc.equals(EQV)) {
				if(!s.contains((Datum)itr.next())) {
					itr.remove();
				}
			} else {
				if(!s.contains(((HashD)itr.next()).datum)) {
					itr.remove();
				}
			}
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return hashSet.hashCode();
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
	 * @see net.morilib.lisp.Datum3#getResult(java.lang.StringBuilder)
	 */
	@Override
	public void getResult(StringBuilder buf) {
		String dlm = "";

		buf.append("#{");
		for(Datum d : this) {
			buf.append(dlm);
			buf.append(LispUtils.getResult(d));
			dlm = " ";
		}
		buf.append("}");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum3#getResult(java.lang.StringBuilder)
	 */
	@Override
	public void print(StringBuilder buf) {
		String dlm = "";

		buf.append("#{");
		for(Datum d : this) {
			buf.append(dlm);
			buf.append(LispUtils.print(d));
			dlm = " ";
		}
		buf.append("}");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.topology.ILispEnumeratableTopology#getJavaSet()
	 */
	public Set<Datum> getJavaSet() {
		Set<Datum> r = new HashSet<Datum>();

		for(Object o : hashSet) {
			if(o instanceof HashD) {
				r.add(((HashD)o).datum);
			} else {
				r.add((Datum)o);
			}
		}
		return r;
	}

}
