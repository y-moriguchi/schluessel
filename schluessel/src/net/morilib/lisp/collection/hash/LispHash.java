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
package net.morilib.lisp.collection.hash;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.SExpressionDatum;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;
import net.morilib.lisp.collection.LispBag;
import net.morilib.lisp.collection.LispCollection;
import net.morilib.lisp.collection.LispCollections;
import net.morilib.lisp.collection.LispMap;
import net.morilib.lisp.r6rs.hash.ILispR6RSHashtable;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.CharCIEqual;
import net.morilib.lisp.subr.CharEqual;
import net.morilib.lisp.subr.IsEq;
import net.morilib.lisp.subr.IsEqual;
import net.morilib.lisp.subr.IsEqv;
import net.morilib.lisp.subr.StringCIEqual;
import net.morilib.lisp.subr.StringEqual;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/04
 */
public class LispHash extends Datum2
implements LispMap, ILispR6RSHashtable {

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
				return Scheme.callva(
						eqproc, activeenv, message,
						datum, ((HashD)o).datum).isTrue();
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
	 * @author MORIGUCHI, Yuichiro 2012/08/21
	 */
	public static abstract class AbstractMakeHashTable extends Subr {

		/**
		 * 
		 * @param eqproc
		 * @param hashproc
		 * @param k
		 * @param env
		 * @param mesg
		 * @return
		 */
		protected LispHash makeHash(Datum eqproc, Datum hashproc,
				int k, Environment env, LispMessage mesg) {
			return createhash(eqproc, hashproc, k, env, mesg);
		}

		/**
		 * 
		 * @param eqproc
		 * @param hashproc
		 * @param env
		 * @param mesg
		 * @return
		 */
		protected LispHash makeHash(Datum eqproc, Datum hashproc,
				Environment env, LispMessage mesg) {
			return createhash(eqproc, hashproc, env, mesg);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class MakeHashTable extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum p1 = Iterators.nextIf(itr, (Datum)EQUAL);
			Datum p2 = Iterators.nextIf(itr, (Datum)null);

			SubrUtils.checkTerminated(itr, body, mesg);
			return createhash(p1, p2, env, mesg);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/21
	 */
	public static class AlistToHashTable extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d  = SubrUtils.nextIf(itr, mesg, body);
			Datum p1 = Iterators.nextIf(itr, (Datum)EQUAL);
			Datum p2 = Iterators.nextIf(itr, (Datum)null);
			ConsIterator it2 = new ConsIterator(d);
			LispHash r;

			r = createhash(p1, p2, env, mesg);
			while(it2.hasNext()) {
				Datum x = it2.next();

				if(x instanceof Cons) {
					r.put0(((Cons)x).getCar(), ((Cons)x).getCdr());
				} else {
					throw mesg.getError("err.require.assoc", d);
				}
			}

			if(!it2.rest().isNil()) {
				throw mesg.getError("err.list", d);
			}
			return r;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class IsHashTable extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(c1a instanceof LispHash);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class HashTableEquivalenceFunction
	extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispHash) {
				return (Datum)((LispHash)c1a).eqproc;
			} else {
				throw mesg.getError(
						"err.srfi69.require.hashtable", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class HashTableHashFunction extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispHash) {
				return (Datum)((LispHash)c1a).hashproc;
			} else {
				throw mesg.getError(
						"err.srfi69.require.hashtable", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class HashTableRef extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum ht = SubrUtils.nextIf(itr, mesg, body);
			Datum k  = SubrUtils.nextIf(itr, mesg, body);
			Datum th = Iterators.nextIf(itr, (Datum)null);

			if(!(ht instanceof LispHash)) {
				throw mesg.getError(
						"err.srfi69.require.hashtable", ht);
			} else if(th != null && !(th instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", th);
			} else {
				LispHash h = (LispHash)ht;
				Datum r = h.get(k);

				if(r != null) {
					return r;
				} else if(th != null) {
					return Scheme.callva((Procedure)th, env, mesg);
				} else {
					throw mesg.getError("err.srfi69.keynotfound", k);
				}
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class HashTableRefWithDefault extends TernaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
				Environment env, LispMessage mesg) {
			if(!(c1a instanceof LispHash)) {
				throw mesg.getError(
						"err.srfi69.require.hashtable", c1a);
			} else {
				LispHash h = (LispHash)c1a;
				Datum d;

				return ((d = h.get(c2a)) != null) ? d : c3a;
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class HashTableSetS extends TernaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
				Environment env, LispMessage mesg) {
			if(!(c1a instanceof LispHash)) {
				throw mesg.getError(
						"err.srfi69.require.hashtable", c1a);
			} else {
				((LispHash)c1a).put0(c2a, c3a);
				return Undef.UNDEF;
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class HashTableDeleteS extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a,
				Environment env, LispMessage mesg) {
			if(!(c1a instanceof LispHash)) {
				throw mesg.getError(
						"err.srfi69.require.hashtable", c1a);
			} else {
				((LispHash)c1a).remove(c2a);
				return Undef.UNDEF;
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class IsHashTableExists extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a,
				Environment env, LispMessage mesg) {
			if(!(c1a instanceof LispHash)) {
				throw mesg.getError(
						"err.srfi69.require.hashtable", c1a);
			} else {
				return LispBoolean.getInstance(
						((LispHash)c1a).containsKey(c2a));
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class HashTableUpdateS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum ht = SubrUtils.nextIf(itr, mesg, body);
			Datum k  = SubrUtils.nextIf(itr, mesg, body);
			Datum f  = SubrUtils.nextIf(itr, mesg, body);
			Datum th = Iterators.nextIf(itr, (Datum)null);

			SubrUtils.checkTerminated(itr, body, mesg);
			if(!(ht instanceof LispHash)) {
				throw mesg.getError(
						"err.srfi69.require.hashtable", ht);
			} else if(!(f instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", f);
			} else if(th != null && !(th instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", th);
			} else {
				LispHash h = (LispHash)ht;
				Datum r = h.get(k);

				if(r != null) {
					// do nothing
				} else if(th != null) {
					r = Scheme.callva((Procedure)th, env, mesg);
				} else {
					throw mesg.getError("err.srfi69.keynotfound", k);
				}
				return Scheme.callva((Procedure)f, env, mesg, r);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class HashTableUpdateSWithDefault extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum ht = SubrUtils.nextIf(itr, mesg, body);
			Datum k  = SubrUtils.nextIf(itr, mesg, body);
			Datum f  = SubrUtils.nextIf(itr, mesg, body);
			Datum df = SubrUtils.nextIf(itr, mesg, body);

			SubrUtils.checkTerminated(itr, body, mesg);
			if(!(ht instanceof LispHash)) {
				throw mesg.getError(
						"err.srfi69.require.hashtable", ht);
			} else if(!(f instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", f);
			} else {
				LispHash h = (LispHash)ht;
				Datum r = h.get(k);

				if(r == null) {
					r = df;
				}
				return Scheme.callva((Procedure)f, env, mesg, r);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class HashTableSize extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispHash) {
				return LispInteger.valueOf(
						((LispHash)c1a).hashmap.size());
			} else {
				throw mesg.getError(
						"err.srfi69.require.hashtable", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class HashTableKeys extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispHash) {
				return ((LispHash)c1a).keyConsList();
			} else {
				throw mesg.getError(
						"err.srfi69.require.hashtable", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class HashTableValues extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispHash) {
				return ((LispHash)c1a).valueConsList();
			} else {
				throw mesg.getError(
						"err.srfi69.require.hashtable", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class HashTableWalk extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a,
				Datum c2a, Environment env, LispMessage mesg) {
			if(!(c2a instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", c2a);
			} else if(c1a instanceof LispHash) {
				ConsIterator itr = new ConsIterator(
						((LispHash)c1a).entryConsList());

				while(itr.hasNext()) {
					Scheme.call((Procedure)c2a, env, mesg, itr.next());
				}
				return Undef.UNDEF;
			} else {
				throw mesg.getError(
						"err.srfi69.require.hashtable", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class HashTableFold extends TernaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a,
				Datum c3a, Environment env, LispMessage mesg) {
			if(!(c2a instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", c2a);
			} else if(c1a instanceof LispHash) {
				ConsIterator itr = new ConsIterator(
						((LispHash)c1a).entryConsList());
				Datum a = c3a;

				while(itr.hasNext()) {
					Datum arg;
					Cons  c = (Cons)itr.next();

					arg = LispUtils.list(c.getCar(),
							((Cons)c.getCdr()).getCar(), a);
					a   = Scheme.call((Procedure)c2a, env, mesg, arg);
				}
				return a;
			} else {
				throw mesg.getError(
						"err.srfi69.require.hashtable", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class HashTableToAlist extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispHash) {
				return ((LispHash)c1a).entryAlist();
			} else {
				throw mesg.getError(
						"err.srfi69.require.hashtable", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class HashTableCopy extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispHash) {
				LispHash h = (LispHash)c1a;
				Iterator<Map.Entry<Datum, Datum>> ei;
				LispHash r = createhash((Datum)h.eqproc,
						(Datum)h.hashproc, h.activeenv, h.message);

				ei = h.entryIterator();
				while(ei.hasNext()) {
					Map.Entry<Datum, Datum> e = ei.next();

					r.put0(e.getKey(), e.getValue());
				}
				return r;
			} else {
				throw mesg.getError(
						"err.srfi69.require.hashtable", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class HashTableMergeS extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a,
				Datum c2a, Environment env, LispMessage mesg) {
			if(!(c2a instanceof LispHash)) {
				throw mesg.getError(
						"err.srfi69.require.hashtable", c2a);
			} else if(c1a instanceof LispHash) {
				LispHash h = (LispHash)c1a;
				LispHash g = (LispHash)c2a;
				Iterator<Map.Entry<Datum, Datum>> ei;

//				h.hashmap.putAll(g.hashmap);
				ei = g.entryIterator();
				while(ei.hasNext()) {
					Map.Entry<Datum, Datum> e = ei.next();

					h.put0(e.getKey(), e.getValue());
				}
				return h;
			} else {
				throw mesg.getError(
						"err.srfi69.require.hashtable", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class MakeHashMap extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum p1 = Iterators.nextIf(itr, (Datum)EQUAL);

			SubrUtils.checkTerminated(itr, body, mesg);
			return createhash(p1, null, env, mesg);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/21
	 */
	public static class SubrHashMap extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum p1 = Iterators.nextIf(itr, (Datum)EQUAL);
			Datum d  = itr.rest();
			ConsIterator it2 = new ConsIterator(d);
			LispHash r;

			if(!(p1 instanceof Procedure)) {
				p1 = (Datum)EQUAL;
				d = new Cons(p1, d);
			}

			r = createhash(p1, null, env, mesg);
			while(it2.hasNext()) {
				Datum x = it2.next();

				if(x instanceof Cons) {
					r.put0(((Cons)x).getCar(), ((Cons)x).getCdr());
				} else {
					throw mesg.getError("err.require.pair", x);
				}
			}

			if(!it2.rest().isNil()) {
				throw mesg.getError("err.list", d);
			}
			return r;
		}

	}

	//
	private static final Map<Datum, Datum> DEFHASH;

	/**
	 * 
	 */
	public static final Procedure EQ         = new IsEq();

	/**
	 * 
	 */
	public static final Procedure EQV        = new IsEqv();

	/**
	 * 
	 */
	public static final Procedure EQUAL      = new IsEqual();

	/**
	 * 
	 */
	public static final Procedure EQ_HASH    = new HashByIdentity();

	/**
	 * 
	 */
	public static final Procedure EQV_HASH   = new Hasv();

	/**
	 * 
	 */
	public static final Procedure EQUAL_HASH = new Hash();

	static {
		DEFHASH = new HashMap<Datum, Datum>();
		DEFHASH.put(new StringEqual(),   new StringHash());
		DEFHASH.put(new StringCIEqual(), new StringCiHash());
		DEFHASH.put(new CharEqual(),     new CharHash());
		DEFHASH.put(new CharCIEqual(),   new CharCiHash());
		DEFHASH.put((Datum)EQ,           (Datum)EQ_HASH);
		DEFHASH.put((Datum)EQV,          (Datum)EQV_HASH);
		DEFHASH.put((Datum)EQUAL,        (Datum)EQUAL_HASH);
	}

	//
	private Map<Object, Datum> hashmap;
	private Procedure eqproc;
	private Procedure hashproc;
	private Environment activeenv;
	private LispMessage message;

	//
	private LispHash(Map<Object, Datum> hashmap,
			Procedure eqproc, Procedure hashproc,
			Environment activeenv, LispMessage message) {
		this.hashmap   = hashmap;
		this.eqproc    = eqproc;
		this.hashproc  = hashproc;
		this.activeenv = activeenv;
		this.message   = message;
	}

	/**
	 * 
	 */
	public LispHash() {
		this.hashmap   = new HashMap<Object, Datum>();
		this.eqproc    = EQV;
		this.hashproc  = EQV_HASH;
		this.activeenv = new Environment();
		this.message   = LispMessage.getInstance();
	}

	/**
	 * @param lispHash
	 */
	public LispHash(LispHash hash) {
		this(new HashMap<Object, Datum>(hash.hashmap),
				hash.eqproc, hash.hashproc, hash.activeenv,
				hash.message);
	}

	//
	private static LispHash createhash(Datum p1, Datum p2,
			Environment env, LispMessage mesg) {
		if(!(p1 instanceof Procedure)) {
			throw mesg.getError("err.require.procedure", p1);
		} else if(p2 != null && !(p2 instanceof Procedure)) {
			throw mesg.getError("err.require.procedure", p2);
		} else if(p1.equals(EQ)) {
			return new LispHash(
					new IdentityHashMap<Object, Datum>(),
					EQ, EQ_HASH, env, mesg);
		} else if(p2 != null || (p2 = DEFHASH.get(p1)) != null) {
			return new LispHash(new HashMap<Object, Datum>(),
					(Procedure)p1, (Procedure)p2, env, mesg);
		} else {
			return new LispHash(new HashMap<Object, Datum>(),
					EQUAL, EQUAL_HASH, env, mesg);
		}
	}

	//
	private static LispHash createhash(Datum p1, Datum p2, int k,
			Environment env, LispMessage mesg) {
		if(!(p1 instanceof Procedure)) {
			throw mesg.getError("err.require.procedure", p1);
		} else if(p2 != null && !(p2 instanceof Procedure)) {
			throw mesg.getError("err.require.procedure", p2);
		} else if(p1.equals(EQ)) {
			return new LispHash(
					new IdentityHashMap<Object, Datum>(),
					EQ, EQ_HASH, env, mesg);
		} else if(p2 != null || (p2 = DEFHASH.get(p1)) != null) {
			return new LispHash(new HashMap<Object, Datum>(k),
					(Procedure)p1, (Procedure)p2, env, mesg);
		} else {
			return new LispHash(new HashMap<Object, Datum>(k),
					EQUAL, EQUAL_HASH, env, mesg);
		}
	}

	/**
	 * 
	 * @return
	 */
	public Datum keyConsList() {
		ConsListBuilder b = new ConsListBuilder();

		for(Object x : hashmap.keySet()) {
			if(eqproc.equals(EQ) || eqproc.equals(EQV)) {
				b.append((Datum)x);
			} else {
				b.append(((HashD)x).datum);
			}
		}
		return b.get();
	}

	/**
	 * 
	 * @return
	 */
	public Datum valueConsList() {
		ConsListBuilder b = new ConsListBuilder();

		for(Object x : hashmap.values()) {
			b.append((Datum)x);
		}
		return b.get();
	}

	/**
	 * 
	 * @return
	 */
	public Datum entryConsList() {
		ConsListBuilder b = new ConsListBuilder();

		for(Map.Entry<Object, Datum> x : hashmap.entrySet()) {
			if(eqproc.equals(EQ) || eqproc.equals(EQV)) {
				b.append(LispUtils.list(
						(Datum)x.getKey(), x.getValue()));
			} else {
				b.append(LispUtils.list(
						((HashD)x.getKey()).datum, x.getValue()));
			}
		}
		return b.get();
	}

	/**
	 * 
	 * @return
	 */
	public Datum entryAlist() {
		ConsListBuilder b = new ConsListBuilder();

		for(Map.Entry<Object, Datum> x : hashmap.entrySet()) {
			if(eqproc.equals(EQ) || eqproc.equals(EQV)) {
				b.append(new Cons(
						(Datum)x.getKey(), x.getValue()));
			} else {
				b.append(new Cons(
						((HashD)x.getKey()).datum, x.getValue()));
			}
		}
		return b.get();
	}

	/**
	 * @param k
	 * @return
	 */
	public boolean containsKey(Datum k) {
		if(eqproc.equals(EQ) || eqproc.equals(EQV)) {
			return hashmap.containsKey(k);
		} else {
			return hashmap.containsKey(new HashD(k));
		}
	}

	//
	/*package*/ Datum put0(Datum k, Datum v) {
		Datum r;

		if(eqproc.equals(EQ) || eqproc.equals(EQV)) {
			r = hashmap.put(k, v);
		} else {
			r = hashmap.put(new HashD(k), v);
		}
		return r;
	}

	/**
	 * @param k
	 */
	public Datum remove(Datum k) {
		Datum r;

		if(eqproc.equals(EQ) || eqproc.equals(EQV)) {
			r = hashmap.remove(k);
		} else {
			r = hashmap.remove(new HashD(k));
		}
		return r;
	}

	/**
	 * 
	 * @param k
	 * @return
	 */
	public Datum get(Datum k) {
		Datum r;

		if(eqproc.equals(EQ) || eqproc.equals(EQV)) {
			r = hashmap.get(k);
		} else {
			r = hashmap.get(new HashD(k));
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<hash>");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#getCollectionName()
	 */
	public Symbol getCollectionName() {
		return Symbol.getSymbol("hash-map");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#size()
	 */
	public int size() {
		return hashmap.size();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equivalence(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public boolean equivalence(Datum a, Datum b) {
//		if(a instanceof Cons && b instanceof Cons) {
//			Cons x = (Cons)a;
//			Cons y = (Cons)b;
//
//			return (equivalenceKey  (x.getCar(), y.getCar()) &&
//					equivalenceValue(x.getCdr(), y.getCdr()));
//		} else {
//			return equivalenceKey(a, b);
//		}
		return LispCollections.equivalence(this, a, b);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#prototype()
	 */
	public Datum prototype() {
		return new LispHash(new HashMap<Object, Datum>(), eqproc,
				hashproc, activeenv, message);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#clear()
	 */
	public Datum clear() {
		hashmap.clear();
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection)
	 */
	public boolean equalTo(LispCollection col) {
		if(col instanceof LispHash) {
			return hashmap.equals(((LispHash)col).hashmap);
		} else {
//			for(Datum x : col) {
//				if(x instanceof Cons) {
//					if(!contains(x)) {
//						return false;
//					}
//				} else {
//					return false;
//				}
//			}
//
//			for(Datum e : this) {
//				if(!col.contains(e)) {
//					return false;
//				}
//			}
//			return true;
			return LispCollections.equalsMapEntry(this, col);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection, net.morilib.lisp.Procedure, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public boolean equalTo(LispCollection col, Procedure proc,
			Environment env, LispMessage mesg) {
		for(Datum e : this) {
			if(!col.contains(e)) {
				return false;
			}
		}

		for(Datum e : col) {
			if(!contains(e)) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#duplicate()
	 */
	public LispHash duplicate() {
		return new LispHash(this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#contains(net.morilib.lisp.Datum)
	 */
	public boolean contains(Datum d) {
		if(d instanceof Cons) {
			return equivalence(get(((Cons)d).getCar()),
					((Cons)d).getCdr());
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Iterable#iterator()
	 */
	public Iterator<Datum> iterator() {
		final Iterator<Map.Entry<Object, Datum>> itr;

		itr = hashmap.entrySet().iterator();
		return new Iterator<Datum>() {

			public boolean hasNext() {
				return itr.hasNext();
			}

			public Datum next() {
				final Map.Entry<Object, Datum> e = itr.next();
				final Datum d;

				if(eqproc.equals(EQ) || eqproc.equals(EQV)) {
					d = (Datum)e.getKey();
				} else {
					d = ((HashD)e.getKey()).datum;
				}
				return new Cons(d, e.getValue());
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
		final Iterator<Map.Entry<Object, Datum>> itr;

		itr = hashmap.entrySet().iterator();
		return new Iterator<Map.Entry<Datum, Datum>>() {

			public boolean hasNext() {
				return itr.hasNext();
			}

			public Map.Entry<Datum, Datum> next() {
				final Map.Entry<Object, Datum> e = itr.next();
				final Datum d;

				if(eqproc.equals(EQ) || eqproc.equals(EQV)) {
					d = (Datum)e.getKey();
				} else {
					d = ((HashD)e.getKey()).datum;
				}

				return new Map.Entry<Datum, Datum>() {

					public Datum getKey() {
						return d;
					}

					public Datum getValue() {
						return e.getValue();
					}

					public Datum setValue(Datum value) {
						throw new UnsupportedOperationException();
					}
				};
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
		return eqproc;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#valueEquivalence()
	 */
	public Procedure valueEquivalence() {
		return EQUAL;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#keysToList()
	 */
	public SExpressionDatum keysToList() {
		ConsListBuilder b = new ConsListBuilder();

		for(Object o : hashmap.keySet()) {
			if(eqproc.equals(EQ) || eqproc.equals(EQV)) {
				b.append((Datum)o);
			} else {
				b.append(((HashD)o).datum);
			}
		}
		return b.get();
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
		Datum[] r = new Datum[2];

		r[0] = this;
		r[1] = put0(k, v);
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#copyDeleteFromMap(net.morilib.lisp.Datum)
	 */
	public Datum copyDeleteKey(Datum k) {
		return duplicate().deleteKey(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#deleteFromMap(net.morilib.lisp.Datum)
	 */
	public Datum deleteKey(Datum k) {
		remove(k);
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#copyDeleteFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteFromKey(LispBag k) {
		return duplicate().deleteFromKey(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#deleteFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteFromKey(LispBag k) {
		for(Datum x : k) {
			remove(x);
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
		Iterator<Map.Entry<Datum, Datum>> itr = m.entryIterator();

		while(itr.hasNext()) {
			Map.Entry<Datum, Datum> e = itr.next();

			put0(e.getKey(), e.getValue());
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispMap#equivalenceKey(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public boolean equivalenceKey(Datum a, Datum b) {
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
	 * @see net.morilib.lisp.collection.LispMap#equivalenceValue(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public boolean equivalenceValue(Datum a, Datum b) {
		return LispUtils.equals(a, b);
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
	 * @see net.morilib.lisp.collection.LispCollection#toList()
	 */
	public Datum toList() {
		return LispUtils.toAlist(entryIterator());
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
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#setBang(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public void setBang(Datum k, Datum v) {
		put0(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#clearBang(int)
	 */
	@Override
	public void clearBang(int k) {
		hashmap.clear();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#keysToVector()
	 */
	@Override
	public LispVector keysToVector() {
		List<Datum> l = new ArrayList<Datum>();

		for(Object k : hashmap.keySet()) {
			if(eqproc.equals(EQ) || eqproc.equals(EQV)) {
				l.add((Datum)k);
			} else {
				l.add(((HashD)k).datum);
			}
		}
		return new LispVector(l);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#valuesToVector()
	 */
	@Override
	public LispVector valuesToVector() {
		return new LispVector(hashmap.values());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#entriesToVector()
	 */
	@Override
	public LispVector[] entriesToVector() {
		return new LispVector[] {
				keysToVector(), valuesToVector()
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#hashFunction()
	 */
	@Override
	public Procedure hashFunction() {
		return hashproc;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.hash.ILispR6RSHashtable#isMutableHashtable()
	 */
	@Override
	public boolean isMutableHashtable() {
		return true;
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
