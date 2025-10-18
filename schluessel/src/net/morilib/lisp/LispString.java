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
import java.util.List;
import java.util.NoSuchElementException;

import net.morilib.lisp.array.ILispArray;
import net.morilib.lisp.array.LispArrayPrototype;
import net.morilib.lisp.array.LispCharArray;
import net.morilib.lisp.collection.LispBag;
import net.morilib.lisp.collection.LispCollection;
import net.morilib.lisp.collection.LispPurelyMutableCollection;
import net.morilib.lisp.collection.LispSequence;
import net.morilib.lisp.iterator.ILispIterable;
import net.morilib.lisp.iterator.ILispIterator;
import net.morilib.lisp.math.algebra.ILispAddable;
import net.morilib.lisp.sort.SRFI95Sequence;
import net.morilib.lisp.sos.LispType;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;
import net.morilib.util.Iterators;
import net.morilib.util.Strings;
import net.morilib.util.io.UTF16;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class LispString extends Atom
implements JavaObjective, LispSequence, LispPurelyMutableCollection,
LispArrayPrototype, ILispAddable<LispString>,
SRFI95Sequence<LispString>, ILispIterable, java.io.Serializable {

	//
	private static final long serialVersionUID = -2492989855710883745L;

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/10
	 */
	public static class StringSetS extends TernaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Datum c3a,
				Environment env, LispMessage mesg) {
			char[] cs = SubrUtils.getString(c1a, mesg).toCharArray();
			int i = SubrUtils.getSmallInt(c2a, mesg);
			char ch = SubrUtils.getCharacter(c3a, mesg);

			if(i < 0 || i >= cs.length) {
				throw mesg.getError("err.string.outofrange", c2a);
			}
			cs[i] = ch;
			((LispString)c1a).value = new String(cs);
			return Undef.UNDEF;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/10
	 */
	public static class StringFillS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum c1a = Iterators.nextIf(
					itr, mesg.getError("err.argument", body));
			Datum c2a = Iterators.nextIf(
					itr, mesg.getError("err.argument", body));
			Datum c3a = Iterators.nextIf(itr, LispInteger.ZERO);
			Datum c4a = Iterators.nextIf(itr, (Datum)null);
			char[] cs = SubrUtils.getString(c1a, mesg).toCharArray();
			char   ch = SubrUtils.getCharacter(c2a, mesg);
			int b, e = cs.length;

			SubrUtils.checkTerminated(itr, body, mesg);
			b = SubrUtils.getSmallInt(c3a, mesg);
			if(c4a != null) {
				e = SubrUtils.getSmallInt(c4a, mesg);
			}

			for(int i = b; i < e; i++) {
				cs[i] = ch;
			}
			((LispString)c1a).value = new String(cs);
			return Undef.UNDEF;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/10
	 */
	public static class StringCopyS extends Subr {

		/**
		 * 
		 * @param c1a
		 * @param c2a
		 * @param c3a
		 * @param env
		 * @param mesg
		 * @return
		 */
		protected Datum execute(
				Datum c1a, Datum c2a, Datum c3a,
				Environment env, LispMessage mesg) {
			String s = SubrUtils.getString(c3a, mesg);

			return execute(c1a, c2a, s, 0, s.length(), mesg);
		}

		/**
		 * 
		 * @param c1a
		 * @param c2a
		 * @param s
		 * @param b
		 * @param e
		 * @return
		 */
		protected Datum execute(
				Datum c1a, Datum c2a,
				String str, int b, int e, LispMessage mesg) {
			char[] t = SubrUtils.getString(c1a, mesg).toCharArray();
			char[] s = str.toCharArray();
			int ts = SubrUtils.getSmallInt(c2a, mesg);

			if(ts >= t.length) {
				throw mesg.getError("err.string.outofrange", c1a);
			} else if(e - b > t.length - ts) {
				throw mesg.getError("err.string.outofrange");
			}
			System.arraycopy(s, b, t, ts, e - b);
			((LispString)c1a).value = new String(t);
			return Undef.UNDEF;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env, LispMessage mesg) {
			List<Datum> l = LispUtils.consToList(body, mesg);
			String s;
			int b, e;

			if(l.size() == 3) {
				return execute(
						l.get(0), l.get(1), l.get(2), env, mesg);
			} else if(l.size() == 4) {
				s = SubrUtils.getString(l.get(2), mesg);
				b = SubrUtils.getSmallInt(l.get(3), mesg);
				e = s.length();
				if(b >= s.length()) {
					throw mesg.getError("err.string.outofrange", l.get(3));
				}
			} else if(l.size() == 5) {
				s = SubrUtils.getString(l.get(2), mesg);
				b = SubrUtils.getSmallInt(l.get(3), mesg);
				e = SubrUtils.getSmallInt(l.get(4), mesg);
				if(b >= s.length()) {
					throw mesg.getError("err.string.outofrange", l.get(3));
				} else if(e > s.length()) {
					throw mesg.getError("err.string.outofrange", l.get(4));
				} else if(b > e) {
					throw mesg.getError("err.range.invalid");
				}
			} else {
				throw mesg.getError("err.argument", body);
			}
			return execute(l.get(0), l.get(1), s, b, e, mesg);
		}

	}

	/**
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/10
	 */
	public static class StringTitlecaseS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d1 = SubrUtils.nextIf(
					itr, mesg, "err.argument", body);
			String s = SubrUtils.getString(d1, mesg);
			int b = SubrUtils.nextSmallInt(itr, 0, mesg);
			int e = SubrUtils.nextSmallInt(itr, s.length(), mesg);

			SubrUtils.checkTerminated(itr, body, mesg);
			((LispString)d1).value = new String(Strings.toTitleCase(
					s.toCharArray(), b, e));
			return Undef.UNDEF;
		}

	}

	/**
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/10
	 */
	public static class StringUpcaseS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d1 = SubrUtils.nextIf(
					itr, mesg, "err.argument", body);
			String s = SubrUtils.getString(d1, mesg);
			int b = SubrUtils.nextSmallInt(itr, 0, mesg);
			int e = SubrUtils.nextSmallInt(itr, s.length(), mesg);

			SubrUtils.checkTerminated(itr, body, mesg);
			((LispString)d1).value = new String(Strings.toUpperCase(
					s.toCharArray(), b, e));
			return Undef.UNDEF;
		}

	}

	/**
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/10
	 */
	public static class StringDowncaseS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d1 = SubrUtils.nextIf(
					itr, mesg, "err.argument", body);
			String s = SubrUtils.getString(d1, mesg);
			int b = SubrUtils.nextSmallInt(itr, 0, mesg);
			int e = SubrUtils.nextSmallInt(itr, s.length(), mesg);

			SubrUtils.checkTerminated(itr, body, mesg);
			((LispString)d1).value = new String(Strings.toLowerCase(
					s.toCharArray(), b, e));
			return Undef.UNDEF;
		}

	}

	/**
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/10
	 */
	public static class StringReverseS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d1 = SubrUtils.nextIf(
					itr, mesg, "err.argument", body);
			String s = SubrUtils.getString(d1, mesg);
			int b = SubrUtils.nextSmallInt(itr, 0, mesg);
			int e = SubrUtils.nextSmallInt(itr, s.length(), mesg);

			SubrUtils.checkTerminated(itr, body, mesg);
			((LispString)d1).value = new String(Strings.reverse(
					s.toCharArray(), b, e));
			return Undef.UNDEF;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/04/12
	 */
	public static class StringXcopyS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d1 = SubrUtils.nextIf(
					itr, mesg, "err.argument", body);
			String w = SubrUtils.getString(d1, mesg);
			int z = SubrUtils.nextSmallInt(itr, mesg, body);
			String s = SubrUtils.nextString(itr, mesg, body);
			int f = SubrUtils.nextSmallInt(itr, mesg, body);
			int t = SubrUtils.nextSmallInt(itr, s.length() + f, mesg);
			int b = SubrUtils.nextSmallInt(itr, 0, mesg);
			int e = SubrUtils.nextSmallInt(itr, s.length(), mesg);
			char[] c = w.toCharArray();

			Strings.xcopy(c, z, s, f, t, b, e);
			((LispString)d1).value = new String(c);
			return Undef.UNDEF;
		}

	}

	/**
	 * 
	 */
	public static final LispString EMPTY = new LispString("");

	//
	private String value;

	/**
	 * 
	 * @param value
	 */
	public LispString(String value) {
		if(value == null) {
			throw new NullPointerException("string is null");
		}
		this.value = value;
	}

	/**
	 * 
	 * @param str
	 */
	public LispString(LispString str) {
		if(str == null) {
			throw new NullPointerException();
		}
		value = str.value;
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static LispString valueOf(String s) {
		return new LispString(s);
	}

	/**
	 * 
	 * @param a
	 * @return
	 */
	public boolean isEqualTo(LispString a) {
		return value.equals(a.value);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getString()
	 */
	public String getString() {
		return value;
	}

	/**
	 * 
	 * @param value
	 */
	public void setString(String value) {
		this.value = value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#toLispString()
	 */
	public LispString toLispString() {
		return this;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#print()
	 */
	public String print() {
		return value;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Atom#getResult()
	 */
	public String getResult() {
		StringBuilder b = new StringBuilder();

		for(int i = 0; i < value.length(); i++) {
			char c = value.charAt(i);

			if(c == '\\') {
				b.append("\\\\");
			} else if(c == '\"') {
				b.append("\\\"");
			} else {
				b.append(c);
			}
		}

		return "\"" + b.toString() + "\"";
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#isTypeString()
	 */
	public boolean isTypeString() {
		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	public LispType getType() {
		return LispType.STRING;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#toObject()
	 */
	public Object toObject() {
		return value;
	}

	/**
	 * 
	 * @return
	 */
	public LispString copy() {
		return new LispString(this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#equivalence()
	 */
	public Procedure equivalence() {
		return new net.morilib.lisp.subr.CharEqual();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyAdd(net.morilib.lisp.Datum)
	 */
	public Datum copyAdd(Datum d) {
		return copy().add(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#add(net.morilib.lisp.Datum)
	 */
	public Datum add(Datum d) {
		if(d instanceof LispCharacter) {
			value += Strings.newString(d.getCharacter());
			return this;
		} else {
			throw new ClassCastException("err.require.char");
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDelete(net.morilib.lisp.Datum)
	 */
	public Datum copyDelete(Datum d) {
		return copy().delete(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#delete(net.morilib.lisp.Datum)
	 */
	public Datum delete(Datum d) {
		if(d instanceof LispCharacter) {
			StringBuilder b = new StringBuilder(value);
			int i = value.indexOf(d.getCharacter());

			if(i >= 0) {
				b.deleteCharAt(i);
			}
			value = b.toString();
			return this;
		} else {
			throw new ClassCastException("err.require.char");
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteAll(net.morilib.lisp.Datum)
	 */
	public Datum copyDeleteAll(Datum d) {
		return copy().deleteAll(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteAll(net.morilib.lisp.Datum)
	 */
	public Datum deleteAll(Datum d) {
		if(d instanceof LispCharacter) {
			StringBuilder b = new StringBuilder(value);
			int i;

			while((i = b.toString().indexOf(d.getCharacter())) >= 0) {
				b.deleteCharAt(i);
			}
			value = b.toString();
			return this;
		} else {
			throw new ClassCastException("err.require.char");
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyAddFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyAddFrom(LispBag d) {
		return copy().addFrom(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#addFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum addFrom(LispBag d) {
		for(Datum x : d) {
			add(x);
		}
		return this;
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
		for(Datum x : d) {
			delete(x);
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#copyDeleteAllFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum copyDeleteAllFrom(LispBag d) {
		return copy().deleteAllFrom(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispBag#deleteAllFrom(net.morilib.lisp.collection.LispBag)
	 */
	public Datum deleteAllFrom(LispBag d) {
		for(Datum x : d) {
			deleteAll(x);
		}
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#getCollectionName()
	 */
	public Symbol getCollectionName() {
		return Symbol.getSymbol("string");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#size()
	 */
	public int size() {
		return value.length();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equivalence(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public boolean equivalence(Datum a, Datum b) {
		if(a instanceof LispCharacter && b instanceof LispCharacter) {
			return a.getCharacter() == b.getCharacter();
		} else {
			throw new ClassCastException("err.require.char");
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#prototype()
	 */
	public Datum prototype() {
		return new LispString("");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#clear()
	 */
	public Datum clear() {
		value = "";
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection)
	 */
	public boolean equalTo(LispCollection col) {
		int i = 0;

		for(Datum x : col) {
			if(!(x instanceof LispCharacter)) {
				throw new ClassCastException("err.require.char");
			} else if(x.getCharacter() != value.charAt(i++)) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#equalTo(net.morilib.lisp.collection.LispCollection, net.morilib.lisp.Procedure, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	public boolean equalTo(LispCollection col, Procedure proc,
			Environment env, LispMessage mesg) {
		int i = 0;

		for(Datum x : col) {
			if(!(x instanceof LispCharacter)) {
				throw new ClassCastException("err.require.char");
			} else if(!Scheme.callva(proc, env, mesg, x,
					LispCharacter.valueOf(value.charAt(i++)))
					.isTrue()) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#duplicate()
	 */
	public Datum duplicate() {
		return new LispString(this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#contains(net.morilib.lisp.Datum)
	 */
	public boolean contains(Datum d) {
		if(d instanceof LispCharacter) {
			return value.indexOf(d.getCharacter()) >= 0;
		} else {
			throw new ClassCastException("err.require.char");
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Iterable#iterator()
	 */
	public Iterator<Datum> iterator() {
		final int[] i = new int[1];

		i[0] = 0;
		return new Iterator<Datum>() {

			public boolean hasNext() {
				return i[0] < value.length();
			}

			public Datum next() {
				return new LispCharacter(value.charAt(i[0]++));
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#get(int)
	 */
	public LispCharacter get(int index) {
		return new LispCharacter(value.charAt(index));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#copySet(int, net.morilib.lisp.Datum)
	 */
	public Datum copySet(int index, Datum d) {
		char[] cs = value.toCharArray();

		if(d instanceof LispCharacter) {
			cs[index] = d.getCharacter();
			return new LispString(new String(cs));
		} else {
			throw new ClassCastException("err.require.char");
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#set(int, net.morilib.lisp.Datum)
	 */
	public Datum set(int index, Datum d) {
		return copySet(index, d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#replace(net.morilib.lisp.collection.LispSequence, int, int, int)
	 */
	public Datum replace(LispSequence src, int srcPos, int destPos,
			int len) {
		char[] cs = value.toCharArray();

		for(int i = 0; i < len; i++) {
			Datum d = src.get(i + srcPos);

			if(d instanceof LispCharacter) {
				cs[i + destPos] = d.getCharacter();
			} else {
				throw new ClassCastException("err.require.char");
			}
		}
		return new LispString(new String(cs));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#arraycopy(net.morilib.lisp.collection.LispSequence, int, int, int)
	 */
	public Datum arraycopy(LispSequence src, int srcPos, int destPos,
			int len) {
		return replace(src, srcPos, destPos, len);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#copy(int, int)
	 */
	public Datum copy(int b, int e) {
		if(b >= value.length()) {
			throw new IndexOutOfBoundsException("" + b);
		}
		return new LispString(value.substring(b, e));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#count(net.morilib.lisp.Datum)
	 */
	public int count(Datum c2a) {
		char c;
		int  r = 0;

		if(c2a instanceof LispCharacter) {
			c = c2a.getCharacter();

			for(int i = 0; i < value.length(); i++) {
				if(c == value.charAt(i)) {
					r++;
				}
			}
			return r;
		} else {
			throw new ClassCastException("err.require.char");
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispCollection#toList()
	 */
	public Datum toList() {
		return LispUtils.stringToList(value);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#first()
	 */
	public Datum first() {
		if(value.length() == 0) {
			throw new NoSuchElementException();
		}
		return LispCharacter.valueOf(value.charAt(0));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.collection.LispSequence#last()
	 */
	public Datum last() {
		if(value.length() == 0) {
			throw new NoSuchElementException();
		}
		return LispCharacter.valueOf(value.charAt(value.length() - 1));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArrayPrototype#makeArray(int[])
	 */
	public ILispArray makeArray(int... is) {
		return new LispCharArray(
				value.isEmpty() ? (char)0 : value.charAt(0), is);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispAddable#add(net.morilib.lisp.math.algebra.ILispAddable)
	 */
	public LispString add(LispString y) {
		return valueOf(value + y.value);
	}

	/**
	 * 
	 * @return
	 */
	public int[] toUTF32() {
		return UTF16.getInts(value);
	}

	/**
	 * 
	 * @return
	 */
	public LispVector toVector() {
		int[]   r1 = toUTF32();
		Datum[] r2 = new Datum[r1.length];

		for(int i = 0; i < r1.length; i++) {
			r2[i] = LispCharacter.valueOf(r1[i]);
		}
		return new LispVector(r2);
	}

	/**
	 * 
	 * @param v
	 * @param mesg
	 * @return
	 */
	public static LispString toString(LispVector v, LispMessage mesg) {
		StringBuilder b = new StringBuilder();

		for(int i = 0; i < v.size(); i++) {
			b.appendCodePoint(SubrUtils.getCharacterCodePoint(
					v.get(i), mesg));
		}
		return new LispString(b.toString());
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public static LispString toString(LispVector v) {
		StringBuilder b = new StringBuilder();
		int c;

		for(int i = 0; i < v.size(); i++) {
			c = ((LispCharacter)v.get(i)).getCharacterCodePoint();
			b.appendCodePoint(c);
		}
		return new LispString(b.toString());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#isSorted(java.util.Comparator)
	 */
	public boolean isSorted(Comparator<Datum> cmp) {
		return toVector().isSorted(cmp);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#merge(net.morilib.lisp.sort.SRFI95Sequence, java.util.Comparator)
	 */
	public LispString merge(LispString m, Comparator<Datum> cmp) {
		return toString(toVector().merge(m.toVector(), cmp));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#mergeS(net.morilib.lisp.sort.SRFI95Sequence, java.util.Comparator)
	 */
	public LispString mergeS(LispString m, Comparator<Datum> cmp) {
		LispVector r = toVector();

		r.mergeS(m.toVector(), cmp);
		value = toString(r).value;
		return this;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#sort(java.util.Comparator)
	 */
	public LispString sort(Comparator<Datum> cmp) {
		return toString(toVector().sort(cmp));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sort.SRFI95Sequence#sortS(java.util.Comparator)
	 */
	public void sortS(Comparator<Datum> cmp) {
		LispVector r = toVector();

		r.sortS(cmp);
		value = toString(r).value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterable#lispIterator()
	 */
	public ILispIterator lispIterator() {
		final int[] ptr = new int[1];

		ptr[0] = 0;
		return new ILispIterator() {

			public boolean isTerminated() {
				return ptr[0] >= value.length();
			}

			public ILispIterator next() {
				if(isTerminated()) {
					throw new NoSuchElementException();
				} else if(Character.isHighSurrogate(
						value.charAt(ptr[0]++))) {
					ptr[0]++;
				}
				return this;
			}

			public Datum getCurrentDatum() {
				if(isTerminated()) {
					throw new NoSuchElementException();
				}
				return LispCharacter.valueOf(
						value.codePointAt(ptr[0]));
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.accessor.ILispRef#ref(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum ref(Datum arg, LispMessage mesg) {
		int k = SubrUtils.getSmallInt(arg, mesg), j;

		try {
			j = value.offsetByCodePoints(0, k);
			return LispCharacter.valueOf(value.codePointAt(j));
		} catch(IndexOutOfBoundsException e) {
			throw mesg.getError("err.accessor.ref.outofrange", arg);
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof LispString) {
			return value.equals(((LispString)obj).value);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return value.hashCode();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toString()
	 */
	public String toString() {
		return "\"" + value + "\"";
	}

}
