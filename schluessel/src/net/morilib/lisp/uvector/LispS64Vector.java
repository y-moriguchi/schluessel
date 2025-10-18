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
package net.morilib.lisp.uvector;

import java.util.List;

import net.morilib.lisp.Atom;
import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.primitive.LongArrayVector;
import net.morilib.util.primitive.LongVector;
import net.morilib.util.uvector.LongArray;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/06
 */
public class LispS64Vector extends Atom
implements HomogeneousSignedArray, java.io.Serializable {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class S64Vector extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			return new LispS64Vector(toLongList(body, mesg));
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class IsS64Vector extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(
					c1a instanceof LispS64Vector);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class MakeS64Vector extends Subr {

		//
		private long[] makeVector(int len, long d) {
			LongVector b = new LongArrayVector();

			for(int i = 0; i < len; i++) {
				b.add(d);
			}
			return b.toLongArray();
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> lst = LispUtils.consToList(body, mesg);

			if(lst.size() == 1) {
				int t = SubrUtils.getSmallInt(
						lst.get(0), mesg);

				if(t < 0) {
					throw mesg.getError(
							"err.require.int.nonnegative", lst.get(0));
				}
				return new LispS64Vector(makeVector(t, 0));
			} else if(lst.size() == 2) {
				int t = SubrUtils.getSmallInt(
						lst.get(0), mesg);

				if(t < 0) {
					throw mesg.getError(
							"err.require.int.nonnegative", lst.get(0));
				} else if(!checkRange(lst.get(1))) {
					throw mesg.getError(
							"err.uvector.outofrange.s64", lst.get(1));
				}
				return new LispS64Vector(
						makeVector(t, lst.get(1).getLong()));
			} else {
				throw mesg.getError("err.argument", symbolName);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class S64VectorToList extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispS64Vector) {
				ConsListBuilder b = new ConsListBuilder();
				LispS64Vector v = (LispS64Vector)c1a;

				for(int i = 0; i < v.vector.size(); i++) {
					b.append(LispInteger.valueOf(
							v.vector.getLong(i)));
				}
				return b.get();
			} else {
				throw mesg.getError("err.uvector.outofrange.s64", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class ListToS64Vector extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof Cons || c1a.equals(Nil.NIL)) {
				return new LispS64Vector(toLongList(c1a, mesg));
			} else {
				throw mesg.getError("err.require.list", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class EqualS64Vector extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(!(c1a instanceof LispS64Vector)) {
				throw mesg.getError("err.uvector.require.s64", c1a);
			} else if(!(c2a instanceof LispS32Vector)) {
				throw mesg.getError("err.uvector.require.s64", c2a);
			}
			return LispBoolean.getInstance(
					((LispS64Vector)c1a).vector.equals(
							((LispS64Vector)c2a).vector));
		}

	}

	//
	private LongArray vector;

	//
	private LispS64Vector() {
		// do nothing
	}

	/**
	 * 
	 * @param bs
	 */
	public LispS64Vector(long... bs) {
		vector = LongArray.newArray(bs);
	}

	/**
	 * 
	 * @param size
	 * @return
	 */
	public static LispS64Vector malloc(int size) {
		LispS64Vector res;

		res = new LispS64Vector();
		res.vector = LongArray.malloc(size);
		return res;
	}

	//
	private static boolean checkRange(Datum d) {
		if(!(d instanceof LispReal)) {
			return false;
		} else {
			return ((LispReal)d).inLongRange();
		}
	}

	//
	private static long[] toLongList(Datum body, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		LongVector v = new LongArrayVector();

		while(itr.hasNext()) {
			Datum d = itr.next();

			if(!checkRange(d)) {
				throw mesg.getError("err.uvector.outofrange.s64", d);
			}
			v.add(d.getLong());
		}

		if(!itr.getTerminal().equals(Nil.NIL)) {
			throw mesg.getError("err.list", body);
		}
		return v.toLongArray();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#print()
	 */
	@Override
	public String print() {
		StringBuilder b = new StringBuilder();
		String d = "";

		b.append("#s64(");
		for(int i = 0; i < vector.size(); i++) {
			b.append(d);
			b.append((int)vector.getLong(i));
			d = " ";
		}
		b.append(")");
		return b.toString();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#getResult()
	 */
	@Override
	public String getResult() {
		return print();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#toLispString()
	 */
	@Override
	public LispString toLispString() {
		return new LispString(print());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.HomogeneousArray#equalsArray(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	public boolean equalsArray(Datum d1, Datum d2) {
		if(!(d1 instanceof LispS64Vector)) {
			return false;
		} else if(!(d2 instanceof LispS64Vector)) {
			return false;
		}
		return ((LispS64Vector)d1).vector.equals(
				((LispS64Vector)d2).vector);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.HomogeneousArray#get(int)
	 */
	public LispInteger get(int index) {
		return LispInteger.valueOf(vector.getLong(index));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.HomogeneousArray#set(int, net.morilib.lisp.LispReal)
	 */
	public void set(int index, LispReal x) {
		vector.setLong(index, x.castLong());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.HomogeneousArray#size()
	 */
	public int size() {
		return vector.size();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.HomogeneousArray#checkRange(net.morilib.lisp.LispReal, net.morilib.lisp.LispMessage)
	 */
	public void checkRange(LispReal x, LispMessage mesg) {
		if(!checkRange(x)) {
			throw mesg.getError(
					"err.uvector.outofrange.s64", x);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.HomogeneousArray#set(int, int)
	 */
	public void set(int index, int x) {
		vector.setInt(index, x);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.HomogeneousArray#set(int, long)
	 */
	public void set(int index, long x) {
		vector.setLong(index, x);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.HomogeneousArray#set(int, double)
	 */
	public void set(int index, double x) {
		vector.setDouble(index, x);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.accessor.ILispRef#ref(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum ref(Datum arg, LispMessage mesg) {
		int k = SubrUtils.getSmallInt(arg, mesg);

		if(k < 0 || k >= size()) {
			throw mesg.getError("err.accessor.ref.outofrange", arg);
		}
		return get(k);
	}

}
