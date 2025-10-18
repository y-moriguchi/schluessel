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
import net.morilib.util.Bytes;
import net.morilib.util.primitive.ShortArrayVector;
import net.morilib.util.primitive.ShortVector;
import net.morilib.util.uvector.ShortArray;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/06
 */
public class LispU16Vector extends Atom
implements HomogeneousUnsignedArray, java.io.Serializable {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class U16Vector extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			return new LispU16Vector(toShortList(body, mesg));
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class IsU16Vector extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(
					c1a instanceof LispU16Vector);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class MakeU16Vector extends Subr {

		//
		private short[] makeVector(int len, int d) {
			ShortVector b = new ShortArrayVector();

			for(int i = 0; i < len; i++) {
				b.add((short)d);
			}
			return b.toShortArray();
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
				return new LispU16Vector(makeVector(t, 0));
			} else if(lst.size() == 2) {
				int t = SubrUtils.getSmallInt(
						lst.get(0), mesg);

				if(t < 0) {
					throw mesg.getError(
							"err.require.int.nonnegative", lst.get(0));
				} else if(!checkRange(lst.get(1))) {
					throw mesg.getError(
							"err.uvector.outofrange.u16", lst.get(1));
				}
				return new LispU16Vector(makeVector(
						t, lst.get(1).getInt()));
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
	public static class U16VectorToList extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispU16Vector) {
				ConsListBuilder b = new ConsListBuilder();
				LispU16Vector v = (LispU16Vector)c1a;

				for(int i = 0; i < v.vector.size(); i++) {
					b.append(LispInteger.valueOf(
							Bytes.ushortToInt(v.vector.getShort(i))));
				}
				return b.get();
			} else {
				throw mesg.getError("err.uvector.outofrange.u16", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class ListToU16Vector extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof Cons || c1a.equals(Nil.NIL)) {
				return new LispU16Vector(toShortList(c1a, mesg));
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
	public static class EqualU16Vector extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(!(c1a instanceof LispU16Vector)) {
				throw mesg.getError("err.uvector.require.u16", c1a);
			} else if(!(c2a instanceof LispU16Vector)) {
				throw mesg.getError("err.uvector.require.u16", c2a);
			}
			return LispBoolean.getInstance(
					((LispU16Vector)c1a).vector.equals(
							((LispU16Vector)c2a).vector));
		}

	}

	//
	private ShortArray vector;

	//
	private LispU16Vector() {
		// do nothing
	}

	/**
	 * 
	 * @param bs
	 */
	public LispU16Vector(short... bs) {
		vector = ShortArray.newArray(bs);
	}

	/**
	 * 
	 * @param size
	 * @return
	 */
	public static LispU16Vector malloc(int size) {
		LispU16Vector res;

		res = new LispU16Vector();
		res.vector = ShortArray.malloc(size);
		return res;
	}

	//
	private static boolean checkRange(Datum d) {
		if(!(d instanceof LispReal)) {
			return false;
		} else {
			return ((LispReal)d).inUnsignedShortRange();
		}
	}

	//
	private static short[] toShortList(Datum body, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		ShortVector v = new ShortArrayVector();

		while(itr.hasNext()) {
			Datum d = itr.next();

			if(!checkRange(d)) {
				throw mesg.getError("err.uvector.outofrange.u16", d);
			}
			v.add((short)d.getInt());
		}

		if(!itr.getTerminal().equals(Nil.NIL)) {
			throw mesg.getError("err.list", body);
		}
		return v.toShortArray();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#print()
	 */
	@Override
	public String print() {
		StringBuilder b = new StringBuilder();
		String d = "";

		b.append("#u16(");
		for(int i = 0; i < vector.size(); i++) {
			b.append(d);
			b.append(Bytes.ushortToInt(vector.getShort(i)));
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
		if(!(d1 instanceof LispU16Vector)) {
			return false;
		} else if(!(d2 instanceof LispU16Vector)) {
			return false;
		}
		return ((LispU16Vector)d1).vector.equals(
				((LispU16Vector)d2).vector);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.HomogeneousArray#get(int)
	 */
	public LispReal get(int index) {
		return LispInteger.valueOf(
				Bytes.ushortToInt(vector.getShort(index)));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.HomogeneousArray#set(int, net.morilib.lisp.LispReal)
	 */
	public void set(int index, LispReal x) {
		vector.setShort(index, (short)x.getInt());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.HomogeneousArray#size()
	 */
	public int size() {
		return vector.size();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.HomogeneousArray#checkRange(net.morilib.lisp.LispReal)
	 */
	public void checkRange(LispReal x, LispMessage mesg) {
		if(!checkRange(x)) {
			throw mesg.getError(
					"err.uvector.outofrange.u16", x);
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
