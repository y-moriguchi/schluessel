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
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.Bytes;
import net.morilib.util.primitive.ByteArrayVector;
import net.morilib.util.primitive.ByteVector;
import net.morilib.util.uvector.ByteArray;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/06
 */
public class LispU8Vector extends Atom
implements HomogeneousUnsignedArray, ILispBytevector,
java.io.Serializable {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class U8Vector extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			return new LispU8Vector(toByteList(body, mesg));
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class IsU8Vector extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(
					c1a instanceof LispU8Vector);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class MakeU8Vector extends Subr {

		//
		private byte[] makeVector(int len, int d) {
			ByteVector b = new ByteArrayVector();

			for(int i = 0; i < len; i++) {
				b.add((byte)d);
			}
			return b.toByteArray();
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
				return new LispU8Vector(makeVector(t, 0));
			} else if(lst.size() == 2) {
				int t = SubrUtils.getSmallInt(
						lst.get(0), mesg);

				if(t < 0) {
					throw mesg.getError(
							"err.require.int.nonnegative", lst.get(0));
				} else if(!checkRange(lst.get(1))) {
					throw mesg.getError(
							"err.uvector.outofrange.u8", lst.get(1));
				}
				return new LispU8Vector(makeVector(
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
	public static class U8VectorToList extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispU8Vector) {
				ConsListBuilder b = new ConsListBuilder();
				LispU8Vector v = (LispU8Vector)c1a;

				for(int i = 0; i < v.vector.size(); i++) {
					b.append(LispInteger.valueOf(
							Bytes.ubyteToInt(v.vector.getByte(i))));
				}
				return b.get();
			} else {
				throw mesg.getError("err.uvector.outofrange.u8", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class ListToU8Vector extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof Cons || c1a.equals(Nil.NIL)) {
				return new LispU8Vector(toByteList(c1a, mesg));
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
	public static class EqualU8Vector extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(!(c1a instanceof LispU8Vector)) {
				throw mesg.getError("err.uvector.require.u8", c1a);
			} else if(!(c2a instanceof LispU8Vector)) {
				throw mesg.getError("err.uvector.require.u8", c2a);
			}
			return LispBoolean.getInstance(
					((LispU8Vector)c1a).vector.equals(
							((LispU8Vector)c2a).vector));
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/24
	 */
	public static class VectorCompare extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			LispU8Vector a, b;

			if(!(c1a instanceof LispU8Vector)) {
				throw mesg.getError("err.uvector.require.u8", c1a);
			} else if(!(c2a instanceof LispU8Vector)) {
				throw mesg.getError("err.uvector.require.u8", c2a);
			}
			a = (LispU8Vector)c1a;
			b = (LispU8Vector)c2a;
			if(a.size() == b.size()) {
				for(int i = 0; i < a.size(); i++) {
					if(a.get(i).compareTo(b.get(i)) < 0) {
						return LispInteger.valueOf(-1);
					} else if(a.get(i).compareTo(b.get(i)) > 0) {
						return LispInteger.valueOf(1);
					}
				}
				return LispInteger.ZERO;
			} else if(a.size() < b.size()) {
				return LispInteger.valueOf(-1);
			} else {
				return LispInteger.valueOf(1);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/24
	 */
	public static class U8VectorCopyS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum s0 = SubrUtils.nextIf(itr, mesg, body);
			int   ss = SubrUtils.nextSmallInt(itr, mesg, body);
			Datum d0 = SubrUtils.nextIf(itr, mesg, body);
			int   sd = SubrUtils.nextSmallInt(itr, mesg, body);
			int   l  = SubrUtils.nextSmallInt(itr, mesg, body);
			LispU8Vector s, d;

			SubrUtils.checkTerminated(itr, body, mesg);
			if(!(s0 instanceof LispU8Vector)) {
				throw mesg.getError("err.uvector.require.u8", s0);
			} else if(!(d0 instanceof LispU8Vector)) {
				throw mesg.getError("err.uvector.require.u8", d0);
			}
			s = (LispU8Vector)s0;
			d = (LispU8Vector)d0;

			if(ss < 0 || ss > s.size()) {
				throw mesg.getError("err.vector.outofrange",
						LispInteger.valueOf(ss));
			} else if(sd < 0 || sd >= d.size()) {
				throw mesg.getError("err.vector.outofrange",
						LispInteger.valueOf(sd));
			} else if(l < 0 ||
					l + ss > s.size() ||
					l + sd > d.size()) {
				throw mesg.getError("err.vector.outofrange",
						LispInteger.valueOf(l));
			}
			d.arraycopy(sd, s, ss, l);
			return Undef.UNDEF;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/24
	 */
	public static class U8VectorCopy extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof LispU8Vector) {
				return ((LispU8Vector)c1a).duplicate();
			} else {
				throw mesg.getError("err.uvector.outofrange.u8", c1a);
			}
		}

	}

	//
	private ByteArray vector;

	//
	private LispU8Vector() {
		// do nothing
	}

	/**
	 * 
	 * @param bs
	 */
	public LispU8Vector(byte... bs) {
		vector = ByteArray.newArray(bs);
	}

	/**
	 * 
	 * @param bs
	 */
	public LispU8Vector(ByteArray bs) {
		vector = new ByteArray(bs);
	}

	/**
	 * 
	 * @param size
	 * @return
	 */
	public static LispU8Vector malloc(int size) {
		LispU8Vector res;

		res = new LispU8Vector();
		res.vector = ByteArray.malloc(size);
		return res;
	}

	//
	private static boolean checkRange(Datum d) {
		if(!(d instanceof LispReal)) {
			return false;
		} else {
			return ((LispReal)d).inUnsignedByteRange();
		}
	}

	//
	private static byte[] toByteList(Datum body, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		ByteVector v = new ByteArrayVector();

		while(itr.hasNext()) {
			Datum d = itr.next();

			if(!checkRange(d)) {
				throw mesg.getError("err.uvector.outofrange.u8", d);
			}
			v.add((byte)d.getInt());
		}

		if(!itr.getTerminal().equals(Nil.NIL)) {
			throw mesg.getError("err.list", body);
		}
		return v.toByteArray();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#print()
	 */
	@Override
	public String print() {
		StringBuilder b = new StringBuilder();
		String d = "";

		b.append("#u8(");
		for(int i = 0; i < vector.size(); i++) {
			b.append(d);
			b.append(Bytes.ubyteToInt(vector.getByte(i)));
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
		if(!(d1 instanceof LispU8Vector)) {
			return false;
		} else if(!(d2 instanceof LispU8Vector)) {
			return false;
		}
		return ((LispU8Vector)d1).vector.equals(
				((LispU8Vector)d2).vector);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.HomogeneousArray#get(int)
	 */
	public LispReal get(int index) {
		return LispInteger.valueOf(
				Bytes.ubyteToInt(vector.getByte(index)));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.HomogeneousArray#set(int, net.morilib.lisp.LispReal)
	 */
	public void set(int index, LispReal x) {
		vector.setByte(index, (byte)x.getInt());
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
					"err.uvector.outofrange.u8", x);
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

	/**
	 * @param sd
	 * @param s
	 * @param ss
	 * @param l
	 */
	public void arraycopy(int sd, LispU8Vector s, int ss, int l) {
		vector.arraycopy(sd, s.vector, ss, l);
	}

	/**
	 * @return
	 */
	public Datum duplicate() {
		return new LispU8Vector(vector);
	}

	/**
	 * 
	 * @return
	 */
	public byte[] toArray() {
		return vector.toByteArray();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.ILispByteVector#toBytes()
	 */
	public byte[] toBytes() {
		return toArray();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.ILispBytevector#setBytes(byte[])
	 */
	public void setBytes(byte[] b) {
		for(int i = 0; i < vector.size() && i < b.length; i++) {
			vector.setByte(i, b[i]);
		}
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
