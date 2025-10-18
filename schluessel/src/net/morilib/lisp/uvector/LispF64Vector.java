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
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.primitive.DoubleArrayVector;
import net.morilib.util.primitive.DoubleVector;
import net.morilib.util.uvector.DoubleArray;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/06
 */
public class LispF64Vector extends Atom
implements HomogeneousFloatArray, java.io.Serializable {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class F64Vector extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			return new LispF64Vector(toDoubleList(body, mesg));
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class IsF64Vector extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(
					c1a instanceof LispF64Vector);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class MakeF64Vector extends Subr {

		//
		private double[] makeVector(int len, double d) {
			DoubleVector b = new DoubleArrayVector();

			for(int i = 0; i < len; i++) {
				b.add((double)d);
			}
			return b.toDoubleArray();
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
				return new LispF64Vector(makeVector(t, 0.0));
			} else if(lst.size() == 2) {
				int t = SubrUtils.getSmallInt(
						lst.get(0), mesg);

				if(t < 0) {
					throw mesg.getError(
							"err.require.int.nonnegative", lst.get(0));
				} else if(!checkRange(lst.get(1))) {
					throw mesg.getError(
							"err.uvector.outofrange.f64", lst.get(1));
				}
				return new LispF64Vector(
						makeVector(t, lst.get(1).getRealDouble()));
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
	public static class F64VectorToList extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispF64Vector) {
				ConsListBuilder b = new ConsListBuilder();
				LispF64Vector v = (LispF64Vector)c1a;

				for(int i = 0; i < v.vector.size(); i++) {
					b.append(new LispDouble(
							v.vector.getDouble(i)));
				}
				return b.get();
			} else {
				throw mesg.getError("err.uvector.outofrange.f64", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/06
	 */
	public static class ListToF64Vector extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof Cons || c1a.equals(Nil.NIL)) {
				return new LispF64Vector(toDoubleList(c1a, mesg));
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
	public static class EqualF64Vector extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(!(c1a instanceof LispF32Vector)) {
				throw mesg.getError("err.uvector.require.f64", c1a);
			} else if(!(c2a instanceof LispF32Vector)) {
				throw mesg.getError("err.uvector.require.f64", c2a);
			}
			return LispBoolean.getInstance(
					((LispF64Vector)c1a).vector.equals(
							((LispF64Vector)c2a).vector));
		}

	}

	//
	private DoubleArray vector;

	//
	private LispF64Vector() {
		// do nothing
	}

	/**
	 * 
	 * @param bs
	 */
	public LispF64Vector(double... bs) {
		vector = DoubleArray.newArray(bs);
	}

	/**
	 * 
	 * @param size
	 * @return
	 */
	public static LispF64Vector malloc(int size) {
		LispF64Vector res;

		res = new LispF64Vector();
		res.vector = DoubleArray.malloc(size);
		return res;
	}

	//
	private static boolean checkRange(Datum d) {
		return (d instanceof LispReal);
	}

	//
	private static double[] toDoubleList(
			Datum body, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		DoubleVector v = new DoubleArrayVector();

		while(itr.hasNext()) {
			Datum d = itr.next();

			if(!checkRange(d)) {
				throw mesg.getError("err.uvector.outofrange.f64", d);
			}
			v.add((double)d.getRealDouble());
		}

		if(!itr.getTerminal().equals(Nil.NIL)) {
			throw mesg.getError("err.list", body);
		}
		return v.toDoubleArray();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#print()
	 */
	@Override
	public String print() {
		StringBuilder b = new StringBuilder();
		String d = "";

		b.append("#f64(");
		for(int i = 0; i < vector.size(); i++) {
			b.append(d);
			b.append(vector.getDouble(i));
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
		if(!(d1 instanceof LispF64Vector)) {
			return false;
		} else if(!(d2 instanceof LispF64Vector)) {
			return false;
		}
		return ((LispF64Vector)d1).vector.equals(
				((LispF64Vector)d2).vector);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.HomogeneousArray#get(int)
	 */
	public LispDouble get(int index) {
		return new LispDouble(vector.getDouble(index));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.uvector.HomogeneousArray#set(int, net.morilib.lisp.LispReal)
	 */
	public void set(int index, LispReal x) {
		vector.setDouble(index, x.getRealDouble());
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
					"err.uvector.outofrange.f64", x);
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
