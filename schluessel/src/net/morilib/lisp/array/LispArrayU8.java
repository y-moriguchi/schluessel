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
package net.morilib.lisp.array;

import java.util.Arrays;
import java.util.Iterator;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Bytes;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/23
 */
public class LispArrayU8 extends SRFI47Array
implements java.io.Serializable {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/23
	 */
	public static class Au8 extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d = Iterators.nextIf(itr);

			SubrUtils.checkTerminated(itr, body, mesg);
			if(d == null || d instanceof LispReal) {
				return new Prototype((d == null) ?
						0 : (byte)d.getInt());
			} else {
				throw mesg.getError("err.require.real", d);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/23
	 */
	public static class Prototype extends Datum2
	implements LispArrayPrototype {

		//
		private byte real;

		/**
		 * 
		 */
		public Prototype(byte real) {
			this.real = real;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.array.LispArrayPrototype#makeArray(int[])
		 */
		public ILispArray makeArray(int... is) {
			return new LispArrayU8(real, is);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<au8 prototype>");
		}

	}

	//
	private byte[] real;

	/**
	 * 
	 * @param is
	 */
	public LispArrayU8(byte r, int... is) {
		super(is);
		real     = new byte[LispArrayShape.arraylength(is)];
		Arrays.fill(real, r);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#rank()
	 */
	public int rank() {
		return eIndices.length;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#startIndex(int)
	 */
	public int startIndex(int dim) {
		if(dim < 0 || dim >= eIndices.length) {
			throw new IndexOutOfBoundsException();
		}
		return 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#endIndex(int)
	 */
	public int endIndex(int dim) {
		return eIndices[dim];
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#toVector()
	 */
	public LispVector toVector() {
		Datum[] r = new Datum[LispArrayShape.arraylength(eIndices)];

		for(int i = 0; i < r.length; i++) {
			r[i] = LispInteger.valueOf(Bytes.ubyteToInt(real[i]));
		}
		return new LispVector(r);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getFromArray(int[])
	 */
	public Datum getFromArray(int... is) {
		return LispInteger.valueOf(Bytes.ubyteToInt(
				real[LispArrayShape.arrayindex(eIndices, is)]));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#setToArray(net.morilib.lisp.Datum, int[])
	 */
	public void setToArray(Datum d, int... is) {
		if(!(d instanceof LispReal)) {
			throw new ClassCastException();
		} else if(!((LispReal)d).inUnsignedByteRange()) {
			throw new ValueOutOfBoundsException(d.toString());
		} else {
			real[LispArrayShape.arrayindex(
					eIndices, is)] = (byte)d.getInt();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#isEqualTo(net.morilib.lisp.array.LispArray)
	 */
	public boolean isEqualTo(ILispArray a) {
		if(!isIndexEqualTo(a)) {
			return false;
		} else if(a instanceof LispArrayU8) {
			return Arrays.equals(real, ((LispArrayU8)a).real);
		} else {
			return LispDefaultArray.isEqualTo(this, a);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getTypeSpecifier()
	 */
	public String getTypeSpecifier() {
		return "fixN8b";
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#fill(java.util.Iterator)
	 */
	public void fill(Iterator<Datum> itr) {
		for(int i = 0; i < real.length && itr.hasNext(); i++) {
			Datum x = itr.next();

			if(!(x instanceof LispReal)) {
				throw new ClassCastException();
			} else if(!((LispReal)x).inUnsignedByteRange()) {
				throw new ValueOutOfBoundsException(x.toString());
			} else {
				real[i] = (byte)x.getInt();
			}
		}
	}

}
