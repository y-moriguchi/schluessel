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
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/23
 */
public class LispArrayT1 extends SRFI47Array
implements java.io.Serializable {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/23
	 */
	public static class At1 extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d = Iterators.nextIf(itr);

			SubrUtils.checkTerminated(itr, body, mesg);
			return new Prototype(d != null && d.isTrue());
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
		private boolean bool;

		/**
		 * 
		 */
		public Prototype(boolean bool) {
			this.bool = bool;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.array.LispArrayPrototype#makeArray(int[])
		 */
		public ILispArray makeArray(int... is) {
			return new LispArrayT1(bool, is);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<at1 prototype>");
		}

	}

	//
	private int[] bool;

	/**
	 * 
	 * @param is
	 */
	public LispArrayT1(boolean r, int... is) {
		super(is);
		int l;

		l        = LispArrayShape.arraylength(is);
		bool     = new int[(l + 31) >>> 5];
		Arrays.fill(bool, r ? 0xffffffff : 0);
		if((l & 0x1f) != 0) {
			bool[((l + 31) >>> 5) - 1] &= ~(0xffffffff << (l & 0x1f));
		}
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
			r[i] = LispBoolean.getInstance(
					(bool[i >>> 5] & (i & 0x1f)) != 0);
		}
		return new LispVector(r);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getFromArray(int[])
	 */
	public Datum getFromArray(int... is) {
		int i = LispArrayShape.arrayindex(eIndices, is);

		return LispBoolean.getInstance(
				(bool[i >>> 5] & (1 << (i & 0x1f))) != 0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#setToArray(net.morilib.lisp.Datum, int[])
	 */
	public void setToArray(Datum d, int... is) {
		int i = LispArrayShape.arrayindex(eIndices, is);

		if(d.isTrue()) {
			bool[i >>> 5] |= 1 << (i & 0x1f);
		} else {
			bool[i >>> 5] &= ~(1 << (i & 0x1f));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#isEqualTo(net.morilib.lisp.array.LispArray)
	 */
	public boolean isEqualTo(ILispArray a) {
		if(!isIndexEqualTo(a)) {
			return false;
		} else if(a instanceof LispArrayT1) {
			return Arrays.equals(bool, ((LispArrayT1)a).bool);
		} else {
			return LispDefaultArray.isEqualTo(this, a);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getTypeSpecifier()
	 */
	public String getTypeSpecifier() {
		return "bool";
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#fill(java.util.Iterator)
	 */
	public void fill(Iterator<Datum> itr) {
		int l = LispArrayShape.arraylength(eIndices);

		for(int i = 0; i < l && itr.hasNext(); i++) {
			Datum x = itr.next();

			if(x.isTrue()) {
				bool[i >>> 5] |= (1 << (i & 0x1f));
			} else {
				bool[i >>> 5] &= ~(1 << (i & 0x1f));
			}
		}
	}

}
