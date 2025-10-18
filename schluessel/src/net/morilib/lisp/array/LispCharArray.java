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

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.LispVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/23
 */
public class LispCharArray extends SRFI47Array
implements java.io.Serializable {

	//
	private char[] chr;

	/**
	 * 
	 * @param is
	 */
	public LispCharArray(char r, int... is) {
		super(is);
		chr      = new char[LispArrayShape.arraylength(is)];
		Arrays.fill(chr, r);
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
			r[i] = LispCharacter.valueOf(chr[i]);
		}
		return new LispVector(r);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getFromArray(int[])
	 */
	public Datum getFromArray(int... is) {
		return LispCharacter.valueOf(chr[LispArrayShape.arrayindex(
				eIndices, is)]);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#setToArray(net.morilib.lisp.Datum, int[])
	 */
	public void setToArray(Datum d, int... is) {
		if(!(d instanceof LispCharacter)) {
			throw new ClassCastException();
		} else {
			chr[LispArrayShape.arrayindex(
					eIndices, is)] = d.getCharacter();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#isEqualTo(net.morilib.lisp.array.LispArray)
	 */
	public boolean isEqualTo(ILispArray a) {
		if(!isIndexEqualTo(a)) {
			return false;
		} else if(a instanceof LispCharArray) {
			return Arrays.equals(chr, ((LispCharArray)a).chr);
		} else {
			return LispDefaultArray.isEqualTo(this, a);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getTypeSpecifier()
	 */
	public String getTypeSpecifier() {
		return "char";
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#fill(java.util.Iterator)
	 */
	public void fill(Iterator<Datum> itr) {
		for(int i = 0; i < chr.length && itr.hasNext(); i++) {
			Datum x = itr.next();

			if(x instanceof LispCharacter) {
				chr[i] = x.getCharacter();
			} else {
				throw new ClassCastException();
			}
		}
	}

}
