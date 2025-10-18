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

import java.util.Iterator;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.LispVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/28
 */
public class LispRank0Array extends SRFI25Array
implements java.io.Serializable {

	//
	private Datum datum;

	//
	/*package*/ LispRank0Array(Datum d) {
		this.datum = d;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#rank()
	 */
	public int rank() {
		return 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#startIndex(int)
	 */
	public int startIndex(int dim) {
		throw new IndexOutOfBoundsException("" + dim);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#endIndex(int)
	 */
	public int endIndex(int dim) {
		throw new IndexOutOfBoundsException("" + dim);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#toVector()
	 */
	public LispVector toVector() {
		return new LispVector(datum);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getFromArray(int[])
	 */
	public Datum getFromArray(int... is) {
		if(is.length > 0) {
			throw new InvalidDimensionException();
		}
		return datum;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#setToArray(net.morilib.lisp.Datum, int[])
	 */
	public void setToArray(Datum d, int... is) {
		if(is.length > 0) {
			throw new InvalidDimensionException();
		}
		datum = d;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#isIndexEqualTo(net.morilib.lisp.array.LispArray)
	 */
	public boolean isIndexEqualTo(ILispArray a) {
		return a.rank() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#isEqualTo(net.morilib.lisp.array.LispArray)
	 */
	public boolean isEqualTo(ILispArray a) {
		if(a.rank() == 0) {
			return LispUtils.equals(datum, a.getFromArray());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.HeterogeniousLispArray#fill(net.morilib.lisp.ConsIterator)
	 */
	public void fill(Iterator<Datum> itr) {
		if(itr.hasNext()) {
			datum = itr.next();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getTypeSpecifier()
	 */
	public String getTypeSpecifier() {
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getShape()
	 */
	public LispArrayShape getShape() {
		return LispArrayShape.RANK0;
	}

}
