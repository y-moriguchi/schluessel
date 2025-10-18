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

import net.morilib.lisp.Atom;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/26
 */
public abstract class SRFI47Array extends Atom implements ILispArray {

	/**
	 * 
	 */
	protected int[] eIndices;

	/**
	 * 
	 * @param eIndices
	 */
	protected SRFI47Array(int[] is) {
		eIndices = new int[is.length];
		System.arraycopy(is, 0, eIndices, 0, is.length);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#isIndexEqualTo(net.morilib.lisp.array.LispArray)
	 */
	public boolean isIndexEqualTo(ILispArray a) {
		if(a instanceof SRFI47Array) {
			return Arrays.equals(eIndices, ((SRFI47Array)a).eIndices);
		} else if(rank() == a.rank()) {
			for(int i = 0; i < rank(); i++) {
				if(startIndex(i) != a.startIndex(i)) {
					return false;
				} else if(endIndex(i) != a.endIndex(i)) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#print()
	 */
	@Override
	public String print() {
		return LispUtils.print(this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#getResult()
	 */
	@Override
	public String getResult() {
		return LispUtils.getResult(this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Atom#toLispString()
	 */
	@Override
	public LispString toLispString() {
		return new LispString(LispUtils.print(this));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.array.LispArray#getShape()
	 */
	public LispArrayShape getShape() {
		return new LispArrayShape(eIndices);
	}

}
