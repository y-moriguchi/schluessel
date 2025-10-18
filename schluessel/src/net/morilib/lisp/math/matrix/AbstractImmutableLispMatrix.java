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
package net.morilib.lisp.math.matrix;

import net.morilib.lang.Hashes;
import net.morilib.lisp.Datum;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/21
 */
public abstract class AbstractImmutableLispMatrix
extends AbstractLispMatrix implements ILispMatrix {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#set(int, int, net.morilib.lisp.Datum)
	 */
	public final void set(int row, int column,
			Datum x) throws LispMatrixException {
		throw new LispMatrixException();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		for(int i = 0; i < rowSize(); i++) {
			for(int j = 0; j < columnSize(); j++) {
				r = Hashes.A * (r + get(i, j).hashCode());
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof AbstractImmutableLispMatrix) {
			ILispMatrix y = (ILispMatrix)o;

			if(rowSize() != y.rowSize() ||
					columnSize() != y.columnSize()) {
				return false;
			} else {
				for(int i = 0; i < rowSize(); i++) {
					for(int j = 0; j < columnSize(); j++) {
						if(!get(i, j).equals(y.get(i, j))) {
							return false;
						}
					}
				}
				return true;
			}
		}
		return false;
	}

}
