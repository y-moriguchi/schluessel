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

import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispNumber;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/21
 */
public class LUDecomposedLMatrix extends AbstractImmutableLispMatrix {

	//
	private ILispMatrix a;

	//
	/*package*/ LUDecomposedLMatrix(ILispMatrix a) {
		this.a = a;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberMatrix#get(int, int)
	 */
	public LispNumber get(int row, int column) {
		if(row > column) {
			return a.get(row, column);
		} else if(row == column) {
			return LispInteger.ONE;
		} else {
			return LispInteger.ZERO;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#rowSize()
	 */
	public int rowSize() {
		return a.rowSize();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#columnSize()
	 */
	public int columnSize() {
		return a.columnSize();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#determinant()
	 */
	public LispNumber determinant() {
		return LispInteger.ONE;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#invert()
	 */
	public ILispMatrix inv() {
		LispMatrix r = new LispMatrix(this);
		LispNumber x;

		for(int c = 1; c < columnSize(); c++) {
			for(int i = c; i < rowSize(); i++) {
				int j = i - c;

				x = LispInteger.ZERO;
				for(int k = j + 1; k <= i; k++) {
					x = x.add(r.get(i, k).mul(get(k, j)));
				}
				// x.uminus(),mul(r.get(i, i))
				r.set(i, j, x.uminus());
			}
		}
		return r;
	}

}
