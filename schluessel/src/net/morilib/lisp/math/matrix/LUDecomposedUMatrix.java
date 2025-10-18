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
public class LUDecomposedUMatrix extends AbstractImmutableLispMatrix {

	//
	private ILispMatrix a;

	//
	/*package*/ LUDecomposedUMatrix(ILispMatrix a) {
		this.a = a;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberMatrix#get(int, int)
	 */
	public LispNumber get(int row, int column) {
		return (row > column) ? LispInteger.ZERO : a.get(row, column);
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
		LispNumber r = LispInteger.ONE;

		for(int i = 0; i < a.rowSize(); i++) {
			r = r.mul(a.get(i, i));
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#invert()
	 */
	public ILispMatrix inv() throws LispMatrixException {
		LispMatrix r = new LispMatrix(this);
		LispNumber x;

		if(determinant().isZero()) {
			throw new LispMatrixException();
		}

		for(int i = 0; i < columnSize(); i++) {
			r.set(i, i, LispInteger.ONE.div(r.get(i, i)));
		}
		for(int c = 1; c < rowSize(); c++) {
			for(int i = c; i < columnSize(); i++) {
				int j = i - c;

				x = LispInteger.ZERO;
				for(int k = j + 1; k <= i; k++) {
					LispNumber c1, c2;

					c1 = get(j, k);
					c2 = r.get(k, i);
					x  = x.add(c1.mul(c2));
				}
				r.set(j, i, x.mul(r.get(j, j)).uminus());
			}
		}
		return r;
	}

}
