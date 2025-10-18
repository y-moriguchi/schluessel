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
public class LispPermutationMatrix extends AbstractImmutableLispMatrix {

	//
	private int[] order;
	private int   parity;

	//
	/*package*/ LispPermutationMatrix(int[] order, int parity) {
		this.order  = order;
		this.parity = parity;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberMatrix#get(int, int)
	 */
	public LispNumber get(int row, int column) {
		return (order[row] == column) ?
				LispInteger.ONE : LispInteger.ZERO;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#rowSize()
	 */
	public int rowSize() {
		return order.length;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#columnSize()
	 */
	public int columnSize() {
		return order.length;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#determinant()
	 */
	public LispNumber determinant() {
		return LispInteger.valueOf(((parity & 1) == 0) ? 1 : -1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#invert()
	 */
	public LispPermutationMatrix inv() {
		int[] no = new int[order.length];

		for(int i = 0; i < no.length; i++) {
			no[order[i]] = i;
		}
		return new LispPermutationMatrix(no, parity);
	}

}
