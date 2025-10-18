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

import java.util.Arrays;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispNumber;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/19
 */
public class LispMatrix extends AbstractLispArrayMatrix
implements java.io.Serializable {

	//
	private LispNumber[] array;
	private transient AbstractImmutableLispMatrix[] decomposed = null;

	/**
	 * 
	 * @param rows
	 * @param columns
	 */
	public LispMatrix(int rows, int columns) {
		this(rows, columns, LispInteger.ZERO);
	}

	/**
	 * 
	 * @param rows
	 * @param columns
	 * @param fill
	 */
	public LispMatrix(int rows, int columns, LispNumber fill) {
		this.array = new LispNumber[rows * columns];
		this.rows  = rows;
		this.columns = columns;
		Arrays.fill(this.array, fill);
	}

	/**
	 * 
	 * @param m
	 */
	public LispMatrix(LispMatrix m) {
		array   = new LispNumber[m.rows * m.columns];
		rows    = m.rows;
		columns = m.columns;
		System.arraycopy(m.array, 0, array, 0, array.length);
	}

	/**
	 * 
	 * @param m
	 */
	public LispMatrix(ILispMatrix m) {
		array   = new LispNumber[m.rowSize() * m.columnSize()];
		rows    = m.rowSize();
		columns = m.columnSize();
		for(int i = 0; i < m.rowSize(); i++) {
			for(int j = 0; j < m.columnSize(); j++) {
				set(i, j, m.get(i, j));
			}
		}
	}

	/**
	 * 
	 * @param size
	 * @return
	 */
	public static LispMatrix newElementMatrix(int size) {
		LispMatrix r = new LispMatrix(size, size);

		for(int i = 0; i < size; i++) {
			r.set(i, i, LispInteger.ONE);
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.AbstractLispMatrix#array(int)
	 */
	@Override
	protected LispNumber array(int index) {
		return array[index];
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.AbstractLispMatrix#arrayset(int, net.morilib.lisp.Datum)
	 */
	@Override
	protected void arrayset(int index, Datum x) {
		array[index] = (LispNumber)x;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.AbstractLispMatrix#prototype()
	 */
	@Override
	protected LispMatrix prototype() {
		return new LispMatrix(rows, columns);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.AbstractLispMatrix#prototype(int, int)
	 */
	@Override
	protected LispMatrix prototype(int rows, int columns) {
		return new LispMatrix(rows, columns);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.AbstractLispMatrix#clone()
	 */
	@Override
	public LispMatrix clone() {
		return new LispMatrix(this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.AbstractLispMatrix#arraycopy(int, java.lang.Object, int, int)
	 */
	@Override
	protected void arraycopy(int srcPos, Object dest, int destPos,
			int len) {
		System.arraycopy(array, srcPos, dest, destPos, len);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#determinant()
	 */
	public LispNumber determinant() throws LispMatrixException {
		if(columns != rows) {
			throw new LispMatrixException();
		} else {
			if(decomposed == null) {
				decomposed = LispMatrices.decomposeLU(this);
			}
			// det(P) * det(U)
			return decomposed[0].determinant().mul(
					decomposed[2].determinant());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#invert()
	 */
	public ILispMatrix inv() throws LispMatrixException {
		ILispMatrix x, y;

		if(columns != rows) {
			throw new LispMatrixException();
		} else {
			if(decomposed == null) {
				decomposed = LispMatrices.decomposeLU(this);
			}
			x = decomposed[2].inv();
			y = decomposed[1].inv();
			x = x.mul(y);
			y = decomposed[0].inv();
			x = x.mul(y);
			return x;
		}
	}

}
