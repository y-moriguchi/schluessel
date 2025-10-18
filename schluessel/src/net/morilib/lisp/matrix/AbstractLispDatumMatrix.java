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
package net.morilib.lisp.matrix;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.ILispVector;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.math.LispPermutation;
import net.morilib.lisp.math.matrix.LispMatrices;
import net.morilib.lisp.math.matrix.LispMatrixException;
import net.morilib.lisp.uvector.HomogeneousUnsignedArray;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/19
 */
public abstract class AbstractLispDatumMatrix extends Datum2
implements ILispDatumMatrix, ILispVector {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.ILispVector#get(int)
	 */
	public Datum get(int index) {
		if(index < 0 || index >= size()) {
			throw new IndexOutOfBoundsException();
		}
		return get(index / columnSize(), index % columnSize());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.ILispVector#size()
	 */
	public int size() {
		return rowSize() * columnSize();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#getRowVector(int)
	 */
	public ILispVector getRowVector(int row) {
		Datum[] r = new Datum[columnSize()];

		if(row < 0 || row >= rowSize()) {
			throw new IndexOutOfBoundsException();
		} else {
			for(int i = 0; i < columnSize(); i++) {
				r[i] = get(row, i);
			}
			return new LispVector(r);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#getColumnVector(int)
	 */
	public ILispVector getColumnVector(int column) {
		Datum[] ra = new Datum[columnSize()];

		if(column < 0 || column >= columnSize()) {
			throw new IndexOutOfBoundsException();
		} else {
			for(int i = 0; i < rowSize(); i++) {
				ra[i] = get(i, column);
			}
			return new LispVector(ra);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#transpose()
	 */
	public ILispDatumMatrix transpose() {
		LispDatumMatrix r = new LispDatumMatrix(this);

		for(int i = 0; i < rowSize(); i++) {
			for(int j = 0; j < columnSize(); j++) {
				r.set(j, i, get(i, j));
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispDatumMatrix#changeRow(net.morilib.lisp.math.LispPermutation)
	 */
	public ILispDatumMatrix changeRow(final LispPermutation rowp) {
		if(rowp.size() != rowSize()) {
			throw new IllegalArgumentException();
		}
		return new AbstractLispDatumMatrix() {

			public Datum get(int row, int column) {
				return AbstractLispDatumMatrix.this.get(
						rowp.get(row), column);
			}

			public void set(int row, int column, Datum x)
					throws LispMatrixException {
				AbstractLispDatumMatrix.this.set(
						rowp.get(row), column, x);
			}

			public int rowSize() {
				return AbstractLispDatumMatrix.this.rowSize();
			}

			public int columnSize() {
				return AbstractLispDatumMatrix.this.columnSize();
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispDatumMatrix#changeColumn(net.morilib.lisp.math.LispPermutation)
	 */
	public ILispDatumMatrix changeColumn(final LispPermutation colp) {
		if(colp.size() != columnSize()) {
			throw new IllegalArgumentException();
		}
		return new AbstractLispDatumMatrix() {

			public Datum get(int row, int column) {
				return AbstractLispDatumMatrix.this.get(
						row, colp.get(column));
			}

			public void set(int row, int column, Datum x)
					throws LispMatrixException {
				AbstractLispDatumMatrix.this.set(
						row, colp.get(column), x);
			}

			public int rowSize() {
				return AbstractLispDatumMatrix.this.rowSize();
			}

			public int columnSize() {
				return AbstractLispDatumMatrix.this.columnSize();
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispDatumMatrix#submatrix(int, int, int, int)
	 */
	public ILispDatumMatrix submatrix(
			final int rowb, final int rowe,
			final int colb, final int cole) {
		if(rowb < 0 || rowb >= rowSize()) {
			throw new IndexOutOfBoundsException();
		} else if(rowe < 0 || rowe >= rowSize()) {
			throw new IndexOutOfBoundsException();
		} else if(colb < 0 || colb >= columnSize()) {
			throw new IndexOutOfBoundsException();
		} else if(cole < 0 || cole >= columnSize()) {
			throw new IndexOutOfBoundsException();
		} else if(rowe <= rowb) {
			throw new IllegalArgumentException();
		} else if(cole <= colb) {
			throw new IllegalArgumentException();
		}

		return new AbstractLispDatumMatrix() {

			public Datum get(int row, int column) {
				if(row < 0 || row >= rowe - rowb) {
					throw new IndexOutOfBoundsException();
				} else if(column < 0 || column >= cole - colb) {
					throw new IndexOutOfBoundsException();
				}
				return AbstractLispDatumMatrix.this.get(
						row + rowb, column + colb);
			}

			public void set(int row, int column,
					Datum x) throws LispMatrixException {
				if(row < 0 || row >= rowe - rowb) {
					throw new IndexOutOfBoundsException();
				} else if(column < 0 || column >= cole - colb) {
					throw new IndexOutOfBoundsException();
				}
				AbstractLispDatumMatrix.this.set(
						row + rowb, column + colb, x);
			}

			public int rowSize() {
				return rowe - rowb;
			}

			public int columnSize() {
				return cole - colb;
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispDatumMatrix#submatrix(net.morilib.lisp.uvector.HomogeneousUnsignedArray, net.morilib.lisp.uvector.HomogeneousUnsignedArray)
	 */
	public ILispDatumMatrix submatrix(
			final HomogeneousUnsignedArray rowa,
			final HomogeneousUnsignedArray cola
			) throws LispMatrixException {
		// check row indices
		for(int i = 0; i < rowa.size(); i++) {
			if(!(rowa.get(i) instanceof LispSmallInt)) {
				throw new LispMatrixException("err.require.smallint");
			} else if(rowa.get(i).getInt() >= rowSize()) {
				throw new LispMatrixException("err.range.invalid");
			}
		}

		// check column indices
		for(int i = 0; i < cola.size(); i++) {
			if(!(cola.get(i) instanceof LispSmallInt)) {
				throw new LispMatrixException("err.require.smallint");
			} else if(cola.get(i).getInt() >= columnSize()) {
				throw new LispMatrixException("err.range.invalid");
			}
		}

		return new AbstractLispDatumMatrix() {

			public Datum get(int row, int column) {
				return AbstractLispDatumMatrix.this.get(
						rowa.get(row).getInt(),
						cola.get(column).getInt());
			}

			public void set(int row, int column, Datum x)
					throws LispMatrixException {
				AbstractLispDatumMatrix.this.set(
						rowa.get(row).getInt(),
						cola.get(column).getInt(), x);				
			}

			public int rowSize() {
				return rowa.size();
			}

			public int columnSize() {
				return cola.size();
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispDatumMatrix#isSquare()
	 */
	public boolean isSquare() {
		return rowSize() == columnSize();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		LispMatrices.print(buf, this);
	}

}
