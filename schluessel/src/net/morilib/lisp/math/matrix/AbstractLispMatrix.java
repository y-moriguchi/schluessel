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

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.math.LispPermutation;
import net.morilib.lisp.uvector.HomogeneousUnsignedArray;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/21
 */
public abstract class AbstractLispMatrix extends Datum2
implements ILispMatrix {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#getRowVector(int)
	 */
	public ILispNumberVector getRowVector(int row) {
		LispNumber[] r = new LispNumber[columnSize()];

		if(row < 0 || row >= rowSize()) {
			throw new IndexOutOfBoundsException();
		} else {
			for(int i = 0; i < columnSize(); i++) {
				r[i] = get(row, i);
			}
			return new LispNumberVector(r);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#getColumnVector(int)
	 */
	public ILispNumberVector getColumnVector(int column) {
		LispNumber[] ra = new LispNumber[columnSize()];

		if(column < 0 || column >= columnSize()) {
			throw new IndexOutOfBoundsException();
		} else {
			for(int i = 0; i < rowSize(); i++) {
				ra[i] = get(i, column);
			}
			return new LispNumberVector(ra);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#transpose()
	 */
	public ILispMatrix transpose() {
		LispMatrix r = new LispMatrix(this);

		for(int i = 0; i < rowSize(); i++) {
			for(int j = 0; j < columnSize(); j++) {
				r.set(j, i, get(i, j));
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#transpose()
	 */
	public ILispMatrix adjoint() {
		LispMatrix r = new LispMatrix(this);

		for(int i = 0; i < rowSize(); i++) {
			for(int j = 0; j < columnSize(); j++) {
				r.set(j, i, get(i, j).conjugate());
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberMatrix#add(net.morilib.lisp.matrix.ILispNumberMatrix)
	 */
	public ILispMatrix add(
			ILispMatrix a) throws LispMatrixException {
		if(rowSize() != a.rowSize() ||
				columnSize() != a.columnSize()) {
			throw new LispMatrixException();
		} else {
			LispMatrix r = new LispMatrix(this);

			for(int i = 0; i < rowSize(); i++) {
				for(int j = 0; j < columnSize(); j++) {
					r.set(i, j, r.get(i, j).add(a.get(i, j)));
				}
			}
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberMatrix#subtract(net.morilib.lisp.matrix.ILispNumberMatrix)
	 */
	public ILispMatrix sub(
			ILispMatrix a) throws LispMatrixException {
		if(rowSize() != a.rowSize() ||
				columnSize() != a.columnSize()) {
			throw new LispMatrixException();
		} else {
			LispMatrix r = new LispMatrix(this);

			for(int i = 0; i < rowSize(); i++) {
				for(int j = 0; j < columnSize(); j++) {
					r.set(i, j, r.get(i, j).sub(a.get(i, j)));
				}
			}
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberMatrix#multiply(net.morilib.lisp.matrix.ILispNumberMatrix)
	 */
	public ILispNumberVector mul(
			ILispNumberVector a) throws LispMatrixException {
		LispNumber x;
		LispNumberVector v;

		if(columnSize() != a.size()) {
			throw new LispMatrixException();
		} else {
			v = new LispNumberVector(columnSize());
			for(int i = 0; i < rowSize(); i++) {
				x = LispInteger.ZERO;
				for(int k = 0; k < columnSize(); k++) {
					x = x.add(get(i, k).mul(a.get(k)));
				}
				v.set(i, x);
			}
			return v;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberMatrix#multiply(net.morilib.lisp.LispNumber)
	 */
	public ILispMatrix mul(LispNumber a) {
		LispMatrix r = new LispMatrix(this);

		for(int i = 0; i < rowSize(); i++) {
			for(int j = 0; j < columnSize(); j++) {
				r.set(i, j, r.get(i, j).mul(a));
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberMatrix#negate()
	 */
	public ILispMatrix uminus() {
		LispMatrix r = new LispMatrix(this);

		for(int i = 0; i < rowSize(); i++) {
			for(int j = 0; j < columnSize(); j++) {
				r.set(i, j, r.get(i, j).uminus());
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberMatrix#multiply(net.morilib.lisp.matrix.ILispNumberMatrix)
	 */
	public ILispMatrix mul(
			ILispMatrix a) throws LispMatrixException {
		LispNumber x;
		LispMatrix m;

		if(columnSize() != a.rowSize()) {
			throw new LispMatrixException();
		} else {
			m = new LispMatrix(rowSize(), a.columnSize());
			for(int i = 0; i < rowSize(); i++) {
				for(int j = 0; j < a.columnSize(); j++) {
					x = LispInteger.ZERO;
					for(int k = 0; k < columnSize(); k++) {
						x = x.add(get(i, k).mul(a.get(k, j)));
					}
					m.set(i, j, x);
				}
			}
			return m;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#isHermite()
	 */
	public boolean isHermite() {
		for(int i = 0; i < rowSize(); i++) {
			for(int j = 0; j <= i; j++) {
				if(!get(i, j).equals(get(j, i).conjugate())) {
					return false;
				}
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#determinant()
	 */
	public LispNumber determinant() throws LispMatrixException {
		ILispMatrix[] decomposed;

		if(columnSize() != rowSize()) {
			throw new LispMatrixException();
		} else {
			decomposed = LispMatrices.decomposeLU(this);
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
		ILispMatrix[] decomposed;

		if(columnSize() != rowSize()) {
			throw new LispMatrixException();
		} else {
			decomposed = LispMatrices.decomposeLU(this);
			x = decomposed[2].inv();
			y = decomposed[1].inv();
			x = x.mul(y);
			y = decomposed[0].inv();
			x = x.mul(y);
			return x;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#changeRow(net.morilib.lisp.math.LispPermutation)
	 */
	public ILispMatrix changeRow(final LispPermutation rowp) {
		if(rowp.size() != rowSize()) {
			throw new IllegalArgumentException();
		}
		return new AbstractLispMatrix() {

			public LispNumber get(int row, int column) {
				return AbstractLispMatrix.this.get(
						rowp.get(row), column);
			}

			public void set(int row, int column,
					Datum x) throws LispMatrixException {
				AbstractLispMatrix.this.set(
						rowp.get(row), column, x);
			}

			public int rowSize() {
				return AbstractLispMatrix.this.rowSize();
			}

			public int columnSize() {
				return AbstractLispMatrix.this.columnSize();
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#changeColumn(net.morilib.lisp.math.LispPermutation)
	 */
	public ILispMatrix changeColumn(final LispPermutation colp) {
		if(colp.size() != columnSize()) {
			throw new IllegalArgumentException();
		}
		return new AbstractLispMatrix() {

			public LispNumber get(int row, int column) {
				return AbstractLispMatrix.this.get(
						row, colp.get(column));
			}

			public void set(int row, int column,
					Datum x) throws LispMatrixException {
				AbstractLispMatrix.this.set(
						row, colp.get(column), x);
			}

			public int rowSize() {
				return AbstractLispMatrix.this.rowSize();
			}

			public int columnSize() {
				return AbstractLispMatrix.this.columnSize();
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#submatrix(int, int, int, int)
	 */
	public ILispMatrix submatrix(
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

		return new AbstractLispMatrix() {

			public LispNumber get(int row, int column) {
				if(row < 0 || row >= rowe - rowb) {
					throw new IndexOutOfBoundsException();
				} else if(column < 0 || column >= cole - colb) {
					throw new IndexOutOfBoundsException();
				}
				return AbstractLispMatrix.this.get(
						row + rowb, column + colb);
			}

			public void set(int row, int column,
					Datum x) throws LispMatrixException {
				if(row < 0 || row >= rowe - rowb) {
					throw new IndexOutOfBoundsException();
				} else if(column < 0 || column >= cole - colb) {
					throw new IndexOutOfBoundsException();
				}
				AbstractLispMatrix.this.set(
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
	 * @see net.morilib.lisp.matrix.ILispMatrix#submatrix(net.morilib.lisp.uvector.HomogeneousUnsignedArray, net.morilib.lisp.uvector.HomogeneousUnsignedArray)
	 */
	public ILispMatrix submatrix(
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

		return new AbstractLispMatrix() {

			public LispNumber get(int row, int column) {
				return AbstractLispMatrix.this.get(
						rowa.get(row).getInt(),
						cola.get(column).getInt());
			}

			public void set(int row, int column,
					Datum x) throws LispMatrixException {
				AbstractLispMatrix.this.set(
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

	/**
	 * 
	 * @param y
	 * @return
	 */
	public boolean isEqualTo(ILispMatrix y) {
		if(rowSize() != y.rowSize() ||
				columnSize() != y.columnSize()) {
			return false;
		} else {
			for(int i = 0; i < rowSize(); i++) {
				for(int j = 0; j < columnSize(); j++) {
					if(!get(i, j).isEqualTo(y.get(i, j))) {
						return false;
					}
				}
			}
			return true;
		}
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
