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
import net.morilib.lisp.ILispVector;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.matrix.ILispDatumMatrix;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/19
 */
public abstract class AbstractLispArrayMatrix
extends AbstractLispMatrix
implements ILispMatrix, ILispVector, Cloneable {

	/**
	 * 
	 */
	protected int rows;

	/**
	 * 
	 */
	protected int columns;

	/**
	 * 
	 * @param index
	 * @return
	 */
	protected abstract LispNumber array(int index);

	/**
	 * 
	 * @param index
	 * @param x
	 * @return
	 */
	protected abstract void arrayset(int index, Datum x);

	/**
	 * 
	 * @return
	 */
	protected abstract AbstractLispArrayMatrix prototype();

	/**
	 * 
	 * @return
	 */
	protected abstract AbstractLispArrayMatrix prototype(int rows,
			int columns);

	/* (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	@Override
	public abstract ILispDatumMatrix clone();

	/**
	 * 
	 * @param srcPos
	 * @param dest
	 * @param destPos
	 * @param len
	 */
	protected abstract void arraycopy(int srcPos, Object dest,
			int destPos, int len);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#get(int, int)
	 */
	public LispNumber get(int row, int column) {
		if(row < 0 || row >= rows) {
			throw new IndexOutOfBoundsException();
		} else if(column < 0 || column >= columns) {
			throw new IndexOutOfBoundsException();
		}
		return array(row * columns + column);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#getRowVector(int)
	 */
	public ILispNumberVector getRowVector(int row) {
		LispNumber[] ra = new LispNumber[columns];

		if(row < 0 || row >= rows) {
			throw new IndexOutOfBoundsException();
		}
		arraycopy(row * columns, ra, 0, columns);
		return new LispNumberVector(ra);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#rowSize()
	 */
	public int rowSize() {
		return rows;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#columnSize()
	 */
	public int columnSize() {
		return columns;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.ILispVector#get(int)
	 */
	public Datum get(int index) {
		return array(index);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.ILispVector#size()
	 */
	public int size() {
		return rows * columns;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#set(int, int, net.morilib.lisp.Datum)
	 */
	public void set(int row, int column, Datum x) {
		if(row < 0 || row >= rows) {
			throw new IndexOutOfBoundsException();
		} else if(column < 0 || column >= columns) {
			throw new IndexOutOfBoundsException();
		}
		arrayset(row * columns + column, x);		
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		LispMatrices.print(buf, this);
	}

}
