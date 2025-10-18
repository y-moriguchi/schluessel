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
import net.morilib.lisp.ILispVector;
import net.morilib.lisp.LispVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/19
 */
public abstract class AbstractLispArrayDatumMatrix
extends AbstractLispDatumMatrix implements Cloneable {

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
	protected abstract Datum array(int index);

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
	protected abstract AbstractLispArrayDatumMatrix prototype();

	/**
	 * 
	 * @return
	 */
	protected abstract AbstractLispArrayDatumMatrix prototype(int rows,
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
	public Datum get(int row, int column) {
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
	public ILispVector getRowVector(int row) {
		Datum[] ra = new Datum[columns];

		if(row < 0 || row >= rows) {
			throw new IndexOutOfBoundsException();
		}
		arraycopy(row * columns, ra, 0, columns);
		return new LispVector(ra);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispMatrix#getColumnVector(int)
	 */
	public ILispVector getColumnVector(int column) {
		Datum[] ra = new Datum[columns];

		if(column < 0 || column >= columns) {
			throw new IndexOutOfBoundsException();
		} else {
			for(int i = 0; i < rows; i++) {
				ra[i] = array(i * rows + column);
			}
			return new LispVector(ra);
		}
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

}
