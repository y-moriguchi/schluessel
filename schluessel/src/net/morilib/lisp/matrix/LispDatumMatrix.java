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

import java.util.Arrays;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Undef;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/19
 */
public class LispDatumMatrix extends AbstractLispArrayDatumMatrix
implements java.io.Serializable {

	//
	private Datum[] array;

	/**
	 * 
	 * @param rows
	 * @param columns
	 */
	public LispDatumMatrix(int rows, int columns) {
		this(rows, columns, Undef.UNDEF);
	}

	/**
	 * 
	 * @param rows
	 * @param columns
	 * @param fill
	 */
	public LispDatumMatrix(int rows, int columns, Datum fill) {
		if(rows <= 0 || columns <= 0) {
			throw new IllegalArgumentException();
		}
		this.array   = new Datum[rows * columns];
		this.rows    = rows;
		this.columns = columns;
		Arrays.fill(array, fill);
	}

	/**
	 * 
	 * @param m
	 */
	public LispDatumMatrix(LispDatumMatrix m) {
		this.array   = new Datum[m.rows * m.columns];
		this.rows    = m.rows;
		this.columns = m.columns;
		System.arraycopy(m.array, 0, array, 0, array.length);
	}

	/**
	 * 
	 * @param m
	 */
	public LispDatumMatrix(ILispDatumMatrix m) {
		this.array   = new Datum[m.rowSize() * m.columnSize()];
		this.rows    = m.rowSize();
		this.columns = m.columnSize();
		for(int i = 0; i < m.rowSize(); i++) {
			for(int j = 0; j < m.columnSize(); j++) {
				set(i, j, m.get(i, j));
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.AbstractLispDatumMatrix#array(int)
	 */
	@Override
	protected Datum array(int index) {
		return array[index];
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.AbstractLispMatrix#arrayset(int, net.morilib.lisp.Datum)
	 */
	@Override
	protected void arrayset(int index, Datum x) {
		array[index] = x;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.AbstractLispDatumMatrix#arraycopy(int, java.lang.Object, int, int)
	 */
	@Override
	protected void arraycopy(int srcPos, Object dest, int destPos,
			int len) {
		System.arraycopy(array, srcPos, dest, destPos, len);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.AbstractLispMatrix#prototype()
	 */
	@Override
	protected LispDatumMatrix prototype() {
		return new LispDatumMatrix(rows, columns);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.AbstractLispMatrix#prototype(int, int)
	 */
	@Override
	protected AbstractLispArrayDatumMatrix prototype(int rows,
			int columns) {
		return new LispDatumMatrix(rows, columns);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.AbstractLispMatrix#clone()
	 */
	@Override
	public LispDatumMatrix clone() {
		return new LispDatumMatrix(this);
	}

}
