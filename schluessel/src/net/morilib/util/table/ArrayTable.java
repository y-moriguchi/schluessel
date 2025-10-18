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
package net.morilib.util.table;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * Implementation of the two dimensional array.
 * <p>2次元配列の実装である.
 * 
 * @author MORIGUCHI, Yuichiro 2006/04/29
 */
public class ArrayTable<E> extends AbstractTable<E>
implements Serializable {

	//
	private static final long serialVersionUID = 4486229552291087150L;

	//
	private Object[][] table;

	/**
	 * 
	 * @param table
	 */
	public ArrayTable(E[]... table) {
		int len;

		if(table == null) {
			throw new NullPointerException();
		} else if(table.length == 0) {
			this.table = new Object[0][];
		}

		len = table[0].length;
		this.table = new Object[table.length][len];
		for(int i = 0; i < table.length; i++) {
			E[] e = table[i];

			if(e.length != len) {
				throw new IllegalArgumentException(
						"length must be equal" + e.length);
			}
			System.arraycopy(e, 0, this.table[i], 0, len);
		}
	}

	/**
	 * creates the new ObjectArrayTable.
	 * <p>新しくObjectArrayTableを生成する.
	 * 
	 * @param rows  列サイズ
	 * @param cols  行サイズ
	 */
	public ArrayTable(int rows, int cols) {
		if(rows < 0) {
			throw new IllegalArgumentException("rows" + rows);
		} else if(cols < 0) {
			throw new IllegalArgumentException("columns" + cols);
		}
		table = new Object[rows][cols];
	}

	/**
	 * creates the new ObjectArrayTable.
	 * <p>新しくObjectArrayTableを生成する.
	 * 
	 * @param table  複製元の表
	 */
	public ArrayTable(Table<E> table) {
		if(table == null) {
			throw new NullPointerException();
		}

		this.table = new Object[table.rowSize()][table.columnSize()];

		for(int i = 0; i < table.rowSize(); i++) {
			for(int j = 0; j < table.columnSize(); j++) {
				this.table[i][j] = table.get(i, j);
			}
		}
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#get(int, int)
	 */
	@SuppressWarnings("unchecked")
	public E get(int r, int c) {
		if(r < 0 || r >= rowSize()) {
			throw new IndexOutOfBoundsException("row " + r);
		} else if(c < 0 || c >= columnSize()) {
			throw new IndexOutOfBoundsException("column " + c);
		}
		return (E)table[r][c];
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#rowSize()
	 */
	public int rowSize() {
		return table.length;
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#columnSize()
	 */
	public int columnSize() {
		return (table.length > 0) ? table[0].length : 0;
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#set(int, int, java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	public E set(int r, int c, E o) {
		if(r < 0 || r >= rowSize()) {
			throw new IndexOutOfBoundsException("row " + r);
		} else if(c < 0 || c >= columnSize()) {
			throw new IndexOutOfBoundsException("column " + c);
		}

		E res = (E)table[r][c];
		table[r][c] = o;
		return res;
	}

	@Override
	protected void addColumn(int col) {
		throw new UnsupportedOperationException();
	}

	@Override
	protected void addRow(int row) {
		throw new UnsupportedOperationException();
	}

	@SuppressWarnings("unchecked")
	public List<E> getRow(int row) {
		return (List<E>)Collections.unmodifiableList(
				Arrays.asList(table[row]));
	}

}
