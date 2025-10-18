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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import net.morilib.util.Iterators;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2006
 */
public class ArrayListTable<E> extends AbstractTable<E>
implements Serializable {

	//
	private static final long serialVersionUID = -3584020203907523829L;

	//
	private List<List<E>> table;
	private int rowsize, colsize;

	/**
	 * 
	 * @param table
	 */
	public ArrayListTable(E[]... table) {
		int len;

		if(table == null) {
			throw new NullPointerException();
		} else if(table.length == 0) {
			this.table = Collections.emptyList();
		}

		len = table[0].length;
		this.table = new ArrayList<List<E>>();
		for(int i = 0; i < table.length; i++) {
			E[] e = table[i];

			if(e.length != len) {
				throw new IllegalArgumentException(
						"length must be equal" + e.length);
			}
			this.table.add(new ArrayList<E>(Arrays.asList(e)));
		}
	}

	/**
	 * creates the new ArrayListTable.
	 * <p>新しくArrayListTableを生成する.
	 * 
	 * @param table  複製元の表
	 */
	public ArrayListTable(Table<E> table) {
		if(table == null) {
			throw new NullPointerException();
		}

		this.table = new ArrayList<List<E>>();
		for(int i = 0; i < table.rowSize(); i++) {
			this.table.add(table.getRow(i));
		}
	}

	@Override
	protected void addColumn(int col) {
		if(col < 0) {
			throw new IndexOutOfBoundsException("" + col);
		} else if(col <= colsize) {
			for(List<E> l : table) {
				l.add(col, null);
			}
			colsize++;
		} else {
			throw new IndexOutOfBoundsException("" + col);
//			for(int i = colsize; i <= col; i++) {
//				for(List<E> l : table) {
//					l.add(null);
//				}
//				colsize++;
//			}
		}
	}

	@Override
	protected void addRow(int row) {
		if(row < 0) {
			throw new IndexOutOfBoundsException("" + row);
		} else if(row <= colsize) {
			table.add(row, new ArrayList<E>());
			rowsize++;
		} else {
			throw new IndexOutOfBoundsException("" + row);
//			for(int i = colsize; i <= row; i++) {
//				table.add(new ArrayList<E>());
//				rowsize++;
//			}
		}
	}

	@Override
	public int columnSize() {
		return colsize;
	}

	@Override
	public E get(int r, int c) {
		if(r < 0 || r >= rowSize()) {
			throw new IndexOutOfBoundsException("row " + r);
		} else if(c < 0 || c >= columnSize()) {
			throw new IndexOutOfBoundsException("column " + c);
		}
		return table.get(r).get(c);
	}

	@Override
	public int rowSize() {
		return rowsize;
	}

	@Override
	public List<E> getRow(int row) {
		return Collections.unmodifiableList(table.get(row));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.table.AbstractTable#clear()
	 */
	@Override
	public void clear() {
		for(List<E> l : table) {
			l.clear();
		}
		table.clear();
		rowsize = colsize = 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.table.AbstractTable#set(int, int, java.lang.Object)
	 */
	@Override
	public E set(int r, int c, E o) {
		if(r < 0 || r >= rowSize()) {
			throw new IndexOutOfBoundsException("row " + r);
		} else if(c < 0 || c >= columnSize()) {
			throw new IndexOutOfBoundsException("column " + c);
		}

		return table.get(r).set(c, o);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.table.AbstractTable#rowIterator(int)
	 */
	@Override
	public Iterator<E> rowIterator(int row) {
		return Iterators.unmodifiable(table.get(row).iterator());
	}

}
