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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import net.morilib.util.iterator.IndexIterator;
import net.morilib.util.iterator.NullSkipIterator;

/**
 * Abstract implementation of Table.
 * <p>Tableの抽象クラスである.
 * 
 * @author MORIGUCHI, Yuichiro 2006/04/29
 */
public abstract class AbstractTable<E> implements Table<E> {

	//
	private static class _Iter<E> implements Iterator<E> {

		//
		private Table<E> table;
		private int _ptrr;
		private int _ptrc;
		private int r2, c1, c2;

		//
		private _Iter(Table<E> tbl, int r1, int r2, int c1, int c2) {
			this.table = tbl;
			this._ptrr = r1;
			this.r2    = r2;
			this.c1    = this._ptrc = c1;
			this.c2    = c2;
		}

		//
		public void remove() {
			throw new UnsupportedOperationException();
		}

		//
		public boolean hasNext() {
			return _ptrr < r2;
		}

		//
		public E next() {
			E res = table.get(_ptrr, _ptrc++);
			if(_ptrc >= c2) {
				_ptrr++;
				_ptrc = c1;
			}
			return res;
		}

	};

	//
	private static class _RowIter<E> extends IndexIterator<E> {

		//
		private Table<E> table;
		private int row;

		//
		public _RowIter(Table<E> tbl, int row, int begin, int end) {
			super(begin, end);
			this.table = tbl;
			this.row   = row;
		}

		/* (non-Javadoc)
		 * @see org.usei.data.iterator.IndexIterator#get(int)
		 */
		protected E get(int index) {
			return table.get(row, index);
		}

		/* (non-Javadoc)
		 * @see org.usei.data.iterator.IndexIterator#set(int, java.lang.Object)
		 */
		protected void set(int index, E o) {
			table.set(row, index, o);
		}

	};

	//
	private static class _ColumnIter<E> extends IndexIterator<E> {

		//
		private Table<E> table;
		private int col;

		//
		public _ColumnIter(Table<E> tbl, int col, int begin, int end) {
			super(begin, end);
			this.table = tbl;
			this.col = col;
		}

		/* (non-Javadoc)
		 * @see org.usei.data.iterator.IndexIterator#get(int)
		 */
		protected E get(int index) {
			return table.get(index, col);
		}

		/* (non-Javadoc)
		 * @see org.usei.data.iterator.IndexIterator#set(int, java.lang.Object)
		 */
		protected void set(int index, E o) {
			table.set(index, col, o);
		}

	};

	//
	/*package*/ static class SubTable<E, T extends Table<E>>
	implements Table<E> {

		//
		/*package*/ int r1, r2, c1, c2;
		/*package*/ T   table;

		//
		/*package*/ SubTable(T table, int r1, int r2, int c1, int c2) {
			this.table = table;
			this.r1    = r1;
			this.r2    = r2;
			this.c1    = c1;
			this.c2    = c2;
		}

		//
		/*package*/ void checkRange(int x, int y) {
			if(x < 0 || x >= r2 - r1) {
				throw new IndexOutOfBoundsException("row " + x);
			} else if(y < 0 || y >= c2 - c1) {
				throw new IndexOutOfBoundsException("column " + y);
			}
		}

		//
		/*package*/ void checkRange(int x1, int x2, int y1, int y2) {
			if(x1 < 0 || x1 > r2 - r1) {
				throw new IndexOutOfBoundsException("row " + x1);
			} else if(y1 < 0 || y1 > c2 - c1) {
				throw new IndexOutOfBoundsException("column " + y1);
			} else if(x2 < 0 || x2 > r2 - r1) {
				throw new IndexOutOfBoundsException("row " + x2);
			} else if(y2 < 0 || y2 > c2 - c1) {
				throw new IndexOutOfBoundsException("column " + y2);
			} else if(x1 > x2 || y1 > y2) {
				throw new IllegalArgumentException();
			}
		}

		//
		public E get(int x, int y) {
			checkRange(x, y);
			return table.get(x + r1, y + c1);
		}

		//
		public E set(int x, int y, E o) {
			checkRange(x, y);
			return table.set(x + r1, y + c1, o);
		}

		//
		public int rowSize() {
			return r2 - r1;
		}

		//
		public int columnSize() {
			return c2 - c1;
		}

		//
		public Iterator<E> wholeIterator() {
			return new _Iter<E>(this, r1, r2, c1, c2);
		}

		//
		public Iterator<E> rowIterator(int row) {
			if(row < 0 || row >= rowSize()) {
				throw new IndexOutOfBoundsException("row " + row);
			}
			return new _RowIter<E>(this, row + r1, c1, c2);
		}

		//
		public Iterator<E> columnIterator(int column) {
			if(column < 0 || column >= columnSize()) {
				throw new IndexOutOfBoundsException("column " + column);
			}
			return new _ColumnIter<E>(this, column + c1, r1, r2);
		}

		//
		public Table<E> subTable(int x1, int x2, int y1, int y2) {
			checkRange(x1, x2, y1, y2);
			return new SubTable<E, Table<E>>(
					this, x1 + r1, x2 + r1, y1 + c1, y2 + c1);
		}

		//
		public int setRow(int row, List<E> list) {
			return setRow(row, list, 0, columnSize());
		}

		//
		public int addRow(int row, List<E> list) {
			if(row < 0 || row > rowSize()) {
				throw new IndexOutOfBoundsException("" + row);
			}
			r2++;
			return table.addRow(row + r1, list);
		}

		//
		public int addRow(List<E> list) {
			return table.addRow(r2++, list);
		}

		//
		public void deleteRow(int row) {
			if(row < 0 || row >= rowSize()) {
				throw new IndexOutOfBoundsException("" + row);
			}
			r2--;
			table.deleteRow(row + r1);
		}

		//
		public int setColumn(int col, List<E> list) {
			return setColumn(col, list, 0, rowSize());
		}

		//
		public int addColumn(int col, List<E> list) {
			if(col < 0 || col > columnSize()) {
				throw new IndexOutOfBoundsException("" + col);
			}
			c2++;
			return table.addColumn(col + c1, list);
		}

		//
		public int addColumn(List<E> list) {
			return table.addColumn(c2++, list);
		}

		//
		public void deleteColumn(int col) {
			if(col < 0 || col >= columnSize()) {
				throw new IndexOutOfBoundsException("" + col);
			}
			c2--;
			table.deleteRow(col + c1);
		}

		//
		public void clear() {
			throw new UnsupportedOperationException();
		}

		//
		public int setRow(int row, List<E> list, int cbgn, int cend) {
			if(row < 0 || row >= rowSize()) {
				throw new IndexOutOfBoundsException("row " + row);
			} else if(cbgn < 0 || cbgn > columnSize()) {
				throw new IndexOutOfBoundsException("begin " + cbgn);
			} else if(cend < 0 || cend > columnSize()) {
				throw new IndexOutOfBoundsException("end " + cend);
			} else if(cbgn > cend) {
				throw new IllegalArgumentException(cbgn + ">" + cend);
			}

			return table.setRow(row, list, cbgn + c1, cend + c1);
		}

		//
		public int setColumn(int col, List<E> list, int rbgn, int rend) {
			if(col < 0 || col >= columnSize()) {
				throw new IndexOutOfBoundsException("column " + col);
			} else if(rbgn < 0 || rbgn > rowSize()) {
				throw new IndexOutOfBoundsException("begin " + rbgn);
			} else if(rend < 0 || rend > rowSize()) {
				throw new IndexOutOfBoundsException("end " + rend);
			} else if(rbgn > rend) {
				throw new IllegalArgumentException(rbgn + ">" + rend);
			}

			return table.setColumn(col, list, rbgn + r1, rend + r1);
		}

		public List<E> getColumn(int col) {
			return table.getColumn(col).subList(c1, c2);
		}

		public List<E> getRow(int row) {
			return table.getRow(row).subList(r1, r2);
		}

		public Iterator<E> columnIteratorNotNull(int column) {
			return new NullSkipIterator<E>(columnIterator(column));
		}

		public Iterator<E> rowIteratorNotNull(int row) {
			return new NullSkipIterator<E>(rowIterator(row));
		}

		public Iterator<E> wholeIteratorNotNull() {
			return new NullSkipIterator<E>(wholeIterator());
		}

	};

	//
	protected AbstractTable() { }

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#get(int, int)
	 */
	public abstract E get(int x, int y);

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#rowSize()
	 */
	public abstract int rowSize();

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#columnSize()
	 */
	public abstract int columnSize();


	public List<E> getColumn(int col) {
		List<E> res = new ArrayList<E>();

		for(int i = 0; i < rowSize(); i++) {
			res.add(get(i, col));
		}
		return Collections.unmodifiableList(res);
	}


	public List<E> getRow(int row) {
		List<E> res = new ArrayList<E>();

		for(int i = 0; i < columnSize(); i++) {
			res.add(get(row, i));
		}
		return Collections.unmodifiableList(res);
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#rowIterator(int)
	 */
	public Iterator<E> rowIterator(int row) {
		if(row < 0 || row >= rowSize()) {
			throw new IndexOutOfBoundsException("row " + row);
		}
		return new _RowIter<E>(this, row, 0, columnSize());
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#columnIterator(int)
	 */
	public Iterator<E> columnIterator(int column) {
		if(column < 0 || column >= columnSize()) {
			throw new IndexOutOfBoundsException("column " + column);
		}
		return new _ColumnIter<E>(this, column, 0, rowSize());
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#iterator()
	 */
	public Iterator<E> wholeIterator() {
		return new _Iter<E>(this, 0, rowSize(), 0, columnSize());
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#set(int, int, java.lang.Object)
	 */
	public E set(int x, int y, E o) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#setRow(int, java.util.List)
	 */
	public int setRow(int row, List<E> list) {
		return setRow(row, list, 0, columnSize());
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#setRow(int, java.util.List, int, int)
	 */
	public int setRow(int row, List<E> list, int cbgn, int cend) {
		if(row < 0 || row >= rowSize()) {
			throw new IndexOutOfBoundsException("row " + row);
		} else if(cbgn < 0 || cbgn > columnSize()) {
			throw new IndexOutOfBoundsException("begin " + cbgn);
		} else if(cend < 0 || cend > columnSize()) {
			throw new IndexOutOfBoundsException("end " + cend);
		} else if(cbgn > cend) {
			throw new IllegalArgumentException(cbgn + ">" + cend);
		}

		int i = cbgn;
		for(; i < cend && i - cbgn < list.size(); i++) {
			set(row, i, list.get(i - cbgn));
		}
		return i - cbgn;
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#setColumn(int, java.util.List)
	 */
	public int setColumn(int col, List<E> list) {
		return setColumn(col, list, 0, rowSize());
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#setColumn(int, java.util.List, int, int)
	 */
	public int setColumn(int col, List<E> list, int rbgn, int rend) {
		if(col < 0 || col >= columnSize()) {
			throw new IndexOutOfBoundsException("column " + col);
		} else if(rbgn < 0 || rbgn > rowSize()) {
			throw new IndexOutOfBoundsException("begin " + rbgn);
		} else if(rend < 0 || rend > rowSize()) {
			throw new IndexOutOfBoundsException("end " + rend);
		} else if(rbgn > rend) {
			throw new IllegalArgumentException(rbgn + ">" + rend);
		}

		int i = rbgn;
		for(; i < rend && i - rbgn < list.size(); i++) {
			set(i, col, list.get(i - rbgn));
		}
		return i - rbgn;
	}


	protected abstract void addRow(int row);

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#addRow(int, java.util.List)
	 */
	public int addRow(int row, List<E> list) {
		addRow(row);
		return setRow(row, list);
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#addRow(java.util.List)
	 */
	public int addRow(List<E> list) {
		return addRow(rowSize(), list);
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#deleteRow(int)
	 */
	public void deleteRow(int row) {
		throw new UnsupportedOperationException();
	}


	protected abstract void addColumn(int col);

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#addColumn(int, java.util.List)
	 */
	public int addColumn(int col, List<E> list) {
		addColumn(col);
		return setColumn(col, list);
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#addColumn(java.util.List)
	 */
	public int addColumn(List<E> list) {
		return addColumn(columnSize(), list);
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#deleteColumn(int)
	 */
	public void deleteColumn(int col) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#clear()
	 */
	public void clear() {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see org.usei.data.table.Table#subTable(int, int, int, int)
	 */
	public Table<E> subTable(int r1, int r2, int c1, int c2) {
		if(r1 < 0 || r1 > rowSize()) {
			throw new IndexOutOfBoundsException("row " + r1);
		} else if(c1 < 0 || c1 > columnSize()) {
			throw new IndexOutOfBoundsException("column " + c1);
		} else if(r2 < 0 || r2 > rowSize()) {
			throw new IndexOutOfBoundsException("row " + r2);
		} else if(c2 < 0 || c2 > columnSize()) {
			throw new IndexOutOfBoundsException("column " + c2);
		} else if(r1 > r2 || c1 > c2) {
			throw new IllegalArgumentException();
		}
		return new SubTable<E, Table<E>>(this, r1, r2, c1, c2);
	}

	public Iterator<E> columnIteratorNotNull(int column) {
		return new NullSkipIterator<E>(columnIterator(column));
	}

	public Iterator<E> rowIteratorNotNull(int row) {
		return new NullSkipIterator<E>(rowIterator(row));
	}

	public Iterator<E> wholeIteratorNotNull() {
		return new NullSkipIterator<E>(wholeIterator());
	}

}
