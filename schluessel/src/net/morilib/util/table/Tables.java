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
import java.util.Iterator;
import java.util.List;

import net.morilib.lang.Hashes;
import net.morilib.util.Iterators;

/**
 * <i>USEful Implements</i> for tables.<br>
 * 表に関する便利な関数である.
 * 
 * @author MORIGUCHI, Yuichiro 2006/04/30
 */
public final class Tables {

	//
	public Tables() { }

	//
	/*package*/ static class Pos {
		//
		private int x, y;

		/*package*/ Pos(int x, int y) {
			this.x = x;
			this.y = y;
		}

		public int getX() {
			return x;
		}

		public int getY() {
			return y;
		}

		public boolean equals(Object o) {
			if(o instanceof Pos) {
				Pos p = (Pos)o;

				return x == p.x && y == p.y;
			}
			return false;
		}

		public int hashCode() {
			int res = Hashes.INIT;

			res = Hashes.A * res + x;
			res = Hashes.A * res + y;
			return res;
		}

		public String toString() {
			return "(" + x + "," + y + ")";
		}

	}

	//
	/*package*/ abstract static class ReadOnlyTable<E>
	extends AbstractTable<E> {

		//
		@Override
		public E set(int r, int c, E o) {
			throw new UnsupportedOperationException();
		}

		//
		@Override
		public int setRow(int row, List<E> list) {
			throw new UnsupportedOperationException();
		}

		//
		@Override
		public int setRow(int row, List<E> list, int cbgn, int cend) {
			throw new UnsupportedOperationException();
		}

		//
		@Override
		public int addRow(int row, List<E> list) {
			throw new UnsupportedOperationException();
		}

		//
		@Override
		public int addRow(List<E> list) {
			throw new UnsupportedOperationException();
		}

		//
		@Override
		public void deleteRow(int row) {
			throw new UnsupportedOperationException();
		}

		//
		@Override
		public int setColumn(int col, List<E> list) {
			throw new UnsupportedOperationException();
		}

		//
		@Override
		public int setColumn(int col, List<E> list, int rbgn, int rend) {
			throw new UnsupportedOperationException();
		}

		//
		@Override
		public int addColumn(int col, List<E> list) {
			throw new UnsupportedOperationException();
		}

		//
		@Override
		public int addColumn(List<E> list) {
			throw new UnsupportedOperationException();
		}

		//
		@Override
		public void deleteColumn(int col) {
			throw new UnsupportedOperationException();
		}

		//
		@Override
		public void clear() {
			throw new UnsupportedOperationException();
		}

		@Override
		protected void addColumn(int col) {
			throw new UnsupportedOperationException();
		}

		@Override
		protected void addRow(int row) {
			throw new UnsupportedOperationException();
		}

	};

	//
	private static class _Unmodifiable<E> extends ReadOnlyTable<E>
	implements Serializable {

		//
		private static final long serialVersionUID = 7048452960324140160L;

		//
		private Table<E> w;

		//
		private _Unmodifiable(Table<E> w) {
			this.w = w;
		}

		//
		public E get(int r, int c) {
			return w.get(r, c);
		}

		//
		public int rowSize() {
			return w.rowSize();
		}

		//
		public int columnSize() {
			return w.columnSize();
		}

		//
		public Iterator<E> wholeIterator() {
			return w.wholeIterator();
		}

		//
		public Iterator<E> rowIterator(int row) {
			return w.rowIterator(row);
		}

		//
		public Iterator<E> columnIterator(int column) {
			return w.columnIterator(column);
		}

		//
		public Table<E> subTable(int r1, int r2, int c1, int c2) {
			Table<E> res = w.subTable(r1, r2, c1, c2);
			return new _Unmodifiable<E>(res);
		}

	};

	//
	private static class _Empty extends ReadOnlyTable<Object>
	implements Serializable {

		//
		private static final long serialVersionUID = 2098001818079704026L;

		public Object get(int r, int c) {
			throw new IndexOutOfBoundsException();
		}

		public int rowSize() {
			return 0;
		}

		public int columnSize() {
			return 0;
		}

		public Iterator<Object> wholeIterator() {
			return Iterators.emptyIterator();
		}

		public Iterator<Object> rowIterator(int row) {
			return Iterators.emptyIterator();
		}

		public Iterator<Object> columnIterator(int column) {
			return Iterators.emptyIterator();
		}

		public Table<Object> subTable(int r1, int c1, int r2, int c2) {
			if(r1 == 0 && c1 == 0 && r2 == 0 && c2 == 0) {
				return this;
			} else {
				throw new IndexOutOfBoundsException();
			}
		}

	};

	/**
	 * An empty table.
	 * <p>空白の表である.
	 */
	public static final Table<Object> EMPTY = new _Empty();

	/**
	 * 
	 * @param <E>
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static<E> Table<E> emptyTable() {
		return (Table<E>)EMPTY;
	}

	/**
	 * returns the unmodifiable view of the table.
	 * <p>テーブルにおける変更できない表のビューを得る.
	 * 
	 * @param t  変更できなくしたい表
	 * @return  変更できない表のビュー
	 */
	public static<E> Table<E> unmodifiable(Table<E> t) {
		return new _Unmodifiable<E>(t);
	}

}
