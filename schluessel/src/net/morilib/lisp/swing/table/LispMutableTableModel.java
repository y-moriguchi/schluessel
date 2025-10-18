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
package net.morilib.lisp.swing.table;

import java.util.ArrayList;
import java.util.List;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Undef;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/19
 */
public class LispMutableTableModel extends AbstractLispTableModel {

	/**
	 * 
	 */
	public static final MyTableColumnNameModel
	DEFAULT_COLUMN_NAME = new MyTableColumnNameModel() {

		@Override
		public String getColumnName(int column) {
			return Integer.toString(column + 1);
		}

		@Override
		public void addColumn(Object columnName) {
		}

	};
			
	//
	private List<List<Datum>> table;
	private MyTableColumnNameModel columnNameModel;
	private int columns;

	/**
	 * 
	 * @param d
	 */
	public LispMutableTableModel(Datum d,
			MyTableColumnNameModel columnNameModel) {
		_inittable(d);
		this.columnNameModel = columnNameModel;
	}

	/**
	 * 
	 * @param d
	 */
	public LispMutableTableModel(Datum d, Datum columnNames) {
		ConsIterator itr = new ConsIterator(d);
		List<Datum> l;

		_inittable(d);
		l   = new ArrayList<Datum>();
		itr = new ConsIterator(columnNames);
		while(itr.hasNext()) {
			l.add(itr.next());
		}

		if(l.size() != columns) {
			throw new IllegalArgumentException();
		}
		columnNameModel =
				new LispDefaulTableColumnNameModel(l, false);
	}

	/**
	 * 
	 * @param d
	 */
	public LispMutableTableModel(Datum d) {
		this(d, DEFAULT_COLUMN_NAME);
	}

	//
	private void _inittable(Datum d) {
		ConsIterator itr = new ConsIterator(d), jtr;
		List<Datum> l;
		Datum e;
		int c = -1;

		table = new ArrayList<List<Datum>>();
		while(itr.hasNext()) {
			if(!(e = itr.next()).isNil() && !(e instanceof Cons)) {
				throw new IllegalArgumentException();
			}
			jtr = new ConsIterator(e);
			l   = new ArrayList<Datum>();
			while(jtr.hasNext()) {
				l.add(jtr.next());
			}
			table.add(l);
			c = Math.max(c, l.size());
		}
		columns = c;
	}

	/* (non-Javadoc)
	 * @see javax.swing.table.TableModel#getRowCount()
	 */
	@Override
	public int getRowCount() {
		return table.size();
	}

	/* (non-Javadoc)
	 * @see javax.swing.table.TableModel#getColumnCount()
	 */
	@Override
	public int getColumnCount() {
		return columns;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.table.AbstractLispTableModel#getColumnCount1(int)
	 */
	@Override
	public int getColumnCount1(int rowIndex) {
		return table.get(rowIndex).size();
	}

	/* (non-Javadoc)
	 * @see javax.swing.table.TableModel#isCellEditable(int, int)
	 */
	@Override
	public boolean isCellEditable(int rowIndex, int columnIndex) {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.table.AbstractLispTableModel#getColumnName(int)
	 */
	@Override
	public String getColumnName(int columnIndex) {
		return columnNameModel.getColumnName(columnIndex);
	}

	/* (non-Javadoc)
	 * @see javax.swing.table.TableModel#setValueAt(java.lang.Object, int, int)
	 */
	@Override
	public void setValueAt(Object val, int r, int c) {
		List<Datum> l = table.get(r);

		if(c < l.size()) {
			l.set(c, (Datum)val);
		} else {
			for(int i = l.size(); i < c; i++) {
				l.add(Undef.UNDEF);
			}
			l.add((Datum)val);
		}
		fireTableCellUpdated(r, c);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.table.AbstractMyTableModel#getValueAt(int, int)
	 */
	@Override
	public Datum getValueAt(int r, int c) {
		List<Datum> l = table.get(r);

		return c < l.size() ? l.get(c) : Undef.UNDEF;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.table.AbstractLispTableModel#addColumn(net.morilib.lisp.Datum)
	 */
	@Override
	public void addColumn(Datum name) {
		addColumn(name, Nil.NIL);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.table.AbstractLispTableModel#addColumn(net.morilib.lisp.Datum, net.morilib.lisp.Datum)
	 */
	@Override
	public void addColumn(Datum name, Datum d) {
		ConsIterator itr;
		List<Datum> l;

		columnNameModel.addColumn(name.isTrue() ?
				name : LispInteger.valueOf(columns + 1));

		itr = new ConsIterator(d);
		for(int r = 0; itr.hasNext(); r++) {
			if(r < table.size()) {
				l = table.get(r);
			} else {
				l = new ArrayList<Datum>();
				table.add(l);
			}

			for(int i = l.size(); i < columns; i++) {
				l.add(Undef.UNDEF);
			}
			l.add(itr.next());
		}
		columns++;
		fireTableStructureChanged();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.table.AbstractLispTableModel#addRow(net.morilib.lisp.Datum)
	 */
	@Override
	public void addRow(Datum d) {
		insertRow(table.size(), d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.table.AbstractLispTableModel#insertRow(int, net.morilib.lisp.Datum)
	 */
	@Override
	public void insertRow(int row, Datum d) {
		ConsIterator itr = new ConsIterator(d);
		List<Datum> l = new ArrayList<Datum>();

		for(int i = 0; i < columns && itr.hasNext(); i++) {
			l.add(itr.next());
		}
		table.add(row, l);
		fireTableRowsInserted(row, row);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.table.AbstractLispTableModel#moveRow(int, int, int)
	 */
	@Override
	public void moveRow(int start, int end, int to) {
		List<List<Datum>> t;

		if(start < to) {
			t = new ArrayList<List<Datum>>();
			for(int i = start; i < end; i++) {
				t.add(table.get(i));
				table.remove(i);
			}

			for(int i = 0; i < t.size(); i++) {
				table.add(to - start + i, t.get(i));
			}
			fireTableRowsInserted(start, end);
		} else if(start == to) {
			// do nothing
		} else {
			t = new ArrayList<List<Datum>>();
			for(int i = start; i < end; i++) {
				t.add(table.get(i));
				table.remove(i);
			}

			for(int i = 0; i < t.size(); i++) {
				table.add(to, t.get(i));
			}
			fireTableRowsInserted(start, end);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.table.AbstractLispTableModel#removeRow(int)
	 */
	@Override
	public void removeRow(int row) {
		table.remove(row);
		fireTableRowsDeleted(row, row);
	}

}
