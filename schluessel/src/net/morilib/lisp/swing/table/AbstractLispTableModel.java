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

import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.TableModel;

import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/18
 */
public abstract class AbstractLispTableModel extends Datum2
implements TableModel, java.io.Serializable {

	//
	private List<TableModelListener> listeners =
			new ArrayList<TableModelListener>();

	/* (non-Javadoc)
	 * @see javax.swing.table.TableModel#getValueAt(int, int)
	 */
	@Override
	public abstract Datum getValueAt(int rowIndex, int columnIndex);

	/**
	 * 
	 * @param rowIndex
	 * @return
	 */
	public abstract int getColumnCount1(int rowIndex);

	/**
	 * 
	 * @return
	 */
	public Datum toList() {
		ConsListBuilder b = new ConsListBuilder(), c;

		for(int i = 0; i < getRowCount(); i++) {
			c = new ConsListBuilder();
			for(int j = 0; j < getColumnCount1(i); j++) {
				c.append(getValueAt(i, j));
			}
			b.append(c.get());
		}
		return b.get();
	}

	/* (non-Javadoc)
	 * @see javax.swing.table.TableModel#getColumnName(int)
	 */
	@Override
	public String getColumnName(int columnIndex) {
		return Integer.toString(columnIndex);
	}

	/* (non-Javadoc)
	 * @see javax.swing.table.TableModel#getColumnClass(int)
	 */
	@Override
	public Class<?> getColumnClass(int columnIndex) {
		return Datum.class;
	}

	/* (non-Javadoc)
	 * @see javax.swing.table.TableModel#addTableModelListener(javax.swing.event.TableModelListener)
	 */
	@Override
	public void addTableModelListener(TableModelListener l) {
		synchronized(this) {
			listeners.add(l);
		}
	}

	/* (non-Javadoc)
	 * @see javax.swing.table.TableModel#removeTableModelListener(javax.swing.event.TableModelListener)
	 */
	@Override
	public void removeTableModelListener(TableModelListener l) {
		synchronized(this) {
			listeners.remove(l);
		}
	}

	/**
	 * 
	 * @param e
	 */
	public void fireTableChanged(TableModelEvent e) {
		synchronized(this) {
			for(TableModelListener l : listeners) {
				l.tableChanged(e);
			}
		}
	}

	/**
	 * 
	 * @param row
	 * @param column
	 */
	public void fireTableCellUpdated(int row, int column) {
		fireTableChanged(new TableModelEvent(this, row, row, column));
	}

	/**
	 * 
	 */
	public void fireTableDataChanged() {
		fireTableChanged(new TableModelEvent(this));
	}

	/**
	 * 
	 * @param firstRow
	 * @param lastRow
	 */
	public void fireTableRowsDeleted(int firstRow, int lastRow) {
		fireTableChanged(new TableModelEvent(
				this, firstRow, lastRow,
				TableModelEvent.ALL_COLUMNS,
				TableModelEvent.DELETE));
	}

	/**
	 * 
	 * @param firstRow
	 * @param lastRow
	 */
	public void fireTableRowsInserted(int firstRow, int lastRow) {
		fireTableChanged(new TableModelEvent(
				this, firstRow, lastRow,
				TableModelEvent.ALL_COLUMNS,
				TableModelEvent.INSERT));
	}

	/**
	 * 
	 * @param firstRow
	 * @param lastRow
	 */
	public void fireTableRowsUpdated(int firstRow, int lastRow) {
		fireTableChanged(new TableModelEvent(
				this, firstRow, lastRow,
				TableModelEvent.ALL_COLUMNS,
				TableModelEvent.UPDATE));
	}

	/**
	 * 
	 */
	public void fireTableStructureChanged() {
		fireTableChanged(new TableModelEvent(
				this, TableModelEvent.HEADER_ROW));
	}

	/**
	 * 
	 * @param name
	 */
	public void addColumn(Datum name) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @param name
	 */
	public void addColumn(Datum name, Datum d) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @param name
	 */
	public void addRow(Datum d) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @param rowNum
	 * @param row
	 */
	public void insertRow(int row, Datum d) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @param start
	 * @param end
	 * @param to
	 */
	public void moveRow(int start, int end, int to) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @param row
	 */
 	public void removeRow(int row) {
		throw new UnsupportedOperationException();
 	}

 	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<jtable-model>");
	}

}
