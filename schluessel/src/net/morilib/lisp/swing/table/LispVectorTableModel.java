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

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.Undef;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/18
 */
public class LispVectorTableModel extends AbstractLispTableModel {

	//
	private LispVector table, columnNames;
	private final int column;

	/**
	 * 
	 * @param table
	 * @throws IllegalArgumentException
	 */
	public LispVectorTableModel(LispVector table) {
		this.table  = table;
		this.column = checkVector(table);
		this.columnNames = null;
	}

	/**
	 * 
	 * @param table
	 * @throws IllegalArgumentException
	 */
	public LispVectorTableModel(LispVector table,
			LispVector columnNames) {
		this.table  = table;
		this.column = checkVector(table);
		if(columnNames.size() != this.column) {
			throw new IllegalArgumentException();
		}
		this.columnNames = columnNames;
	}

	//
	private int checkVector(LispVector table) {
		LispVector v;
		int c = -1;

		for(int i = 0; i < table.size(); i++) {
			if(table.get(i) instanceof LispVector) {
				v = (LispVector)table.get(i);
				c = Math.max(c, v.size());
			} else {
				throw new IllegalArgumentException();
			}
		}
		return c;
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
		return column;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.table.AbstractLispTableModel#getColumnCount1(int)
	 */
	@Override
	public int getColumnCount1(int rowIndex) {
		return ((LispVector)table.get(rowIndex)).size();
	}

	/* (non-Javadoc)
	 * @see javax.swing.table.TableModel#isCellEditable(int, int)
	 */
	@Override
	public boolean isCellEditable(int rowIndex, int columnIndex) {
		return false;
	}

	/* (non-Javadoc)
	 * @see javax.swing.table.TableModel#getValueAt(int, int)
	 */
	@Override
	public Datum getValueAt(int r, int c) {
		LispVector v;

		v = (LispVector)table.get(r);
		return c < v.size() ? v.get(c) : Undef.UNDEF;
	}

	/* (non-Javadoc)
	 * @see javax.swing.table.TableModel#setValueAt(java.lang.Object, int, int)
	 */
	@Override
	public void setValueAt(Object aValue, int rowIndex,
			int columnIndex) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.table.AbstractLispTableModel#getColumnName(int)
	 */
	@Override
	public String getColumnName(int c) {
		if(columnNames == null) {
			return Integer.toString(c + 1);
		}
		return LispUtils.print(columnNames.get(c));
	}

}
