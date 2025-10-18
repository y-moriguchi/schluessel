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

import java.awt.Component;

import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.JTable;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.swing.LightweightGUIElement;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/01
 */
public class LispJTable extends LightweightGUIElement {

	//
	JScrollPane component;
	JTable table;

	/**
	 * 
	 * @param table
	 */
	public LispJTable(JTable table) {
		this.table = table;
		this.component = new JScrollPane(table);
	}

	/**
	 * 
	 * @param d
	 * @param columnNames
	 */
	public LispJTable(Datum d, Datum columnNames) {
		if(d instanceof LispVector) {
			if(!(columnNames instanceof LispVector)) {
				throw new IllegalArgumentException();
			}
			table = new MyJTable(new LispVectorTableModel(
					(LispVector)d, (LispVector)columnNames));
		} else {
			table = new MyJTable(
					new LispMutableTableModel(d, columnNames));
		}
		this.component = new JScrollPane(table);
	}

	/**
	 * 
	 * @param d
	 */
	public LispJTable(Datum d) {
		if(d instanceof LispVector) {
			table = new MyJTable(new LispVectorTableModel(
					(LispVector)d));
		} else {
			table = new MyJTable(new LispMutableTableModel(d));
		}
		this.component = new JScrollPane(table);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ILispComponent#getComponent()
	 */
	@Override
	public JComponent getComponent() {
		return component;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return component;
	}

	/**
	 * 
	 * @return
	 */
	public int getRowCount() {
		return table.getRowCount();
	}

	/**
	 * 
	 * @return
	 */
	public int getColumnCount() {
		return table.getColumnCount();
	}

	/**
	 * 
	 * @param r
	 * @param c
	 * @return
	 */
	public Datum get(int r, int c) {
		return (Datum)table.getValueAt(r, c);
	}

	/**
	 * 
	 * @param r
	 * @param c
	 * @param d
	 */
	public void set(int r, int c, Datum d) {
		table.setValueAt((Datum)d, r, c);
	}

	/**
	 * 
	 * @param d
	 */
	public void addRow(Datum d) {
		((AbstractLispTableModel)table.getModel()).addRow(d);
	}

	/**
	 * 
	 * @param r
	 * @param d
	 */
	public void insertRow(int r, Datum d) {
		((AbstractLispTableModel)table.getModel()).insertRow(r, d);
	}

	/**
	 * 
	 * @param r
	 */
	public void removeRow(int r) {
		((AbstractLispTableModel)table.getModel()).removeRow(r);
	}

	/**
	 * 
	 * @return
	 */
	public Datum toDatum() {
		return ((AbstractLispTableModel)table.getModel()).toList();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<gui-table>");
	}

}
