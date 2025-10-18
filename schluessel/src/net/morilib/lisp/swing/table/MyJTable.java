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

import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableModel;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Undef;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/20
 */
public class MyJTable extends JTable {

	//
	static final TableCellRenderer
	DATUM_RENDERER = new DefaultTableCellRenderer() {

		/* (non-Javadoc)
		 * @see javax.swing.table.DefaultTableCellRenderer#getTableCellRendererComponent(javax.swing.JTable, java.lang.Object, boolean, boolean, int, int)
		 */
		@Override
		public Component getTableCellRendererComponent(JTable table,
				Object value, boolean isSelected, boolean hasFocus,
				int row, int column) {
			Datum d = (Datum)value;

			super.getTableCellRendererComponent(table, value,
					isSelected, hasFocus, row, column);

			if(d.isNil() || d instanceof Undef) {
				setText("");
			} else if(d instanceof ILispTableRenderable) {
				((ILispTableRenderable)d).renderCell(this, table,
						value, isSelected, hasFocus, row, column);
			} else {
				setText(LispUtils.print((Datum)d));
			}
			return this;
		}

	};

	/**
	 * 
	 */
	public MyJTable() {
		super();
	}

	/**
	 * @param dm
	 */
	public MyJTable(TableModel dm) {
		super(dm);
	}

	/* (non-Javadoc)
	 * @see javax.swing.JTable#getDefaultRenderer(java.lang.Class)
	 */
	@Override
	public TableCellRenderer getDefaultRenderer(Class<?> cls) {
		if(Datum.class.isAssignableFrom(cls)) {
			return DATUM_RENDERER;
		} else {
			return super.getDefaultRenderer(cls);
		}
	}

}
