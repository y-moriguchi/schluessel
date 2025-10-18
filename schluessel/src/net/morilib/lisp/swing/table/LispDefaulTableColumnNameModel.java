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

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/19
 */
public class LispDefaulTableColumnNameModel extends Datum2
implements MyTableColumnNameModel {

	//
	private List<Datum> columnNames;

	//
	LispDefaulTableColumnNameModel(List<Datum> l, boolean dummy) {
		this.columnNames = l;
	}

	/**
	 * 
	 * @param l
	 */
	public LispDefaulTableColumnNameModel() {
		this(new ArrayList<Datum>(), false);
	}

	/**
	 * 
	 * @param l
	 */
	public LispDefaulTableColumnNameModel(List<Datum> l) {
		this(new ArrayList<Datum>(l), false);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.table.MyTableColumnNameModel#getColumnName(int)
	 */
	@Override
	public String getColumnName(int column) {
		return LispUtils.print(columnNames.get(column));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.table.MyTableColumnNameModel#addColumn(java.lang.Object)
	 */
	@Override
	public void addColumn(Object columnName) {
		columnNames.add((Datum)columnName);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<default-column-name-model>");
	}

}
