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
package net.morilib.lisp.swing;

import java.awt.Container;
import java.awt.GridLayout;
import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.LispUtils.CallTable;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/18
 */
public class LayoutGrid extends Subr {

	//
	private GridLayout addGrid(
			final Container comp, Datum d, final LispMessage mesg) {
		CallTable<GridLayout, Datum>
		maktbl = new CallTable<GridLayout, Datum>() {

			public GridLayout create(int rows, int cols) {
				GridLayout g = new GridLayout(rows, cols);

				comp.setLayout(g);
				return g;
			}

			public void call(GridLayout tbl, int r, int c, Datum t) {
				if(t instanceof ILispComponent) {
					comp.add(((ILispComponent)t).getComponent());
				} else {
					throw mesg.getError(
							"err.swing.require.component", t);
				}
			}

		};

		return LispUtils.consToTable(d, mesg, maktbl);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);
		GridLayout r;

		if(l.size() == 2) {
			if(!(l.get(0) instanceof ILispComposite)) {
				throw mesg.getError(
						"err.swing.require.composite", l.get(0));
			}
			r = addGrid(((ILispComposite)l.get(0)).getPane(),
					l.get(1), mesg);
		} else if(l.size() == 4) {
			if(!(l.get(0) instanceof ILispComposite)) {
				throw mesg.getError(
						"err.swing.require.composite", l.get(0));
			} else if(!(l.get(2) instanceof LispSmallInt)) {
				throw mesg.getError("err.require.smallint", l.get(2));
			} else if(!(l.get(3) instanceof LispSmallInt)) {
				throw mesg.getError("err.require.smallint", l.get(3));
			}
			r = addGrid(((ILispComposite)l.get(0)).getPane(),
					l.get(1), mesg);
			r.setHgap(l.get(2).getInt());
			r.setVgap(l.get(3).getInt());
		} else {
			throw mesg.getError("err.argument", body);
		}
		return Undef.UNDEF;
	}

}
