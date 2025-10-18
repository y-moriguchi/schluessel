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
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.QuaternaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.table.ILispTable;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/20
 */
public class JtableCellSetS extends QuaternaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.QuaternaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a, Datum c4a,
			Environment env, LispMessage mesg) {
		ILispTable t;
		int r = SubrUtils.getSmallInt(c2a, mesg);
		int c = SubrUtils.getSmallInt(c3a, mesg);

		if(c1a instanceof LispJTable) {
			t = (ILispTable)c1a;
			if(r < 0) {
				throw mesg.getError("err.require.int.nonnegative",
						c2a);
			} else if(c < 0) {
				throw mesg.getError("err.require.int.nonnegative",
						c3a);
			} else if(r >= t.getRowCount() ||
					c >= t.getColumnCount()) {
				throw mesg.getError("err.table.index.outofrange");
			}

			try {
				t.set(r, c, c4a);
				return Undef.UNDEF;
			} catch(UnsupportedOperationException e) {
				throw mesg.getError("err.table.immutable");
			}
		} else {
			throw mesg.getError("err.table.require.table", c1a);
		}
	}

}
