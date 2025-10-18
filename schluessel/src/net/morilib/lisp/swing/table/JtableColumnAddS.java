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

import javax.swing.JTable;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/20
 */
public class JtableColumnAddS extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		AbstractLispTableModel md;
		JTable tb;
		ConsIterator itr = new ConsIterator(body);
		Datum td = SubrUtils.nextIf(itr, mesg, body);
		Datum tc = SubrUtils.nextIf(itr, mesg, body);
		Datum tn = Iterators.nextIf(itr, LispBoolean.FALSE);
//		int wd;

		SubrUtils.checkTerminated(itr, body, mesg);
		if(!(td instanceof LispJTable)) {
			throw mesg.getError("err.swing.require.jtable", td);
		} else {
			tb = ((LispJTable)td).table;
			md = (AbstractLispTableModel)tb.getModel();
//			wd = tb.getSize().width / (tb.getColumnCount() + 1);
//			for(int i = 0; i < tb.getColumnCount(); i++) {
//				tb.getColumn(i).setPreferredWidth(wd);
//			}
			try {
				md.addColumn(tn, tc);
			} catch(UnsupportedOperationException e) {
				throw mesg.getError("err.swing.jtree.immutable");
			}
//			tb.addColumn(new TableColumn(
//					tb.getComponentCount(), wd));
			return Undef.UNDEF;
		}
	}

}
