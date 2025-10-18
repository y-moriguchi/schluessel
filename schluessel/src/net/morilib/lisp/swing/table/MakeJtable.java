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

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/19
 */
public class MakeJtable extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum td = SubrUtils.nextIf(itr, mesg, body);
		Datum tc = Iterators.nextIf(itr);
		LispJTable r;

		SubrUtils.checkTerminated(itr, body, mesg);
		if(tc == null) {
			try {
				r = new LispJTable(td);
			} catch(IllegalArgumentException e) {
				throw mesg.getError("err.swing.jtable.init");
			}
		} else {
			try {
				r = new LispJTable(td, tc);
			} catch(IllegalArgumentException e) {
				throw mesg.getError("err.swing.jtable.init");
			}
		}
		return r;
	}

}
