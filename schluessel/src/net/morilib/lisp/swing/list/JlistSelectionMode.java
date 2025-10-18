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
package net.morilib.lisp.swing.list;

import javax.swing.ListSelectionModel;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/18
 */
public class JlistSelectionMode extends UnaryArgs {

	//
	static final Symbol SINGLE = Symbol.getSymbol("single");
	static final Symbol INTERVAL = Symbol.getSymbol("interval");
	static final Symbol MULTIPLE = Symbol.getSymbol("multiple");

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		ListSelectionModel m;

		if(c1a instanceof LispJList) {
			m = ((LispJList)c1a).list.getSelectionModel();
			switch(m.getSelectionMode()) {
			case ListSelectionModel.SINGLE_SELECTION:
				return SINGLE;
			case ListSelectionModel.SINGLE_INTERVAL_SELECTION:
				return INTERVAL;
			case ListSelectionModel.MULTIPLE_INTERVAL_SELECTION:
				return MULTIPLE;
			default:  return LispBoolean.FALSE;
			}
		} else {
			throw mesg.getError("err.swing.require.jlist", c1a);
		}
	}

}
