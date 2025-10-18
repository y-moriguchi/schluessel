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
package net.morilib.lisp.awt.cursor;

import java.awt.AWTException;
import java.awt.Component;
import java.awt.HeadlessException;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.swing.GUIElement;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/07
 */
public class CursorSetS extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum c1a = SubrUtils.nextIf(itr, mesg, body);
		Datum c2a = Iterators.nextIf(itr);
		LispCursor lc;
		Component c;

		SubrUtils.checkTerminated(itr, body, mesg);
		try {
			if(!(c1a instanceof GUIElement)) {
				throw mesg.getError("err.swing.require.guielement",
						c1a);
			} else {
				if(c2a == null) {
					lc = LispCursor.getDefault();
				} else if(c2a instanceof LispString) {
					lc = LispCursor.getInstance(c2a.getString());
				} else if(c2a instanceof Symbol) {
					lc = LispCursor.getInstance(
							((Symbol)c2a).getName());
				} else {
					throw mesg.getError("err.require.symbol", c2a);
				}
				c = ((GUIElement)c1a).getAWTComponent();
				c.setCursor(lc.cursor);
				return LispBoolean.TRUE;
			}
		} catch(HeadlessException e) {
			return LispBoolean.FALSE;
		} catch(AWTException e) {
			return LispBoolean.FALSE;
		}
	}

}
