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
package net.morilib.lisp.painter;

import java.util.Iterator;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.Subr;
import net.morilib.lisp.util.ConsIterable;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/03
 */
public class DeriveFont extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		Iterator<Datum> d = new ConsIterable(body, mesg).iterator();
		Datum c1a, c2a, c3a, res;

		if(!d.hasNext()) {
			throw mesg.getError("err.argument", body);
		} else if(!((c1a = d.next()) instanceof SchlushFont)) {
			throw mesg.getError("err.require.font", c1a);
		}
		c2a = d.next();
		if(c2a instanceof SchlushFont.Style) {
			if(!d.hasNext()) {
				res = ((SchlushFont)c1a).deriveFont(
						(SchlushFont.Style)c2a);
			} else if((c3a = d.next()) instanceof LispSmallInt) {
				res = ((SchlushFont)c1a).deriveFont(
						(SchlushFont.Style)c2a, c3a.getInt());
			} else {
				throw mesg.getError("err.require.smallint", c3a);
			}
		} else if(c2a instanceof LispSmallInt) {
			res = ((SchlushFont)c1a).deriveFont(c2a.getInt());
		} else {
			throw mesg.getError("err.require.fontstyle", c2a);
		}

		if(d.hasNext()) {
			throw mesg.getError("err.argument", body);
		}
		return res;
	}

}
