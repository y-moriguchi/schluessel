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

import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/03
 */
public class MakeFont extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);

		if(l.size() == 1) {
			if(!(l.get(0) instanceof LispString)) {
				throw mesg.getError("err.require.string", l.get(0));
			}

			return SchlushFont.getDefaultFont(l.get(0).getString());
		} else if(l.size() == 3) {
			if(!(l.get(0) instanceof LispString)) {
				throw mesg.getError("err.require.string", l.get(0));
			} else if(!(l.get(1) instanceof SchlushFont.Style)) {
				throw mesg.getError("err.require.fontstyle", l.get(1));
			} else if(!(l.get(2) instanceof LispSmallInt)) {
				throw mesg.getError("err.require.smallint", l.get(2));
			}

			return new SchlushFont(
					l.get(0).getString(),
					(SchlushFont.Style)l.get(1),
					l.get(2).getInt());
		} else {
			throw mesg.getError("err.argument", body);
		}
	}

}
