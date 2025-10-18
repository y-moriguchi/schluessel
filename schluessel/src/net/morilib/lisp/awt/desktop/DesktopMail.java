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
package net.morilib.lisp.awt.desktop;

import java.awt.Desktop;
import java.io.IOException;
import java.net.URI;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/28
 */
public class DesktopMail extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum c1a = Iterators.nextIf(itr);
		URI s = (c1a == null) ? SubrUtils.getURI(c1a, mesg) : null;
		Desktop dsk;

		SubrUtils.checkTerminated(itr, body, mesg);
		if(!Desktop.isDesktopSupported()) {
			return LispBoolean.FALSE;
		} else if(!(dsk = Desktop.getDesktop()).isSupported(
				Desktop.Action.BROWSE)) {
			return LispBoolean.FALSE;
		} else if(s == null) {
			try {
				dsk.mail();
				return LispBoolean.TRUE;
			} catch (IOException e) {
				throw mesg.getError("err.io");
			}
		} else {
			try {
				dsk.mail(s);
				return LispBoolean.TRUE;
			} catch (IOException e) {
				throw mesg.getError("err.io");
			}
		}
	}

}
