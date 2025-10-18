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

import java.io.IOException;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.file.LispFiles;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/01
 */
public class SaveComponent extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum d = SubrUtils.nextIf(itr, mesg, body);
		Datum f = SubrUtils.nextIf(itr, mesg, body);
		String enc = SubrUtils.nextString(itr, null, mesg);

		SubrUtils.checkTerminated(itr, body, mesg);
		if(d instanceof ILispSavableComponent) {
			try {
				((ILispSavableComponent)d).save(
						LispFiles.getFile(env, f, mesg),
						enc);
				return Undef.UNDEF;
			} catch (IOException e) {
				throw mesg.getError("err.io");
			}
		} else {
			throw mesg.getError("err.swing.require.component.export",
					d);
		}
	}

}
