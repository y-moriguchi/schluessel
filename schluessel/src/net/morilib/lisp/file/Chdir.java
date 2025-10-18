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
package net.morilib.lisp.file;

import java.io.File;
import java.io.IOException;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/24
 */
public class Chdir extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		String s = SubrUtils.nextString(itr, null, mesg);

		if(itr.hasNext()) {
			throw mesg.getError("err.argument", body);
		} else if(s == null) {
			return new LispString(LispFiles.pwd(env).toString());
		} else {
			try {
				File f = LispFiles.getFile(env, s);

				if(!f.isDirectory()) {
					throw mesg.getError("err.file.notdirectory", s);
				}
				LispFiles.chdir = f.toString();
				return Undef.UNDEF;
			} catch (IOException e) {
				throw mesg.getError("err.io", e.getMessage());
			}
		}
	}

}
