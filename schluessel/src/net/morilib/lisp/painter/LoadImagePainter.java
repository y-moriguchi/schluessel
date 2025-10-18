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

import java.io.IOException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.file.LispFiles;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/12
 */
public class LoadImagePainter extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(
			Datum c1a, Environment env, LispMessage mesg) {
		if(!(c1a instanceof LispString)) {
			throw mesg.getError("err.require.string", c1a);
		}

		try {
			return new FileImagePainter(LispFiles.getFile(
					env, c1a.getString()).toString());
		} catch (IOException e) {
			throw mesg.getError("err.io", e.getMessage());
		}
	}

}
