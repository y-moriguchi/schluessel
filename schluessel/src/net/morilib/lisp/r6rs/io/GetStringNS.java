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
package net.morilib.lisp.r6rs.io;

import java.io.IOException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.EOFObject;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.subr.QuaternaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.io.UTF16;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/07
 */
public class GetStringNS extends QuaternaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.QuaternaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a, Datum c4a,
			Environment env, LispMessage mesg) {
		int st = SubrUtils.getSmallInt(c3a, mesg);
		int en = SubrUtils.getSmallInt(c4a, mesg) + st;

		if(!(c2a instanceof LispString)) {
			throw mesg.getError("err.require.string", c2a);
		} else if(c1a instanceof ILispTextualInputPort) {
			try {
				int[] b = UTF16.getInts(c2a.getString());
				int l;

				if(st < 0 || st >= b.length) {
					throw mesg.getError("err.range.invalid");
				} else if(en < 0 || en > b.length) {
					throw mesg.getError("err.range.invalid");
				} else if(st > en) {
					throw mesg.getError("err.range.invalid");
				}
				l = ((ILispTextualInputPort)c1a).getChars(b, st, en);
				if(l < 0) {
					return EOFObject.EOF;
				} else {
					((LispString)c2a).setString(UTF16.toString(b));
					return LispInteger.valueOf(l);
				}
			} catch (IOException e) {
				throw mesg.getError("err.io", e.getMessage());
			}
		} else {
			throw mesg.getError("err.io.require.port.textual.input",
					c1a);
		}
	}

}
