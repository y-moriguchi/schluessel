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
package net.morilib.lisp.charset;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/09
 */
public class CharSetRef extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		if(!(c1a instanceof LispCharSet)) {
			throw mesg.getError("err.charset.require.charset", c1a);
		} else if(c2a instanceof LispCharSetCursor) {
			if(((LispCharSetCursor)c2a).cur < 0) {
				throw mesg.getError("err.charset.endofcursor");
			}
			return LispCharacter.valueOf(
					((LispCharSetCursor)c2a).cur);
		} else {
			throw mesg.getError("err.charset.require.charsetcursor",
					c2a);
		}
	}

}
