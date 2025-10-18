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
import net.morilib.lisp.LispString;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.range.integer.IntInterval;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/10
 */
public class ToCharSet extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		if(c1a instanceof LispString) {
			String s = c1a.getString();
			LispCharSetBuilder b = new LispCharSetBuilder(null);
			int c;

			for(int i = 0; i < s.length();) {
				c = s.codePointAt(i);
				b.add(c);
				i += (c > Character.MAX_SURROGATE) ? 2 : 1;
			}
			return b.toCharSet();
		} else if(c1a instanceof LispCharacter) {
			char c = c1a.getCharacter();

			return new LispCharSet(new IntInterval(c, c));
		} else if(c1a instanceof LispCharSet) {
			return c1a;
		} else {
			throw mesg.getError(
					"err.charset.require.stringcharcharset", c1a);
		}
	}

}
