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
package net.morilib.lisp.lib.srfi013;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/09
 */
public class StringPadRight extends StringPad {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.lib.srfi013.StringPad#execute(java.lang.String, int, int, int, int, net.morilib.lisp.LispMessage)
	 */
	protected Datum execute(
			String s, int le, int ch, int b, int e,
			LispMessage mesg) {
		if(le <= e - b) {
			return new LispString(s.substring(b, b + le));
		} else {
			StringBuilder bf = new StringBuilder();

			bf.append(s.substring(b, e));
			for(int i = e - b; i < le; i++) {
				bf.append((char)ch);
			}
			return new LispString(bf.toString());
		}
	}

}
