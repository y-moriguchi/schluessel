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
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.util.Strings;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/10
 */
public class StringContains extends String2Base {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.lib.srfi013.String2Base#execute(java.lang.String, java.lang.String, int, int, int, int, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(
			String s1, String s2, int b1, int e1, int b2,
			int e2, LispMessage mesg) {
		int r = Strings.contains(s1, s2, b1, e1, b2, e2);

		return r >= 0 ? LispInteger.valueOf(r) : LispBoolean.FALSE;
	}

}
