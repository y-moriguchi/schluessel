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

import net.morilib.lisp.LispMessage;
import net.morilib.util.Strings;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/10
 */
public class StringCiGt extends StringCompare2 {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.lib.srfi013.StringCompare2#execute(java.lang.String, java.lang.String, int, int, int, int, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected boolean compare(
			String s, String t, int bs, int es, int bt,
			int et, LispMessage mesg) {
		return Strings.compareToIgnoreCase(s, t, bs, es, bt, et) > 0;
	}

}
