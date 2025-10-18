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

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.SyntaxSimple;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/02
 */
public class SynsFileOptions extends SyntaxSimple {

	//
	private static final Symbol SYM_NO_CREATE =
		Symbol.getSymbol("no-create");
	private static final Symbol SYM_NO_FAIL =
		Symbol.getSymbol("no-fail");
	private static final Symbol SYM_NO_TRUNCATE =
		Symbol.getSymbol("no-truncate");

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SyntaxSimple#toDatum(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum toDatum(Datum body, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		int opts = 0;
		Datum d;

		while(itr.hasNext()) {
			if((d = itr.next()).equals(SYM_NO_CREATE)) {
				opts |= LispFileOptions.NO_CREATE;
			} else if(d.equals(SYM_NO_FAIL)) {
				opts |= LispFileOptions.NO_FAIL;
			} else if(d.equals(SYM_NO_TRUNCATE)) {
				opts |= LispFileOptions.NO_TRUNCATE;
			}
		}
		SubrUtils.checkTerminated(itr, body, mesg);
		return new LispFileOptions(opts);
	}

}
