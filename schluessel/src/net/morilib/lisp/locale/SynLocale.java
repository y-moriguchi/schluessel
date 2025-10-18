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
package net.morilib.lisp.locale;

import java.util.Locale;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.SyntaxSimple;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/21
 */
public class SynLocale extends SyntaxSimple {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SyntaxSimple#toDatum(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum toDatum(Datum body, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		String la = SubrUtils.nextSymbolName(itr, mesg, body);
		String cn = SubrUtils.nextSymbolName(itr, null, mesg);
		String va = SubrUtils.nextSymbolName(itr, null, mesg);
		Locale lc;

		SubrUtils.checkTerminated(itr, body, mesg);
		if(cn == null) {
			lc = new Locale(la);
		} else if(va == null) {
			lc = new Locale(la, cn);
		} else {
			lc = new Locale(la, cn, va);
		}
		return new LispLocale(lc);
	}

}
