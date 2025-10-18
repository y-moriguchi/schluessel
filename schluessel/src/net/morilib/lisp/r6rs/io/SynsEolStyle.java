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
import net.morilib.lisp.r6rs.io.transcd.LispEolStyle;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/03
 */
public class SynsEolStyle extends SyntaxSimple {

	//
	private static final Symbol CR = Symbol.getSymbol("cr");
	private static final Symbol LF = Symbol.getSymbol("lf");
	private static final Symbol CRLF = Symbol.getSymbol("crlf");
	private static final Symbol NEL = Symbol.getSymbol("nel");
	private static final Symbol CRNEL = Symbol.getSymbol("crnel");
	private static final Symbol LS = Symbol.getSymbol("ls");
	private static final Symbol NONE = Symbol.getSymbol("none");

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SyntaxSimple#toDatum(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum toDatum(Datum body, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum d = SubrUtils.nextIf(itr, mesg, body);

		SubrUtils.checkTerminated(itr, body, mesg);
		if(d.equals(CR)) {
			return LispEolStyle.CR;
		} else if(d.equals(LF)) {
			return LispEolStyle.LF;
		} else if(d.equals(CRLF)) {
			return LispEolStyle.CRLF;
		} else if(d.equals(NEL)) {
			return LispEolStyle.NEL;
		} else if(d.equals(CRNEL)) {
			return LispEolStyle.CRNEL;
		} else if(d.equals(LS)) {
			return LispEolStyle.LS;
		} else if(d.equals(NONE)) {
			return LispEolStyle.NONE;
		} else {
			return LispEolStyle.NONE;
		}
	}

}
