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
package net.morilib.lisp.net.ftp;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.SyntaxSimple;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/03
 */
public class SynsFtpTransferMode extends SyntaxSimple {

	//
	private static final Symbol BINARY = Symbol.getSymbol("binary");
	private static final Symbol ASCII  = Symbol.getSymbol("ascii");

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SyntaxSimple#toDatum(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum toDatum(Datum body, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum d = SubrUtils.nextIf(itr, mesg, body);

		SubrUtils.checkTerminated(itr, body, mesg);
		if(d.equals(BINARY)) {
			return LispFTPTransferMode.BINARY;
		} else if(d.equals(ASCII)) {
			return LispFTPTransferMode.ASCII;
		} else {
			throw mesg.getError("err.net.ftpmode.invalid", d);
		}
	}

}
