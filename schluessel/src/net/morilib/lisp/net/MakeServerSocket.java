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
package net.morilib.lisp.net;

import java.io.IOException;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/16
 */
public class MakeServerSocket extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		int prt = SubrUtils.nextSmallInt(itr, mesg, body);
		Datum d = Iterators.nextIf(itr);

		try {
			SubrUtils.checkTerminated(itr, body, mesg);
			if(prt < 1 || prt > 65535) {
				throw mesg.getError("err.net.port.illegal",
						Integer.toString(prt));
			} else if(d == null) {
				return new LispServerSocket(prt);
			} else if(d instanceof LispInetAddress) {
				return new LispServerSocket(prt, (LispInetAddress)d);
			} else {
				throw mesg.getError("err.net.require.address", d);
			}
		} catch (IOException e) {
			throw mesg.getError("err.io");
		}
	}

}
