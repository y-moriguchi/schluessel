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

import java.io.IOException;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.uvector.ILispBytevector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/07
 */
public class PutBytevector extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ILispBytevector b;
		ConsIterator itr = new ConsIterator(body);
		Datum d1 = SubrUtils.nextIf(itr, mesg, body);
		Datum d2 = SubrUtils.nextIf(itr, mesg, body);
		int st, en;

		if(!(d1 instanceof ILispBinaryOutputPort)) {
			throw mesg.getError("err.io.require.port.binary.output",
					d1);
		} else if(d2 instanceof ILispBytevector) {
			b  = (ILispBytevector)d2;
			st = SubrUtils.nextSmallInt(itr, 0, mesg);
			en = SubrUtils.nextSmallInt(itr, b.size() - st, mesg) + st;
			SubrUtils.checkTerminated(itr, body, mesg);

			if(st < 0 || st >= b.size()) {
				throw mesg.getError("err.range.invalid");
			} else if(en < 0 || en > b.size()) {
				throw mesg.getError("err.range.invalid");
			} else if(st > en) {
				throw mesg.getError("err.range.invalid");
			}

			try {
				((ILispBinaryOutputPort)d1).putBytes(b.toBytes(),
						st, en);
				return Undef.UNDEF;
			} catch (IOException e) {
				throw mesg.getError("err.io", e.getMessage());
			}
		} else {
			throw mesg.getError("err.uvector.require.8", d2);
		}
	}

}
