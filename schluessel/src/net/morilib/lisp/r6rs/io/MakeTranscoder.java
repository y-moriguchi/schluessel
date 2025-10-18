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
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.r6rs.io.transcd.ILispCodec;
import net.morilib.lisp.r6rs.io.transcd.LispEolStyle;
import net.morilib.lisp.r6rs.io.transcd.LispErrorHandlingMode;
import net.morilib.lisp.r6rs.io.transcd.LispTranscoder;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/03
 */
public class MakeTranscoder extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum d1 = SubrUtils.nextIf(itr, mesg, body);
		Datum d2 = Iterators.nextIf(itr, LispEolStyle.getNative());
		Datum d3 = Iterators.nextIf(itr,
				LispErrorHandlingMode.REPLACE);

		SubrUtils.checkTerminated(itr, body, mesg);
		if(!(d1 instanceof ILispCodec)) {
			throw mesg.getError("err.io.require.codec", d1);
		} else if(!(d2 instanceof LispEolStyle)) {
			throw mesg.getError("err.io.require.eolstyle", d2);
		} else if(!(d3 instanceof LispErrorHandlingMode)) {
			throw mesg.getError("err.io.require.errormode", d3);
		} else {
			return new LispTranscoder((ILispCodec)d1, (LispEolStyle)d2,
					(LispErrorHandlingMode)d3);
		}
	}

}
