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
package net.morilib.lisp.array;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.subr.TernaryArgs;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/22
 */
public class ShareArray extends TernaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
			Environment env, LispMessage mesg) {
		if(!(c1a instanceof ILispArray)) {
			throw mesg.getError("err.srfi25.require.array", c1a);
		} else if(!(c2a instanceof LispArrayShape)) {
			throw mesg.getError("err.srfi25.require.shape", c2a);
		} else if(!(c3a instanceof Procedure)) {
			throw mesg.getError("err.require.procedure", c3a);
		} else {
			return new LispSharedArray((ILispArray)c1a,
					(LispArrayShape)c2a, (Procedure)c3a, env,
					mesg);
		}
	}

}