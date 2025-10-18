/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp.subr;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Subr;
import net.morilib.lisp.math.algebra.ILispNumberEqual;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class NumEqual extends Subr {

//	@Override
//	protected boolean compare(
//			LispNumber c1a, LispNumber c2a, LispMessage mesg) {
//		return c1a.isEqualTo(c2a);
//	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		ILispNumberEqual p = null;
		Datum d = Nil.NIL;

		while(itr.hasNext()) {
			try {
				if(!((d = itr.next()) instanceof ILispNumberEqual)) {
					throw mesg.getError("err.require.number", d);
				} else if(p != null && !p.isEqualTo(d)) {
					return LispBoolean.FALSE;
				} else {
					p = (ILispNumberEqual)d;
				}
			} catch(ClassCastException e) {
				throw mesg.getError("err.math.mismatch", d);
			}
		}
		SubrUtils.checkTerminated(itr, body, mesg);
		return LispBoolean.TRUE;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#toString()
	 */
	public String toString() {
		return "Subr:=";
	}

}
