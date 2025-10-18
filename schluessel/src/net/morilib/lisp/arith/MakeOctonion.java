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
package net.morilib.lisp.arith;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispOctonion;
import net.morilib.lisp.LispQuaternion;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/20
 */
public class MakeOctonion extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		LispReal d0 = SubrUtils.nextReal(itr, mesg, body);
		LispReal d1 = SubrUtils.nextReal(itr, mesg, body);
		LispReal d2 = SubrUtils.nextReal(itr, mesg, body);
		LispReal d3 = SubrUtils.nextReal(itr, mesg, body);
		LispReal d4 = SubrUtils.nextReal(itr, mesg, body);
		LispReal d5 = SubrUtils.nextReal(itr, mesg, body);
		LispReal d6 = SubrUtils.nextReal(itr, mesg, body);
		LispReal d7 = SubrUtils.nextReal(itr, mesg, body);

		SubrUtils.checkTerminated(itr, body, mesg);
		return LispOctonion.newOctonion(
				LispQuaternion.newQuaternion(d0, d1, d2, d3),
				LispQuaternion.newQuaternion(d4, d5, d6, d7));
	}

}
