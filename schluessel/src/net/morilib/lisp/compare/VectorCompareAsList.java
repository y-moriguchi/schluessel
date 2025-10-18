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
package net.morilib.lisp.compare;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.ILispVector;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/20
 */
public class VectorCompareAsList extends VectorCompare {

	//
	/*package*/ Datum comparevec(ILispVector x, ILispVector y, Datum p,
			Datum s, Datum r, Environment env, LispMessage mesg) {
		int sx = SRFI67.callSize(s, x, env, mesg);
		int sy = SRFI67.callSize(s, y, env, mesg);

		for(int i = 0; i < sx && i < sy; i++) {
			Datum x0 = SRFI67.callRef(r, x, i, env, mesg);
			Datum y0 = SRFI67.callRef(r, y, i, env, mesg);
			LispInteger c;

			c = SRFI67.callCompare(p, x0, y0, env, mesg);
			if(c.signum() != 0) {
				return c;
			}
		}
		return SRFI67.compareInt(sx, sy);
	}

}
