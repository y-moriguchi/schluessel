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

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispRational;

public class Truncate extends UnaryArgs {

	@Override
	protected Datum execute(
			Datum c1a, Environment env, LispMessage mesg) {
		if(c1a instanceof LispInteger) {
			return c1a;
		} else if(c1a instanceof LispRational) {
			LispRational q = (LispRational)c1a;
			
			return LispInteger.valueOf(
					q.getNumerator().divide(q.getDenominator()));
		} else if(c1a instanceof LispDouble) {
			double d = ((LispDouble)c1a).doubleValue();
			
			if(d > 0) {
				return new LispDouble(Math.floor(d));
			} else {
				return new LispDouble(Math.ceil(d));
			}
		}
		//throw new LispException("real number required");
		throw mesg.getError("err.require.real", c1a);
	}

}
