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

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Nil;

public class Length extends UnaryArgs {

	@Override
	protected Datum execute(
			Datum c1a, Environment env, LispMessage mesg) {
		//BigInteger l = BigInteger.ZERO;
		long l = 0;
		Datum dd = c1a;
		
		while(dd != Nil.NIL) {
			if(dd instanceof Cons) {
				l++;
				dd = ((Cons)dd).getCdr();
			} else {
				throw mesg.getError("err.list", dd);
				//throw new NotProperException("proper list required");
			}
		}
		
		return LispInteger.valueOf(l);
	}

}
