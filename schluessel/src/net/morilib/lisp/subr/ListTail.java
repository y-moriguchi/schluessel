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
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;

public class ListTail extends BinaryArgs {

	@Override
	protected Datum execute(
			Datum c1a, Datum c2a, Environment env, LispMessage mesg) {
		if(!(c2a instanceof LispSmallInt)) {
			throw mesg.getError("err.require.smallint", c2a);
			//throw new LispException("small integer required");
		}
		
		int ind = ((LispSmallInt)c2a).getExactSmallInt();
		if(ind < 0) {
			throw mesg.getError("err.list.outofrange", c2a);
			//throw new LispException("argument out of range");
		}
		
		int l = 0;
		Datum dd = c1a;
		while(dd instanceof Cons) {
			if(l == ind) {
				return dd;
			}
			l++;
			dd = ((Cons)dd).getCdr();
		}
		
		if(l == ind) {
			return dd;
		} else {
			throw mesg.getError("err.list.outofrange", c2a);
			//throw new LispException("argument out of range");
		}
	}

}
