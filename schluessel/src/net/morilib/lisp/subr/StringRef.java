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
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.LispString;

public class StringRef extends BinaryArgs {

	@Override
	protected Datum execute(
			Datum c1a, Datum c2a, Environment env, LispMessage mesg) {
		if(!(c1a instanceof LispString)) {
			//throw new LispException("string required");
			throw mesg.getError("err.require.string", c1a);
		} else if(!(c2a instanceof LispSmallInt)) {
			//throw new LispException("exact small integer required");
			throw mesg.getError("err.require.smallint", c2a);
		}
		
		String str = ((LispString)c1a).getString();
		int ind = ((LispSmallInt)c2a).getExactSmallInt();
		
		if(ind < 0 || ind >= str.length()) {
			//throw new LispException("index out of range");
			throw mesg.getError("err.string.outofrange", c2a);
		} else {
			char c = str.charAt(ind);
			
			return new LispCharacter(c);
		}
	}

}
