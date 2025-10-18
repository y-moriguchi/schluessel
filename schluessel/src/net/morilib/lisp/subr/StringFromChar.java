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

import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;

public class StringFromChar extends Subr {

	@Override
	public Datum eval(
			Datum body, Environment env, LispMessage mesg) {
		StringBuilder b = new StringBuilder();
		List<Datum> lst = LispUtils.consToList(body, mesg);
		
		for(int i = 0; i < lst.size(); i++) {
			if(lst.get(i) instanceof LispCharacter) {
				LispCharacter lc = (LispCharacter)lst.get(i);
				
				b.append(lc.getCharacter());
			} else {
				//throw new LispException("character required");
				throw mesg.getError("err.require.char", lst.get(i));
			}
		}
		
		return new LispString(b.toString());
	}

}
