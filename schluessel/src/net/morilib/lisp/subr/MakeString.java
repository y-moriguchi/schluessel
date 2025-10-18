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
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;

public class MakeString extends Subr {
	
	private String makeString(int len, char ch) {
		StringBuilder b = new StringBuilder();
		
		for(int i = 0; i < len; i++) {
			b.append(ch);
		}
		
		return b.toString();
	}
	
	@Override
	public Datum eval(
			Datum body, Environment env, LispMessage mesg) {
		List<Datum> lst = LispUtils.consToList(body, mesg);
		
		if(lst.size() == 1) {
			int t = SubrUtils.getSmallInt(lst.get(0), mesg);
			
			if(t < 0) {
				throw mesg.getError(
						"err.require.int.nonnegative", lst.get(0));
			}
			return new LispString(makeString(t, ' '));
		} else if(lst.size() == 2) {
			int t = SubrUtils.getSmallInt(lst.get(0), mesg);
			char ch = SubrUtils.getCharacter(lst.get(1), mesg);
			
			if(t < 0) {
				throw mesg.getError(
						"err.require.int.nonnegative", lst.get(0));
			}
			return new LispString(makeString(t, ch));
		} else {
			throw mesg.getError("err.argument", symbolName);
			//throw new LispException("wrong number of arguments");
		}
	}

}
