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
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;

public abstract class StringCompare extends Subr {
	
	protected abstract boolean compare(LispString c1a, LispString c2a);
	
	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment)
	 */
	public Datum eval(
			Datum body,
			Environment env,
			LispMessage mesg) {
		List<Datum> lst = LispUtils.consToList(body, mesg);
		
		if(lst.size() < 2) {
			//throw new LispException("more than 2 arguments required");
			throw mesg.getError("err.argument", symbolName);
		} else if(!(lst.get(0) instanceof LispString)) {
			//throw new LispException("string required");
			throw mesg.getError("err.require.string", lst.get(0));
		}
		
		LispString prev = (LispString)lst.get(0);
		for(int i = 1; i < lst.size(); i++) {
			Datum d = lst.get(i);
			
			if(d instanceof LispString) {
				if(compare(prev, (LispString)d)) {
					prev = (LispString)d;
				} else {
					return LispBoolean.FALSE;
				}
			} else {
				//throw new LispException("string required");
				throw mesg.getError("err.require.string", d);
			}
		}
		return LispBoolean.TRUE;
	}
	
}
