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
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;

public class StringToNumber extends Subr {
	
	private Datum process(Datum d, int radix, LispMessage mesg) {
		if(d instanceof LispString) {
			LispNumber n = LispNumber.parse(
					((LispString)d).getString(), radix);
			
			return (n != null) ? n : LispBoolean.FALSE;
		} else {
			//throw new LispException("string required");
			throw mesg.getError("err.require.string", d);
		}
	}
	
	public Datum eval(
			Datum body, Environment env, LispMessage mesg) {
		List<Datum> lst = LispUtils.consToList(body, mesg);
		
		if(lst.size() == 1) {
			return process(lst.get(0), 10, mesg);
		} else if(lst.size() == 2) {
			Datum d = lst.get(1);
			
			if(d instanceof LispNumber) {
				LispNumber n = (LispNumber)d;
				
				if(!n.isInteger()) {
					//throw new LispException("integer required");
					throw mesg.getError("err.require.smallint", n);
				} else {
					int ii = SubrUtils.getSmallInt(n, mesg);
					
					if(ii < 2 || ii > 16) {
						throw mesg.getError("err.radix.invalid");
					} else {
						return process(lst.get(0), ii, mesg);
					}
				}
			} else {
				//throw new LispException("integer required");
				throw mesg.getError("err.require.smallint", d);
			}
		} else {
			//throw new LispException("wrong number of arguments");
			throw mesg.getError("err.argument", symbolName);
		}
	}

}
