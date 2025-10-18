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
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;

public class Substring extends Subr {

	@Override
	public Datum eval(
			Datum body, Environment env, LispMessage mesg) {
		List<Datum> lst = LispUtils.consToList(body, mesg);
		
		if(lst.size() != 3) {
			//throw new LispException("wrong number of arguments");
			throw mesg.getError("err.argument", symbolName);
		} else if(!(lst.get(0) instanceof LispString)) {
			//throw new LispException("string required");
			throw mesg.getError("err.require.string", lst.get(0));
		} else if(!(lst.get(1) instanceof LispSmallInt)) {
			//throw new LispException("exact small number required");
			throw mesg.getError("err.require.smallint", lst.get(1));
		} else if(!(lst.get(2) instanceof LispSmallInt)) {
			//throw new LispException("exact small number required");
			throw mesg.getError("err.require.smallint", lst.get(2));
		}
		
		String str = ((LispString)lst.get(0)).getString();
		int inds = ((LispSmallInt)lst.get(1)).getExactSmallInt();
		int inde = ((LispSmallInt)lst.get(2)).getExactSmallInt();
		
		if(inds < 0 || inds >= str.length()) {
			//throw new LispException("index out of range");
			throw mesg.getError("err.string.outofrange", lst.get(1));
		} else if(inde < 0 || inde > str.length()) {
			//throw new LispException("index out of range");
			throw mesg.getError("err.string.outofrange", lst.get(2));
		} else if(inde < inds) {
			//throw new LispException("wrong range");
			//throw mesg.getError("err.range.invalid", symbolName);
			return new LispString("");
		} else {
			return new LispString(str.substring(inds, inde));
		}
	}

}
