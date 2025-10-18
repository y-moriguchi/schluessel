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
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.OutputPort;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;

public class Newline extends Subr {

	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> lst = LispUtils.consToList(body, mesg);
		
		if(lst.size() == 0) {
			//System.out.println();
			//OutputPort.STANDARD.newline();
			OutputPort.getStandard(mesg).newline();
			
			return Undef.UNDEF;
		} else if(lst.size() == 1) {
			if(lst.get(0) instanceof OutputPort) {
				((OutputPort)lst.get(0)).newline();
				return Undef.UNDEF;
			} else {
				throw mesg.getError("err.require.oport", lst.get(0));
				//throw new LispException("output port required");
			}
		} else {
			throw mesg.getError("err.argument", symbolName);
			//throw new LispException(
			//		"newline: wrong number of arguments");
		}
	}

}
