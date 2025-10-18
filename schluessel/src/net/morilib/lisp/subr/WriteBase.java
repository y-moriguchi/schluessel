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


public abstract class WriteBase extends Subr {
	
	
	protected abstract void action(
			OutputPort outp, Datum d, LispMessage mesg);
	
	
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> lst = LispUtils.consToList(body, mesg);
		OutputPort outp;
		
		if(lst.size() < 1 || lst.size() > 2) {
			//throw new LispException("bad argument");
			throw mesg.getError("err.argument", symbolName);
		}
		
		if(lst.size() == 2) {
			if(lst.get(1) instanceof OutputPort) {
				outp = (OutputPort)lst.get(1);
			} else {
				//throw new LispException("output port required");
				throw mesg.getError("err.require.oport", lst.get(1));
			}
		} else {
			//outp = OutputPort.STANDARD;
			outp = OutputPort.getStandard(mesg);
		}
		
		action(outp, lst.get(0), mesg);
		return Undef.UNDEF;
	}

}
