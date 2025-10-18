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
package net.morilib.lisp.exlib;

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Subr;
import net.morilib.lisp.SymbolName;

public class GetDefaultMessage extends Subr {

	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		Datum c1a, c2a = Nil.NIL;
		
		if(body instanceof Cons) {
			Cons c1 = (Cons)body;
			
			c1a = c1.getCar();
			if(c1.getCdr() instanceof Cons) {
				Cons c2 = (Cons)c1.getCdr();
				
				c2a = c2.getCar();
				if(c2.getCdr() != Nil.NIL) {
					throw mesg.getError("err.parameter.insufficient");
				}
			} else if(c1.getCdr() != Nil.NIL) {
				throw mesg.getError("err.list");
			}
		} else {
			throw mesg.getError("err.parameter.insufficient");
		}
		
		if(c1a instanceof SymbolName) {
			String rmsg;
			
			rmsg = mesg.get(((SymbolName)c1a).getName());
			if(c2a != Nil.NIL) {
				rmsg += ":" + LispUtils.print(c2a);
			}
			return new LispString(rmsg);
		} else {
			throw mesg.getError("err.require.symbol", c1a);
		}
	}

}
