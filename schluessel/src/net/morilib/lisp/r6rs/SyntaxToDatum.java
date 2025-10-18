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
package net.morilib.lisp.r6rs;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.SymbolName;
import net.morilib.lisp.subr.UnaryArgs;

public class SyntaxToDatum extends UnaryArgs {
	
	//
	private static Datum removeScope1(Datum d) {
		if(d instanceof SymbolName) {
			return ((SymbolName)d).getSymbol();
		}
		return d;
	}
	
	@Override
	protected Datum execute(
			Datum c1a, Environment env, LispMessage mesg) {
		return removeScope1(c1a);
	}

}
