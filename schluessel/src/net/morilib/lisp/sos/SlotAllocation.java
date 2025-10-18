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
package net.morilib.lisp.sos;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.Keyword;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.SymbolName;
import net.morilib.lisp.subr.BinaryArgs;

public class SlotAllocation extends BinaryArgs {

	@Override
	protected Datum execute(
			Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		LispClass cls;
		Symbol sym;
		Keyword res;
		
		if(c1a instanceof LispObject) {
			cls = SOS.getInstance().getLispClass(
					((LispObject)c1a).getType());
		} else if(c1a instanceof LispClass) {
			cls = (LispClass)c1a;
		} else {
			return LispBoolean.FALSE;
		}
		
		if(c2a instanceof SymbolName) {
			sym = ((SymbolName)c2a).getSymbol();
		} else {
			throw mesg.getError("err.require.symbol", c2a);
		}
		
		res = cls.getSlotKeyword(sym);
		return (res == null) ? LispBoolean.FALSE : res;
	}

}
