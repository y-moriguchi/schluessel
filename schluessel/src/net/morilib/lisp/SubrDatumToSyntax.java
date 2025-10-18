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
package net.morilib.lisp;

import net.morilib.lisp.subr.BinaryArgs;

public class SubrDatumToSyntax extends BinaryArgs {
	
	/*private class Wrap extends Datum implements SyntaxUtils.SafeWrap {
		private Datum wrapee;
		
		private Wrap(Datum w) {
			wrapee = w;
		}
		
		public Datum getWrapee() {
			return wrapee;
		}
	}*/

	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		Datum d1;
		
		//if(c1a instanceof Cons) {
		//	Cons c0 = (Cons)c1a;
		//	
		//	if(c0.getCdr() instanceof Cons) {
		//		Cons c01 = (Cons)c0.getCdr();
		//		
		//		if(c01.getCar() instanceof Cons) {
		//			d1 = ((Cons)c01.getCar()).getCar();
		//		} else {
		//			throw mesg.getError("err.syntax.malform");
		//		}
		//	} else {
		//		throw mesg.getError("err.syntax.malform");
		//	}
		//} else {
		//	throw mesg.getError("err.syntax.malform");
		//}
		//d1 = c1a;
		
		if(c1a instanceof SymbolName) {
			d1 = env.findDatum(((SymbolName)c1a).getSymbol());
		} else {
			throw mesg.getError("err.require.symbol");
		}
		
		if(!(d1 instanceof UserSyntax)) {
			throw mesg.getError("err.require.usersyntax");
		}
		
		if(c2a instanceof SymbolName) {
			SymbolName s = (SymbolName)c2a;
			Datum d = new SymbolScope(
					s.getSymbol(), (UserSyntax)d1, true);
			
			//return new Wrap(d);
			return d;
		} else {
			return c2a;
		}
	}

}
