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

import net.morilib.automata.nfa.NFABuildException;
import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Nil;
import net.morilib.lisp.ParserSharpSyntax;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;

public class DefineSharpSyntax extends Subr {

	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		Datum bl = LispBoolean.FALSE;
		ParserSharpSyntax pss = ParserSharpSyntax.getInstance();
		
		if(body instanceof Cons) {
			Cons c1 = (Cons)body;
			
			if(c1.getCdr() instanceof Cons) {
				Cons c2 = (Cons)c1.getCdr();
				
				if(c2.getCdr() instanceof Cons) {
					Cons c3 = (Cons)c2.getCdr();
					
					bl = c3.getCar();
					if(c3.getCdr() != Nil.NIL) {
						throw mesg.getError("err.argument");
					}
				} else if(c2.getCdr() != Nil.NIL) {
					throw mesg.getError("err.argument");
				}
				
				if(!(c1.getCar() instanceof LispString)) {
					throw mesg.getError(
							"err.require.string",
							c1.getCar());
				}
				
				try {
					pss.addRule(
							((LispString)c1.getCar()).getString(),
							c2.getCar(), bl.isTrue(), true);
					return Undef.UNDEF;
				} catch(NFABuildException e) {
					throw mesg.getError(
							"err.pattern.syntax", c1.getCar());
				}
			}
		}
		throw mesg.getError("err.argument");
	}

}
