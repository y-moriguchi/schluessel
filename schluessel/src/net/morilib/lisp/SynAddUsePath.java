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

import java.util.List;

import net.morilib.lisp.CompiledCode.Builder;
import net.morilib.lisp.LispCompiler.MiscInfo;

public class SynAddUsePath extends Syntax {
	
	
	@Override
	void compile(
			Datum body,
			Environment env,
			LispCompiler comp,
			Builder build,
			boolean toplevel,
			Cons callsym,
			boolean istail,
			LispMessage mesg,
			List<Cons> symlist, CodeExecutor exec, IntStack memento, MiscInfo syncased) {
		Symbol usepath = Symbol.getSymbol("*use-path*");
		Datum  pth = env.findDatum(usepath);
		
		if(!toplevel) {
			throw mesg.getError("err.nottoplevel");
		} else if(pth == null) {
			throw mesg.getError("err.unbound", "*use-path*");
		} else if(!(pth instanceof Cons || pth == Nil.NIL)) {
			throw mesg.getError("err.require.list", "*use-path*");
		}
		
		if(body instanceof Cons) {
			Cons c1 = (Cons)body;
			
			if(c1.getCdr() == Nil.NIL) {
				if(c1.getCar() instanceof Symbol) {
					pth = new Cons(c1.getCar(), pth);
					env.bindDatum(usepath, pth);
					build.addPush(Undef.UNDEF);
					return;
				} else {
					throw mesg.getError(
							"err.require.symbol", c1.getCar());
				}
			} else if(c1.getCdr() instanceof Cons) {
				Cons c2 = (Cons)c1.getCdr();
				
				if(c2.getCdr() != Nil.NIL) {
					throw mesg.getError("err.parameter.insufficient");
				} else if(c2.getCar().equals(LispBoolean.TRUE)) {
					if(pth == Nil.NIL) {
						pth = new Cons(c1.getCar(), Nil.NIL);
					} else {
						pth = LispUtils.nconc(
								(Cons)pth,
								new Cons(c1.getCar(), Nil.NIL));
					}
					env.bindDatum(usepath, pth);
					build.addPush(Undef.UNDEF);
					return;
				} else {
					pth = new Cons(c1.getCar(), pth);
					env.bindDatum(usepath, pth);
					build.addPush(Undef.UNDEF);
					return;
				}
			}
		}
		throw mesg.getError("err.parameter.insufficient");
	}

	@Override
	Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg, boolean toplv, int ttype) {
		return body;
	}

}
