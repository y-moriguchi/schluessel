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

import net.morilib.lisp.LispCompiler.MiscInfo;

public class SynDelay extends Syntax {
	
	/*package*/ void compile(
			Datum body,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder build,
			boolean toplevel,
			Cons callsym,
			boolean istail,
			LispMessage mesg,
			List<Cons> symlist, CodeExecutor exec, IntStack memento, MiscInfo syncased) {
		if(body instanceof Cons) {
			Cons c = (Cons)body;
			CompiledCode.Builder nbuild = new CompiledCode.Builder();
			Promise prms;
			
			if(c.getCdr() != Nil.NIL) {
				throw mesg.getError("err.delay.malform");
			}
			
			// コンパイル済みコードを構築する
			comp.compile(
					c.getCar(), env, nbuild, callsym, false, symlist,
					exec, memento, syncased);
			nbuild.addReturnOp();
			
			prms = new Promise(nbuild.getCodeRef());
			build.addNewPromise(prms);
		} else {
			//throw new LispException("invalid delay");
			throw mesg.getError("err.delay.malform");
		}
	}
	
	
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg, boolean toplv, int ttype) {
		if(body instanceof Cons) {
			Cons c = (Cons)body;
			Datum ca1;
			
			if(c.getCdr() != Nil.NIL) {
				throw mesg.getError("err.delay.malform");
			}
			
			ca1 = comp.replaceLocalVals(
					c.getCar(), env, ienv, false, ttype);
			return new Cons(ca1, Nil.NIL);
		} else {
			//throw new LispException("invalid delay");
			throw mesg.getError("err.delay.malform");
		}
	}
	
	
	public String toString() {
		return "Syntax:delay";
	}

}
