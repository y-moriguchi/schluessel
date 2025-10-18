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

public class SynSyntax extends Syntax {
	
	private static class Wrap extends Datum
	implements SyntaxUtils.SafeWrap {
		
		private Datum wrapee;
		
		private Wrap(Datum w) {
			wrapee = w;
		}
		
		public Datum getWrapee() {
			return wrapee;
		}
	}
	
	
	public String toString() {
		return "Syntax:syntax";
	}

	/*package*/ void compile(
			Datum body,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder build,
			boolean toplevel,
			Cons callsym,
			boolean istail,
			LispMessage mesg,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento,
			MiscInfo syncased) {
		if(body instanceof Cons) {
			build.addPush(((Cons)body).getCar());
		} else {
			//throw new LispException("invalid quote");
			throw mesg.getError("err.syntax.malform");
		}
		//build.addPush(body);
	}
	
	
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg,
			boolean toplv, int ttype) {
		if(ttype == 0) {
			Cons r0 = new Cons();
			Cons r1 = new Cons();
			//Cons r2 = new Cons();
			
			r0.setCar(this);
			r0.setCdr(r1);
			//r1.setCar(r2);
			r1.setCar(body);
			return new Wrap(r0);
			//return body;
		} else if(ttype == 1 || ttype == 2) {
			return comp.replaceLocalVals(
					body, env, ienv, toplv, ttype);
		} else {
			throw new RuntimeException();
		}
		//return comp.replaceLocalVals(body, env, ienv, toplv, ttype);
		//return body;
	}
	
}
