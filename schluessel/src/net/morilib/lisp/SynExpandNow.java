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
import net.morilib.lisp.subr.SubrUtils;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/01
 */
public class SynExpandNow extends Syntax implements MacroDefinition {

	//
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
		CompiledCode.Builder nbuild = new CompiledCode.Builder();
		ConsIterator itr = new ConsIterator(body);
		Datum d;

		d = SubrUtils.nextIf(itr, mesg, "err.argument", body);
		SubrUtils.checkTerminated(itr, body, mesg);
		comp.compile(d, env, nbuild, false,
				callsym, istail, symlist, exec, memento, syncased);
		nbuild.addReturnOp();
		comp.compile(exec.exec(nbuild.getCodeRef(), env, memento),
				env, build, toplevel, callsym, istail, symlist, exec,
				memento, syncased);
	}
	
	//
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv, LispMessage mesg, boolean toplv, int ttype) {
		//throw new LispException(
		//		"define-macro can not use in define-syntax");
		throw mesg.getError("err.definemacro.definesyntax");
	}
	
}
