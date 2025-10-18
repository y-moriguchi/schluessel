/*
 * Copyright 2009-2010 Yuichiro Moriguchi
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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/02
 */
public abstract class SyntaxSimple extends Syntax {

	/**
	 * 
	 * @param body
	 * @param mesg
	 */
	public abstract Datum toDatum(Datum body, LispMessage mesg);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Syntax#compile(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispCompiler, net.morilib.lisp.CompiledCode.Builder, boolean, net.morilib.lisp.Cons, boolean, net.morilib.lisp.LispMessage, java.util.List, net.morilib.lisp.CodeExecutor, net.morilib.lisp.IntStack, net.morilib.lisp.LispCompiler.MiscInfo)
	 */
	@Override
	/*package*/ void compile(Datum body, Environment env,
			LispCompiler comp, Builder build, boolean toplevel,
			Cons callsym, boolean istail, LispMessage mesg,
			List<Cons> symlist, CodeExecutor exec, IntStack memento,
			MiscInfo syncased) {
		build.addPush(toDatum(body, mesg));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Syntax#replaceLocalVals(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispCompiler, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage, boolean, int)
	 */
	@Override
	/*package*/ Datum replaceLocalVals(Datum body, Environment env,
			LispCompiler comp, Environment ienv, LispMessage mesg,
			boolean toplv, int ttype) {
		return body;
	}

}
