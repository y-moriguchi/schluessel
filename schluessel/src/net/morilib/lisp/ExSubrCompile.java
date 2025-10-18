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

import java.util.LinkedList;

import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/06
 */
public class ExSubrCompile extends UnaryArgs
implements ILispDynamicSubr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		CompiledCode.Builder build = new CompiledCode.Builder();
		LispCompiler comp = CompilerFactory.getInstance(mesg);
		CodeExecutor exec = CodeExecutorFactory.getInstance(mesg);
		IntStack     memento = exec.newMemento();
		Datum d = c1a;

		d = comp.expandMacro(d, env, exec, memento);
//		d = MacroUtils72.unquote(d, new EnvironmentObject(env, false));
		comp.compile(d, env, build, false, new Cons(), false,
				new LinkedList<Cons>(), exec, memento,
				new LispCompiler.MiscInfo(null));
		build.addReturnOp();
		return new Closure(
				new ClosureClass(Nil.NIL, build.getCodeRef()),
				env);
	}

}
