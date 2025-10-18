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

import java.util.ArrayList;
import java.util.List;

import net.morilib.lisp.LispCompiler.MiscInfo;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class SynDefineMacro72 extends Syntax
implements MacroDefinition {

	//
	private static final Symbol LAMBDA = Symbol.getSymbol("lambda");

//	//
//	private void defun(
//			Datum bcar,
//			Datum bcdr,
//			Environment env,
//			LispCompiler comp,
//			CompiledCode.Builder build,
//			Cons callsym,
//			LispMessage mesg,
//			List<Cons> symlist,
//			boolean toplevel,
//			CodeExecutor exec,
//			IntStack memento,
//			LispCompiler.MiscInfo syncased) {
//		// function definition
//		CompiledCode.Builder nbuild = new CompiledCode.Builder();
//		Cons c = (Cons)bcar;
//		Environment nenv = new Environment(env);
//
//		// compile the list
//		//symlist.add(callsym);
//		SyntaxUtils.compileList(
//				bcdr, nenv, comp, nbuild, c, true, mesg,
//				new ArrayList<Cons>(),
//				exec, memento, syncased);
//		//symlist.remove(0);
//
//		nbuild.addReturnOp();
//		ClosureClass cl = new ClosureClass(
//				c.getCdr(), nbuild.getCodeRef());
//
//		build.addPush(cl);
//		build.addBindMacro72(c.getCar());
//		build.addPush(Undef.UNDEF);
//	}

	//
	/*package*/ void compile(
			Datum body0,
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
		if(!toplevel) {
			throw mesg.getError("err.nottoplevel");
		} else if(body0 instanceof Cons) {
//			Datum body = comp.expandMacro(body0, env, exec, memento);
			Datum body = body0;
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();

//			bcdr = comp.expandMacro(bcdr, env, exec, memento);
			if(bcar instanceof Cons) {
				if(!(((Cons)bcar).getCar() instanceof Symbol)) {
					throw mesg.getError("err.definemacro.malform");
				}

				bcdr = LispUtils.listDot(bcdr,
						LAMBDA, ((Cons)bcar).getCdr());
				SyntaxUtils.compileBindMacro(
						((Cons)bcar).getCar(),
						LispUtils.list(bcdr),
						env, comp, build, callsym,
						mesg, "err.definemacro.malform",
						new ArrayList<Cons>(),
						exec, memento, syncased);
			} else if(bcar instanceof SymbolName) {
				//Cons  l0 = IntLispUtils.extractLambda(bcdr, bcar);
				//Datum lt = IntLispUtils.extractLambdaList(bcdr);

				// constant definition
				SyntaxUtils.compileBindMacro72(
						bcar, bcdr, env, comp, build, callsym,
						mesg, "err.definemacro.malform",
						new ArrayList<Cons>(),
						exec, memento, syncased);
			} else {
				//throw new LispException("Type mismatch");
				throw mesg.getError("err.definemacro.malform");
			}
		} else {
			//throw new LispException("Wrong arguments");
			throw mesg.getError("err.definemacro.malform");
		}
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
