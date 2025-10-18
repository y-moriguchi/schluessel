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

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class SynSetS extends Syntax {

	//
	private Datum getSetval(Datum bcdr, LispMessage mesg) {
		if(bcdr instanceof Cons) {
			Cons c1 = (Cons)bcdr;

			if(c1.getCdr() != Nil.NIL) {
				throw mesg.getError("err.set.malform");
			}
			return c1.getCar();
		} else {
			throw mesg.getError("err.set.malform");
		}
	}

	//
	private Datum replaceSetval(
			Datum bcar,
			Datum bcdr,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg,
			int ttype) {
		if(bcdr instanceof Cons) {
			Datum d1 = ((Cons)bcdr).getCar();
			Cons r1 = new Cons();
			Cons r2 = new Cons();
			//Datum fd = ienv.findDatum(bcar);

			// replace if a symbol is in the replacement list
			//if(fd != null) {
			//	bcar = fd;
			//}

			//r1.setCar(bcar);
			r1.setCar(comp.replaceLocalVals(
					bcar, env, ienv, false, ttype));
			r1.setCdr(r2);
			r2.setCar(comp.replaceLocalVals(
					d1, env, ienv, false, ttype));
			return r1;
		} else {
			throw mesg.getError("err.set.malform");
		}
	}

	//
	private UserSyntax isSetMacro(Datum bcar, Environment env) {
		Datum b;

		if(bcar instanceof Symbol) {
			b = env.findDatum(bcar);
			if(b instanceof UserIdentifierSyntax &&
					((UserIdentifierSyntax)b).toset != null) {
				return ((UserIdentifierSyntax)b).toset;
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Syntax#compile(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispCompiler, net.morilib.lisp.CompiledCode.Builder, boolean, net.morilib.lisp.Cons, boolean, net.morilib.lisp.LispMessage, java.util.List, net.morilib.lisp.CodeExecutor, net.morilib.lisp.IntStack, net.morilib.lisp.LispCompiler.MiscInfo)
	 */
	void compile(
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
			UserSyntax uis;
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();
			Datum b2;

			if((uis = isSetMacro(bcar, env)) != null) {
				b2 = comp.expandSyntax(
						uis, getSetval(bcdr, mesg), env, false);
				comp.compile(b2, env, build, toplevel, callsym, istail,
						symlist, exec, memento, syncased);
			} else if(bcar instanceof SymbolName) {
				// constant definition
				// create a temporary Closure
				CompiledCode.Builder mbuild = new CompiledCode.Builder();
				Environment  menv = new Environment(env);

				comp.compile(
						getSetval(bcdr, mesg),
						menv, mbuild,
						callsym, false, symlist,
						exec, memento, syncased);
				mbuild.addReturnOp();

				// create a temporary Closure
				ClosureClass cl = new ClosureClass(
						Nil.NIL, mbuild.getCodeRef());

				// call the temporary Closure
				build.addPush(cl);
				build.addBeginList();
				build.addEndList();
				build.addCall();

				build.addSet(bcar);
				build.addPop();
				build.addPush(Undef.UNDEF);
			} else if(bcar instanceof Cons) {
				// SRFI-17
				Cons c1 = (Cons)bcar;

				build.addReferSetter(c1.getCar());
				build.addBeginList();
				SyntaxUtils.compileListApply(
						c1.getCdr(), env, comp, build,
						callsym, mesg, symlist,
						exec, memento, syncased);
				comp.compile(
						getSetval(bcdr, mesg),
						env, build,
						callsym, false, symlist,
						exec, memento, syncased);
				build.addAppendList();
				build.addEndList();
				build.addCall();
				build.addPop();
				build.addPush(Undef.UNDEF);
			} else {
				throw mesg.getError("err.set.name");
			}
		} else {
			throw mesg.getError("err.set.malform");
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Syntax#replaceLocalVals(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispCompiler, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage, boolean, int)
	 */
	Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg,
			boolean toplv,
			int ttype) {
		if(body instanceof Cons) {
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();

			if(bcar instanceof SymbolName) {
				// define a constant
				return replaceSetval(
						bcar, bcdr, env, comp, ienv, mesg, ttype);
			} else if(bcar instanceof Cons) {
				// SRFI-17
				return replaceSetval(
						bcar, bcdr, env, comp, ienv, mesg, ttype);
			} else {
				throw mesg.getError("err.set.name");
			}
		} else {
			throw mesg.getError("err.set.malform");
		}
	}

}
