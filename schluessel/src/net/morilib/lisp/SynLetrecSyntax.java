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
public class SynLetrecSyntax extends SynLetSyntaxBase
implements SynLetType {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SynLetSyntaxBase#getName()
	 */
	String getName() {
		return "letrec-syntax";
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.SynLetSyntaxBase#getError(net.morilib.lisp.LispMessage)
	 */
	LispException getError(LispMessage mesg) {
		return mesg.getError("err.letrecsyntax.malform");
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
			CompiledCode.Builder nbuild = new CompiledCode.Builder();
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();
			List<Datum> lvars = new ArrayList<Datum>();
			List<Datum> lvals = new ArrayList<Datum>();
			Cons bnam = new Cons();

			// create a temporary environment
			Environment  nenv = new Environment(env);

			// define a local variable
			if(bcar instanceof Cons) {
				Datum d = bcar;

				// evaluate the local variable
				while(d != Nil.NIL) {
					if(d instanceof Cons) {
						List<Datum> l2;

						l2 = LispUtils.consToList(
								((Cons)d).getCar(), mesg);

						if(l2.size() != 2) {
							throw getError(mesg);
						}

						// create a pair of
						// the local variable and value
						lvars.add(l2.get(0));
						lvals.add(l2.get(1));

						d = ((Cons)d).getCdr();
					} else {
						throw getError(mesg);
					}
				}
			} else {
				throw getError(mesg);
			}

			// bind a local syntax
			for(int i = 0; i < lvars.size(); i++) {
				//UserSyntax usyn = SyntaxUtils.processRuleDesc(
				//		name, lvals.get(i), nenv, true, mesg);
				Symbol name;
				UserSyntax usyn;
				Datum lbin;

				if(!(lvars.get(i) instanceof Symbol)) {
					throw mesg.getError("err.letrecsyntax.name");
				}

				name = (Symbol)lvars.get(i);
				if(SyntaxUtils.isSyntaxRules(lvals.get(i))) {
					lbin = usyn = SyntaxUtils.processRuleDesc(
							(Symbol)name, lvals.get(i), nenv,
							mesg, null);
				} else if(SyntaxUtils.isIdentifierSyntax(
						lvals.get(i))) {
					lbin = SyntaxUtils.processIdentifierSyntax(
							(Symbol)name, lvals.get(i), env, mesg);
				} else {
					// compile the sequence(list)
					CompiledCode.Builder mbuild =
						new CompiledCode.Builder();
					Environment menv = new Environment(nenv);
					Datum dz;

					//syncased.depth = 0;   // init depth
					lbin = usyn = new UserSyntax(
							((Symbol)name).getName(),
							env,
							mbuild.getCodeRef());
					dz = PatternMatch.appendScopeCase(
							lvals.get(i), usyn);
					comp.compile(
							dz, menv, nbuild,
							new Cons(), false, symlist,
							exec, memento, syncased);
					mbuild.addReturnOp();
				}

				// define the syntax
				nenv.bindDatum(name, lbin);

				// bind runtime environment later
				nbuild.addPush(lbin);
				nbuild.addBind(name);
			}

			// compile the list
			bnam.setCdr(LispUtils.listToCons(lvars));
			symlist.add(callsym);
			SyntaxUtils.compileList(
					bcdr, nenv, comp, nbuild,
					bnam, istail, mesg, symlist,
					exec, memento, syncased);
			symlist.remove(0);
			nbuild.addReturnOp();

			// create a temporary Closure
			ClosureClass cl = new ClosureClass(
					Nil.NIL, nbuild.getCodeRef());

			// call the temporary Closure
			build.addPush(cl);
			build.addBeginList();
			build.addEndList();
			build.addCall();
		} else {
			throw getError(mesg);
		}
	}

}
