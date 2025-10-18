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
public class SynDefineSyntax extends Syntax
implements MacroDefinition {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Syntax#compile(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispCompiler, net.morilib.lisp.CompiledCode.Builder, boolean, net.morilib.lisp.Cons, boolean, net.morilib.lisp.LispMessage, java.util.List, net.morilib.lisp.CodeExecutor, net.morilib.lisp.IntStack, net.morilib.lisp.LispCompiler.MiscInfo)
	 */
	@Override
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
		if(!(body instanceof Cons)) {
			throw mesg.getError("err.definesyntax.malform");
		}

		Cons b1 = (Cons)body;
		Datum name = b1.getCar();

		if(!(name instanceof Symbol)) {
			throw mesg.getError("err.definesyntax.name", name);
		}

		if(b1.getCdr() instanceof Cons) {
			Cons b2 = (Cons)b1.getCdr();
			UserSyntax usyn;
			Datum lbin;

			if(SyntaxUtils.isSyntaxRules(b2.getCar())) {
				lbin = usyn = SyntaxUtils.processRuleDesc(
						(Symbol)name, b2.getCar(), env,
						mesg, null);
			} else if(SyntaxUtils.isIdentifierSyntax(b2.getCar())) {
				lbin = SyntaxUtils.processIdentifierSyntax(
						(Symbol)name, b2.getCar(), env, mesg);
			} else {
				// compile the sequence(list)
				CompiledCode.Builder nbuild =
					new CompiledCode.Builder();
				Environment nenv = new Environment(env);
				Datum dz;
				MiscInfo info;

				//syncased.depth = 0;   // init depth
				lbin = usyn = new UserSyntax(
						((Symbol)name).getName(),
						env,
						nbuild.getCodeRef());
				info = new MiscInfo(usyn);
				dz = PatternMatch.appendScopeCase(
						b2.getCar(), usyn);
//				dz = b2.getCar();

				comp.compile(
						dz, nenv, nbuild,
						new Cons(), false, symlist,
						exec, memento, info);
				nbuild.addReturnOp();
			}

			// define the syntax now
			env.bindDatum(name, lbin);

			// binds runtime environment later
			build.addPush(lbin);
			build.addBind(name);
			build.addPush(Undef.UNDEF);
		} else {
			throw mesg.getError("err.definesyntax.malform");
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Syntax#replaceLocalVals(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispCompiler, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage, boolean, int)
	 */
	@Override
	Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg,
			boolean toplv,
			int ttype) {
		//throw new LispException(
		//		"define-syntax can not use in define-syntax");

		// evaluate variables in macro definition later
		return body;
	}

}
