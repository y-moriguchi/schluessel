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

import net.morilib.lisp.CompiledCode.Builder;
import net.morilib.lisp.LispCompiler.MiscInfo;

public class SynJavaCatch extends Syntax {

	@Override
	/*package*/ void compile(
			Datum body,
			Environment env,
			LispCompiler comp,
			Builder build,
			boolean toplevel,
			Cons callsym,
			boolean istail,
			LispMessage mesg,
			List<Cons> symlist, CodeExecutor exec, IntStack memento, MiscInfo syncased) {
		CompiledCode.Builder nbuild = new CompiledCode.Builder();
		List<Datum> exclst;
		Datum esym;
		Datum nbody;
		int lbl2 = build.allocLabel();
		int lbl3 = build.allocLabel();
		int nlbl1 = nbuild.allocLabel();
		
		// create a temporary Closure
		Environment  nenv = new Environment(env);
		ClosureClass cl = new ClosureClass();
		
		if(body instanceof Cons) {
			Cons c0 = (Cons)body;
			
			if(c0.getCar() instanceof Cons) {
				Cons c1 = (Cons)c0.getCar();
				
				esym = c1.getCar();
				exclst = LispUtils.consToList(c1.getCdr(), mesg);
			} else {
				throw mesg.getError("err.java-catch.malform");
			}
			nbody = c0.getCdr();
		} else {
			throw mesg.getError("err.java-catch.malform");
		}
		
		if(!(esym instanceof Symbol)) {
			throw mesg.getError("err.require.symbol", esym);
		}
		
		//
		Cons esyml = new Cons(esym, Nil.NIL);
		
		symlist.add(callsym);
		for(Datum d : exclst) {
			int nlblz = nbuild.allocLabel();
			
			if(d instanceof Cons) {
				Cons c2 = (Cons)d;
				
				if(!IntLispUtils.isSymbolName(c2.getCar())) {
					throw mesg.getError(
							"err.require.symbol", c2.getCar());
				}
				
				nbuild.addReferSymbol(esym);
				nbuild.addJavaInstanceof((SymbolName)c2.getCar());
				nbuild.addJmpUnless(nlblz);
				nbuild.addPop();
				SyntaxUtils.compileList(
						c2.getCdr(), nenv, comp, nbuild,
						new Cons(), istail, mesg, symlist,
						exec, memento, syncased);
				nbuild.addJmp(nlbl1);
				nbuild.setCurrentAddressToLabel(nlblz);
				nbuild.addPop();
			} else {
				throw mesg.getError("err.java-catch.malform");
			}
		}
		symlist.remove(0);
		nbuild.addReferSymbol(esym);
		nbuild.addJavaRaise();
		nbuild.setCurrentAddressToLabel(nlbl1);
		nbuild.addReturnOp();
		
		// set values on the temporary Closure
		cl.setParameterList(esyml);
		cl.setCode(nbuild.getCodeRef());
		
		build.addJavaEnterExceptionHandler(cl, lbl3);
		build.setCurrentAddressToLabel(lbl2);
		
		SyntaxUtils.compileList(
				nbody, env, comp, build,
				callsym, istail, mesg, symlist,
				exec, memento, syncased);
		build.addJavaLeaveExceptionHandler();
		build.setCurrentAddressToLabel(lbl3);
	}

	@Override
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg, boolean toplv, int ttype) {
		List<Datum> exclst;
		Datum esym;
		Datum nbody;
		
		Environment nenv = new Environment(ienv);
		List<Datum> reslst = new ArrayList<Datum>();
		Datum ca0, cd0;
		
		if(body instanceof Cons) {
			Cons c0 = (Cons)body;
			
			if(c0.getCar() instanceof Cons) {
				Cons c1 = (Cons)c0.getCar();
				
				esym = SyntaxUtils.putSymbol(nenv, c1.getCar(), mesg);
				exclst = LispUtils.consToList(c1.getCdr(), mesg);
			} else {
				throw mesg.getError("err.java-catch.malform");
			}
			nbody = c0.getCdr();
		} else {
			throw mesg.getError("err.java-catch.malform");
		}
		
		for(Datum d : exclst) {
			if(d instanceof Cons) {
				Datum ca1, cd1;
				Cons c2 = (Cons)d;
				
				if(!(c2.getCar() instanceof Symbol)) {
					throw mesg.getError(
							"err.require.symbol", c2.getCar());
				}
				
				ca1 = c2.getCar();
				cd1 = SyntaxUtils.replaceLocalValsList(
						c2.getCdr(), env, comp,
						nenv, mesg, ttype);
				reslst.add(new Cons(ca1, cd1));
			} else {
				throw mesg.getError("err.java-catch.malform");
			}
		}
		
		ca0 = new Cons(esym, LispUtils.listToCons(reslst));
		cd0 = SyntaxUtils.replaceLocalValsList(
				nbody, env, comp, ienv, mesg, ttype);
		return new Cons(ca0, cd0);
	}

}
