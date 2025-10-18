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
public class SynDefine extends Syntax {

	//
	private void defun(
			Datum bcar,
			Datum bcdr,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder build,
			Cons callsym,
			LispMessage mesg,
			List<Cons> symlist,
			boolean toplevel,
			CodeExecutor exec,
			IntStack memento,
			LispCompiler.MiscInfo syncased) {
		// function definition
		Cons c = (Cons)bcar;

		// create a temporary Closure
		CompiledCode.Builder mbuild = new CompiledCode.Builder();
		Environment  menv = new Environment(env);

		// compile the list
		CompiledCode.Builder nbuild = new CompiledCode.Builder();
		Environment nenv = new Environment(menv);
		//symlist.add(callsym);
		SyntaxUtils.compileList(
				bcdr, nenv, comp, nbuild, c, true, mesg,
				new ArrayList<Cons>(),
				exec, memento, syncased);
		//symlist.remove(0);

		nbuild.addReturnOp();
		ClosureClass cln = new ClosureClass(
				c.getCdr(), nbuild.getCodeRef());

		mbuild.addPush(cln);
		mbuild.addReturnOp();

		// 一時的なClosureを生成する
		ClosureClass clm = new ClosureClass(
				Nil.NIL, mbuild.getCodeRef());

		// 一時的なClosureを呼び出す
		build.addPush(clm);
		build.addBeginList();
		build.addEndList();
		build.addCall();

		//build.addBind(c.getCar());

		//if(toplevel) {
		//	Symbol r;
		//	
		//	if(c.getCar() instanceof Symbol) {
		//		r = ((Symbol)c.getCar()).getEnclosedSymbol();
		//	} else {
		//		r = ((SymbolScope)c.getCar()).getSymbol();
		//		r = r.getEnclosedSymbol();
		//	}
		//	build.addBind(r);
		//	build.addPush(((Cons)bcar).getCar());
		//} else {
		//	build.addBind(c.getCar());
		//	build.addPush(Undef.UNDEF);
		//}
		build.addBind(c.getCar());
		build.addPush(Undef.UNDEF);
	}

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
		if(body instanceof Cons) {
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();

			if(bcar instanceof Cons) {
				Datum cz = ((Cons)bcar).getCar();
				if(!(cz instanceof SymbolName)) {
					throw mesg.getError("err.define.malform");
				}

				defun(bcar, bcdr, env, comp, build,
						callsym, mesg, symlist, toplevel,
						exec, memento, syncased);
			} else if(bcar instanceof SymbolName) {
				//Cons  l0 = IntLispUtils.extractLambda(bcdr, bcar);
				//Datum lt = IntLispUtils.extractLambdaList(bcdr);

				// constant definition
				SyntaxUtils.compileBind(
						bcar, bcdr, env, comp,
						build, callsym, toplevel,
						mesg, "err.define.malform",
						new ArrayList<Cons>(),
						exec, memento, syncased);
				//if(toplevel) {
				//	build.addPop();
				//	build.addPush(bcar);
				//}
			} else {
				//throw new LispException("Type mismatch");
				throw mesg.getError("err.define.malform");
			}
		} else {
			//throw new LispException("Wrong arguments");
			throw mesg.getError("err.define.malform");
		}
	}

	//
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg, boolean toplv, int ttype) {
		if(body instanceof Cons) {
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();

			if(bcar instanceof Cons) {
				// 関数定義
				Cons c = (Cons)bcar;
				Cons res = new Cons();
				Cons arg = new Cons();

				// 環境を新規に作成する
				Environment nenv = new Environment(ienv);

				// initialize
				res.setCar(arg);

				// 引数をリネームリストに追加する
				arg.setCdr(SyntaxUtils.addLocalValsAll(
						nenv, c.getCdr(), mesg));

				// リストをコンパイルする
				res.setCdr(SyntaxUtils.replaceLocalValsList(
						bcdr, env, comp, nenv, mesg, ttype));

				if(!toplv) {
					// defineの束縛変数はリネームする
					arg.setCar(SyntaxUtils.putSymbol(
							ienv, c.getCar(), mesg));
				} else {
					arg.setCar(c.getCar());
				}

				return res;
			} else if(bcar instanceof SymbolName) {
				// 定数定義
				Cons res = new Cons();

				res.setCdr(comp.replaceLocalVals(
						bcdr, env, ienv, false, ttype));

				if(!toplv) {
					// defineの束縛変数はリネームする
					res.setCar(SyntaxUtils.putSymbol(ienv, bcar, mesg));
				//} else if(bcar instanceof Symbol) {
				//	res.setCar(bcar);
				} else {
					res.setCar(((SymbolName)bcar).getSymbol());
				}
				return res;
			} else {
				//throw new LispException("Type mismatch");
				throw mesg.getError("err.define.malform");
			}
		} else {
			//throw new LispException("Wrong arguments");
			throw mesg.getError("err.define.malform");
		}
	}

}
