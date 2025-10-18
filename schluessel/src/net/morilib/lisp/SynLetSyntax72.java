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
 * @author MORIGUCHI, Yuichiro 2012/03/06
 */
public class SynLetSyntax72
extends SynLetSyntaxBase implements SynLetType {

	/**
	 * 
	 */
	public static final Subr COMPILE = new ExSubrCompile();

	/**
	 * 
	 */
	public static final Subr COMPILE_BODY = new ExSubrCompileBody();

	/**
	 * 
	 */
	public static final Subr MAKEMACRO = new SubrMakeMacro();

	//
	/*package*/ String getName() {
		return "let-syntax";
	}

	//
	/*package*/ LispException getError(LispMessage mesg) {
		return mesg.getError("err.letsyntax.malform");
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
			CompiledCode.Builder nbuild = new CompiledCode.Builder();
//			CompiledCode.Builder mbuild = new CompiledCode.Builder();
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();
			List<Datum> lvars = new ArrayList<Datum>();
			List<Datum> lvals = new ArrayList<Datum>();
			Cons bnam = new Cons();

			// 一時的なClosureを生成する
//			Environment  nenv = new Environment(env);

			// ローカル変数の定義
			if(bcar instanceof Cons) {
				Datum d = bcar;

				// ローカル変数の評価
				while(d != Nil.NIL) {
					if(d instanceof Cons) {
						List<Datum> l2 = LispUtils.consToList(
								((Cons)d).getCar(), mesg);

						if(l2.size() != 2) {
							throw getError(mesg);
						}
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

			// ローカル構文に構文をバインドする
			for(int i = 0; i < lvars.size(); i++) {
				Datum lv1 = lvals.get(i);

				if(!(lvars.get(i) instanceof Symbol)) {
					throw mesg.getError("err.letsyntax.name");
				}
//				lv1 = MacroUtils72.unquote2(lv1);
//				lv1 = MacroUtils72.unquote(lv1,
//						new EnvironmentObject(env, false));
//				SyntaxUtils.compileBindMacro72(
//						lvars.get(i), new Cons(lv1, Nil.NIL),
//						nenv, comp, nbuild, callsym, mesg,
//						"err.letsyntax.malform",
//						new ArrayList<Cons>(),
//						exec, memento, syncased);
				nbuild.addPush(MAKEMACRO);
				nbuild.addBeginList();
				nbuild.addPush(COMPILE);
				nbuild.addBeginList();
				nbuild.addPush(lv1);
				nbuild.addAppendList();
				nbuild.addEndList();
				nbuild.addCall();
				nbuild.addAppendList();
				nbuild.addEndList();
				nbuild.addCall();
				nbuild.addBind(lvars.get(i));
			}

			// compile the list
			bnam.setCdr(LispUtils.listToCons(lvars));
//			symlist.add(callsym);

//			SyntaxUtils.compileList(
//					bcdr, nenv, comp, nbuild,
//					bnam, istail, mesg, symlist,
//					exec, memento, syncased);
//			symlist.remove(0);
			nbuild.addPush(COMPILE_BODY);
			nbuild.addBeginList();
			nbuild.addPush(bcdr);
			nbuild.addAppendList();
			nbuild.addEndList();
			nbuild.addCall();
			nbuild.addBeginList();
			nbuild.addEndList();
			nbuild.addCall();
			nbuild.addReturnOp();

			// create a temporary closure
			ClosureClass cl = new ClosureClass(
					Nil.NIL, nbuild.getCodeRef());

			// call the temporary closure
			build.addPush(cl);
			build.addBeginList();
			build.addEndList();
			build.addCall();
		} else {
			throw getError(mesg);
		}
	}

}
