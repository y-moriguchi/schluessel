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

public class SynLetrec extends Syntax implements SynLetType {
	
	/*
	private void defun(
			Datum bcar,
			Datum bcdr,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder build,
			Cons callsym,
			LispMessage mesg,
			List<Cons> symlist) {
		// function definition
		CompiledCode.Builder nbuild = new CompiledCode.Builder();
		Cons c = (Cons)bcar;
		Environment nenv = new Environment(env);
		
		// compile the list
		//symlist.add(callsym);
		SyntaxUtils.compileList(
				bcdr, nenv, comp, nbuild, c, true, mesg,
				new ArrayList<Cons>());
		//symlist.remove(0);
		
		nbuild.addReturnOp();
		ClosureClass cl = new ClosureClass(
				c.getCdr(), nbuild.getCodeRef());
		
		build.addPush(cl);
		build.addBind(c.getCar());
	}
	*/
	
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
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();
			
			// マクロ用の環境を生成する
			Environment nenv = new Environment(env);
			
			// ローカル変数の定義
			if(bcar instanceof Cons) {
				Datum d = bcar;
				while(d != Nil.NIL) {
					if(d instanceof Cons) {
						Datum d2 = ((Cons)d).getCar();
						
						if(d2 instanceof Cons) {
							Cons  d3  = (Cons)d2;
							Datum d3c = d3.getCar();
							/*Cons  l0 = IntLispUtils.extractLambda(
									d3.getCdr(),
									d3.getCar());
							Datum lt = IntLispUtils.extractLambdaList(
									d3.getCdr());*/
							
							if(!(d3c instanceof SymbolName)) {
								throw mesg.getError(
										"err.letrec.malform");
							}
							
							SyntaxUtils.compileBind(
									d3c,
									d3.getCdr(),
									env,
									comp,
									nbuild,
									new Cons(),
									false,
									mesg,
									"err.letrec.malform",
									new ArrayList<Cons>(),
									exec, memento, syncased);
							nbuild.addPop();
							
							// undefine defined syntax
							nenv.bindDatumWithoutScope(
									d3c, Undef.UNDEF2);
						} else {
							//throw new LispException("malformed letrec");
							throw mesg.getError("err.letrec.malform");
						}
						d = ((Cons)d).getCdr();
					} else {
						//throw new LispException("malformed letrec");
						throw mesg.getError("err.letrec.malform");
					}
				}
			} else if(bcar != Nil.NIL) {
				//throw new LispException("malformed letrec");
				throw mesg.getError("err.letrec.malform");
			}
			
			// リストをコンパイルする
			symlist.add(callsym);
			SyntaxUtils.compileList(
					bcdr, nenv, comp, nbuild,
					new Cons(), istail, mesg, symlist,
					exec, memento, syncased);
			symlist.remove(0);
			nbuild.addReturnOp();
			
			// 一時的なClosureを生成する
			ClosureClass cl = new ClosureClass(
					Nil.NIL, nbuild.getCodeRef());
			
			// 一時的なClosureを呼び出す
			build.addPush(cl);
			build.addBeginList();
			build.addEndList();
			build.addCall();
		} else {
			//throw new LispException("malformed letrec");
			throw mesg.getError("err.letrec.malform");
		}
	}
	
	
	/*package*/ Datum replaceLocalVals(
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
			List<Datum> lst = new ArrayList<Datum>();
			
			// 環境を新規に作成する
			Environment nenv = new Environment(ienv);
			
			// ローカル変数の定義
			List<Datum> lst2 = new ArrayList<Datum>();
			
			if(bcar instanceof Cons) {
				Datum d = bcar;
				
				// ローカル変数の評価
				while(d != Nil.NIL) {
					if(d instanceof Cons) {
						List<Datum> l2;
						Cons rc = new Cons();
						Cons r2 = new Cons();
						
						rc.setCdr(r2);
						
						l2 = LispUtils.consToList(
								((Cons)d).getCar(), mesg);
						
						if(l2.size() != 2) {
							//throw new LispException(
							//		"syntax error: letrec");
							throw mesg.getError("err.letrec.malform");
						}
						
						// リネームリストに追加する
						rc.setCar(SyntaxUtils.putSymbol(
								nenv, l2.get(0), mesg));
						
						// ローカル変数の値部分を調査する
						// 変数スコープは新しい変数
						r2.setCar(comp.replaceLocalVals(
								l2.get(1), env, nenv, false, ttype));
						
						d = ((Cons)d).getCdr();
						lst2.add(rc);
					} else {
						//throw new LispException("malformed letrec");
						throw mesg.getError("err.letrec.malform");
					}
				}
			} else if(bcar != Nil.NIL) {
				//throw new LispException("malformed letrec");
				throw mesg.getError("err.letrec.malform");
			}
			lst.add(LispUtils.listToCons(lst2));
			
			// cdr部を調査する
			Datum cdrx = SyntaxUtils.replaceLocalValsList(
					bcdr, env, comp, nenv, mesg, ttype);
			
			return LispUtils.listToCons(lst, cdrx);
		} else {
			//throw new LispException("malformed letrec");
			throw mesg.getError("err.letrec.malform");
		}
	}
	
}
