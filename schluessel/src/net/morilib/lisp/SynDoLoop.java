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

public class SynDoLoop extends Syntax {
	
	private static void compileListForBind(
			Datum b,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder nbuild,
			Cons callsym,
			boolean istail,
			LispMessage mesg,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento,
			LispCompiler.MiscInfo syncased) {
		Datum bcdr = b;
		
		while(bcdr != Nil.NIL) {
			if(bcdr instanceof Cons) {
				Cons bc = (Cons)bcdr;
				//boolean tl = istail && (bc.getCdr() == Nil.NIL);
				
				comp.compile(
						bc.getCar(), env, nbuild,
						callsym, false, symlist,
						exec, memento, syncased);
				bcdr = bc.getCdr();
			} else {
				//throw new LispException("proper list required");
				throw mesg.getError("err.do.improper");
			}
		}
	}
	
	private static void compileBindAll(
			CompiledCode.Builder nbuild,
			List<Datum> lst) {
		for(int i = lst.size() - 1; i >= 0; i--) {
			nbuild.addBind(lst.get(i));
		}
	}
	
	
	/*package*/ void compile(
			Datum body,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder build,
			boolean toplevel,
			Cons callsym,
			boolean istail,
			LispMessage mesg,
			List<Cons> symlist, CodeExecutor exec, IntStack memento, MiscInfo syncased) {
		if(body instanceof Cons) {
			CompiledCode.Builder nbuild = new CompiledCode.Builder();
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();
			List<Datum> lvars = new ArrayList<Datum>();
			List<Datum> lvals = new ArrayList<Datum>();
			List<Datum> lupds = new ArrayList<Datum>();
			Datum test;
			Datum rese;
			Datum eexp;
			
			// 一時的なClosureを生成する
			Environment  nenv = new Environment(env);
			ClosureClass cl = new ClosureClass();
			
			// ローカル変数の定義
			if(bcar instanceof Cons) {
				Datum d = bcar;
				
				// ローカル変数が空のときはエラー
				if(d == Nil.NIL) {
					//throw new LispException("malformed do");
					throw mesg.getError("err.do.malform");
				}
				
				// ローカル変数の評価
				while(d != Nil.NIL) {
					if(d instanceof Cons) {
						List<Datum> l2;
						
						l2 = LispUtils.consToList(
								((Cons)d).getCar(), mesg);
						
						if(l2.size() < 2 || l2.size() > 3) {
							//throw new LispException("syntax error: do");
							throw mesg.getError("err.do.malform");
						}
						
						// ローカル変数と値のリストを生成する
						lvars.add(l2.get(0));
						lvals.add(l2.get(1));
						lupds.add((l2.size() == 2) ?
								l2.get(0) : l2.get(2));
						
						d = ((Cons)d).getCdr();
					} else {
						//throw new LispException("malformed do");
						throw mesg.getError("err.do.malform");
					}
				}
			} else {
				//throw new LispException("malformed do");
				throw mesg.getError("err.do.malform");
			}
			
			// 結果式の評価
			if(bcdr instanceof Cons) {
				Datum d = ((Cons)bcdr).getCar();
				
				if(d instanceof Cons) {
					test = ((Cons)d).getCar();
					rese = ((Cons)d).getCdr();
				} else {
					//throw new LispException("malformed do");
					throw mesg.getError("err.do.malform");
				}
				
				if(rese != Nil.NIL && !(rese instanceof Cons)) {
					throw mesg.getError("err.list", d);
				}
				
				// 条件が偽のときに実行する式
				eexp = ((Cons)bcdr).getCdr();
			} else {
				//throw new LispException("malformed do");
				throw mesg.getError("err.do.malform");
			}
			
			// 使用するラベル
			int l1 = nbuild.allocLabel();
			int l2 = nbuild.allocLabel();
			
			// リストをコンパイルする
			// 条件部分
			nbuild.setCurrentAddressToLabel(l1);
			comp.compile(
					test, env, nbuild, new Cons(), false, symlist,
					exec, memento, syncased);
			
			// else部分
			nbuild.addJmpIf(l2);
			nbuild.addPop();
			SyntaxUtils.compileList(
					eexp, nenv, comp, nbuild,
					new Cons(), false, mesg, symlist,
					exec, memento, syncased);
			nbuild.addPop();
			
			// 再バインドのために一旦スタックに全て積み
			// リストの逆順にバインドする
			compileListForBind(
					LispUtils.listToCons(lupds),
					nenv, comp, nbuild,
					new Cons(), false, mesg, symlist,
					exec, memento, syncased);
			compileBindAll(nbuild, lvars);
			nbuild.addJmp(l1);
			
			// 結果式実行
			nbuild.setCurrentAddressToLabel(l2);
			if(rese != Nil.NIL) {
				nbuild.addPop();
				SyntaxUtils.compileList(
						rese, nenv, comp, nbuild,
						new Cons(), istail, mesg, symlist,
						exec, memento, syncased);
			}
			nbuild.addReturnOp();
			
			// 一時的なClosureに値をセットする
			cl.setParameterList(LispUtils.listToCons(lvars));
			cl.setCode(nbuild.getCodeRef());
			
			// 一時的なClosureを呼び出す
			build.addPush(cl);
			comp.compileArgs(
					LispUtils.listToCons(lvals),
					env, build, callsym, symlist,
					exec, memento, syncased);
			build.addCall();
		} else {
			//throw new LispException("malformed do");
			throw mesg.getError("err.do.malform");
		}
	}
	
	
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv, LispMessage mesg, boolean toplv, int ttype) {
		if(body instanceof Cons) {
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();
			Cons  res = new Cons();
			Cons  re1  = new Cons();
			//Cons  re2  = new Cons();
			
			// 環境を新規に作成する
			Environment nenv = new Environment(ienv);
			
			// ローカル変数の定義
			List<Datum> lst2 = new ArrayList<Datum>();
			
			// 初期設定
			res.setCdr(re1);
			
			if(bcar instanceof Cons) {
				Datum d = bcar;
				
				// ローカル変数が空のときはエラー
				if(d == Nil.NIL) {
					//throw new LispException("malformed do");
					throw mesg.getError("err.do.malform");
				}
				
				// ローカル変数の評価
				while(d != Nil.NIL) {
					if(d instanceof Cons) {
						List<Datum> l2;
						Cons rc = new Cons();
						Cons r2 = new Cons();
						Cons r3 = new Cons();
						
						rc.setCdr(r2);
						r2.setCdr(r3);
						
						l2 = LispUtils.consToList(
								((Cons)d).getCar(), mesg);
						
						if(l2.size() != 3) {
							//throw new LispException("syntax error: do");
							throw mesg.getError("err.do.malform");
						}
						
						// リネームリストに追加する
						rc.setCar(SyntaxUtils.putSymbol(
								nenv, l2.get(0), mesg));
						
						// ローカル変数の値部分を調査する
						// let内の変数スコープは外側のもの
						r2.setCar(comp.replaceLocalVals(
								l2.get(1), env, ienv, false, ttype));
						
						// 増分部分
						r3.setCar(comp.replaceLocalVals(
								l2.get(2), env, nenv, false, ttype));
						
						d = ((Cons)d).getCdr();
						lst2.add(rc);
					} else {
						//throw new LispException("malformed do");
						throw mesg.getError("err.do.malform");
					}
				}
			} else {
				//throw new LispException("malformed do");
				throw mesg.getError("err.do.malform");
			}
			res.setCar(LispUtils.listToCons(lst2));
			
			// 結果式の評価
			if(bcdr instanceof Cons) {
				Datum d = ((Cons)bcdr).getCar();
				
				if(d instanceof Cons) {
					Cons rt = new Cons();
					
					rt.setCar(comp.replaceLocalVals(
							((Cons)d).getCar(), env,
							nenv, false, ttype));
					rt.setCdr(SyntaxUtils.replaceLocalValsList(
							((Cons)d).getCdr(), env, comp,
							nenv, mesg, ttype));
					re1.setCar(rt);
				} else {
					//throw new LispException("malformed do");
					throw mesg.getError("err.do.malform");
				}
				
				// 条件が偽のときに実行する式
				Datum eexp = ((Cons)bcdr).getCdr();
				if(eexp instanceof Cons) {
					//re1.setCdr(re2);
					//re2.setCar(SyntaxUtils.replaceLocalValsList(
					//		eexp, env, comp, nenv, mesg));
					re1.setCdr(SyntaxUtils.replaceLocalValsList(
							eexp, env, comp, nenv, mesg, ttype));
				}
			} else {
				//throw new LispException("malformed do");
				throw mesg.getError("err.do.malform");
			}
			
			return res;
		} else {
			//throw new LispException("malformed do");
			throw mesg.getError("err.do.malform");
		}
	}
	
}
