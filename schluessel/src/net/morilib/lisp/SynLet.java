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

public class SynLet extends Syntax implements SynLetType {
	
	
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
			IntStack memento, MiscInfo syncased) {
		if(body instanceof Cons) {
			CompiledCode.Builder nbuild = new CompiledCode.Builder();
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();
			List<Datum> lvars = new ArrayList<Datum>();
			List<Datum> lvals = new ArrayList<Datum>();
			Cons bnam = new Cons();
			
			// create a temporary Closure
			Environment  nenv = new Environment(env);
			ClosureClass cl = new ClosureClass();
			
			// named let
			if(bcar instanceof SymbolName) {
				bnam.setCar(bcar);
				nbuild.addPush(cl);
				nbuild.addBind(bcar);
				
				if(bcdr instanceof Cons) {
					bcar = ((Cons)bcdr).getCar();
					bcdr = ((Cons)bcdr).getCdr();
				} else {
					throw mesg.getError("err.let.malform");
				}
			}
			
			// define local variables
			if(bcar instanceof Cons) {
				Datum d = bcar;
				
				// evaluate the local variables
				while(d != Nil.NIL) {
					if(d instanceof Cons) {
						List<Datum> l2;
						
						l2 = LispUtils.consToList(
								((Cons)d).getCar(), mesg);
						
						if(l2.size() != 2) {
							throw mesg.getError("err.let.malform");
						} else if(!(l2.get(0) instanceof SymbolName)) {
							throw mesg.getError("err.let.malform");
						}
						
						// generate a list (<local variable> <value>)
						lvars.add(l2.get(0));
						lvals.add(l2.get(1));
						
						// undefine defined syntax
						nenv.bindDatumWithoutScope(
								l2.get(0), Undef.UNDEF2);
						
						d = ((Cons)d).getCdr();
					} else {
						throw mesg.getError("err.let.malform");
					}
				}
			} else if(bcar != Nil.NIL) {
				//throw new LispException("malformed let");
				throw mesg.getError("err.let.malform");
			}
			
			// compile the list
			bnam.setCdr(LispUtils.listToCons(lvars));
			if(bnam.getCar() == Nil.NIL) {
				symlist.add(callsym);
				SyntaxUtils.compileList(
						bcdr, nenv, comp, nbuild,
						bnam, istail, mesg,
						symlist,
						exec, memento, syncased);
				symlist.remove(0);
			} else {
				// named let
				//symlist.add(callsym);
				SyntaxUtils.compileList(
						bcdr, nenv, comp, nbuild,
						bnam, true, mesg,
						new ArrayList<Cons>(),
						exec, memento, syncased);
				//		symlist);
				//symlist.remove(0);
			}
			nbuild.addReturnOp();
			
			// set values on the temporary Closure
			cl.setParameterList(LispUtils.listToCons(lvars));
			cl.setCode(nbuild.getCodeRef());
			
			// call the temporary Closure
			build.addPush(cl);
			comp.compileArgs(
					LispUtils.listToCons(lvals),
					env, build, callsym, symlist,
					exec, memento, syncased);
			//build.addCall();
			if(bnam.getCar() != Nil.NIL && istail) {
				build.addCallTail(symlist.size());
			} else {
				build.addCall();
			}
		} else {
			//throw new LispException("malformed let");
			throw mesg.getError("err.let.malform");
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
			List<Datum> lst = new ArrayList<Datum>();
			
			// 環境を新規に作成する
			Environment nenv = new Environment(ienv);
			
			// named let
			if(bcar instanceof SymbolName) {
				lst.add(SyntaxUtils.putSymbol(nenv, bcar, mesg));
				
				if(bcdr instanceof Cons) {
					bcar = ((Cons)bcdr).getCar();
					bcdr = ((Cons)bcdr).getCdr();
				} else {
					//throw new LispException("syntax error: let");
					throw mesg.getError("err.let.malform");
				}
			}
			
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
							throw mesg.getError("err.let.malform");
						}
						
						// リネームリストに追加する
						if(l2.get(0) instanceof SymbolName ||
								l2.get(0)
								instanceof PatternMatch.IndSym) {
							rc.setCar(SyntaxUtils.putSymbol(
									nenv, l2.get(0), mesg));
						}
						
						// ローカル変数の値部分を調査する
						// let内の変数スコープは外側のもの
						r2.setCar(comp.replaceLocalVals(
								l2.get(1), env, ienv, false, ttype));
						
						d = ((Cons)d).getCdr();
						lst2.add(rc);
					} else {
						throw mesg.getError("err.let.malform");
					}
				}
			} else if(bcar != Nil.NIL) {
				throw mesg.getError("err.let.malform");
			}
			lst.add(LispUtils.listToCons(lst2));
			
			// cdr部を調査する
			Datum cdrx = SyntaxUtils.replaceLocalValsList(
					bcdr, env, comp, nenv, mesg, ttype);
			
			return LispUtils.listToCons(lst, cdrx);
		} else {
			throw mesg.getError("err.let.malform");
		}
	}
	
}
