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

public class SynCase extends Syntax {
	
	private static final Symbol ELSE_SYM = Symbol.getSymbol("else");
	
	
	private boolean isElseScope(Datum d) {
		return SyntaxUtils.equalsReserved(ELSE_SYM, d);
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
		Datum d = body;
		int last = build.allocLabel();
		boolean notelse = true;
		
		// caseで評価される式の評価
		if(d instanceof Cons) {
			Cons c0 = (Cons)d;
			
			comp.compile(
					c0.getCar(), env, build, callsym, false, symlist,
					exec, memento, syncased);
			d = c0.getCdr();
		} else {
			throw mesg.getError("err.case.malform");
		}
		
		// キー句の評価
		while(true) {
			if(d == Nil.NIL) {
				if(notelse) {
					build.addPop();    // テスト値をポップ
					build.addPush(Undef.UNDEF);
					build.setCurrentAddressToLabel(last);
				}
				break;
			} else if(!notelse) {
				throw mesg.getError("err.else");
			} else if(d instanceof Cons) {
				Datum d1 = ((Cons)d).getCar();
				
				if(!(d1 instanceof Cons)) {
					throw mesg.getError("err.case.malform", d1);
				}
				
				Cons d2 = (Cons)d1;
				
				if(isElseScope(d2.getCar())) {
					// else句
					build.addPop();    // テスト値をポップ
					SyntaxUtils.compileList(
							d2.getCdr(), env, comp, build,
							callsym, istail, mesg, symlist,
							exec, memento, syncased);
					build.setCurrentAddressToLabel(last);
					
					notelse = false;
					//break;
				} else {
					int l1 = build.allocLabel();   // 使用するラベル
					List<Datum> keys;
					
					if(!(d2.getCar() instanceof Cons)) {
						throw mesg.getError(
								"err.case.malform.key", d2.getCar());
					}
					
					// キー部分
					keys = LispUtils.consToList(d2.getCar(), mesg);
					build.addBeginList();
					for(int i = 0; i < keys.size(); i++) {
						//comp.compile(
						//		keys.get(i), env,
						//		build, callsym, false);
						build.addPush(keys.get(i));
						build.addAppendList();
					}
					build.addEndList();
					build.addFindList();
					
					// then部分
					build.addJmpUnless(l1);
					build.addPop();    // listの結果をポップ
					build.addPop();    // テスト値をポップ
					SyntaxUtils.compileList(
							d2.getCdr(), env, comp, build,
							callsym, istail, mesg, symlist,
							exec, memento, syncased);
					build.addJmp(last);
					build.setCurrentAddressToLabel(l1);
					build.addPop();    // listの結果をポップ
				}
				d = ((Cons)d).getCdr();
			} else {
				//throw new LispException("Cond: proper list required");
				throw mesg.getError("err.case.improper");
			}
		}
		
		//if(notelse) {
		//	build.addPop();    // テスト値をポップ
		//	build.addPush(Undef.UNDEF);
		//	build.setCurrentAddressToLabel(last);
		//}
	}
	
	
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg,
			boolean toplv,
			int ttype) {
		Datum d = body;
		List<Datum> lst = new ArrayList<Datum>();
		
		// caseで評価される式の評価
		if(d instanceof Cons) {
			Cons c0 = (Cons)d;
			
			lst.add(comp.replaceLocalVals(
					c0.getCar(), env, ienv, false, ttype));
			d = c0.getCdr();
		} else {
			throw mesg.getError("err.case.malform");
		}
		
		while(true) {
			if(d == Nil.NIL) {
				break;
			} else if(d instanceof Cons) {
				Datum d1 = ((Cons)d).getCar();
				
				if(!(d1 instanceof Cons)) {
					throw mesg.getError("err.case.malform", d1);
				}
				
				Cons d2 = (Cons)d1;
				
				if(isElseScope(d2.getCar())) {
					// else句
					Cons rc = new Cons();
					
					rc.setCar(ELSE_SYM);
					rc.setCdr(SyntaxUtils.replaceLocalValsList(
							d2.getCdr(), env, comp, ienv,
							mesg, toplv, ttype));
					lst.add(rc);
					break;
				} else {
					Cons rc = new Cons();
					List<Datum> keys, k2;
					
					// キー部分
					keys = LispUtils.consToList(d2.getCar(), mesg);
					k2 = new ArrayList<Datum>();
					for(int i = 0; i < keys.size(); i++) {
						k2.add(comp.replaceLocalVals(
								keys.get(i), env, ienv, false, ttype));
					}
					rc.setCar(LispUtils.listToCons(k2));
					
					rc.setCdr(SyntaxUtils.replaceLocalValsList(
							d2.getCdr(), env, comp, ienv,
							mesg, toplv, ttype));
					
					lst.add(rc);
				}
				d = ((Cons)d).getCdr();
			} else {
				//throw new LispException("Cond: proper list required");
				throw mesg.getError("err.case.improper");
			}
		}
		
		return LispUtils.listToCons(lst);
	}
	
}
