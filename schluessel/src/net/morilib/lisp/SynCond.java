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

public class SynCond extends Syntax {
	
	private static final Symbol ELSE_SYM = Symbol.getSymbol("else");
	
	private static final Symbol THIRD_SYM = Symbol.getSymbol("=>");
	
	
	private boolean isElseScope(Datum d) {
		return SyntaxUtils.equalsReserved(ELSE_SYM, d);
	}
	
	private boolean isThirdScope(Datum d) {
		return SyntaxUtils.equalsReserved(THIRD_SYM, d);
	}
	
	private Datum isThirdForm(Cons d2, LispMessage mesg) {
		if(d2.getCdr() instanceof Cons) {
			Cons d3 = (Cons)d2.getCdr();
			
			if(isThirdScope(d3.getCar())) {
				if(d3.getCdr() instanceof Cons) {
					Cons d4 = (Cons)d3.getCdr();
					
					if(d4.getCdr() == Nil.NIL) {
						return d4.getCar();
					}
				}
				throw mesg.getError("err.cond.malform");
			}
		}
		return null;
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
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento,
			MiscInfo syncased) {
		Datum d = body;
		int last = build.allocLabel();
		boolean notelse = true;
		
		while(true) {
			if(d == Nil.NIL) {
				if(notelse) {
					build.addPush(Undef.UNDEF);
					build.setCurrentAddressToLabel(last);
				}
				break;
			} else if(!notelse) {
				throw mesg.getError("err.else");
			} else if(d instanceof Cons) {
				Datum d1 = ((Cons)d).getCar();
				
				if(!(d1 instanceof Cons)) {
					//throw new LispException("malformed cond");
					throw mesg.getError("err.cond.malform", d1);
				}
				
				Cons d2 = (Cons)d1;
				
				if(isElseScope(d2.getCar())) {
					// else句
					SyntaxUtils.compileList(
							d2.getCdr(), env, comp, build,
							callsym, istail, mesg, symlist,
							exec, memento, syncased);
					build.setCurrentAddressToLabel(last);
					
					notelse = false;
					//break;
				} else {
					int l1 = build.allocLabel();   // 使用するラベル
					Datum d4;
					
					// 条件部分
					comp.compile(
							d2.getCar(),
							env,
							build,
							callsym,
							false,
							symlist,
							exec, memento, syncased);
					
					if(d2.getCdr() == Nil.NIL) {
						build.addJmpIf(last);
						build.addPop();
					} else if((d4 = isThirdForm(d2, mesg)) != null) {
						// =>
						build.addJmpUnless(l1);
						build.addBeginList();
						build.addAppendList();
						comp.compile(
								d4, env, build, callsym, istail,
								symlist, exec, memento, syncased);
						build.addEndList();  // compileと逆順にする
						build.addCall();
						build.addJmp(last);
						build.setCurrentAddressToLabel(l1);
						build.addPop();
					} else {
						// then部分
						build.addJmpUnless(l1);
						build.addPop();
						SyntaxUtils.compileList(
								d2.getCdr(), env, comp, build,
								callsym, istail, mesg, symlist,
								exec, memento, syncased);
						build.addJmp(last);
						build.setCurrentAddressToLabel(l1);
						build.addPop();
					}
				}
				d = ((Cons)d).getCdr();
			} else {
				//throw new LispException("Cond: proper list required");
				throw mesg.getError("err.cond.improper");
			}
		}
		
		//if(notelse) {
		//	build.addPush(Undef.UNDEF);
		//	build.setCurrentAddressToLabel(last);
		//}
	}
	
	
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv, LispMessage mesg, boolean toplv, int ttype) {
		Datum d = body;
		List<Datum> lst = new ArrayList<Datum>();
		
		while(true) {
			if(d == Nil.NIL) {
				break;
			} else if(d instanceof Cons) {
				Datum d1 = ((Cons)d).getCar();
				
				if(!(d1 instanceof Cons)) {
					//throw new LispException("malformed cond");
					throw mesg.getError("err.cond.malform", d1);
				}
				
				Cons d2 = (Cons)d1;
				
				if(isElseScope(d2.getCar())) {
					// else句
					Cons rc = new Cons();
					
					rc.setCar(ELSE_SYM);
					rc.setCdr(SyntaxUtils.replaceLocalValsList(
							d2.getCdr(), env,
							comp, ienv, mesg, toplv, ttype));
					lst.add(rc);
					break;
				} else {
					Cons rc = new Cons();
					Datum d4;
					
					// 条件部分
					rc.setCar(comp.replaceLocalVals(
							d2.getCar(), env, ienv, false, ttype));
					
					if((d4 = isThirdForm(d2, mesg)) != null) {
						Cons rc2 = new Cons();
						Datum lx = comp.replaceLocalVals(
								d4, env, ienv, false, ttype);
						
						rc2.setCar(THIRD_SYM);
						rc2.setCdr(new Cons(lx, Nil.NIL));
						rc.setCdr(rc2);
					} else {
						// then部分
						rc.setCdr(SyntaxUtils.replaceLocalValsList(
								d2.getCdr(), env,
								comp, ienv, mesg, toplv, ttype));
					}
					
					lst.add(rc);
				}
				d = ((Cons)d).getCdr();
			} else {
				//throw new LispException("Cond: proper list required");
				throw mesg.getError("err.cond.improper");
			}
		}
		
		return LispUtils.listToCons(lst);
	}
	
}
