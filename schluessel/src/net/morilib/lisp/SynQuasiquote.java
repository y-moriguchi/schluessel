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

public class SynQuasiquote extends Syntax {
	
	private static Datum getCaar(Cons c) {
		if(c.getCar() instanceof Cons) {
			return ((Cons)c.getCar()).getCar();
		} else {
			return null;
		}
	}
	
	/*package*/ boolean equalsQuasiquote(Datum d) {
		return SyntaxUtils.equalsReserved(Symbol.QUASIQUOTE, d);
	}
	
	/*package*/ boolean equalsUnquote(Datum d) {
		return SyntaxUtils.equalsReserved(Symbol.UNQUOTE, d);
	}
	
	/*package*/ boolean equalsUnquoteSplicing(Datum d) {
		return SyntaxUtils.equalsReserved(Symbol.UNQUOTE_SPLICING, d);
	}
	
	/*package*/ LispException error(LispMessage mesg) {
		return mesg.getError("err.quasiquote.malform");
	}
	
	public String toString() {
		return "Syntax:quasiquote";
	}
	
	private void quote1(
			int level,
			Cons c,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder build,
			Cons callsym,
			LispMessage mesg,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento,
			LispCompiler.MiscInfo syncased) {
		// (car (cdr c))がないときは内部エラー
		if(!(c.getCdr() instanceof Cons)) {
			throw error(mesg);
		}
		Cons c2 = (Cons)c.getCdr();
		
		//build.addPush(new SubrList());
		build.addBeginList();
		build.addPush(c.getCar());
		build.addAppendList();
		expand(level, c2.getCar(), env, comp, build,
				callsym, mesg, symlist,
				exec, memento, syncased);
		build.addAppendList();
		build.addEndList();
	}
	
	private void expand(
			int level,
			Datum body,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder build,
			Cons callsym,
			LispMessage mesg,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento,
			LispCompiler.MiscInfo syncased) {
		if(body instanceof Cons) {
			Cons c = (Cons)body;
			
			if(equalsQuasiquote(c.getCar())) {
				quote1(level + 1, c, env, comp, build,
						callsym, mesg, symlist,
						exec, memento, syncased);
			} else if(equalsUnquote(c.getCar())) {
				// (car (cdr c))がないときは内部エラー
				Cons c2 = (Cons)c.getCdr(), c3 = c2;
				int zi = 0;

//				while(c3.getCar() instanceof Cons &&
//						equalsUnquote(((Cons)c3.getCar()).getCar())) {
//					c3 = (Cons)((Cons)c3.getCar()).getCdr();
//					zi++;
//				}
				if(level > zi) {
					quote1(level - 1, c, env, comp, build,
							callsym, mesg, symlist,
							exec, memento, syncased);
				} else if(c.getCdr() instanceof Cons) {
					comp.compile(
							c3.getCar(), env, build,
							callsym, false, symlist,
							exec, memento, syncased);
				} else {
					throw error(mesg);
				}
			} else if(equalsUnquoteSplicing(c.getCar())) {
				if(level > 0) {
					quote1(level - 1, c, env, comp, build,
							callsym, mesg, symlist,
							exec, memento, syncased);
				} else if(c.getCdr() instanceof Cons) {
					// (car (cdr c))がないときは内部エラー
					Cons c2 = (Cons)c.getCdr();
					comp.compile(
							c2.getCar(), env, build,
							callsym, false, symlist,
							exec, memento, syncased);
				} else {
					throw error(mesg);
				}
			} else {
				//build.addPush(new SubrList());
				build.addBeginList();
				while(true) {
					Datum caar = getCaar(c);
					
					expand(level, c.getCar(), env, comp, build,
							callsym, mesg, symlist,
							exec, memento, syncased);
					
					if(c.getCdr() instanceof Cons) {
						Cons c2 = (Cons)c.getCdr();
						Datum d2 = c2.getCar();
						
						if(equalsUnquote(d2)) {
							// `(a b . ,(expr))
							if(equalsUnquoteSplicing(caar)) {
								build.addAppendListSplicing();
							} else {
								build.addAppendList();
							}
							expand(level, c.getCdr(), env, comp, build,
									callsym, mesg, symlist,
									exec, memento, syncased);
							build.addEndListDot();
							break;
						} else if(equalsUnquoteSplicing(d2)) {
							// `(a b . ,@(expr))
							//throw new LispException(
							//		"syntax error: malformed ,@");
							throw error(mesg);
						} else if(level == 0 &&
								equalsUnquoteSplicing(caar)) {
							// `(a b ,@(expr) d ...)のとき
							build.addAppendListSplicing();
							c = (Cons)c.getCdr();
						} else {
							build.addAppendList();
							c = (Cons)c.getCdr();
						}
					} else if(c.getCdr() == Nil.NIL) {
						if(level == 0 && equalsUnquoteSplicing(caar)) {
							// `(a b ,@(expr))のとき
							// 最後の引数をドットリストに連結する
							build.addEndListDot();
						} else {
							build.addAppendList();
							build.addEndList();
						}
						break;
					} else {
						if(level == 0 && equalsUnquoteSplicing(caar)) {
							// `(a b .@(expr) . d)のとき
							build.addAppendListSplicing();
						} else {
							build.addAppendList();
						}
						expand(level, c.getCdr(), env, comp, build,
								callsym, mesg, symlist,
								exec, memento, syncased);
						build.addEndListDot();
						break;
					}
				}
			}
		} else if(body instanceof LispVector) {
			LispVector v = (LispVector)body;
			
			build.addBeginList();
			for(int i = 0; i < v.size(); i++) {
				expand(level, v.get(i), env, comp, build,
						callsym, mesg, symlist,
						exec, memento, syncased);
				if(v.get(i) instanceof Cons) {
					Cons cz = (Cons)v.get(i);
					
					if(equalsUnquoteSplicing(cz.getCar())) {
						build.addAppendListSplicing();
					} else {
						build.addAppendList();
					}
				} else {
					build.addAppendList();
				}
			}
			build.addEndListVector();
		} else {
			build.addPush(body);
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
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento,
			MiscInfo syncased) {
		if(body instanceof Cons) {
			expand(0, ((Cons)body).getCar(),
					env, comp, build, callsym, mesg, symlist,
					exec, memento, syncased);
		} else {
			//throw new LispException("invalid quote");
			throw error(mesg);
		}
	}
	
	
	private Datum quoteE1(
			int level,
			Cons c,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg,
			int ttype) {
		// (car (cdr c))がないときは内部エラー
		Cons c2 = (Cons)c.getCdr();
		Cons res = new Cons();
		Cons r2  = new Cons();
		
		res.setCar(c.getCar());
		res.setCdr(r2);
		r2.setCar(extract1(
				level, c2.getCar(), env, comp, ienv, mesg, ttype));
		
		return res;
	}
	
	private Datum extract1(
			int level,
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg,
			int ttype) {
		if(body instanceof Cons) {
			Cons c = (Cons)body;
			
			if(equalsQuasiquote(c.getCar())) {
				return quoteE1(
						level + 1, c, env, comp, ienv, mesg, ttype);
			} else if(equalsUnquote(c.getCar())) {
				if(level > 0) {
					return quoteE1(
							level - 1, c, env, comp, ienv, mesg, ttype);
				} else if(c.getCdr() instanceof Cons) {
					// (car (cdr c))がないときは内部エラー
					Cons c2 = (Cons)c.getCdr();
					Cons r1 = new Cons();
					Cons r2 = new Cons();
					
					r1.setCar(c.getCar());
					r1.setCdr(r2);
					r2.setCar(comp.replaceLocalVals(
							c2.getCar(), env, ienv, false, ttype));
					return r1;
				} else {
					throw error(mesg);
				}
			} else if(equalsUnquoteSplicing(c.getCar())) {
				if(level > 0) {
					return quoteE1(
							level - 1, c, env, comp, ienv, mesg, ttype);
				} else if(c.getCdr() instanceof Cons) {
					// (car (cdr c))がないときは内部エラー
					Cons c2 = (Cons)c.getCdr();
					Cons r1 = new Cons();
					Cons r2 = new Cons();
					
					r1.setCar(c.getCar());
					r1.setCdr(r2);
					r2.setCar(comp.replaceLocalVals(
							c2.getCar(), env, ienv, false, ttype));
					return r1;
				} else {
					throw error(mesg);
				}
			} else {
				List<Datum> lst = new ArrayList<Datum>();
				
				while(true) {
					Datum caar = getCaar(c);
					
					lst.add(extract1(
							level, c.getCar(), env,
							comp, ienv, mesg, ttype));
					
					if(c.getCdr() instanceof Cons) {
						Cons c2 = (Cons)c.getCdr();
						Datum d2 = c2.getCar();
						
						if(equalsUnquote(d2)) {
							// `(a b . ,(expr))
							Datum dz = extract1(
									level, c.getCdr(),
									env, comp, ienv, mesg, ttype);
							
							return LispUtils.listToCons(lst, dz);
							//break;
						} else if(equalsUnquoteSplicing(d2)) {
							// `(a b . ,@(expr))
							//throw new LispException(
							//		"syntax error: malformed ,@");
							throw mesg.getError(
									"err.quasiquote.malform");
						} else if(level == 0 &&
								equalsUnquoteSplicing(caar)) {
							// `(a b ,@(expr) d ...)のとき
							c = (Cons)c.getCdr();
						} else {
							c = (Cons)c.getCdr();
						}
					} else if(c.getCdr() == Nil.NIL) {
						return LispUtils.listToCons(lst);
					} else {
						return LispUtils.listToCons(
								lst, extract1(
								level, c.getCdr(),
								env, comp, ienv, mesg, ttype));
						//break;
					}
				}
			}
		} else if(body instanceof LispVector) {
			List<Datum> lst = new ArrayList<Datum>();
			LispVector v = (LispVector)body;
			
			for(int i = 0; i < v.size(); i++) {
				lst.add(extract1(
						level, v.get(i), env,
						comp, ienv, mesg, ttype));
			}
			return new LispVector(lst);
		} else {
			return body;
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
			Cons res = new Cons();
			
			res.setCar(extract1(
					0, ((Cons)body).getCar(), env,
					comp, ienv, mesg, ttype));
			return res;
		} else {
			//throw new LispException("invalid quote");
			throw error(mesg);
		}
	}
	
}
