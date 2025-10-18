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
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;

import net.morilib.lisp.LispCompiler.MiscInfo;

public class SynDefineMethod extends Syntax {
	
	//
	private static final Collection<Datum> REPL_MTH;
	
	/*package*/ static final Symbol TOPSYM = Symbol.gensym();
	
	
	static {
		Collection<Datum> rp = new HashSet<Datum>();
		rp.add(Symbol.getSymbol("next-method"));
		REPL_MTH = Collections.unmodifiableCollection(rp);
	}
	
	
	private Cons chkp2(Datum d, LispMessage mesg) {
		Cons res = new Cons();
		Cons r01 = new Cons();
		
		res.setCdr(r01);
		if(d instanceof Cons) {
			Cons d0 = (Cons)d;
			
			if(!(d0.getCar() instanceof SymbolName)) {
				throw mesg.getError("err.definemethod.malform");
			} else if(d0.getCdr() instanceof Cons) {
				Cons d1 = (Cons)d0.getCdr();
				
				if(!(d1.getCar() instanceof SymbolName)) {
					throw mesg.getError("err.definemethod.malform");
				} else if(d1.getCdr() != Nil.NIL) {
					throw mesg.getError("err.definemethod.malform");
				}
				res.setCar(((SymbolName)d0.getCar()).getSymbol());
				r01.setCar(d1.getCar());
			}
		}
		return res;
	}
	
	private Datum gcar(Datum d, LispMessage mesg) {
		if(d instanceof Cons) {
			Cons d0 = (Cons)d;
			
			return d0.getCar();
		}
		throw mesg.getError("err.definemethod.malform");
	}
	
	private Datum gcadr(Datum d, LispMessage mesg) {
		if(d instanceof Cons) {
			Cons d0 = (Cons)d;
			
			if(d0.getCdr() instanceof Cons) {
				Cons d1 = (Cons)d0.getCdr();
				
				return d1.getCar();
			}
		}
		throw mesg.getError("err.definemethod.malform");
	}
	
	private Datum extvar(Datum arg, LispMessage mesg) {
		ConsListBuilder cns = new ConsListBuilder();
		Datum pt = arg;
		
		while(true) {
			if(pt instanceof Cons) {
				Cons c0 = (Cons)pt;
				
				if(c0.getCar() instanceof SymbolName) {
					cns.append(((SymbolName)c0.getCar()).getSymbol());
				} else if(c0.getCar() instanceof Cons) {
					Cons c1 = (Cons)c0.getCar();
					
					if(c1.getCar() instanceof SymbolName) {
						c1 = chkp2(c1, mesg);
						cns.append(gcar(c1, mesg));
					} else {
						throw mesg.getError("err.require.symbol");
					}
				} else {
					throw mesg.getError("err.definemethod.malform");
				}
				pt = c0.getCdr();
			} else {
				if(pt == Nil.NIL) {
					return cns.get();
				} else if(pt instanceof SymbolName) {
					return cns.get(((SymbolName)pt).getSymbol());
				} else {
					throw mesg.getError("err.definemethod.malform");
				}
			}
		}
	}
	
	private Datum extcls(Datum arg, LispMessage mesg) {
		ConsListBuilder cns = new ConsListBuilder();
		Datum pt = arg;
		
		while(true) {
			if(pt instanceof Cons) {
				Cons c0 = (Cons)pt;
				
				if(c0.getCar() instanceof SymbolName) {
					cns.append(TOPSYM);
				} else if(c0.getCar() instanceof Cons) {
					Cons c1 = (Cons)c0.getCar();
					
					if(c1.getCar() instanceof SymbolName) {
						chkp2(c1, mesg);
						cns.append(gcadr(c1, mesg));
					} else {
						throw mesg.getError("err.require.symbol");
					}
				} else {
					throw mesg.getError("err.definemethod.malform");
				}
				pt = c0.getCdr();
			} else {
				if(pt == Nil.NIL) {
					return cns.get();
				} else if(pt instanceof SymbolName) {
					return cns.get(TOPSYM);
				} else {
					throw mesg.getError("err.definemethod.malform");
				}
			}
		}
	}
	
	private Datum reparg(
			Environment ienv, Datum arg, LispMessage mesg) {
		ConsListBuilder cns = new ConsListBuilder();
		Datum pt = arg;
		
		while(true) {
			if(pt instanceof Cons) {
				Cons c0 = (Cons)pt;
				
				if(c0.getCar() instanceof SymbolName) {
					Datum r = SyntaxUtils.putSymbol(
							ienv, c0.getCar(), mesg);
					
					cns.append(r);
				} else if(c0.getCar() instanceof Cons) {
					Datum r = SyntaxUtils.putSymbol(
							ienv, gcar(c0.getCar(), mesg), mesg);
					Cons rc0 = new Cons();
					Cons rc1 = new Cons();
					
					rc0.setCar(r);  rc0.setCdr(rc1);
					rc1.setCar(gcadr(c0.getCar(), mesg));
					cns.append(rc0);
				} else {
					throw mesg.getError("err.definemethod.malform");
				}
				pt = c0.getCdr();
			} else {
				if(pt == Nil.NIL) {
					return cns.get();
				} else if(pt instanceof SymbolName) {
					Datum r = SyntaxUtils.putSymbol(ienv, pt, mesg);
					
					return cns.get(r);
				} else {
					throw mesg.getError("err.definemethod.malform");
				}
			}
		}
	}
	
	private void defun(
			Datum bnam,
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
		Datum vars = extvar(bcar, mesg);
		Cons  c = new Cons(bnam, vars);
		
		// create a temporary Closure
		CompiledCode.Builder mbuild = new CompiledCode.Builder();
		Environment  menv = new Environment(env);
		
		// compile the list
		CompiledCode.Builder nbuild = new CompiledCode.Builder();
		Environment nenv = new Environment(menv);
		//symlist.add(callsym);
		SyntaxUtils.compileList(
				//bcdr,
				SyntaxUtils.removeScope(bcdr, REPL_MTH),
				nenv, comp, nbuild, c, true, mesg,
				new ArrayList<Cons>(),
				exec, memento, syncased);
		//symlist.remove(0);
		
		nbuild.addReturnOp();
		ClosureClassMethod cln = new ClosureClassMethod(
				c.getCdr(), nbuild.getCodeRef(),
				extcls(bcar, mesg));
		
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
		
		build.addBindMethod(c.getCar());
		build.addPush(Undef.UNDEF);
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
			IntStack memento, MiscInfo syncased) {
		if(body instanceof Cons) {
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();
			
			if(bcar instanceof SymbolName) {
				Datum cz = bcar;
				if(!(cz instanceof SymbolName)) {
					throw mesg.getError("err.definemethod.malform");
				} else if(!(bcdr instanceof Cons)) {
					throw mesg.getError("err.definemethod.malform");
				}
				
				Cons bcd0 = (Cons)bcdr;
				defun(bcar, bcd0.getCar(), bcd0.getCdr(),
						env, comp, build,
						callsym, mesg, symlist, toplevel,
						exec, memento, syncased);
			} else {
				//throw new LispException("Type mismatch");
				throw mesg.getError("err.definemethod.malform");
			}
		} else {
			//throw new LispException("Wrong arguments");
			throw mesg.getError("err.definemethod.malform");
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
			
			if(bcar instanceof SymbolName) {
				// 関数定義
				if(!(bcdr instanceof Cons)) {
					throw mesg.getError("err.definemethod.malform");
				}
				
				Cons c = (Cons)bcdr;
				Cons res = new Cons();
				Cons arg = new Cons();
				
				// 環境を新規に作成する
				Environment nenv = new Environment(ienv);
				
				// initialize
				res.setCdr(arg);
				arg.setCar(reparg(nenv, c.getCar(), mesg));
				
				// リストをコンパイルする
				arg.setCdr(SyntaxUtils.replaceLocalValsList(
						c.getCdr(), env, comp, nenv, mesg, ttype));
				
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
				throw mesg.getError("err.definemethod.malform");
			}
		} else {
			//throw new LispException("Wrong arguments");
			throw mesg.getError("err.definemethod.malform");
		}
	}

}
