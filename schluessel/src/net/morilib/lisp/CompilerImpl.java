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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.morilib.lisp.CompiledCode.Builder;
import net.morilib.lisp.chihaya.LispSyntax;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.UnaryArgs;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/24
 */
/*package*/ class CompilerImpl extends LispCompiler {

	//
	//private static Logger _log = LogEnv.init("schlush.compiler");

	//
	private LispMessage message;

	//
	/*package*/ CompilerImpl(LispMessage msg) {
		message = msg;
	}

	//
	/*package*/ Datum expandSyntax(
			UserSyntax syn, Datum body,
			Environment env2, boolean syncase) {
		List<Datum> pat = syn.getPatternList();
		List<Datum> tmp = syn.getTemplateList();
		PatternDepthMap mp;
		boolean mtc = false;
		int ind = 0;

		//System.out.println("bb:" + body);
		//System.out.println("pt:" + pat);
		do {
			// パターン照合結果を初期化
			mp = new PatternDepthMap(syn.getParamList().get(ind));

			// パターンマッチング
			mtc = PatternMatch.match(
					pat.get(ind), body, mp, syn.getReservedSet());
		} while(!mtc && ++ind < pat.size());

		if(mtc) {
			try {
				// パターンにマッチした
				Environment menv = new Environment();
				Datum t;
				Map<Symbol, Symbol> box = new HashMap<Symbol, Symbol>();

				//// ローカル変数を置き換える
				//t = replaceLocalVals(tmp.get(ind), env, menv);
				t = tmp.get(ind);

				// テンプレートコンパイル
				Datum tpl = PatternMatch.compileTemplate(t, mp);
				//_log.finer(LispUtils.print(tpl));

				// テンプレート展開
				// シンボルに「マクロ引数由来」のマークを追加する
				Datum res = PatternMatch.expand(
						tpl, mp, syn, box, env2, syncase);
				//_log.finer(LispUtils.print(res));

				// ローカル変数を置き換える
				res = replaceLocalVals(res, env2, menv, true, 0);
				//_log.finer(LispUtils.print(res));

				// スコープ情報を追加する
				res = PatternMatch.appendScope(res, mp, syn);
				//_log.finer(LispUtils.print(res));

				// マークを外す
				res = PatternMatch.markReplace(box, res, false);

				// unwrap
				res = PatternMatch.gtUnwrap(res);

				//_log.finer(LispUtils.print(res));
//				System.out.println("ex:" + LispUtils.print(res));

				return res;
			} catch(PatternDepthException e) {
				e.printStackTrace();
				throw message.getError(
						"err.wronglevel", e.getMessage());
			}
		} else {
			throw message.getError("err.malform", syn.getName());
			//throw new LispException("malformed " + syn.getName());
		}
	}

	//
	private Datum expandLambda(
			UserSyntax syn,
			Datum body,
			Environment env2,
			CodeExecutor exec,
			IntStack memento) {
		// create a temporary Closure
		CompiledCode.Builder mbuild = new CompiledCode.Builder();
		ClosureClass clm = new ClosureClass(
				Nil.NIL, syn.getLambda());

		mbuild.addPush(clm);
		mbuild.addBeginList();
		mbuild.addEndList();
		mbuild.addCall();
		mbuild.addBeginList();
		mbuild.addPush(body);
		//mbuild.addPush(cbld.get(citr.getTerminal()));
		mbuild.addAppendList();
		mbuild.addEndList();
		mbuild.addCall();
		mbuild.addReturnOp();

		// execute lambda
		Datum res = exec.exec(mbuild.getCodeRef(), env2, memento);
		//if(res == Nil.NIL) {
		//	throw message.getError("err.syntaxcase.empty");
		//}
//		System.out.println("rs:" + res);
		return res;
	}

	//
	private void compileArgsBind(
			Datum bcdr,
			Environment env,
			Builder builder,
			Cons symcall,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento,
			LispCompiler.MiscInfo syncased) {
		Datum prms = symcall.getCdr();
		List<Datum> vals = new ArrayList<Datum>();

		while(true) {
			if(prms == Nil.NIL) {
				if(bcdr != Nil.NIL) {
					throw message.getError(
							"err.parameter.insufficient");
				}
				break;
			} else if(prms instanceof Atom) {
				compile(bcdr, env, builder, symcall, false, symlist,
						exec, memento, syncased);
				builder.addBind(prms);
				break;
			} else if(!(bcdr instanceof Cons)) {
				throw message.getError("err.parameter.insufficient");
				//throw new LispException("insufficient parameter");
			} else if(prms instanceof Cons) {
				Datum a1 = ((Cons)prms).getCar();
				Datum a2 = ((Cons)bcdr).getCar();

				compile(a2, env, builder, symcall, false, symlist,
						exec, memento, syncased);
				//builder.addBind(a1);
				vals.add(a1);

				prms = ((Cons)prms).getCdr();
				bcdr = ((Cons)bcdr).getCdr();
			} else {
				throw message.getError("err.type.invalid");
				//throw new LispException("Invalid type");
			}
		}

		for(int i = vals.size() - 1; i >= 0; i--) {
			builder.addBind(vals.get(i));
		}
	}

	//
	/*private int poppos(Datum bcar, List<Cons> symlist) {
		int res = 0;

		for(Cons c : symlist) {
			if(bcar.equals(c.getCar())) {
				return res;
			} else {
				res++;
			}
		}
		return -1;
	}*/

	//
	private void compileSexp(
			Datum bcar,
			Datum bcdr,
			Environment env,
			Builder builder,
			Cons symcall,
			boolean istail,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento,
			boolean toplv,
			LispCompiler.MiscInfo syncased) {
		//int pos = -1;

		if(istail && bcar.equals(symcall.getCar())) {
			compileArgsBind(
					bcdr, env, builder, symcall, symlist,
					exec, memento, syncased);
			builder.addJmpTop();
		/*} else if(istail && (pos = poppos(bcar, symlist)) >= 0) {
			Cons       s2 = symlist.get(pos);
			List<Cons> sl = symlist.subList(pos + 1, symlist.size());

			builder.addRewindEnv(pos);
			compileArgsBind(bcdr, env, builder, s2, sl);
			builder.addRewindCont(pos);*/
		} else {
			boolean tlv;

			tlv = (toplv && bcar instanceof Cons &&
					((Cons)bcar).getCar() instanceof SymbolName &&
					((SymbolName)((Cons)bcar).getCar())
					.getName().equals(SynBegin.BEGIN.getName()));
			compile(bcar, env, builder, tlv, symcall, false, symlist,
					exec, memento, syncased);
			compileArgs(
					bcdr, env, builder, symcall, symlist,
					exec, memento, syncased);

			if(istail) {
				builder.addCallTail(symlist.size());
			} else {
				builder.addCall();
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispCompiler#compileArgs(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.CompiledCode.Builder, net.morilib.lisp.Cons, java.util.List, net.morilib.lisp.CodeExecutor, net.morilib.lisp.IntStack, net.morilib.lisp.LispCompiler.MiscInfo)
	 */
	public void compileArgs(
			Datum bcdr,
			Environment env,
			Builder builder,
			Cons symcall,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento, MiscInfo syncased) {
		builder.addBeginList();
		while(true) {
			if(bcdr instanceof Nil) {
				builder.addEndList();
				break;
			} else if(bcdr instanceof Atom) {
				compile(bcdr, env, builder, symcall, false, symlist,
						exec, memento, syncased);
				builder.addEndListDot();
				break;
			} else if(bcdr instanceof Cons) {
				compile(((Cons)bcdr).getCar(), env, builder,
						symcall, false, symlist,
						exec, memento, syncased);
				builder.addAppendList();

				bcdr = ((Cons)bcdr).getCdr();
			} else {
				throw message.getError("err.type.invalid");
				//throw new LispException("Invalid type");
			}
		}
	}

	//
	private Datum getSym(Datum bcar, Environment env) {
		Datum ddd;

		if(bcar instanceof Symbol) {
			ddd = env.findDatum((Symbol)bcar);
//			if(ddd == null && ((Symbol)bcar).getNamespace() != null) {
//				ddd = env.findDatum(
//						((Symbol)bcar).getNamespace()
//						.getSymbol(((Symbol)bcar).getName()));
//			}
		} else {
			SymbolScope ss   = (SymbolScope)bcar;
			Environment menv =
				ss.getUserSyntax().getCompileEnv();

			//System.out.println(ss);
			//System.out.println(menv);
			ddd = menv.findDatum(ss.getSymbol());
//			if(ddd == null && ss.getSymbol().getNamespace() != null) {
//				ddd = env.findDatum(
//						ss.getSymbol().getNamespace()
//						.getSymbol(ss.getSymbol().getName()));
//			}
		}
		return ddd;
	}

	//
	private UserSyntax getUSyn(Datum d4, Environment env) {
		if(!(d4 instanceof SymbolName)) {
			return null;
		}

		Datum d5 = getSym(d4, env);
		if(d5 instanceof UserSyntax) {
			return (UserSyntax)d5;
		} else {
			return null;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispCompiler#compile(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.CompiledCode.Builder, boolean, net.morilib.lisp.Cons, boolean, java.util.List, net.morilib.lisp.CodeExecutor, net.morilib.lisp.IntStack, net.morilib.lisp.LispCompiler.MiscInfo)
	 */
	public void compile(
			Datum body,          // S-expression to be compiled
			Environment env,     // environment to be compiled
			Builder builder,     // builder of byte code
			boolean toplevel,    // is in top level
			Cons symcall,        // list of (<procedure name> <args>)
			boolean istail,      // is in tail
			List<Cons> symlist,  // list of symcall
			CodeExecutor exec,
			IntStack memento,
			MiscInfo syncased) {
		if(body instanceof SymbolName) {
			Datum ddd = getSym(body, env);

			if(ddd instanceof UserSyntax) {
				UserSyntax sn = (UserSyntax)ddd;
				Datum exp;
				LispCompiler.MiscInfo mif = syncased;

				if(sn.getLambda() == null) {
					builder.addReferSymbol(body);
				} else {
					Datum e2 = expandLambda(
							sn, body, env, exec, memento);

					exp = e2;
					if(mif.getUserSyntax() == null) {
						mif = new LispCompiler.MiscInfo(sn);
					}

					compile(exp, env, builder, toplevel,
							symcall, istail, symlist,
							exec, memento, mif);
				}
			} else {
				builder.addReferSymbol(body);
			}
		//} else if(body instanceof SymbolScope) {
		//	builder.addReferSymbol(body);
		} else if(body instanceof Cons) {
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();

			if(bcar instanceof Symbol || bcar instanceof SymbolScope) {
				Datum ddd = getSym(bcar, env);

				if(ddd instanceof LispLazySubrSyn) {
					ddd = ((LispLazySubrSyn)ddd).setup();
				}

				if(ddd instanceof SynSetS) {
					if(bcdr instanceof Cons) {
						Datum d4 = ((Cons)bcdr).getCar();
						UserSyntax sn;

						if((sn = getUSyn(d4, env)) == null) {
							Syntax syn = (Syntax)ddd;
							Datum dz = bcdr;

							syn.compile(
									dz, env, this, builder, toplevel,
									symcall, istail, message, symlist,
									exec, memento, syncased);
						} else {
							//Environment env2 = sn.getCompileEnv();
							Datum exp;
							LispCompiler.MiscInfo mif = syncased;

							if(sn.getLambda() == null) {
								//exp = expandSyntax(sn, body, env2);
								throw message.getError(
										"err.set.malform");
							} else {
								Datum e2 = expandLambda(
										sn, body, env, exec, memento);

								exp = e2;
								if(mif.getUserSyntax() == null) {
									mif = new LispCompiler.MiscInfo(sn);
								}
							}

							compile(exp, env, builder, toplevel,
									symcall, istail, symlist,
									exec, memento, mif);
						}
					} else {
						throw message.getError("err.set.malform");
					}
				} else if(ddd instanceof Syntax) {
					// syntax
					Syntax syn = (Syntax)ddd;
					Datum dz = bcdr;

					/*if(ddd instanceof SynSyntax) {
						//Cons r0 = new Cons();
						//Cons r1 = new Cons();
						//Cons r2 = new Cons();
						dz = ((Syntax)ddd).replaceLocalVals(
								((Cons)bcdr).getCar(),
								env, this,
								new Environment(),
								message, true, 1);

						//r0.setCar(bcar);
						//r0.setCdr(r1);
						//r1.setCar(r2);
						//r2.setCar(dz);
						//dz = r2;
					}*/

					syn.compile(dz, env, this, builder, toplevel,
							symcall, istail, message, symlist,
							exec, memento, syncased);
				} else if(ddd instanceof UserSyntax) {
					// user defined syntax (RnRS macro)
					UserSyntax usyn = (UserSyntax)ddd;
					Environment env2 = usyn.getCompileEnv();
					Datum exp;
					LispCompiler.MiscInfo mif = syncased;

					if(usyn.getLambda() == null) {
						exp = expandSyntax(usyn, body, env2, false);
					} else {
						Datum e2 = expandLambda(
								usyn, body, env, exec, memento);

						//CompiledCode.Builder mbuild =
						//	new CompiledCode.Builder();
						//compile(e2, env, mbuild, toplevel,
						//		symcall, istail, symlist,
						//		exec, memento);
						//mbuild.addReturnOp();
						//exp = exec.exec(
						//		mbuild.getCodeRef(), env, memento);
						exp = e2;
						if(mif.getUserSyntax() == null) {
							mif = new LispCompiler.MiscInfo(usyn);
						}

//						System.out.println("e2:" + exp);
						exp = AUX_S.call("letren", exp);
//						System.out.println("e3:" + exp);
					}

					compile(exp, env, builder, toplevel,
							symcall, istail, symlist,
							exec, memento, mif);
				} else if(ddd instanceof Macro) {
					// traditional macro
					Datum exp = expandMacro(body, env, exec, memento);

					compile(exp, env, builder, toplevel,
							symcall, istail, symlist,
							exec, memento, syncased);
				} else if(ddd instanceof MacroQuote) {
					// traditional macro
					Datum exp = expandMacro(body, env, exec, memento);

					builder.addPush(exp);
				} else {
					compileSexp(
							bcar, bcdr, env, builder,
							symcall, istail, symlist,
							exec, memento, toplevel, syncased);
				}
			} else if(bcar instanceof SynSyntax ||
					bcar instanceof SynQuasisyntax) {
				Datum dz = ((Syntax)bcar).replaceLocalVals(
						((Cons)bcdr).getCar(),
						env, this,
						new Environment(),
						message, true, 1);

				((Syntax)bcar).compile(
						dz, env, this, builder, toplevel,
						symcall, istail, message, symlist,
						exec, memento, syncased);
			} else {
				compileSexp(
						bcar, bcdr, env, builder,
						symcall, istail, symlist,
						exec, memento, toplevel, syncased);
			}
		} else if(body instanceof LispSyntax) {
			builder.addPush(((LispSyntax)body).getWrapped());
		} else {
			builder.addPush(body);
		}
		//builder.addReturnOp();
	}

	//
	private Datum replaceLocalValsSexp(
			Datum bcar,
			Datum bcdr,
			Environment env,
			Environment ienv,
			boolean toplv,
			int ttype) {
		Cons res = new Cons();
		boolean tlv;

		tlv = (toplv && bcar instanceof Cons &&
				(!(((Cons)bcar).getCar() instanceof SymbolName) ||
						((SymbolName)((Cons)bcar).getCar())
						.getName().equals(SynBegin.BEGIN.getName())));
		res.setCar(replaceLocalVals(bcar, env, ienv, tlv, ttype));
		res.setCdr(replaceLocalValsArgs(bcdr, env, ienv, ttype));
		return res;
	}

	//
	/*package*/ Datum replaceLocalValsOnly(
			Datum body,
			Environment env,
			Environment ienv,
			boolean toplv,
			int ttype) {
		if(body instanceof SymbolName) {
			// 束縛変数はない
			Symbol sym = ((SymbolName)body).getSymbol();
			Datum d = ienv.findDatum(sym);

			return (d != null) ? d : body;
		} else if(body instanceof Cons) {
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();
			Cons  res  = new Cons();

			res.setCar(replaceLocalValsOnly(
					bcar, env, ienv, toplv, ttype));
			res.setCdr(replaceLocalValsOnly(
					bcdr, env, ienv, toplv, ttype));
			return res;
		} else {
			return body;
		}
	}

	//
	private Datum replaceLocalValsWithSyn(
			Datum bcdr,
			Environment env,
			Environment ienv,
			int ttype) {
		ConsListBuilder res = new ConsListBuilder();
		ConsIterator    itr = new ConsIterator(bcdr);
		ConsListBuilder re2 = new ConsListBuilder();
		ConsIterator    it2;

		if(!itr.hasNext()) {
			throw message.getError("err.withsyntax.malform");
		}
		res.append(itr.next());

		if(!itr.hasNext()) {
			throw message.getError("err.withsyntax.malform");
		}

		it2 = new ConsIterator(itr.next());
		while(it2.hasNext()) {
			ConsListBuilder re3 = new ConsListBuilder();
			ConsIterator    it3 = new ConsIterator(it2.next());

			if(!it3.hasNext()) {
				throw message.getError("err.withsyntax.malform");
			}
			re3.append(it3.next());

			if(!it3.hasNext()) {
				throw message.getError("err.withsyntax.malform");
			}
			re3.append(replaceLocalValsOnly(
					it3.next(), env, ienv, false, ttype));

			if(it3.hasNext()) {
				throw message.getError("err.withsyntax.malform");
			}

			re2.append(re3.get());
		}
		res.append(re2.get());

		while(itr.hasNext()) {
			res.append(replaceLocalValsOnly(
					itr.next(), env, ienv, false, ttype));
		}

//		System.out.println("wb:" + bcdr);
//		System.out.println("ws:" + res.get());
		return res.get();
	}

	//
	public Datum replaceLocalValsArgs(
			Datum bcdr,
			Environment env,
			Environment ienv,
			int ttype) {
		List<Datum> res = new ArrayList<Datum>();

		while(true) {
			if(bcdr instanceof Nil) {
				// 束縛変数はない
				return LispUtils.listToCons(res);
				//break;
			} else if(bcdr instanceof Atom) {
				// 束縛変数はない
				return LispUtils.listToCons(res,
						replaceLocalVals(bcdr, env, ienv, false, ttype));
				//break;
			} else if(bcdr instanceof Cons) {
				res.add(replaceLocalVals(
						((Cons)bcdr).getCar(), env, ienv, false, ttype));

				bcdr = ((Cons)bcdr).getCdr();
			} else {
				throw message.getError("err.type.invalid");
				//throw new LispException("Invalid type");
			}
		}
	}

	//
	public Datum replaceLocalVals(
			Datum body,
			Environment env,
			Environment ienv,
			boolean toplv,
			int ttype) {
		if(body instanceof SymbolName) {
			// 束縛変数はない
			Symbol sym = ((SymbolName)body).getSymbol();
			Datum d = ienv.findDatum(sym);

			return (d != null) ? d : body;
		//} else if(body instanceof SymbolScope) {
		//	// 束縛変数はない
		//	Symbol sym = ((SymbolScope)body).getSymbol();
		//	Datum d = ienv.findDatum(sym);
		//	
		//	return (d != null) ? d : body;
		} else if(body instanceof Cons) {
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();

			if(bcar instanceof Symbol || bcar instanceof SymbolScope) {
				Symbol sym = ((SymbolName)bcar).getSymbol();
				Datum  d2  = getSym(bcar, env);

				if(d2 instanceof LispLazySubrSyn) {
					d2 = ((LispLazySubrSyn)d2).setup();
				}

				if(d2 instanceof SynSetS) {
					if(bcdr instanceof Cons) {
						Datum d4 = ((Cons)bcdr).getCar();
						UserSyntax sn;

						if((sn = getUSyn(d4, env)) == null) {
							Syntax syn = (Syntax)d2;
							Cons res = new Cons();

							res.setCar(bcar);
							res.setCdr(syn.replaceLocalVals(
									bcdr, env, this, ienv,
									message, toplv, ttype));

							return res;
						} else {
							Datum exp;

							if(sn.getLambda() == null) {
								throw message.getError(
										"err.set.malform");
							} else {
								exp = body;
							}
							return exp;
						}
					} else {
						throw message.getError("err.set.malform");
					}
				} else if(d2 instanceof Syntax) {
					Syntax syn = (Syntax)d2;
					Cons res = new Cons();

					res.setCar(bcar);
					res.setCdr(syn.replaceLocalVals(
							bcdr, env, this, ienv,
							message, toplv, ttype));

					return res;
				} else if(SynSyntax.WITH_SYNTAX.equals(sym)) {
					return replaceLocalValsWithSyn(
							body, env, ienv, ttype);
//					return body;
				} else if(d2 instanceof UserSyntax) {
					// user defined syntax (RnRS macro)
					UserSyntax usyn = (UserSyntax)d2;
					Datum exp;

					if(usyn.getLambda() == null) {
						exp = replaceLocalValsSexp(
								bcar, bcdr, env, ienv, toplv, ttype);
					} else {
						exp = body;
					}
					return exp;
				} else {
					return replaceLocalValsSexp(
							bcar, bcdr, env, ienv, toplv, ttype);
				}
			} else {
				return replaceLocalValsSexp(
						bcar, bcdr, env, ienv, toplv, ttype);
			}
		} else {
			// 束縛変数はない
			return body;
		}
	}

	//
	// 戻り値と変更フラグを両方管理するためのデータ格納クラス
	private class D2 {
		private Datum d;         // 戻り値となるデータ
		private boolean dirty;   // 変更されたときはtrue

		private D2() {
			// default
		}

		private D2(Datum d, boolean dirty) {
			this.d = d;
			this.dirty = dirty;
		}

	}

	//
	/*private static final Symbol DEFMACRO = 
		Symbol.getSymbol("define-macro");*/

	//
	private D2 expandMacroSexp(
			Datum car,
			Datum cdr,
			Environment env,
			CodeExecutor exec,
			IntStack memento) {
		Cons res = new Cons();
		boolean dirty;

		D2 d2 = expandMacro1i(car, env, exec, memento);
		res.setCar(d2.d);
		dirty = d2.dirty;

		List<Datum> lst = new ArrayList<Datum>();
		while(true) {
			if(cdr instanceof Cons) {
				Cons c = (Cons)cdr;
				D2 d3 = expandMacro1i(c.getCar(), env, exec, memento);

				lst.add(d3.d);
				dirty = d3.dirty || dirty;
				cdr = c.getCdr();
			} else if(cdr == Nil.NIL) {
				res.setCdr(LispUtils.listToCons(lst));
				return new D2(res, dirty);
			} else {
				res.setCdr(LispUtils.listToCons(lst, cdr));
				return new D2(res, dirty);
			}
		}
	}

	//
	private D2 expandMacro1i(
			Datum body,
			Environment env,
			CodeExecutor exec,
			IntStack memento) {
		D2 res = new D2();

		if(body instanceof Cons) {
			Cons c = (Cons)body;

			if(c.getCar() instanceof SymbolName) {
				Datum d = env.findDatum(
						((SymbolName)c.getCar()).getSymbol());

				if(d instanceof MacroDefinition) {
					return new D2(body, false);
				} else if(d instanceof Macro) {
					// macro expansion
					Macro   m  = (Macro)d;
					Closure cl = m.getClosure();
					Environment nenv = new Environment(env);
					//Object memento = exec.newMemento();

					IntLispUtils.bindLocal(
							cl.getParameterList(),
							c.getCdr(), nenv, message);
					res.d = exec.exec(cl.getCode(), nenv, memento);
					if(res.d instanceof LispSyntax) {
						res.d = ((LispSyntax)res.d).getWrapped();
					}
//					res.d = MacroUtils72.unquote(res.d,
//							new EnvironmentObject(nenv, false));
					res.dirty = true;
					return res;
				} else if(d instanceof MacroQuote) {
					// macro expansion
					MacroQuote m  = (MacroQuote)d;
					Closure    cl = m.getClosure();
					Environment nenv = new Environment(env);
					//Object memento = exec.newMemento();

					IntLispUtils.bindLocal(
							cl.getParameterList(),
							c.getCdr(), nenv, message);
					res.d = exec.exec(cl.getCode(), nenv, memento);
//					if(res.d instanceof LispSyntax) {
//						res.d = ((LispSyntax)res.d).getWrapped();
//					}
					res.dirty = true;
					return res;
				} else if(d instanceof Macro72) {
					// macro expansion
					Macro72 m  = (Macro72)d;
					Closure cl = m.getClosure();
					Environment nenv = new Environment(env);

					IntLispUtils.bindLocal(
							cl.getParameterList(),
							new Cons(c, Nil.NIL), nenv, message);
					res.d = exec.exec(cl.getCode(), nenv, memento);
					if(res.d instanceof LispSyntax) {
						res.d = ((LispSyntax)res.d).getWrapped();
					} else {
//						throw message.getError(
//								"err.require.syntaxobject", res.d);
					}
					res.dirty = true;
					return res;
				} else if(d instanceof SynQuote ||
						d instanceof SynQuasiquote) {
					// quote: not expand macro
					res.d = body;
					res.dirty = false;
					return res;
//				} else if(d instanceof LispSyntax) {
//					// quote: not expand macro
//					res.d = body;
//					res.dirty = false;
//					return res;
				} else {
					return expandMacroSexp(
							c.getCar(), c.getCdr(), env,
							exec, memento);
				}
			} else {
				return expandMacroSexp(
						c.getCar(), c.getCdr(), env,
						exec, memento);
			}
//		} else if(body instanceof LispSyntax) {
////			return expandMacro1i(((LispSyntax)body).getWrapped(),
////					env, exec, memento);
//			res.d = ((LispSyntax)body).getWrapped();
//			res.dirty = false;
//			return res;
		} else {
			res.d = body;
			res.dirty = false;
			return res;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispCompiler#expandMacro1(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.CodeExecutor, net.morilib.lisp.IntStack)
	 */
	public Datum expandMacro1(
			Datum body,
			Environment env,
			CodeExecutor exec,
			IntStack memento) {
		return expandMacro1i(body, env, exec, memento).d;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispCompiler#expandMacro(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.CodeExecutor, net.morilib.lisp.IntStack)
	 */
	public Datum expandMacro(
			Datum body,
			Environment env,
			CodeExecutor exec,
			IntStack memento) {
		Datum d = LispFeature.evalSRFI7(body, env, message);
		D2 loop = new D2(d, false);

		// 展開するマクロがなくなるまでループ
		do {
			Datum b = loop.d;

			// define-macroのときはマクロ展開しない
			if(b instanceof Cons) {
				Datum c1 = ((Cons)b).getCar();

				if(c1 instanceof Symbol) {
					if(env.findDatum(c1) instanceof MacroDefinition) {
						return b;
					}
				} else if(c1 instanceof SymbolScope) {
					Datum d1 =
						env.findDatum(((SymbolScope)c1).getSymbol());

					if(d1 instanceof SynDefineMacro) {
						return b;
					}
				}
				/*if(DEFMACRO.equals(((Cons)body).getCar())) {
					return body;
				}*/
			}

			// macro expansion
			loop = expandMacro1i(b, env, exec, memento);
		} while(loop.dirty);
//		System.out.println(LispUtils.print(loop.d));
		return loop.d;
	}

	//
	private static final Scheme AUX_S;

	//
	private static class EqWithSyn extends BinaryArgs {

		@Override
		protected Datum execute(
				Datum c1a, Datum c2a,
				Environment env, LispMessage mesg) {
			if(c1a instanceof SymbolScope &&
					c2a instanceof SymbolScope) {
				SymbolScope s1 = (SymbolScope)c1a;
				SymbolScope s2 = (SymbolScope)c2a;

				return 	LispBoolean.getInstance(
						s1.getSymbol().equals(s2.getSymbol()) &&
						s1.isSameScope(s2));
			}
			return LispBoolean.FALSE;
		}

	}

	//
	private static class RmWithSyn extends UnaryArgs {

		@Override
		protected Datum execute(
				Datum c1a,
				Environment env, LispMessage mesg) {
			if(c1a instanceof SymbolScope) {
				SymbolScope s1 = (SymbolScope)c1a;

				return s1.getSymbol();
			}
			return c1a;
		}

	}

	static {
		AUX_S = new Scheme(
				Scheme.newRnRSEnv(Scheme.SCHEME_VERSION),
				LispMessage.getInstance());

		AUX_S.set("samescp?", new EqWithSyn());
		AUX_S.set("remvscp", new RmWithSyn());
		AUX_S.exec(
				"(define (mapv f vec)" +
				"  (let ((v1 (make-vector (vector-length vec))))" +
				"    (let loop ((i (vector-length vec)))" +
				"      (cond ((zero? i) v1)" +
				"            (else" +
				"              (vector-set!" +
				"                v1" +
				"                (- i 1)" +
				"                (f (vector-ref vec (- i 1))))" +
				"              (loop (- i 1)))))))");
		AUX_S.exec(
				"(define (asss x lis)" +
//				"(display (samescp? (caar lis) x)) (newline)" +
				"  (cond ((null? lis) #f)" +
				"        ((not (pair? (car lis)))" +
				"          (error (get-default-message" +
				"            'err.require.pair)))" +
				"        ((samescp? (caar lis) x) (car lis))" +
				"        (else (asss x (cdr lis)))))");
		AUX_S.exec("(define (letren x) (lren0 x '() #f))");
		AUX_S.exec(
				"(define (lren0 x as lv)" +
//				"(display x) (newline)" +
//				"(display as) (newline)" +
				"  (cond ((and #;(not lv)" +
				"              (pair? x)" +
				"              (memq (remvscp (car x)) '(let letrec)))" +
				"          (if (symbol? (cadr x))" +
				"              (chletn x" +
				"                      as" +
				"                      (cons" +
				"                        (cons (cadr x)" +
				"                              (remvscp (cadr x)))" +
				"                        (append" +
				"                          (makeasc (caddr x))" +
				"                          as))" +
				"                      lv)" +
				"              (chlet  x" +
				"                      as" +
				"                      (append (makeasc (cadr x)) as)" +
				"                      lv)))" +
				"        ((and #;(not lv)" +
				"              (pair? x)" +
				"              (memq (remvscp (car x)) '(let* letrec*)))" +
				"          (chlet* x" +
				"                  as" +
				"                  (append (makeasc (cadr x)) as)" +
				"                  lv))" +
				"        ((and #;(not lv)" +
				"              (pair? x)" +
				"              (memq (remvscp (car x)) '(lambda)))" +
				"          (let ((as2 (append (makeascl (cadr x)) as)))" +
				"            (cons (car x)" +
				"             (cons (lren0 (cadr x) as2 lv)" +
				"              (lren0 (cddr x) as2 lv)))))" +
				"        ((pair? x)" +
				"          (cons (lren0 (car x) as lv)" +
				"                (lren0 (cdr x) as lv)))" +
				"        ((vector? x)" +
				"          (mapv (lambda (x) (lren0 x as lv)) x))" +
				"        ((and #;(not lv) (asss x as))" +
				"          (cdr (asss x as)))" +
				"        (else x)))" +
				"");
		AUX_S.exec(
				"(define (makeasc x)" +
				"  (cond ((null? x) '())" +
				"        (else (cons (cons (caar x)" +
				"                          (remvscp (caar x)))" +
				"                    (makeasc (cdr x))))))");
		AUX_S.exec(
				"(define (makeascl x)" +
				"  (cond ((null? x) '())" +
				"        (else" +
				"         (cons (cons (car x) (remvscp (car x)))" +
				"          (makeascl (cdr x))))))");
		AUX_S.exec(
				"(define (chlet x a0 as lv)" +
				"  (cons (car x)" +
				"        (cons (map (lambda (x)" +
				"                     (cons (asssif (car x) as)" +
				"                           (lren0 (cdr x) a0 lv)))" +
				"                   (cadr x))" +
				"              (lren0 (cddr x) as lv))))");
		AUX_S.exec(
				"(define (chlet*-aux x a0 lv)" +
				"  (cond ((null? x) '())" +
				"        (else" +
				"          (cons" +
				"            (cons (remvscp (caar x))" +
				"                  (lren0 (cdar x) a0 lv))" +
				"            (chlet*-aux" +
				"              (cdr x)" +
				"              (cons" +
				"                (cons (caar x) (remvscp (caar x)))" +
				"                a0)" +
				"              lv)))))");
		AUX_S.exec(
				"(define (chlet* x a0 as lv)" +
				"  (cons (car x)" +
				"        (cons (chlet*-aux (cadr x) a0 lv)" +
				"              (lren0 (cddr x) as lv))))");
		AUX_S.exec(
				"(define (chletn x a0 as lv)" +
//				"(display as) (newline)" +
				"  (cons (car x) #;(asssif (car x) as)" +
				"        (cons (asssif (cadr x) as)" +
				"              (cons (map (lambda (x)" +
				"                           (cons (asssif (car x) as)" +
				"                                 (lren0" +
				"                                   (cdr x) a0 lv)))" +
				"                         (caddr x))" +
				"                    (lren0 (cdddr x) as lv)))))");
		AUX_S.exec(
				"(define (asssif x as)" +
//				"(display (asss x as)) (newline)" +
				"  (if (asss x as) (cdr (asss x as)) x))");
	}

}
