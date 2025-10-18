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
import java.util.Set;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
/*package*/ final class SyntaxUtils {

	//
	private static final Symbol SYNTAX_RULES =
		Symbol.getSymbol("syntax-rules");

	//
	private static final Symbol IDENTIFIER_SYNTAX =
		Symbol.getSymbol("identifier-syntax");

	//
	private static final Symbol SETBANG = Symbol.getSymbol("set!");

	//
	private SyntaxUtils() {
		// do nothing
	}

	//
	/*package*/ static interface SafeWrap {

		//
		public Datum getWrapee();

	}

	//
	/*package*/ static boolean isValidSymbolList(Datum d) {
		Datum p = d;

		if(d.isTypeSymbol()) {
			return true;
		}

		while(p != Nil.NIL) {
			if(p instanceof Cons) {
				Cons c = (Cons)p;

				if(!c.getCar().isTypeSymbol()) {
					return false;
				}
				p = c.getCdr();
			} else {
				return p.isTypeSymbol();
			}
		}
		return true;
	}

	//
	/*package*/ static void compileBind(
			Datum sym,
			Datum bcdr,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder build,
			Cons callsym,
			boolean toplevel,
			LispMessage mesg,
			String ercd,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento,
			LispCompiler.MiscInfo syncased) {
		if(bcdr instanceof Cons) {
			Datum d1 = ((Cons)bcdr).getCar();

			// create a temporary Closure
			CompiledCode.Builder nbuild = new CompiledCode.Builder();
			Environment  nenv = new Environment(env);

			comp.compile(
					d1, nenv, nbuild, callsym, true,
					new ArrayList<Cons>(),
					exec, memento, syncased);
			nbuild.addReturnOp();

			// create a temporary Closure
			ClosureClass cl = new ClosureClass(
					Nil.NIL, nbuild.getCodeRef());

			// call the temporary Closure
			build.addPush(cl);
			build.addBeginList();
			build.addEndList();
			build.addCall();

			if(toplevel) {
				Symbol r;

				//if(sym instanceof Symbol) {
				//	r = ((Symbol)sym).getEnclosedSymbol();
				//} else {
				//	r = ((SymbolScope)sym).getSymbol();
				//	r = r.getEnclosedSymbol();
				//}
				if(sym instanceof SymbolName) {
					r = ((SymbolName)sym).getSymbol();
					r = r.getEnclosedSymbol();
				} else {
					throw mesg.getError("err.require.symbol");
				}
				build.addBind(r);
				build.addPush(sym);
			} else {
				build.addBind(sym);
				build.addPush(Undef.UNDEF);
			}
			//build.addBind(sym);
			//build.addPush(bcar);
			/////////build.addPush(Undef.UNDEF);
		} else {
			//throw new LispException("Syntax error");
			throw mesg.getError(ercd);
		}
	}

	//
	/*package*/ static void compileBindMacro(
			Datum sym,
			Datum bcdr,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder build,
			Cons callsym,
			LispMessage mesg,
			String ercd,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento,
			LispCompiler.MiscInfo syncased) {
		if(bcdr instanceof Cons) {
			Datum d1 = ((Cons)bcdr).getCar();

			// create a temporary Closure
			CompiledCode.Builder nbuild = new CompiledCode.Builder();
			Environment  nenv = new Environment(env);

			comp.compile(
					d1, nenv, build, callsym, true,
					new ArrayList<Cons>(),
					exec, memento, syncased);
			nbuild.addReturnOp();

			// create a temporary Closure
			ClosureClass cl = new ClosureClass(
					Nil.NIL, nbuild.getCodeRef());

			// call the temporary Closure
			build.addPush(cl);
			build.addBeginList();
			build.addEndList();
			build.addCall();

			build.addBindMacro(sym);
			//build.addPush(bcar);
			build.addPush(Undef.UNDEF);
		} else {
			//throw new LispException("Syntax error");
			throw mesg.getError(ercd);
		}
	}

	//
	/*package*/ static void compileBindMacro72(
			Datum sym,
			Datum bcdr,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder build,
			Cons callsym,
			LispMessage mesg,
			String ercd,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento,
			LispCompiler.MiscInfo syncased) {
		if(bcdr instanceof Cons) {
			Datum d1 = ((Cons)bcdr).getCar();

			// create a temporary Closure
			CompiledCode.Builder nbuild = new CompiledCode.Builder();
			Environment  nenv = new Environment(env);

			comp.compile(
					d1, nenv, build, callsym, true,
					new ArrayList<Cons>(),
					exec, memento, syncased);
			nbuild.addReturnOp();

			// create a temporary Closure
			ClosureClass cl = new ClosureClass(
					Nil.NIL, nbuild.getCodeRef());

			// call the temporary Closure
			build.addPush(cl);
			build.addBeginList();
			build.addEndList();
			build.addCall();

			build.addBindMacro72(sym);
			//build.addPush(bcar);
			build.addPush(Undef.UNDEF);
		} else {
			//throw new LispException("Syntax error");
			throw mesg.getError(ercd);
		}
	}

	//
	/*package*/ static void compileBindMacroQuote(
			Datum sym,
			Datum bcdr,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder build,
			Cons callsym,
			LispMessage mesg,
			String ercd,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento,
			LispCompiler.MiscInfo syncased) {
		if(bcdr instanceof Cons) {
			Datum d1 = ((Cons)bcdr).getCar();

			// create a temporary Closure
			CompiledCode.Builder nbuild = new CompiledCode.Builder();
			Environment  nenv = new Environment(env);

			comp.compile(
					d1, nenv, build, callsym, true,
					new ArrayList<Cons>(),
					exec, memento, syncased);
			nbuild.addReturnOp();

			// create a temporary Closure
			ClosureClass cl = new ClosureClass(
					Nil.NIL, nbuild.getCodeRef());

			// call the temporary Closure
			build.addPush(cl);
			build.addBeginList();
			build.addEndList();
			build.addCall();

			build.addBindMacroQuote(sym);
			//build.addPush(bcar);
			build.addPush(Undef.UNDEF);
		} else {
			//throw new LispException("Syntax error");
			throw mesg.getError(ercd);
		}
	}

	//
	/*package*/ static void compileList(
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
			boolean toplv,
			LispCompiler.MiscInfo syncased) {
		Datum bcdr = b;

		nbuild.addPush(Undef.UNDEF);
		while(bcdr != Nil.NIL) {
			if(bcdr instanceof Cons) {
				Cons bc = (Cons)bcdr;
				boolean tl = istail && (bc.getCdr() == Nil.NIL);

				// dispose previous value
				nbuild.addPop();
				comp.compile(
						bc.getCar(), env, nbuild, toplv,
						callsym, tl, symlist,
						exec, memento, syncased);
				bcdr = bc.getCdr();
			} else {
				throw mesg.getError("err.explist.malform");
			}
		}
	}

	//
	/*package*/ static void compileList(
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
		compileList(
				b, env, comp, nbuild,
				callsym, istail, mesg,
				symlist, exec, memento, false,
				syncased);
	}

	//
	/*package*/ static void compileListApply(
			Datum b,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder nbuild,
			Cons callsym,
			LispMessage mesg,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento,
			LispCompiler.MiscInfo syncased) {
		Datum bcdr = b;

		while(bcdr != Nil.NIL) {
			if(bcdr instanceof Cons) {
				Cons bc = (Cons)bcdr;

				comp.compile(
						bc.getCar(),
						env, nbuild, callsym, false, symlist,
						exec, memento, syncased);
				nbuild.addAppendList();
				bcdr = bc.getCdr();
			} else {
				throw mesg.getError("err.explist.malform");
			}
		}
	}

	//
	/*package*/ static Datum replaceLocalValsList(
			Datum b,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg,
			boolean toplv,
			int ttype) {
		Datum bcdr = b;
		List<Datum> lst = new ArrayList<Datum>();

		while(bcdr != Nil.NIL) {
			if(bcdr instanceof Cons) {
				lst.add(comp.replaceLocalVals(
						((Cons)bcdr).getCar(),
						env, ienv, toplv, ttype));
				bcdr = ((Cons)bcdr).getCdr();
			} else {
				throw mesg.getError("err.explist.malform");
			}
		}

		return LispUtils.listToCons(lst);
	}

	//
	/*package*/ static Datum replaceLocalValsList(
			Datum b,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg,
			int ttype) {
		return replaceLocalValsList(
				b, env, comp, ienv, mesg, false, ttype);
	}

	//
	/*package*/ static Datum putSymbol(
			Environment ienv, Datum p, LispMessage mesg) {
		if(p instanceof Symbol) {
			//Symbol res = Symbol.gensym();
			Symbol res = Symbol.encloseSymbol((Symbol)p);

			ienv.bindDatum(p, res);
			return res;
		} else if(p instanceof SymbolScope) {
			Symbol s = ((SymbolScope)p).getSymbol();
			//Symbol res = Symbol.gensym();
			//Symbol res = Symbol.encloseSymbol(s);
			Symbol res;

			res = ((SymbolScope)p).isCaptured() ?
					s : Symbol.encloseSymbol(s);
			ienv.bindDatum(s, res);
			return res;
		} else if(p instanceof PatternMatch.IndSym) {
			return p;
		} else {
			//System.out.println(p);
			throw mesg.getError("err.symbol");
		}
	}

	//
	/*package*/ static Datum addLocalValsAll(
			Environment ienv, Datum d, LispMessage mesg) {
		Datum p = d;
		List<Datum> lst = new ArrayList<Datum>();

		while(p != Nil.NIL) {
			if(p instanceof Cons) {
				Datum r = putSymbol(ienv, ((Cons)p).getCar(), mesg);
				p = ((Cons)p).getCdr();

				lst.add(r);
			} else {
				return LispUtils.listToCons(
						lst, putSymbol(ienv, p, mesg));
				//break;
			}
		}
		return LispUtils.listToCons(lst);
	}

	//
	private static void processRule(
			Symbol name,
			Datum body,
			List<Datum> pat,
			List<Datum> tmp,
			List<Set<Symbol>> stl,
			Set<Symbol> reserve,
			LispMessage mesg) {
		List<Datum> lst = LispUtils.consToList(body, mesg);
		Set<Symbol> st = new HashSet<Symbol>();
		//Set<Symbol> sz = NullSet.getInstance();
		PatternDepthMap mpp = new PatternDepthMap();
		PatternDepthMap mpt = new PatternDepthMap();
		Datum ptd;

		if(lst.size() != 2) {
			throw mesg.getError("err.syntaxrules.malform");
		}

		// get a pattern
		//if(lst.get(0) instanceof Cons) {
		//	Cons l0 = (Cons)lst.get(0);
		//	
		//	if(!(l0 instanceof Cons)) {
		//		//throw new LispException("pattern needed");
		//		throw mesg.getError("err.syntaxrules.nopattern");
		//	}
		//	
		//	try {
		//		//ptd = PatternMatch.compilePattern(
		//		//		l0.getCdr(), mpp, st, reserve);
		//		ptd = PatternMatch.compilePattern(l0, mpp, st, reserve);
		//	} catch (PatternEllipsisException e) {
		//		throw mesg.getError("err.wrongellipsis");
		//	}
		//	pat.add(ptd);
		//} else {
		//	throw mesg.getError("err.syntaxrules.malform");
		//}
		try {
			ptd = PatternMatch.compilePattern(
					lst.get(0), mpp, st, reserve);
		} catch (PatternEllipsisException e) {
			throw mesg.getError("err.wrongellipsis");
		}
		pat.add(ptd);

		// validate the template
		Datum tpl = PatternMatch.compileTemplate(lst.get(1), mpt);
		try {
			PatternMatch.validateLevel(ptd, tpl, st);
		} catch (PatternDepthException e) {
			throw mesg.getError("err.wronglevel", e.getMessage());
		}

		// get a template
		tmp.add(lst.get(1));

		// add a set of repeating strings
		stl.add(st);
	}

	//
	/*package*/ static UserSyntax processSyntaxRules(
			Symbol name,
			Datum body,
			Environment env,
			LispMessage mesg,
			UserSyntax rootsyn) {
		List<Datum> lst = LispUtils.consToList(body, mesg);

		if(lst.size() < 2) {
			throw mesg.getError("err.syntaxrules.malform");
		}

		// get reserved words
		Set<Symbol> reserve =
			SyntaxUtils.consToSymbolSet(lst.get(0), mesg);

		// get rules
		List<Datum> pat = new ArrayList<Datum>();
		List<Datum> tmp = new ArrayList<Datum>();
		List<Set<Symbol>> stl = new ArrayList<Set<Symbol>>();

		for(int i = 1; i < lst.size(); i++) {
			processRule(
					name, lst.get(i), pat, tmp, stl, reserve, mesg);
		}
		return new UserSyntax(
				name.getName(), pat, tmp, stl, reserve, env,
				rootsyn);
	}

	//
	/*package*/ static UserSyntax processRuleDesc(
			Symbol name,
			Datum body,
			Environment env,
			LispMessage mesg,
			UserSyntax rootsyn) {
		if(!(body instanceof Cons)) {
			throw mesg.getError("err.syntaxrules.malform");
		}

		Cons b1 = (Cons)body;

		if(SYNTAX_RULES.equals(b1.getCar())) {
			return processSyntaxRules(
					name, b1.getCdr(), env, mesg, rootsyn);
		} else {
			throw mesg.getError("err.syntaxrules.malform");
		}
	}

	//
	/*package*/ static boolean isSyntaxRules(Datum body) {
		if(!(body instanceof Cons)) {
			return false;
		} else {
			return SYNTAX_RULES.equals(((Cons)body).getCar());
		}
	}

	//
	/*package*/ static boolean isIdentifierSyntax(Datum body) {
		if(!(body instanceof Cons)) {
			return false;
		} else {
			return IDENTIFIER_SYNTAX.equals(((Cons)body).getCar());
		}
	}

	//
	/*package*/ static Set<Symbol> consToSymbolSet(
			Datum d, LispMessage mesg) {
		Set<Symbol> res = new HashSet<Symbol>();
		Datum dd = d;

		while(dd != Nil.NIL) {
			if(dd instanceof Cons) {
				Cons cd = (Cons)dd;

				if(cd.getCar() instanceof SymbolName) {
					Symbol smb = ((SymbolName)cd.getCar()).getSymbol();

					//res.add((Symbol)cd.getCar());
					res.add(smb);
				} else {
					throw mesg.getError("err.list.symbol");
				}

				dd = ((Cons)dd).getCdr();
			} else {
				throw mesg.getError("err.list");
			}
		}

		return res;
	}

	//
	/*package*/ static boolean equalsReserved(Symbol rword, Datum d) {
		if(d instanceof SymbolName) {
			Symbol s1 = ((SymbolName)d).getSymbol();

			return Symbol.DEFAULT_NAMESPACE.getSymbol(
					s1.getName()).equals(rword);
		} else {
			return rword.equals(d);
		}
	}

	//
	/*package*/ static Datum removeScope1(
			Datum d, Collection<Datum> col) {
		if(d instanceof SymbolName) {
			Symbol s = ((SymbolName)d).getSymbol();

			if(col.contains(s)) {
				return s;
			}
		}
		return d;
	}

	//
	/*package*/ static Datum removeScope(
			Datum body, Collection<Datum> col) {
		ConsIterator    itr = new ConsIterator(body);

		if(itr.hasNext()) {
			ConsListBuilder bld = new ConsListBuilder();

			while(itr.hasNext()) {
				Datum c = itr.next();

				bld.append(removeScope(c, col));
			}
			return bld.get(removeScope1(itr.getTerminal(), col));
		} else {
			return removeScope1(itr.getTerminal(), col);
		}
	}

	//
	private static Datum _caaaddr(Datum d) {
		Datum e = d;

		if(!(e instanceof Cons)) {
			return Undef.UNDEF;
		} else if(!((e = ((Cons)e).getCdr()) instanceof Cons)) {
			return Undef.UNDEF;
		} else if(!((e = ((Cons)e).getCdr()) instanceof Cons)) {
			return Undef.UNDEF;
		} else if(!((e = ((Cons)e).getCar()) instanceof Cons)) {
			return Undef.UNDEF;
		} else if(!((e = ((Cons)e).getCar()) instanceof Cons)) {
			return Undef.UNDEF;
		} else {
			return ((Cons)e).getCar();
		}
	}

	/**
	 * @param name
	 * @param car
	 * @param mesg
	 * @return
	 */
	public static Datum processIdentifierSyntax(Symbol name, Datum car,
			Environment env, LispMessage mesg) {
		UserSyntax syn;
		Datum ptd, ptn, tmp, tpl;

		if(_caaaddr(car).equals(SETBANG)) {
			Set<Symbol> res = Collections.emptySet();
			Set<Symbol> st  = new HashSet<Symbol>();
			PatternDepthMap mpp = new PatternDepthMap();
			PatternDepthMap mpt = new PatternDepthMap();

			tmp = IntLispUtils.cadaddr(car, mesg);
			ptn = IntLispUtils.caddaaddr(car, mesg);
			try {
				ptd = PatternMatch.compilePattern(ptn, mpp, st, res);
			} catch (PatternEllipsisException e) {
				throw mesg.getError("err.wrongellipsis");
			}

			// validate the template
			tpl = PatternMatch.compileTemplate(tmp, mpt);
			try {
				PatternMatch.validateLevel(ptd, tpl, st);
			} catch (PatternDepthException e) {
				throw mesg.getError("err.wronglevel", e.getMessage());
			}

			syn = new UserSyntax(
					name.getName(),
					Collections.singletonList(ptd),
					Collections.singletonList(tpl),
					Collections.singletonList(st),
					res,
					env,
					null);
			return new UserIdentifierSyntax(
					name.getName(),
					IntLispUtils.cadadr(car, mesg),
					syn);
		} else {
			return new UserIdentifierSyntax(
					name.getName(),
					IntLispUtils.cadr(car, mesg));
		}
	}

}
