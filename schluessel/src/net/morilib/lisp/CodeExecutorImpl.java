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

import java.beans.IntrospectionException;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;
import java.util.logging.Logger;

import net.morilib.lisp.CompiledCode.Oper;
import net.morilib.lisp.chihaya.LispSyntax;
import net.morilib.lisp.r6rs.LibraryID;
import net.morilib.lisp.r6rs.LibraryIDException;
import net.morilib.lisp.sos.LispClass;
import net.morilib.lisp.sos.LispType;
import net.morilib.lisp.sos.LispTypeList;
import net.morilib.lisp.util.LogEnv;
import net.morilib.util.ArrayListStack;
import net.morilib.util.Stack2;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
/*package*/ class CodeExecutorImpl implements CodeExecutor {

	//
	private static Logger _log = LogEnv.init("schlush.vm");

	//
	private LispMessage message;

	//
	/*package*/ CodeExecutorImpl(LispMessage msg) {
		message = msg;
	}

	//
	private static class PrRef {

		private Datum proc;

		/*private static PrRef newIn(Datum ref) {
			if(ref == null) {
				return null;
			} else {
				PrRef res = new PrRef();

				res.proc = ref;
				return res;
			}
		}*/

		public String toString() {
			return "<" + proc + ">";
		}

	}

	private static class Flags {

		private static final Flags INS = new Flags();
		private static final Flags LOAD = new Flags();
		private static final Flags LOAD_OV = new Flags();

	}

	private /*static*/ class Memento implements IntStack {

		//private Stack<Integer>      addrStk   = new Stack<Integer>();
		//private Stack<CompiledCode> codeStk   = new Stack<CompiledCode>();
		//private Stack<Environment>  envStk    = new Stack<Environment>();
		//private Stack<Datum>        dataStk   = new Stack<Datum>();
		//private Stack<List<Datum>>  workStk   = new Stack<List<Datum>>();
		////private Stack<Subr>         callerStk = new Stack<Subr>();
		//private Stack<Datum>        callerStk = new Stack<Datum>();
		//private Stack<Flags>        flgStk    = new Stack<Flags>();
		//
		//// for dynamic-wind
		////private Stack<Datum>        beforeStk = new Stack<Datum>();
		//private Stack<PrRef>        beforeStk = new Stack<PrRef>();
		//private Stack<PrRef>        afterStk  = new Stack<PrRef>();
		//private Stack<Stack<Datum>> pushbkStk = new Stack<Stack<Datum>>();
		//private Stack<Integer>      stateStk  = new Stack<Integer>();
		//private List<PrRef>         befLst    = new ArrayList<PrRef>();
		//
		//// for delay, force
		//private Stack<Promise>      memoStk   = new Stack<Promise>();
		//
		//// for exception handler
		//private Stack<ExcHandler>   hndlStk   = new Stack<ExcHandler>();
		////private Stack<Object>       contStk   = new Stack<Object>();

		private Stack2<Integer>      addrStk   =
			new ArrayListStack<Integer>();
		private Stack2<CompiledCode> codeStk   =
			new ArrayListStack<CompiledCode>();
		private Stack2<Environment>  envStk    = 
			new ArrayListStack<Environment>();
		private Stack2<Datum>        dataStk   = 
			new ArrayListStack<Datum>();
		//private Stack2<List<Datum>>  workStk   = 
		//	new ArrayListStack<List<Datum>>();
		private Stack2<ConsListBuilder>  workStk   = 
			new ArrayListStack<ConsListBuilder>();
		//private Stack2<Subr>         callerStk = new Stack<Subr>();
		private Stack2<Datum>        callerStk = 
			new ArrayListStack<Datum>();
		private Stack2<Flags>        flgStk    = 
			new ArrayListStack<Flags>();

		// for dynamic-wind
		//private Stack2<Datum>        beforeStk = new Stack<Datum>();
		private Stack2<PrRef>        beforeStk = 
			new ArrayListStack<PrRef>();
		private Stack2<PrRef>        afterStk  = 
			new ArrayListStack<PrRef>();
		private Stack2<Stack2<Datum>> pushbkStk = 
			new ArrayListStack<Stack2<Datum>>();
		private Stack2<Integer>      stateStk  = 
			new ArrayListStack<Integer>();
		private List<PrRef>         befLst    = new ArrayList<PrRef>();

		// for delay, force
		private Stack2<Promise>      memoStk   = 
			new ArrayListStack<Promise>();

		// for exception handler
		private Stack2<ExcHandler>   hndlStk   = 
			new ArrayListStack<ExcHandler>();
		//private Stack<Object>       contStk   = new Stack<Object>();

		// for finnaly
		private Stack2<CompiledCode> initStk   =
			new ArrayListStack<CompiledCode>();
		private Stack2<CompiledCode> finlStk   =
			new ArrayListStack<CompiledCode>();
		private CompiledCode aftret = new CompiledCode();

		private Memento() {
			//do nothing
		}

		public Memento copy() {
			Memento r = new Memento();

			r.addrStk.addAll(addrStk);
			r.codeStk.addAll(codeStk);
			//r.envStk.addAll(envStk);
			r.dataStk.addAll(dataStk);
			r.callerStk.addAll(callerStk);
			r.beforeStk.addAll(beforeStk);
			r.afterStk.addAll(afterStk);
			r.memoStk.addAll(memoStk);
			r.pushbkStk.addAll(pushbkStk);
			r.stateStk.addAll(stateStk);
			//r.hndlStk.addAll(hndlStk);
			for(ExcHandler h : hndlStk.toList()) {
				r.hndlStk.add(h.copy());
			}

			for(ConsListBuilder l : workStk.toList()) {
				r.workStk.add(new ConsListBuilder(l, message));
			}

			// 環境はコピーしなければダメ
			for(Environment e : envStk.toList()) {
				//r.envStk.add(e);
				r.envStk.add(e.copyNotRoot());
			}

			r.befLst.addAll(befLst);
			r.flgStk.addAll(flgStk);
			r.initStk.addAll(initStk);
			r.finlStk.addAll(finlStk);
			return r;
		}

		private void overwrite(Memento m) {
			addrStk = m.addrStk;
			codeStk = m.codeStk;
			envStk = m.envStk;
			dataStk = m.dataStk;
			workStk = m.workStk;
			callerStk = m.callerStk;
			beforeStk = m.beforeStk;
			afterStk = m.afterStk;
			memoStk = m.memoStk;
			pushbkStk = m.pushbkStk;
			stateStk = m.stateStk;
			hndlStk = m.hndlStk;
			flgStk = m.flgStk;
			initStk = m.initStk;
			finlStk = m.finlStk;
		}

		/*private void pushInheritBeforeStk() {
			beforeStk.push(
					beforeStk.isEmpty() ? null : beforeStk.peek());
		}*/

		private void push(
				int addr, CompiledCode code, Environment env,
				ExcHandler hndl, Datum callee) {
			addrStk.push(addr);
			codeStk.push(code);
			envStk.push(env);
			callerStk.push(callee);
			beforeStk.push(null);
			//pushInheritBeforeStk();
			afterStk.push(null);
			memoStk.push(null);
			pushbkStk.push(null);
			stateStk.push(0);
			hndlStk.push(hndl/*.copy()*/);
			flgStk.push(Flags.INS);
			initStk.push(null);
			finlStk.push(null);
		}

		private void push(
				int addr, CompiledCode code, Environment env,
				ExcHandler hndl, Datum callee, Flags flgs) {
			addrStk.push(addr);
			codeStk.push(code);
			envStk.push(env);
			callerStk.push(callee);
			beforeStk.push(null);
			//pushInheritBeforeStk();
			afterStk.push(null);
			memoStk.push(null);
			pushbkStk.push(null);
			stateStk.push(0);
			hndlStk.push(hndl/*.copy()*/);
			flgStk.push(flgs);
			initStk.push(null);
			finlStk.push(null);
		}

		private void push(
				int addr, CompiledCode code, Environment env,
				Promise p,
				ExcHandler hndl) {
			addrStk.push(addr);
			codeStk.push(code);
			envStk.push(env);
			callerStk.push(p);
			beforeStk.push(null);
			//pushInheritBeforeStk();
			afterStk.push(null);
			memoStk.push(p);
			pushbkStk.push(null);
			stateStk.push(0);
			hndlStk.push(hndl/*.copy()*/);
			flgStk.push(Flags.INS);
			initStk.push(null);
			finlStk.push(null);
		}

		public String toString() {
			StringBuilder bld = new StringBuilder();

			bld.append(addrStk.size()).append(" , ");
			bld.append(codeStk.size()).append(" , ");
			bld.append(envStk.size()).append(" , ");
			bld.append(dataStk.size()).append(" , ");
			bld.append(workStk.size()).append(" , ");
			bld.append(callerStk.size()).append(" , ");
			bld.append(beforeStk.size()).append(" , ");
			bld.append(afterStk.size()).append(" , ");
			bld.append(memoStk.size()).append(" , ");
			bld.append(pushbkStk.size()).append(" , ");
			bld.append(stateStk.size()).append(" , ");
			bld.append(hndlStk.size()).append("\n");
			bld.append(addrStk).append("\n");
			//bld.append(codeStk).append("\n");
			bld.append(envStk).append("\n");
			bld.append(dataStk).append("\n");
			bld.append(workStk).append("\n");
			bld.append(callerStk).append("\n");
			bld.append(beforeStk).append("\n");
			bld.append(afterStk).append("\n");
			bld.append(memoStk).append("\n");
			return bld.toString();
		}

		private void desc(StringBuilder buf, Datum d) {
			if(d instanceof Subr) {
				Subr s = (Subr)d;

				if(s.symbolName != null &&
						!s.symbolName.equals("error")) {
					buf.append("  -");
					buf.append(message.get("err.stacktrace.subr"));
					buf.append(" ");
					buf.append(s.symbolName);
					buf.append("\n");
				} else {
					//buf.append(Integer.toString(s.hashCode(), 16));
				}
			} else if(d instanceof Closure) {
				Closure c = (Closure)d;

				if(c.getName() != null) {
					buf.append("  -");
					buf.append(message.get("err.stacktrace.closure"));
					buf.append(" ");
					buf.append(c.getName());
					buf.append("\n");
				} else {
					//buf.append(Integer.toString(c.hashCode(), 16));
				}
			} else if(d instanceof Promise) {
				Promise p = (Promise)d;

				buf.append("  -");
				buf.append(message.get("err.stacktrace.promise"));
				buf.append(" ");
				buf.append(Integer.toString(p.hashCode(), 16));
				buf.append("\n");
			} else {
				buf.append("  -");
				buf.append(LispUtils.print(d));
				buf.append("\n");
			}
		}

		public String getStackTrace() {
			StringBuilder buf = new StringBuilder();

			for(int i = callerStk.size() - 1; i >= 0; i--) {
				//buf.append("  -");
				desc(buf, callerStk.get(i));
			}
			return buf.toString();
		}

	}

	private static class ExcHandler {
		private Stack<Integer>      lablStk = new Stack<Integer>();
		private Stack<Datum>        hndlStk = new Stack<Datum>();
		private Stack<CompiledCode> codeStk = new Stack<CompiledCode>();
		private Stack<Environment>  envStk  = new Stack<Environment>();
		private Stack<Memento>      memStk  = new Stack<Memento>();

		private ExcHandler copy() {
			ExcHandler res = new ExcHandler();

			res.lablStk.addAll(lablStk);
			res.hndlStk.addAll(hndlStk);
			res.codeStk.addAll(codeStk);
			res.envStk.addAll(envStk);
			res.memStk.addAll(memStk);
			return res;
		}

	}

	private Datum evalLispMapValid(Datum body) {
		List<Datum> lst1 = LispUtils.consToList(body, message);

		if(lst1.size() < 2) {
			throw message.getError("err.argument");
			//throw new LispException("arguments too few");
		}
		return Undef.UNDEF;
	}

	private Datum evalLispMap(Datum body) {
		List<Datum> lst1 = LispUtils.consToList(body, message);
		List<Datum> lst2 = new ArrayList<Datum>(lst1);
		//List<List<Datum>> lst2 = new ArrayList<List<Datum>>();

		//for(int i = 0; i < lst1.size(); i++) {
		//	lst2.add(LispUtils.consToList(lst1.get(i), message));
		//}

		List<Datum> no2 = new ArrayList<Datum>();

		retPoint:
//		for(int i = 0;; i++) {
		while(true) {
			List<Datum> arg = new ArrayList<Datum>();

			//for(int j = 0; j < lst2.size(); j++) {
			//	if(lst2.get(j).size() <= i) {
			//		break retPoint;
			//	}
			//	arg.add(lst2.get(j).get(i));
			//}
			for(int j = 0; j < lst1.size(); j++) {
				Datum d = lst1.get(j);

				if(d instanceof Cons) {
					arg.add(((Cons)d).getCar());
					lst1.set(j, ((Cons)d).getCdr());
				} else if(d == Nil.NIL) {
					break retPoint;
				} else {
					throw message.getError("err.list", lst2.get(j));
				}
			}
			no2.add(LispUtils.listToCons(arg));
		}

		return LispUtils.listToCons(no2);
	}

	private Datum evalApplyValid(Datum body) {
		List<Datum> lst1 = LispUtils.consToList(body, message);
		if(lst1.size() < 2) {
			throw message.getError("err.argument");
			//throw new LispException("arguments too few");
		}

		return Undef.UNDEF;
	}

	private Datum evalApply(Datum body) {
		List<Datum> lst1 = LispUtils.consToList(body, message);
		List<Datum> arg  = new ArrayList<Datum>();
		List<Datum> elst;

		// 引数オブジェクトの取得
		for(int i = 0; i < lst1.size() - 1; i++) {
			arg.add(lst1.get(i));
		}

		// 残余引数の取得
		elst = LispUtils.consToList(lst1.get(lst1.size() - 1), message);
		arg.addAll(elst);

		return LispUtils.listToCons(arg);
	}


	private void callSubr(
			ISubr sub, int addr,
			Environment env, Datum d3d, Memento m,
			ExcHandler hndl, Datum callee) {
		m.addrStk.push(addr);
		m.codeStk.push(null);
		m.envStk.push(env);
		m.callerStk.push(callee);
		m.beforeStk.push(null);
		//m.pushInheritBeforeStk();
		m.afterStk.push(null);
		m.memoStk.push(null);
		m.pushbkStk.push(null);
		m.stateStk.push(0);
		m.hndlStk.push(hndl/*.copy()*/);
		m.flgStk.push(Flags.INS);
		m.initStk.push(null);
		m.finlStk.push(null);

		try {
			//m.dataStk.push(sub.eval(d3d, env, m, message));
			m.dataStk.push(sub.eval(d3d, env, message));
		} finally {
			m.addrStk.pop();
			m.codeStk.pop();
			m.envStk.pop();
			m.callerStk.pop();
			m.beforeStk.pop();
			m.afterStk.pop();
			m.memoStk.pop();
			m.pushbkStk.pop();
			m.stateStk.pop();
			m.hndlStk.pop();
			m.flgStk.pop();
			m.initStk.pop();
			m.finlStk.pop();
		}
	}

	private void setContinuationArgs(List<Datum> lst, Memento m) {
		/*if(lst.size() < 1) {
			m.dataStk.push(Undef.UNDEF);
		} else {
			m.dataStk.push(lst.get(0));
		}*/
		m.dataStk.push(MultiValues.newValues(lst));
	}

	/*private Closure makeLambda(Datum dt, Environment env) {
		ClosureClass clc;
		Closure      res;
		CompiledCode.Builder build = new CompiledCode.Builder();

		build.addPush(dt);
		build.addPush(Nil.NIL);
		build.addCall();
		build.addReturnOp();
		clc = new ClosureClass(Nil.NIL, build.getCodeRef());
		res = new Closure(clc, env);
		//System.err.println("cl:" + res);
		return res;
	}*/

	private ClosureClass makeLoad(
			InputPort ipt,
			Environment env,
			Memento memento) {
		CompiledCode.Builder b = new CompiledCode.Builder();
		LispCompiler comp = CompilerFactory.getInstance(message);
		ClosureClass clc;
		Datum d;

		ipt.skipShebang();
		while((d = ipt.readS()) != EOFObject.EOF) {
			// convert macro
			d = comp.expandMacro(d, env, this, memento);
			//System.out.println(LispUtils.getResult(d));

			// compile
			comp.compile(
					d, env, b, true, new Cons(), true,
					new LinkedList<Cons>(), this, memento,
					new LispCompiler.MiscInfo(null));
		}
		b.addPop();
		b.addPush(Undef.UNDEF);
		b.addReturnOp();
		clc = new ClosureClass(Nil.NIL, b.getCodeRef());

		return clc;
	}

	private Closure makeEval(
			Datum sexp,
			Environment env,
			Memento memento) {
		Datum d = sexp;
		CompiledCode.Builder b = new CompiledCode.Builder();
		LispCompiler comp = CompilerFactory.getInstance(message);
		ClosureClass clc;
		Closure      res;

		// macro conversion
		d = comp.expandMacro(d, env, this, memento);
		if(d instanceof LispSyntax) {
			d = ((LispSyntax)d).getWrapped();
		}
//		d = MacroUtils72.unquote(d, new EnvironmentObject(env, false));
		//System.out.println(LispUtils.getResult(d));

		// compile
		comp.compile(
				d, env, b, true, new Cons(), true,
				new LinkedList<Cons>(), this, memento,
				new LispCompiler.MiscInfo(null));
		b.addReturnOp();
		clc = new ClosureClass(Nil.NIL, b.getCodeRef());
		res = new Closure(clc, env);
		//System.err.println("cl:" + res);
		return res;
	}

	private ClosureClass makeImport(
			Datum body,
			Environment env,
			Memento memento) {
		CompiledCode.Builder b = new CompiledCode.Builder();
		ClosureClass clc;

		try {
			SynImport.buildImport(
					LibraryID.compileImport(body), b, message);
			b.addPush(Undef.UNDEF);
			b.addReturnOp();
			clc = new ClosureClass(Nil.NIL, b.getCodeRef());

			return clc;
		} catch (LibraryIDException e) {
			throw e.toLispException(message);
		}
	}

	private Environment calltail0(
			CompiledCode.Code c,
			Memento m,
			ExcHandler hndl,
			Closure cl) {
		//
		CompiledCode.Builder bb = new CompiledCode.Builder();

		// tail recursion
		_log.finer(
				"Remove " + c.getRewind() +
				" frame(s) for tail call");

		bb.merge(m.aftret);
		for(int i = 0; i < c.getRewind(); i++) {
			CompiledCode cc;

			m.envStk.pop();
			m.addrStk.pop();
			m.codeStk.pop();
			//m.dataStk.pop();
			//m.workStk.pop();
			m.callerStk.pop();
			m.beforeStk.pop();
			m.afterStk.pop();
			m.memoStk.pop();
			m.pushbkStk.pop();
			m.stateStk.pop();
			hndl  = m.hndlStk.pop();
			m.flgStk.pop();
			m.initStk.pop();
			if((cc = m.finlStk.pop()) != null) {
				bb.merge(cc);
			}
		}
		m.aftret = bb.getCodeRef();

		//env = cl.getEnvironment();
		Environment env = cl.getEnvironment().copyNotRoot();
		//System.out.println(env);

		_log.finer(
				"Tail jump top:" +
				cl.printName() + ":dtstk(" +
				m.dataStk.size() + "):cdstk(" +
				m.codeStk.size() + ")");
		return env;
	}

	private void bind0(Environment env, Symbol sym, Datum dest) {
		if(dest instanceof NamableDatum) {
			((NamableDatum)dest).setName(sym.getName());
		}
		env.bindDatum(sym, dest);
	}

	private boolean set0(Environment env, Symbol sym, Datum dest) {
		if(dest instanceof NamableDatum) {
			((NamableDatum)dest).setName(sym.getName());
		}

		try {
			UserIdentifierSyntax i0;
			Datum d0;

			while((d0 = env.getDatum(sym))
					instanceof UserIdentifierSyntax) {
				i0 = (UserIdentifierSyntax)d0;
				if(i0.toreplace instanceof Symbol) {
					sym = (Symbol)i0.toreplace;
				} else {
					throw message.getError("err.set.malform", sym);
				}
			}
			return env.setDatum(sym, dest);
		} catch (ReadOnlyException e) {
			throw message.getError("err.variablereadonly", sym);
		}
	}

	private void mbind0(Environment env, Symbol sym, Datum dest) {
		Macro mc;

		if(dest instanceof Closure) {
			mc = new Macro((Closure)dest);
			//((Closure)dest).setName(sym.getName());
		} else {
			throw message.getError("err.require.closure");
		}
		env.bindDatum(sym, mc);
	}

	private void mbind72(Environment env, Symbol sym, Datum dest) {
		Macro72 mc;

		if(dest instanceof Closure) {
			mc = new Macro72((Closure)dest);
			//((Closure)dest).setName(sym.getName());
		} else {
			throw message.getError("err.require.closure");
		}
		env.bindDatum(sym, mc);
	}

	private void mbindq(Environment env, Symbol sym, Datum dest) {
		MacroQuote mc;

		if(dest instanceof Closure) {
			mc = new MacroQuote((Closure)dest);
			//((Closure)dest).setName(sym.getName());
		} else {
			throw message.getError("err.require.closure");
		}
		env.bindDatum(sym, mc);
	}

	private Symbol getsym(Datum d) {
		if(d instanceof SymbolName) {
			return ((SymbolName)d).getSymbol();
		//} else if(d instanceof SymbolScope) {
		//	return ((SymbolScope)d).getSymbol();
		} else {
			throw message.getError("err.require.symbol");
		}
	}

	private void bind0m(Environment env, Symbol sym, Datum dest) {
		if(dest instanceof ClosureClassMethod) {
			ClosureClassMethod cm = (ClosureClassMethod)dest;
			//Closure ncl = new Closure(cm, env);
			Closure ncl = new Closure(cm, new Environment(env));
			Datum dtm = env.findDatum(sym);
			LispGeneric lmth;

			if(dtm instanceof LispGeneric) {
				lmth = (LispGeneric)dtm;
			} else {
				lmth = new LispGeneric(sym.getName());
				env.bindDatum(sym, lmth);
			}

			Datum pt = cm.getTypeList();
			List<LispType> lst = new ArrayList<LispType>();
			while(true) {
				if(pt instanceof Cons) {
					Cons c0 = (Cons)pt;

					if(c0.getCar() == SynDefineMethod.TOPSYM) {
						lst.add(LispType.TOP);
					} else {
						Datum fd = env.findDatum(getsym(c0.getCar()));

						if(!(fd instanceof LispClass)) {
							throw message.getError(
									"err.require.class");
						}
						lst.add(((LispClass)fd).getObjectType());
					}
					pt = c0.getCdr();
				} else {
					lmth.put(new LispTypeList(lst, pt != Nil.NIL),
							ncl);
					return;
				}
			}
		} else {
			throw new RuntimeException();
		}
	}

	private Closure findm00(LispGeneric mth, Datum prm) {
		Datum pt = prm;
		List<LispType> lst = new ArrayList<LispType>();

		while(true) {
			if(pt instanceof Cons) {
				Cons c0 = (Cons)pt;

				lst.add(c0.getCar().getType());
				pt = c0.getCdr();
			} else if(pt == Nil.NIL) {
				return mth.get(new LispTypeList(lst));
			} else {
				throw message.getError("err.list");
			}
		}
	}

	private Closure findm(LispGeneric mth, Datum prm) {
		Closure cl = findm00(mth, prm);

		if(cl == null) {
			throw message.getError(
					"err.method.notfound", mth.getName());
		}
		return cl;
	}

	private Datum consapl(LispGeneric mth, Datum prm) {
		return new Cons(mth, new Cons(prm, Nil.NIL));
	}

	private Closure findmapl(
			Environment env, LispGeneric mth, Datum prm) {
		Datum apl = env.findDatum(LispGeneric.APPLY_GENERIC);

		if(apl != null && apl instanceof LispGeneric) {
			return findm00((LispGeneric)apl, consapl(mth, prm));
		} else {
			return null;
		}
	}

	private Datum findmnext(LispGeneric mth, Datum prm) {
		Datum pt = prm;
		List<LispType> lst = new ArrayList<LispType>();

		while(true) {
			if(pt instanceof Cons) {
				Cons c0 = (Cons)pt;

				lst.add(c0.getCar().getType());
				pt = c0.getCdr();
			} else if(pt == Nil.NIL) {
				LispNextMethod cl =
					mth.getNextMethod(new LispTypeList(lst), prm);

				//return (cl != null) ? cl : LispMethod.NO_NEXT;
				return cl;
			} else {
				throw message.getError("err.list");
			}
		}
	}

	private Datum exec(
			CompiledCode   code1,
			Environment    env1,
			IntStack       memento,
			boolean        useCont) {
		int addr  = 0;
		//int state = 0;
		Memento       m = (Memento)memento;
		CompiledCode  code = code1;
		Environment   env = env1;
		Environment   tenv = null;
		ExcHandler    hndl = new ExcHandler();
		//int           expdepth = 1;
		//Stack<PrRef>  beforeStoreStk = new Stack<PrRef>();
		//Stack<Before> beforeArgStk = new Stack<Before>();
		//List<Datum>  beforeStoreQ = new LinkedList<Datum>();
		//List<Before> beforeArgQ = new LinkedList<Before>();
		//List<PrRef>   beforeLst = new ArrayList<PrRef>();
		//long t = System.currentTimeMillis();

		while(true) {
			try {
				CompiledCode.Code c;

				if(LispThread.isTerminated()) {
					LispThread.acceptTerminate();
					throw message.getError(
							"err.srfi18.threadterminated");
				}

				if((c = code.getCode(addr)) == null) {
					return Undef.UNDEF;
				}
				//IntLispUtils.timelog(_log, "Code " + c + " : ", t);
				//t = System.currentTimeMillis();

				//_log.finest(
				//		"[" + m.addrStk.size() + "] " +
				//		c.getOp() + "(" + c.getDatum() +
				//		"): " + m.dataStk);

				//System.out.println("    " + m.workStk);
				//System.out.println(c.getOp());
				//System.out.println(c.getOp() + ":" + c.getDatum());
				//System.out.println(m);
				//System.out.println("dstk:" + m.dataStk);

				//System.out.println(m.stateStk);
				//System.out.println(beforeStoreStk);
				//System.out.println(m.afterStk);
				//System.out.println(env);

				switch(c.getOp()) {
				case PUSH:
					Datum aaa = c.getDatum();

					if(c.getDatum() instanceof LispLazySubrSyn) {
						aaa = ((LispLazySubrSyn)aaa).setup();
					}

					if(aaa instanceof ClosureClass) {
						Closure ncl = new Closure(
								(ClosureClass)aaa, env);
//						Environment e2 = env.copyExceptRoot();
//						Closure ncl = new Closure(
//								(ClosureClass)c.getDatum(),
//								e2);

						//System.out.println(ncl.getParameterList());
						m.dataStk.push(ncl);
					} else if(aaa instanceof UserSyntax) {
						UserSyntax syn = (UserSyntax)aaa;

						if(syn.getExecuteEnv() != null) {
							// do nothing
						} else if(env.getRootenv() != null) {
							//syn.setExecuteEnv(env.getRootenv());
							syn.setExecuteEnv(env);
						} else {
							syn.setExecuteEnv(env);
						}
						m.dataStk.push(syn);
					//} else if(c.getDatum() instanceof Promise) {
					//	Promise p = (Promise)c.getDatum();
					//	
					//	p.setEnvironment(env);
					//	m.dataStk.push(p);
					} else if(aaa instanceof UserIdentifierSyntax) {
						UserIdentifierSyntax syn;

						syn = (UserIdentifierSyntax)aaa;
						if(syn.toset != null) {
							syn.toset.setExecuteEnv(env);
						}
						m.dataStk.push(syn);
					} else {
						m.dataStk.push(aaa);
					}
					addr++;
					break;
				case POP:
					m.dataStk.pop();
					addr++;
					break;
				case BEGIN_LIST:
					//m.workStk.push(new ArrayList<Datum>());
					m.workStk.push(new ConsListBuilder());
					addr++;
					break;
				case APPEND_LIST:
					Datum           d11 = m.dataStk.pop();
					//List<Datum>     d12 = m.workStk.peek();
					ConsListBuilder d12 = m.workStk.peek();

					//d12.add(d11);
					d12.append(d11);
					addr++;
					break;
				case APPEND_LIST_SPLICING:
					Datum           d13 = m.dataStk.pop();
					//List<Datum>     d12 = m.workStk.peek();
					ConsListBuilder d14 = m.workStk.peek();

					//d14.addAll(LispUtils.consToList(d13, message));
					d14.appendAll(d13, message);
					addr++;
					break;
				case APPEND_LIST_MULTI_VALUES:
					Datum           d15 = m.dataStk.pop();
					ConsListBuilder d16 = m.workStk.peek();

					d16.appendAll(
							LispUtils.listToCons(d15.getValues()),
							message);
					addr++;
					break;
				case END_LIST_DOT:
					Datum           d21 = m.dataStk.pop();
					//List<Datum>     d22 = m.workStk.pop();
					ConsListBuilder d22 = m.workStk.pop();

					//m.dataStk.push(LispUtils.listToCons(d22, d21));
					m.dataStk.push(d22.get(d21));
					addr++;
					break;
				case END_LIST:
					//List<Datum> d23 = m.workStk.pop();
					ConsListBuilder d23 = m.workStk.pop();

					//m.dataStk.push(LispUtils.listToCons(d23));
					m.dataStk.push(d23.get());
					addr++;
					break;
				case END_LIST_VECTOR:
					//List<Datum> d24 = m.workStk.pop();
					ConsListBuilder d24 = m.workStk.pop();

					//m.dataStk.push(new LispVector(d24));
					m.dataStk.push(new LispVector(
							LispUtils.consToList(d24.get(), message)));
					addr++;
					break;
				case CALL:
				case CALL_TAIL:
					Datum d3d = m.dataStk.pop();
					Datum d3s = m.dataStk.pop();

					if(d3s instanceof Subr) {
						Subr sub = (Subr)d3s;
						Environment ex2;
						ClosureClass cl;

						ex2 = (d3s instanceof ILispDynamicSubr) ?
								env : env.getGlobal();
						cl = sub.getClosureClass(ex2);
						if(cl == null) {
							// Subr is called with dynamic scope
							callSubr(sub, addr, env, d3d, m, hndl, d3s);
							addr++;
						} else {
							m.push(addr, code, env, hndl, d3s);

							env = new Environment(ex2);
							IntLispUtils.bindLocal(
									cl.getParameterList(),
									d3d, env, message);
							code = cl.getCode();
							addr = 0;
							//state = 0;
						}
					} else if(d3s instanceof ISubr) {
						callSubr((ISubr)d3s, addr, env, d3d, m, hndl,
								d3s);
						addr++;
					} else if(d3s instanceof Closure ||
							d3s instanceof LispGeneric) {
						Closure cl;

						if(d3s instanceof Closure) {
							cl = (Closure)d3s;
						} else {
							cl = findmapl(env, (LispGeneric)d3s, d3d);
							if(cl == null) {
								cl = findm((LispGeneric)d3s, d3d);
							} else {
								d3d = consapl((LispGeneric)d3s, d3d);
								d3s = env.findDatum(
										LispGeneric.APPLY_GENERIC);
							}
						}

						if(c.getOp().equals(Oper.CALL_TAIL)) {
							env = calltail0(c, m, hndl, cl);
						} else if(c.getOp().equals(Oper.CALL)) {
							m.push(addr, code, env, hndl, d3s);

							env = new Environment(cl.getEnvironment());
							_log.finer(
									"Call:" +
									cl.printName() + ":dtstk(" +
									m.dataStk.size() + "):cdstk(" +
									m.codeStk.size() + ")");
						}

//						System.out.println(cl.getParameterList());
//						System.out.println(d3d);
						IntLispUtils.bindLocal(
								cl.getParameterList(),
								d3d, env, message);
						if(d3s instanceof LispGeneric) {
							env.bindDatum(
									Symbol.getSymbol("next-method"),
									findmnext((LispGeneric)d3s, d3d));
						}

						code = cl.getCode();
						addr = 0;
						//state = 0;
					} else if(d3s instanceof LispNextMethod) {
						LispNextMethod dzz = (LispNextMethod)d3s;
						Closure cl = dzz.get();
						Datum dz2 = dzz.getNextMethod();

						if(d3d == Nil.NIL) {
							d3d = dzz.getDefaultParams();
						}

						if(cl == null &&
								dzz.getMethod().isApplyGeneric()) {
							Datum da = IntLispUtils.car (d3d, message);
							Datum dd = IntLispUtils.cadr(d3d, message);

							if(!(da instanceof LispGeneric)) {
								throw message.getError(
										"err.method.next.notfound");
							}
							cl  = findm((LispGeneric)da, dd);
							dz2 = findmnext((LispGeneric)da, dd);
							d3d = dd;
						}

						if(cl == null) {
							throw message.getError(
									"err.method.next.notfound");
						} else if(c.getOp().equals(Oper.CALL_TAIL)) {
							env = calltail0(c, m, hndl, cl);
						} else if(c.getOp().equals(Oper.CALL)) {
							m.push(addr, code, env, hndl, d3s);

							env = new Environment(cl.getEnvironment());
							_log.finer(
									"Call:" +
									cl.printName() + ":dtstk(" +
									m.dataStk.size() + "):cdstk(" +
									m.codeStk.size() + ")");
						}

						IntLispUtils.bindLocal(
								cl.getParameterList(),
								d3d, env, message);
						env.bindDatum(
								Symbol.getSymbol("next-method"),
								dz2);

						code = cl.getCode();
						addr = 0;
						//state = 0;
					} else if(d3s instanceof Continuation) {
						Continuation ct = (Continuation)d3s;
						List<Datum> lst =
							LispUtils.consToList(d3d, message);
						Stack2<Datum> pk;
						Memento k  = (Memento)ct.getMemento();
						CompiledCode cz;

						// rollback & forward
						exec(m.aftret, env, m, false);
						m.aftret = new CompiledCode();
						int iz = 0;
						for(; (iz < m.envStk.size() &&
								iz < k.envStk.size()); iz++) {
							if(m.envStk.get(iz) != k.envStk.get(iz)) {
								break;
							}
						}
						for(int i = m.envStk.size() - 1; i > iz; i--) {
							CompiledCode cx = m.finlStk.get(i);

							if(cx != null) {
								exec(cx, m.envStk.get(i),
										newMemento(), false);
							}
						}
						for(int i = iz; i < k.envStk.size() - 1; i++) {
							CompiledCode cx = k.initStk.get(i);

							if(cx != null) {
								exec(cx, k.envStk.get(i + 1),
										newMemento(), false);
							}
						}

						// overwrite
						m.overwrite(k.copy());

						// pop
						env  = m.envStk.pop();
						code = m.codeStk.pop();
						addr = m.addrStk.pop();
						m.callerStk.pop();
						m.dataStk.pop();
						m.workStk.pop(); // call/ccでworkStkをpushしている
						m.beforeStk.pop();
						m.afterStk.pop();
						m.memoStk.pop();
						pk   = m.pushbkStk.pop();
						/*state = */m.stateStk.pop();
						hndl = m.hndlStk.pop();
						m.flgStk.pop();
						m.initStk.pop();
						cz   = m.finlStk.pop();
						if(cz != null) {
							exec(cz, env, m, false);
						}
						//System.out.println(m);

						// beflist
						//beforeLst.addAll(k.befLst);

						if(pk == null) {
							setContinuationArgs(lst, m);
							addr++;
						} else {
							m.dataStk.addAll(pk);
						}
					} else if(d3s instanceof LispParameter) {
						ConsIterator itr = new ConsIterator(d3d);
						LispParameter p = (LispParameter)d3s;
						Datum s;

						if(!itr.hasNext()) {
							m.dataStk.push(p.get());
						} else {
							s = itr.next();
							if(itr.hasNext()) {
								throw message.getError(
										"err.argument", d3d);
							} else {
								p.set(s);
								m.dataStk.push(Undef.UNDEF);
							}
						}
						addr++;
					} else if(d3s instanceof Undef) {
						m.dataStk.push(Undef.UNDEF);
						addr++;
					} else {
						//System.out.println(d3s);
						throw message.getError("err.invalidap", d3s);
						//throw new LispException("invalid application");
					}
					break;
				case CALL_METHOD:
					Datum d3d1 = m.dataStk.pop();
					Datum d3s1 = m.dataStk.pop();

					if(d3s1 instanceof Symbol) {
						String nm = ((Symbol)d3s1).getName();

						if("aux-map".equals(nm)) {
							m.dataStk.push(evalLispMap(d3d1));
						} else if("aux-map-valid".equals(nm)) {
							m.dataStk.push(evalLispMapValid(d3d1));
						} else if("aux-apply".equals(nm)) {
							m.dataStk.push(evalApply(d3d1));
						} else if("aux-apply-valid".equals(nm)) {
							m.dataStk.push(evalApplyValid(d3d1));
						}
					} else {
						throw new RuntimeException();
					}
					addr++;
					break;
				case JMP:
					addr = code.getAddress(c.getJmpLabel());
					break;
				case JMP_IF:
//					if(!LispBoolean.FALSE.equals(m.dataStk.peek())) {
					if(m.dataStk.peek().isTrue()) {
						addr = code.getAddress(c.getJmpLabel());
					} else {
						addr++;
					}
					break;
				case JMP_UNLESS:
//					if(LispBoolean.FALSE.equals(m.dataStk.peek())) {
					if(!m.dataStk.peek().isTrue()) {
						addr = code.getAddress(c.getJmpLabel());
					} else {
						addr++;
					}
					break;
				case JMP_TOP:
					_log.finer(
							"Jump to top:cdstk(" +
							m.codeStk.size() + ")");
					addr = 0;
					break;
				case REFER_SYMBOL:
				case REFER_SETTER:
					Datum d51;

					//_log.finest("dtm:" + c.getDatum());
					if(c.getDatum() instanceof SymbolScope) {
						SymbolScope ss = (SymbolScope)c.getDatum();
						Environment e2 = ss.getExecuteEnv(env);

						//System.out.println(ss);
						//System.out.println("e2 :" + e2.toString());
						//_log.finest("e2 :" + env.toString());
						d51 = e2.findDatum(ss.getSymbol());
						if(d51 == null) {
							d51 = env.findDatum(ss);
						}
						//System.out.println(d51);
					} else {
						//_log.finest("env:" + env.toString());
						d51 = env.findDatum(c.getDatum());
					}

					if(d51 instanceof UserIdentifierSyntax) {
						Closure cl = makeEval(
								((UserIdentifierSyntax)d51).toreplace,
								env, m);

						m.push(addr, code, env, hndl, cl);
						env = new Environment(cl.getEnvironment());
						code = cl.getCode();
						addr = 0;
						break;
					}

					if(d51 instanceof LispLazySubrSyn) {
						d51 = ((LispLazySubrSyn)d51).setup();
					}

					if(d51 == null) {
						Datum zx = c.getDatum();

						if(zx instanceof SymbolName &&
								((SymbolName)zx)
								.getName().equals("next-method")) {
							d51 = env.findDatum(
									Symbol.getSymbol("next-method"));
						}

						if(d51 == null) {
							throw message.getError(
									"err.unbound", c.getDatum());
							//throw new LispException(
							//		"Unbound symbol:" + c.getDatum());
						}
					}

					if(!c.getOp().equals(Oper.REFER_SETTER)) {
						m.dataStk.push(d51);
					} else if(d51 instanceof Settable) {
						Datum st = ((Settable)d51).getSetter();

						if(st instanceof Callable) {
							m.dataStk.push(st);
						} else {
							throw message.getError(
									"err.srfi17.notdefined.setter",
									d51);
						}
					} else {
						throw message.getError(
								"err.srfi17.require.settable",
								d51);
					}
					addr++;
					break;
				case REFER_DEFINED:
					if(c.getDatum() instanceof SymbolScope) {
						SymbolScope ss = (SymbolScope)c.getDatum();
						Environment e2 = ss.getExecuteEnv(env);

						//_log.finest("e2 :" + env.toString());
						d51 = e2.findDatum(ss.getSymbol());
					} else {
						//_log.finest("env:" + env.toString());
						d51 = env.findDatum(c.getDatum());
					}

					m.dataStk.push(LispBoolean.getInstance(d51 != null));
					addr++;
					break;
				case BIND:
					if(c.getDatum() instanceof SymbolScope) {
						SymbolScope ss = (SymbolScope)c.getDatum();
						Environment e2 = ss.getExecuteEnv(env);

						//e2.bindDatum(ss.getSymbol(), m.dataStk.pop());
						bind0(e2, ss.getSymbol(), m.dataStk.pop());
					} else if(c.getDatum() instanceof Symbol) {
						Symbol sym = (Symbol)c.getDatum();

						if(tenv != null) {
							bind0(tenv, sym, m.dataStk.pop());
						} else {
							bind0(env, sym, m.dataStk.pop());
						}
					} else {
						throw message.getError(
								"err.symbol", c.getDatum());
					}
					addr++;
					break;
				case BIND_MACRO:
					if(c.getDatum() instanceof SymbolScope) {
						SymbolScope ss = (SymbolScope)c.getDatum();
						Environment e2 = ss.getExecuteEnv(env);

						mbind0(e2, ss.getSymbol(), m.dataStk.pop());
					} else if(c.getDatum() instanceof Symbol) {
						Symbol sym = (Symbol)c.getDatum();

						if(tenv != null) {
							mbind0(tenv, sym, m.dataStk.pop());
						} else {
							mbind0(env, sym, m.dataStk.pop());
						}
					} else {
						throw message.getError(
								"err.symbol", c.getDatum());
					}
					addr++;
					break;
				case BIND_MACRO72:
					if(c.getDatum() instanceof SymbolScope) {
						SymbolScope ss = (SymbolScope)c.getDatum();
						Environment e2 = ss.getExecuteEnv(env);

						mbind72(e2, ss.getSymbol(), m.dataStk.pop());
					} else if(c.getDatum() instanceof Symbol) {
						Symbol sym = (Symbol)c.getDatum();

						if(tenv != null) {
							mbind72(tenv, sym, m.dataStk.pop());
						} else {
							mbind72(env, sym, m.dataStk.pop());
						}
					} else {
						throw message.getError(
								"err.symbol", c.getDatum());
					}
					addr++;
					break;
				case BIND_MACRO_QUOTE:
					if(c.getDatum() instanceof SymbolScope) {
						SymbolScope ss = (SymbolScope)c.getDatum();
						Environment e2 = ss.getExecuteEnv(env);

						mbindq(e2, ss.getSymbol(), m.dataStk.pop());
					} else if(c.getDatum() instanceof Symbol) {
						Symbol sym = (Symbol)c.getDatum();

						if(tenv != null) {
							mbindq(tenv, sym, m.dataStk.pop());
						} else {
							mbindq(env, sym, m.dataStk.pop());
						}
					} else {
						throw message.getError(
								"err.symbol", c.getDatum());
					}
					addr++;
					break;
				case SET:
					if(c.getDatum() instanceof SymbolScope) {
						SymbolScope ss = (SymbolScope)c.getDatum();
						Environment e2 = ss.getExecuteEnv(env);

						if(!set0(e2, ss.getSymbol(),
								m.dataStk.peek())) {
							throw message.getError(
									"err.unbound", ss.getSymbol());
						}
					} else if(c.getDatum() instanceof Symbol) {
						if(!set0(env, (Symbol)c.getDatum(),
								m.dataStk.peek())) {
							throw message.getError(
									"err.unbound", c.getDatum());
						}
					}
					addr++;
					break;
				case RETURN_OP:
					exec(m.aftret, env, m, false);
					m.aftret = new CompiledCode();
					if(m.callerStk.isEmpty()) {
						//System.err.println(m.dataStk);
						return m.dataStk.pop();
					} else {
						Promise p;
						Stack2<Datum> pk;
						CompiledCode cz;

						env  = m.envStk.pop();
						code = m.codeStk.pop();
						addr = m.addrStk.pop();
						m.callerStk.pop();
						m.beforeStk.pop();
						m.afterStk.pop();
						p     = m.memoStk.pop();
						pk    = m.pushbkStk.pop();
						/*state = */m.stateStk.pop();
						hndl  = m.hndlStk.pop();
						m.flgStk.pop();
						m.initStk.pop();
						cz   = m.finlStk.pop();
						if(cz != null) {
							exec(cz, env, m, false);
						}
						//System.err.println(m);

						_log.finer(
								"Return:dtstk(" +
								m.dataStk.size() + "):cdstk(" +
								m.codeStk.size() + ")");

						// 結果をメモする
						if(p != null) {
							p.setMemo(m.dataStk.peek());
						}

						if(pk == null) {
							addr++;
						} else {
							m.dataStk.pop();   // 戻り値を無効化
							m.dataStk.addAll(pk);
						}

						/*if(!beforeLst.isEmpty()) {
							state |= 0x80;
						}*/
					}
					break;
				case PUSH_CONTINUATION:
					Memento cm = m.copy();
					Continuation rs0 = new Continuation(cm);

					//cm.befLst.addAll(beforeStoreStk);
					//System.err.println(cm);
					//System.err.println();
					m.dataStk.push(rs0);
					addr++;
					break;
				case PUSH_USYN:
					m.envStk.push(env);
					env = c.getUsyn().getExecuteEnv();
					addr++;
					break;
				case POP_USYN:
					env = m.envStk.pop();
					addr++;
					break;
				case FORCE:
					Datum d61 = m.dataStk.pop();

					if(d61 instanceof Promise) {
						Promise p = (Promise)d61;

						if(p.getMemo() == null) {
							// 遅延評価する
							m.push(addr, code, env, p, hndl);

							env = new Environment(p.getEnvironment());
							code = p.getCode();
							addr = 0;
							//state = 0;
						} else {
							// メモ化された結果を返す
							m.dataStk.push(p.getMemo());
							addr++;
						}
					} else {
						throw message.getError("err.force", "force");
					}
					break;
				case NEW_PROMISE:
					if(c.getDatum() instanceof Promise) {
						Promise p = (Promise)c.getDatum();
						Promise np = new Promise(p.getCode());

						np.setEnvironment(env);
						m.dataStk.push(np);
					} else {
						throw message.getError("err.force");
					}
					addr++;
					break;
				case APPEND_VALUES:
					Datum       d71 = m.dataStk.pop();

					if(d71 instanceof MultiValues) {
						// 多値
						MultiValues d = (MultiValues)d71;

						m.dataStk.push(
								LispUtils.listToCons(d.getValues()));
					} else if(d71 == Undef.UNDEF) {
						// valuesが0件
						m.dataStk.push(Nil.NIL);
					} else {
						// 多値でない
						Cons b = new Cons();

						b.setCar(d71);
						m.dataStk.push(b);
					}
					addr++;
					break;
				case EVAL_CODE:
					Datum d81 = m.dataStk.pop();  // 環境
					Datum d82 = m.dataStk.pop();  // S式
					EnvironmentObject eo;

					if(!(d81 instanceof EnvironmentObject)) {
						throw message.getError("err.environment");
					}

					// スコープ情報を取り除く
					d82 = PatternMatch.removeScope(d82);

					eo = (EnvironmentObject)d81;
					//m.dataStk.push(eo.getFacade().input(d82));
					//addr++;
					if(eo.isInherit()) {
						// interaction environment
						Closure cl = makeEval(
								d82, eo.getEnvironment(), m);

						m.push(addr, code, env, hndl, cl);

						env = new Environment(cl.getEnvironment());
						code = cl.getCode();
						addr = 0;
						//state = 0;
					} else {
						// null/RnRS enviromnent
						Closure cl = makeEval(
								d82, eo.getEnvironment(), m);

						m.dataStk.push(exec(
								cl.getCode(),
								eo.getEnvironment(),
								newMemento()));
						addr++;
					}
					break;
				case OVERRIDE_LOAD_CODE:
				case LOAD_CODE:
					Datum d84 = m.dataStk.pop();  // port

					if(d84 instanceof InputPort) {
						InputPort ipt = (InputPort)d84;

						try {
							ClosureClass clc = makeLoad(ipt, env, m);

							if(c.getOp().equals(Oper.LOAD_CODE)) {
								m.push(addr, code, env, hndl, clc,
										Flags.LOAD);
							} else {
								m.push(addr, code, env, hndl, clc,
										Flags.LOAD_OV);
								env = m.envStk.get(
										m.envStk.size() - 2);
							}
							//env = new Environment(cl.getEnvironment());
							code = clc.getCode();
							addr = 0;
							//state = 0;
						} catch(LispException e) {
							ipt.close();
							throw e;
						}
					} else {
						throw message.getError("err.require.iport");
					}
					break;
				case EXPAND_MACRO:
					Datum d85 = m.dataStk.pop();  // S exp
					LispCompiler cp0 =
						CompilerFactory.getInstance(message);
					Datum rrr;

					rrr = cp0.expandMacro(
							d85, env, this, new Memento());
//					rrr = MacroUtils72.unquote(rrr,
//							new EnvironmentObject(env, false));
					if(rrr instanceof LispSyntax) {
						rrr = ((LispSyntax)rrr).getWrapped();
					}
					m.dataStk.push(rrr);
					addr++;
					break;
				case EXPAND_MACRO1:
					Datum d86 = m.dataStk.pop();  // S exp
					LispCompiler cp1 =
						CompilerFactory.getInstance(message);
					Datum rrs;

					rrs = cp1.expandMacro1(
							d86, env, this, new Memento());
//					rrs = MacroUtils72.unquote(rrs,
//							new EnvironmentObject(env, false));
					if(rrs instanceof LispSyntax) {
						rrs = ((LispSyntax)rrs).getWrapped();
					}
					m.dataStk.push(rrs);
					addr++;
					break;
				case EXPAND_MACRO72:
					Datum d861 = m.dataStk.pop();  // S exp
					Datum d862 = m.dataStk.pop();  // S exp
					LispCompiler cp2 =
						CompilerFactory.getInstance(message);
					Datum rrt;

					if(!(d861 instanceof EnvironmentObject)) {
						throw message.getError(
								"err.environment", d861);
					}
					rrt = cp2.expandMacro(
							d862,
							((EnvironmentObject)d861).getEnvironment(),
							this,
							new Memento());
					if(rrt instanceof LispSyntax) {
						rrt = ((LispSyntax)rrt).getWrapped();
					}
					m.dataStk.push(rrt);
					addr++;
					break;
				case FIND_LIST:
					Datum d91lst = m.dataStk.pop();   // list
					Datum d91k   = m.dataStk.peek();  // test value

					m.dataStk.push(IntLispUtils.findListEqv(
							d91k, d91lst, message));
					addr++;
					break;
				case BIND_METHOD:
					if(c.getDatum() instanceof SymbolScope) {
						SymbolScope ss = (SymbolScope)c.getDatum();
						Environment e2 = ss.getExecuteEnv(env);

						bind0m(e2, ss.getSymbol(), m.dataStk.pop());
					} else if(c.getDatum() instanceof Symbol) {
						Symbol sym = (Symbol)c.getDatum();

						if(tenv != null) {
							bind0m(tenv, sym, m.dataStk.pop());
						} else {
							bind0m(env, sym, m.dataStk.pop());
						}
					} else {
						throw message.getError(
								"err.symbol", c.getDatum());
					}
					addr++;
					break;
				case SET_LIBID:
					LibraryID.putNamespace(
							c.getLibID(), env, c.getExport());
					addr++;
					break;
				case IMPORT_LIB:
					ClosureClass cl0 =
						makeImport(c.getDatum(), env, m);

					m.push(addr, code, env, hndl, cl0, Flags.LOAD);
					code = cl0.getCode();
					addr = 0;
					break;
				case EXPAND_SYNTAX_RULE:
					Datum d911 = m.dataStk.pop();  // input
					LispCompiler lc0 =
						CompilerFactory.getInstance(message);

					Datum d912 = lc0.expandSyntax(c.getSyntax(), d911,
							new Environment(env), true);
					//m.dataStk.push(d912);
					//addr++;
					Closure cl = makeEval(d912, env, m);

					m.push(addr, code, env, hndl, cl);
					env = new Environment(cl.getEnvironment());
					code = cl.getCode();
					addr = 0;
					break;
				case REPLACE_PARAM:
					/*Datum d913 = m.dataStk.pop();  // input
					LispCompiler comp1 =
						CompilerFactory.getInstance(message);

					if(d913 instanceof Cons) {
						Cons c0 = (Cons)d913;

						if(c0.getCar() instanceof SymbolName) {
							Symbol ss =
								((SymbolName)c0.getCar()).getSymbol();

							if(env.findDatum(ss) instanceof SynSyntax) {
								SynSyntax sz =
									(SynSyntax)env.findDatum(ss);
								Cons r0 = new Cons();
								Cons r1 = new Cons();
								Cons r2 = new Cons();
								Datum dz = sz.replaceLocalVals2(
										((Cons)c0.getCdr()).getCar(),
										env, comp1,
										new Environment(),
										message, true);

								r0.setCar(c0.getCar());
								r0.setCdr(r1);
								r1.setCar(r2);
								r2.setCar(dz);
								m.dataStk.push(dz);
							}
						}
					} else {
						//m.dataStk.push(d913);
						throw new LispException("");
					}*/
					addr++;
					break;
				case JAVA_CALL:
					Datum    d921 = m.dataStk.pop();
					Datum    d92k = m.dataStk.pop();
					Object   d92o;
					Class<?> d92c;
					Object   d92r;

					if(d92k instanceof JavaInstance) {
						d92o = ((JavaInstance)d92k).getJavaInstance();
						d92c = d92o.getClass();
					} else if(d92k instanceof JavaClass) {
						d92o = null;
						d92c = ((JavaClass)d92k).getJavaClass();
					} else {
						throw message.getError(
								"err.require.java-callable", d92k);
					}

					try {
						d92r = JavaUtils.invokeMethod(
								d92c, d92o,
								c.getMethod().getName(),
								LispUtils.consToList(d921, message));
						m.dataStk.push(LispUtils.toDatum(d92r));
					} catch (ParameterNotFoundException e) {
						throw message.getError(
								"err.java.method.notfound",
								c.getMethod().getName());
					}
					addr++;
					break;
				case JAVA_GET:
					Datum    d93k = m.dataStk.pop();
					Object   d93o;
					Object   d93r;

					if(d93k instanceof JavaInstance) {
						d93o = ((JavaInstance)d93k).getJavaInstance();
					} else {
						throw message.getError(
								"err.require.java-instance", d93k);
					}

					try {
						if(c.getBeanIndex() != null) {
							d93r = JavaUtils.invokeGetter(
									d93o, c.getMethod().getName(),
									c.getBeanIndex()
									.getExactSmallInt());
						} else {
							d93r = JavaUtils.invokeGetter(
									d93o, c.getMethod().getName());
						}
						m.dataStk.push(LispUtils.toDatum(d93r));
					} catch (ParameterNotFoundException e) {
						throw message.getError(
								"err.java.getter.notfound",
								c.getMethod().getName());
					} catch (IntrospectionException e) {
						throw message.getError(
								"err.java.getter.notfound",
								c.getMethod().getName());
					}
					addr++;
					break;
				case JAVA_SET:
					Datum    d941 = m.dataStk.pop();
					Datum    d94k = m.dataStk.pop();
					Object   d94o;

					if(d94k instanceof JavaInstance) {
						d94o = ((JavaInstance)d94k).getJavaInstance();
					} else {
						throw message.getError(
								"err.require.java-instance",d94k);
					}

					try {
						if(c.getBeanIndex() != null) {
							/*Integer ind = IntLispUtils.toIntExact(
									c.getBeanIndex().bigIntegerValue());

							if(ind == null) {
								throw message.getError(
										"err.require.smallint");
							}*/
							JavaUtils.invokeSetter(
									d94o, c.getMethod().getName(),
									c.getBeanIndex()
									.getExactSmallInt(),
									d941);
						} else {
							JavaUtils.invokeSetter(
									d94o,
									c.getMethod().getName(),
									d941);
						}
						m.dataStk.push(Undef.UNDEF);
					} catch (ParameterNotFoundException e) {
						throw message.getError(
								"err.java.setter.notfound",
								c.getMethod().getName());
					} catch (IntrospectionException e) {
						throw message.getError(
								"err.java.setter.notfound",
								c.getMethod().getName());
					}
					addr++;
					break;
				case JAVA_FIELD_GET:
					Datum    d95k = m.dataStk.pop();
					Object   d95o;
					Object   d95r;

					if(d95k instanceof JavaInstance) {
						d95o = ((JavaInstance)d95k).getJavaInstance();
					} else {
						throw message.getError(
								"err.require.java-instance", d95k);
					}

					try {
						d95r = JavaUtils.getField(
								d95o, c.getMethod().getName());
						m.dataStk.push(LispUtils.toDatum(d95r));
					} catch (NoSuchFieldException e) {
						throw message.getError(
								"err.java.field.notfound",
								c.getMethod().getName());
					}
					addr++;
					break;
				case JAVA_FIELD_SET:
					Datum    d961 = m.dataStk.pop();
					Datum    d96k = m.dataStk.pop();
					Object   d96o;

					if(d96k instanceof JavaInstance) {
						d96o = ((JavaInstance)d96k).getJavaInstance();
					} else {
						throw message.getError(
								"err.require.java-instance", d96k);
					}

					try {
						JavaUtils.setField(
								d96o, c.getMethod().getName(), d961);
						m.dataStk.push(Undef.UNDEF);
					} catch (ParameterNotFoundException e) {
						throw message.getError(
								"err.java.field.notfound",
								c.getMethod().getName());
					} catch (NoSuchFieldException e) {
						throw message.getError(
								"err.java.field.notfound",
								c.getMethod().getName());
					}
					addr++;
					break;
				case JAVA_INSTANCEOF:
					Datum    d97k = m.dataStk.pop();
					Object   d97o;

					if(d97k instanceof JavaInstance) {
						d97o = ((JavaInstance)d97k).getJavaInstance();

						try {
							Class<?> dn =
								Class.forName(c.getMethod().getName());
							boolean rs =
								dn.isAssignableFrom(d97o.getClass());

							m.dataStk.push(
									LispBoolean.getInstance(rs));
						} catch (ClassNotFoundException e) {
							m.dataStk.push(LispBoolean.FALSE);
						}
					} else {
						m.dataStk.push(LispBoolean.FALSE);
					}
					addr++;
					break;
				case JAVA_RAISE:
					Datum d98e = m.dataStk.pop();

					if(d98e instanceof JavaInstance) {
						Object d98o =
							((JavaInstance)d98e).getJavaInstance();

						if(d98o instanceof Throwable) {
							throw new JavaTargetException(
									(Throwable)d98o);
						}
					}
					throw message.getError(
							"err.require.java-exception", d98e);
				case JAVA_ENTER_EXCEPTION_HANDLER:
					hndl.lablStk.push(c.getJmpLabel());
					hndl.hndlStk.push(c.getDatum());
					hndl.codeStk.push(code);
					hndl.envStk.push(env);
					hndl.memStk.push(m.copy());
					addr++;
					break;
				case JAVA_LEAVE_EXCEPTION_HANDLER:
					hndl.lablStk.pop();
					hndl.hndlStk.pop();
					hndl.codeStk.pop();
					hndl.envStk.pop();
					hndl.memStk.pop();
					addr++;
					break;
				case REWIND_ENV:
					tenv = env;
					for(int i = 0; i < c.getRewind(); i++) {
						tenv = m.envStk.pop();
					}
					addr++;
					break;
				case REWIND_CONT:
					_log.finer(
							"Remove " + c.getRewind() + " frame(s)");
					for(int i = 0; i < c.getRewind(); i++) {
						Environment env0;
						CompiledCode cc;

						env0 = m.envStk.pop();
						addr = m.addrStk.pop();
						code = m.codeStk.pop();
						//m.dataStk.pop();
						m.callerStk.pop();
						m.beforeStk.pop();
						m.afterStk.pop();
						m.memoStk.pop();
						m.pushbkStk.pop();
						/*state = */m.stateStk.pop();
						hndl  = m.hndlStk.pop();
						m.flgStk.pop();
						//m.workStk.pop();
						m.initStk.pop();
						if((cc = m.finlStk.pop()) != null) {
							this.exec(cc, env0, m, false);
						}
					}
					env  = tenv;
					tenv = null;
					addr = 0;
					break;
				case ENTER_EXCEPTION_HANDLER:
					hndl.lablStk.push(c.getJmpLabel());
					hndl.hndlStk.push(m.dataStk.pop());
					hndl.codeStk.push(code);
//					hndl.envStk.push(env);
					hndl.memStk.push(m.copy());
					addr++;
					break;
				case LEAVE_EXCEPTION_HANDLER:
					hndl.lablStk.pop();
					hndl.hndlStk.pop();
					hndl.codeStk.pop();
//					hndl.envStk.pop();
					hndl.memStk.pop();
					addr++;
					break;
				case GET_CURRENT_EXCEPTION_HANDLER:
					if(hndl.hndlStk.isEmpty()) {
						m.dataStk.push(
								LispThread
								.DEFAULT_CURRENT_EXCEPTION_HANDLER);
					} else {
						m.dataStk.push(hndl.hndlStk.peek());
					}
					addr++;
					break;
				case PARAMETERIZE:
					Datum dap = m.dataStk.pop();
					Datum dad = m.dataStk.pop();

					if(dap instanceof LispParameter) {
						((LispParameter)dap).parameterize(dad);
					} else {
						throw message.getError(
								"err.srfi39.require.parameter", dap);
					}
					addr++;
					break;
				case DEPARAMETERIZE:
					Datum dbp = m.dataStk.pop();

					((LispParameter)dbp).deparameterize();
					addr++;
					break;
				case INITIALLY:
					m.initStk.pop();
					m.initStk.push(c.getCodes());
					exec(c.getCodes(), env, m, false);
					addr++;
					break;
				case FINALLY:
					m.finlStk.pop();
					m.finlStk.push(c.getCodes());
					addr++;
					break;
				}
//			} catch(JavaTargetException e) {
//				if(hndl.hndlStk.isEmpty()) {
//					throw e;
//				} else {
//					Datum d3s = hndl.hndlStk.pop();
//					Datum d3d = new Cons(
//							new JavaInstance(e.getCause()),
//							Nil.NIL);
//					addr = code.getAddress(hndl.lablStk.pop()) - 1;
//					code = hndl.codeStk.pop();
//					env  = hndl.envStk.pop();
//					m.overwrite(hndl.memStk.pop());
//
//					if(d3s instanceof ClosureClass) {
//						ClosureClass cl = (ClosureClass)d3s;
//
//						m.push(addr, code, env, hndl, d3s);
//
//						env = new Environment(env);
//						IntLispUtils.bindLocal(
//								cl.getParameterList(),
//								d3d, env, message);
//						code = cl.getCode();
//						addr = 0;
//						//state = 0;
//					} else {
//						throw message.getError("err.invalidap", d3s);
//					}
//				}
			} catch(LispException e) {
				if(hndl.hndlStk.isEmpty()) {
					if(!(e instanceof SRFI34.RaisedException)) {
						throw e;
					} else if(((SRFI34.RaisedException)e).raised
							instanceof ExceptionObject) {
						throw ((ExceptionObject)
								((SRFI34.RaisedException)e).raised)
								.getException();
					} else {
						throw e;
					}
				} else {
					Datum d3s = hndl.hndlStk.peek();
					Datum d3d;
//					code = hndl.codeStk.pop();
//					env  = hndl.envStk.pop();
//					addr = code.getAddress(hndl.lablStk.pop()) - 1;
					m.overwrite(hndl.memStk.peek());
					code = hndl.codeStk.peek();
					addr = code.getAddress(hndl.lablStk.peek()) - 1;

					if(e instanceof SRFI34.RaisedException) {
						d3d = new Cons(
								((SRFI34.RaisedException)e).raised,
								Nil.NIL);
					} else {
						d3d = new Cons(
								new ExceptionObject(e),
								Nil.NIL);
					}

					if(d3s instanceof Subr) {
						Subr sub = (Subr)d3s;
						ClosureClass cl = sub.getClosureClass(
								env.getGlobal());

						if(cl == null) {
							callSubr(sub, addr, env,
									d3d, m, hndl, d3s);
							addr++;
						} else {
							m.push(addr, code, env, hndl, d3s);

							env = new Environment(env.getGlobal());
							IntLispUtils.bindLocal(
									cl.getParameterList(),
									d3d, env, message);
							code = cl.getCode();
							addr = 0;
							//state = 0;
						}
					} else if(d3s instanceof ISubr) {
						callSubr((ISubr)d3s, addr, env, d3d, m, hndl,
								d3s);
						addr++;
					} else if(d3s instanceof Closure ||
							d3s instanceof LispGeneric) {
						Closure cl;

						if(d3s instanceof Closure) {
							cl = (Closure)d3s;
						} else {
							cl = findmapl(env, (LispGeneric)d3s, d3d);
							if(cl == null) {
								cl = findm((LispGeneric)d3s, d3d);
							} else {
								d3d = consapl((LispGeneric)d3s, d3d);
								d3s = env.findDatum(
										LispGeneric.APPLY_GENERIC);
							}
						}

						m.push(addr, code, env, hndl, d3s);
						env = new Environment(cl.getEnvironment());
						_log.finer(
								"Call:" +
								cl.printName() + ":dtstk(" +
								m.dataStk.size() + "):cdstk(" +
								m.codeStk.size() + ")");

						IntLispUtils.bindLocal(
								cl.getParameterList(),
								d3d, env, message);
						if(d3s instanceof LispGeneric) {
							env.bindDatum(
									Symbol.getSymbol("next-method"),
									findmnext((LispGeneric)d3s, d3d));
						}

						// leave the handler
						hndl.lablStk.pop();
						hndl.hndlStk.pop();
						hndl.codeStk.pop();
//						hndl.envStk.pop();
						hndl.memStk.pop();

						code = cl.getCode();
						addr = 0;
						//state = 0;
					} else if(d3s instanceof Continuation) {
						Continuation ct = (Continuation)d3s;
						List<Datum> lst =
							LispUtils.consToList(d3d, message);
						Stack2<Datum> pk;
						Memento k  = (Memento)ct.getMemento();
						CompiledCode cz;

						// rollback & forward
						exec(m.aftret, env, m, false);
						m.aftret = new CompiledCode();
						int iz = 0;
						for(; iz < m.envStk.size(); iz++) {
							if(m.envStk.get(iz) != k.envStk.get(iz)) {
								break;
							}
						}
						for(int i = m.envStk.size(); i >= iz; i--) {
							exec(m.finlStk.get(i),
									m.envStk.get(i),
									newMemento(),
									false);
						}
						for(int i = iz; i < k.envStk.size(); i++) {
							exec(k.initStk.get(i),
									k.envStk.get(i),
									newMemento(),
									false);
						}

						// overwrite
						m.overwrite(k.copy());

						// pop
						env  = m.envStk.pop();
						code = m.codeStk.pop();
						addr = m.addrStk.pop();
						m.callerStk.pop();
						m.dataStk.pop();
						m.workStk.pop(); // call/ccでworkStkをpushしている
						m.beforeStk.pop();
						m.afterStk.pop();
						m.memoStk.pop();
						pk    = m.pushbkStk.pop();
						/*state = */m.stateStk.pop();
						hndl  = m.hndlStk.pop();
						m.flgStk.pop();
						m.initStk.pop();
						cz   = m.finlStk.pop();
						if(cz != null) {
							exec(cz, env, m, false);
						}
						//System.out.println(m);

						// beflist
						//beforeLst.addAll(k.befLst);

						if(pk == null) {
							setContinuationArgs(lst, m);
							addr++;
						} else {
							m.dataStk.addAll(pk);
						}
					} else {
						throw message.getError("err.invalidap", d3s);
					}
				}
			}
		}
	}


	public Datum exec(
			CompiledCode code1, Environment env1, IntStack memento) {
		return exec(code1, env1, memento, true);
	}


	public IntStack newMemento() {
		return new Memento();
	}

}
