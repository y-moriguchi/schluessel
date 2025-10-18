/*
 * Copyright 2009-2010 Yuichiro Moriguchi
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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import net.morilib.lisp.CompiledCode.Builder;
import net.morilib.lisp.LispCompiler.MiscInfo;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.IOs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/06/11
 */
public final class LispFeature {

	//
	private static class _AddFeature extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			features.put(c1a, ((Closure)c2a).getCode());
			return Undef.UNDEF;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/06/11
	 */
	public static class SynDefineFeature extends Syntax {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Syntax#compile(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispCompiler, net.morilib.lisp.CompiledCode.Builder, boolean, net.morilib.lisp.Cons, boolean, net.morilib.lisp.LispMessage, java.util.List, net.morilib.lisp.CodeExecutor, net.morilib.lisp.IntStack, net.morilib.lisp.LispCompiler.MiscInfo)
		 */
		@Override
		/*package*/ void compile(Datum body, Environment env,
				LispCompiler comp, Builder build, boolean toplevel,
				Cons callsym, boolean istail, LispMessage mesg,
				List<Cons> symlist, CodeExecutor exec,
				IntStack memento, MiscInfo syncased) {
			CompiledCode.Builder nbuild = new CompiledCode.Builder();
			Environment  nenv = new Environment(env);
			ClosureClass cl = new ClosureClass();
			ConsIterator itr = new ConsIterator(body);
			Datum sym = SubrUtils.nextIf(itr, mesg, body);

			if(!toplevel) {
				throw mesg.getError("err.nottoplevel");
			} else if(!(sym instanceof Symbol)) {
				throw mesg.getError("err.require.symbol", sym);
			}

			SyntaxUtils.compileList(
					itr.rest(), nenv, comp, nbuild, callsym, false,
					mesg, symlist, exec, memento, syncased);
			cl.setParameterList(Nil.NIL);
			cl.setCode(nbuild.getCodeRef());
			build.addPush(_ADD_FEATURE);
			build.addBeginList();
			build.addPush(sym);
			build.addAppendList();
			build.addPush(cl);
			build.addAppendList();
			build.addEndList();
			build.addCall();
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Syntax#replaceLocalVals(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispCompiler, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage, boolean, int)
		 */
		@Override
		Datum replaceLocalVals(Datum body, Environment env,
				LispCompiler comp, Environment ienv, LispMessage mesg,
				boolean toplv, int ttype) {
			throw mesg.getError("err.define.definesyntax");
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/06/11
	 */
	public static class IsFeatureExist extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			return LispBoolean.getInstance(features.containsKey(c1a));
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/06/11
	 */
	public static class ApplyFeature extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			return LispBoolean.getInstance(
					applyFeature(c1a, env, mesg));
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/03/20
	 */
	public static class ApplyFeatures extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			ConsIterator itr;
			boolean r = true;

			if(c1a.isNil()) {
				return LispBoolean.TRUE;
			} else if(c1a instanceof Cons) {
				itr = new ConsIterator(c1a);
				while(itr.hasNext()) {
					r = applyFeature(itr.next(), env, mesg) | r;
				}
				return LispBoolean.getInstance(r);
			} else {
				return LispBoolean.getInstance(
						applyFeature(c1a, env, mesg));
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/03/20
	 */
	public static class FeatureAnd extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Set<Datum>  set = new HashSet<Datum>();
			Datum d;

			while(itr.hasNext()) {
				if(!(d = itr.next()).isTrue()) {
					return LispBoolean.FALSE;
				} else if(d instanceof Cons || d.isNil()) {
					set.addAll(LispUtils.consToList(d, mesg));
				} else if(features.containsKey(d)) {
					set.add(d);
				} else {
					return LispBoolean.FALSE;
				}
			}
			return LispUtils.toCons(set);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/03/20
	 */
	public static class FeatureOr extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Set<Datum>  set = new HashSet<Datum>();
			Datum d;

			while(itr.hasNext()) {
				if(!(d = itr.next()).isTrue()) {
					// do nothing
				} else if(d instanceof Cons || d.isNil()) {
					set.addAll(LispUtils.consToList(d, mesg));
				} else if(features.containsKey(d)) {
					set.add(d);
				} else {
					// do nothing
				}
			}
			return LispUtils.toCons(set);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/03/20
	 */
	public static class FeatureNot extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			return features.containsKey(c1a) ?
					LispBoolean.FALSE : Nil.NIL;
		}

	}

	//
	private static final BinaryArgs _ADD_FEATURE = new _AddFeature();
	private static final String RESOURCE =
		"/net/morilib/lisp/exlib/srfi-7.scm";
	private static final String MAIN = "eval-srfi-7";
	private static final String SRFI7_VALID = "*srfi-7-valid*";
	private static final Symbol SRFI7_VALID_SYM =
		Symbol.getSymbol(SRFI7_VALID);

	//
	private static Scheme srfi7Lang;

	//
	private static ConcurrentHashMap<Datum, CompiledCode> features =
		new ConcurrentHashMap<Datum, CompiledCode>();
	private static ConcurrentHashMap<Datum, Boolean> loaded =
		new ConcurrentHashMap<Datum, Boolean>();

	//
	private static boolean applyFeature(Datum c1a, Environment env,
			LispMessage mesg) {
		CompiledCode cd = features.get(c1a);

		if(loaded.containsKey(c1a)) {
			return true;
		} else if(cd != null) {
			CodeExecutor exe =
				CodeExecutorFactory.getInstance(mesg);

			exe.exec(cd, env, exe.newMemento());
			loaded.put(c1a, Boolean.TRUE);
			return true;
		} else {
			return false;
		}
	}

	//
	private static void initSRFI7() {
		InputStream ins = null;
		Reader rd = null;

		try {
			srfi7Lang = Scheme.newEmpty();
			srfi7Lang.loadRnRS(5);
			ins = LispFeature.class.getResourceAsStream(RESOURCE);
			rd  = new BufferedReader(new InputStreamReader(ins));
			srfi7Lang.readFile(rd);
		} catch (IOException e) {
			throw new RuntimeException(e);
		} finally {
			IOs.close(ins);
		}
	}

	/**
	 * 
	 * @param pgm
	 * @return
	 */
	public static Datum evalSRFI7(Datum pgm, Environment env,
			LispMessage mesg) {
		Datum d;

		try {
			d = env.findDatum(SRFI7_VALID_SYM);
			if(d == null || !d.isTrue()) {
				return pgm;
			} else {
				if(srfi7Lang == null) {
					synchronized(LispFeature.class) {
						initSRFI7();
					}
				}
				return srfi7Lang.call(MAIN, pgm);
			}
		} catch(LispException e) {
			throw mesg.getError("err.srfi7.invalid");
		}
	}

}
