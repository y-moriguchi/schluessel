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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import net.morilib.lisp.subr.QuaternaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.options.IllegalCommandLineException;
import net.morilib.options.OperandProcessor;
import net.morilib.options.OptionObject;
import net.morilib.options.OptionProcessor;
import net.morilib.options.OptionUtils;
import net.morilib.util.Strings;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/18
 */
public final class SRFI37 {

	//
	private SRFI37() {}

	//
	static class Option0 extends Datum2 implements OptionObject {

		//
		private OptionProcessor prcs;

		//
		List<String> names;
		boolean required, optional;
		Procedure proc;

		//
		Option0(Procedure p, boolean req, boolean opt,
				List<String> ns) {
			proc = p;
			required = req;
			optional = opt;
			names = Collections.unmodifiableList(ns);
			prcs = toOption(p);
		}

		//
		Option0(Procedure p, boolean req, boolean opt,
				String... ns) {
			this(p, req, opt, Arrays.asList(ns));
		}

		/* (non-Javadoc)
		 * @see net.morilib.options.OptionObject#getNames()
		 */
		public List<String> getNames() {
			return names;
		}

		/* (non-Javadoc)
		 * @see net.morilib.options.OptionObject#isArgumentRequired()
		 */
		public boolean isArgumentRequired() {
			return required;
		}

		/* (non-Javadoc)
		 * @see net.morilib.options.OptionObject#isArgumentOptional()
		 */
		public boolean isArgumentOptional() {
			return optional;
		}

		/* (non-Javadoc)
		 * @see net.morilib.options.OptionObject#getProcessor()
		 */
		public OptionProcessor getProcessor() {
			return prcs;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<option>");
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/03/18
	 */
	public static class Option extends QuaternaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.QuaternaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
				Datum c4a, Environment env, LispMessage mesg) {
			ConsIterator jtr;
			List<String> ss = new ArrayList<String>();
			Procedure p;
			Datum d;

			jtr = new ConsIterator(c1a);
			while(jtr.hasNext()) {
				if((d = jtr.next()) instanceof LispCharacter) {
					ss.add(Strings.newString(
							d.getCharacterCodePoint()));
				} else {
					ss.add(SubrUtils.getString(d, mesg));
				}
			}
			p = SubrUtils.getProcedure(c4a, mesg);
			return new Option0(p, c2a.isTrue(), c3a.isTrue(), ss);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/03/18
	 */
	public static class OptionNames extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			ConsListBuilder b = new ConsListBuilder();

			if(c1a instanceof Option0) {
				for(String s : ((Option0)c1a).names) {
					b.append(new LispString(s));
				}
				return b.get();
			} else {
				throw mesg.getError("err.srfi37.require.option",
						c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/03/18
	 */
	public static class IsOptionRequiredArg extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof Option0) {
				return LispBoolean.getInstance(
						((Option0)c1a).required);
			} else {
				throw mesg.getError("err.srfi37.require.option",
						c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/03/18
	 */
	public static class IsOptionOptionalArg extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof Option0) {
				return LispBoolean.getInstance(
						((Option0)c1a).optional);
			} else {
				throw mesg.getError("err.srfi37.require.option",
						c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/03/18
	 */
	public static class SubrOptionProcessor extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof Option0) {
				return (Datum)((Option0)c1a).proc;
			} else {
				throw mesg.getError("err.srfi37.require.option",
						c1a);
			}
		}

	}

	//
	private static class ArgsFoldAux extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body), jtr;
			Datum args = SubrUtils.nextIf(itr, mesg, body);
			Datum opts = SubrUtils.nextIf(itr, mesg, body);
			Procedure unrc = SubrUtils.nextProcedure(itr, mesg, body);
			Procedure oprc = SubrUtils.nextProcedure(itr, mesg, body);
			Datum rest = itr.rest();
			List<String> ss = new ArrayList<String>();
			List<OptionObject> oo = new ArrayList<OptionObject>();
			CompiledCode.Builder build = new CompiledCode.Builder();
			Datum mt;
			Datum d;

			jtr = new ConsIterator(args);
			while(jtr.hasNext()) {
				ss.add(SubrUtils.getString(jtr.next(), mesg));
			}

			jtr = new ConsIterator(opts);
			while(jtr.hasNext()) {
				if((d = jtr.next()) instanceof Option0) {
					oo.add((OptionObject)d);
				} else {
					throw mesg.getError("err.srfi37.require.option",
							d);
				}
			}

			try {
				mt = MultiValues.newValues(
						LispUtils.consToList(rest, mesg));
				build.addPush(mt);
				OptionUtils.process(ss.toArray(new String[0]),
						oo.toArray(new OptionObject[0]),
						toOption(unrc),
						toOperand(oprc),
						build);
				build.addReturnOp();
			} catch(IllegalCommandLineException e) {
				throw mesg.getError("err.srfi37.option.error");
			}
			return new Closure(
					new ClosureClass(Nil.NIL, build.getCodeRef()),
					env);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/03/18
	 */
	public static class ArgsFold extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			throw new RuntimeException();
		}

		ClosureClass createClosureClass(Environment env) {
			CompiledCode.Builder bld = new CompiledCode.Builder();

			bld.addPush(AUX);
			bld.addBeginList();
			bld.addReferSymbol(ARGS);
			bld.addAppendListSplicing();
			bld.addEndList();
			bld.addCall();
			bld.addBeginList();
			bld.addEndList();
			bld.addCall();
			bld.addReturnOp();
			return new ClosureClass(ARGS, bld.getCodeRef());
		}

	}

	//
	private static final Symbol TMP  = Symbol.gensym();
	private static final Symbol ARGS = Symbol.gensym();
	private static final ArgsFoldAux AUX = new ArgsFoldAux();

	//
	private static OptionProcessor toOption(final Procedure proc) {
		return new OptionProcessor() {

			public Object call(OptionObject option, String name,
					String arg, Object extra) {
				Option0 opt;
				CompiledCode.Builder build;

				build = (CompiledCode.Builder)extra;
				if(option == null) {
					opt = new Option0(proc, false, false, name);
				} else {
					opt = (Option0)option;
				}

				build.addBind(TMP);
				build.addPush((Datum)proc);
				build.addBeginList();
				build.addPush(opt);
				build.addAppendList();
				build.addPush(new LispString(name));
				build.addAppendList();
				build.addPush(arg == null ?
						LispBoolean.FALSE : new LispString(arg));
				build.addAppendList();
				build.addReferSymbol(TMP);
				build.addAppendListMultiValues();
				build.addEndList();
				build.addCall();
				return extra;
			}

		};
	}

	//
	private static OperandProcessor toOperand(final Procedure proc) {
		return new OperandProcessor() {

			public Object call(String operand, Object extra) {
				CompiledCode.Builder build;

				build = (CompiledCode.Builder)extra;
				build.addBind(TMP);
				build.addPush((Datum)proc);
				build.addBeginList();
				build.addPush(new LispString(operand));
				build.addAppendList();
				build.addReferSymbol(TMP);
				build.addAppendListMultiValues();
				build.addEndList();
				build.addCall();
				return extra;
			}

		};
	}

}
