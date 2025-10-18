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

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;

import net.morilib.lisp.file.LispFiles;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.IOs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/23
 */
public class SubrWithOutputToFile extends Subr {

	//
	private static final Subr REDI1 = new Redirect1();
	private static final Subr REDI2 = new Redirect2();
	private static final Subr REDI3 = new Redirect3();

	//
	private static class Redirect1 extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			String s = SubrUtils.getString(c1a, mesg);
			PrintStream ps;

			try {
				ps = new PrintStream(new FileOutputStream(
						LispFiles.getFile(env, s)));
				IOs.pushOut(ps);
				return Undef.UNDEF;
			} catch (FileNotFoundException e) {
				throw mesg.getError("err.filenotfound", s);
			} catch (IOException e) {
				throw mesg.getError("err.io", e.getMessage());
			}
		}

	}

	//
	private static class Redirect2 extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			IOs.popOut();
			return Undef.UNDEF;
		}

	}

	//
	private static class Redirect3 extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			System.out.close();
			return Undef.UNDEF;
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		throw new RuntimeException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#createClosureClass(net.morilib.lisp.Environment)
	 */
	@Override
	/*package*/ ClosureClass createClosureClass(Environment env) {
		CompiledCode.Builder build  = new CompiledCode.Builder();
		CompiledCode.Builder ibuild = new CompiledCode.Builder();
		CompiledCode.Builder fbuild = new CompiledCode.Builder();

		// init build
		ibuild.addPush(REDI1);
		ibuild.addBeginList();
		ibuild.addReferSymbol(Symbol.getSymbol("fn"));
		ibuild.addAppendList();
		ibuild.addEndList();
		ibuild.addCall();
		ibuild.addPop();

		// final build
		fbuild.addPush(REDI2);
		fbuild.addBeginList();
		fbuild.addEndList();
		fbuild.addCall();
		fbuild.addPop();

		//
		build.addInitially(ibuild.getCodeRef());
		build.addFinally(fbuild.getCodeRef());
		build.addReferSymbol(Symbol.getSymbol("thunk"));
		build.addBeginList();
		build.addEndList();
		build.addCall();

		//
		build.addPush(REDI3);
		build.addBeginList();
		build.addEndList();
		build.addCall();
		build.addPop();
		build.addReturnOp();

		//
		ClosureClass cl1 = new ClosureClass();
		cl1.setParameterList(LispUtils.list(
				Symbol.getSymbol("fn"),
				Symbol.getSymbol("thunk")));
		cl1.setCode(build.getCodeRef());
		return cl1;
	}

}
