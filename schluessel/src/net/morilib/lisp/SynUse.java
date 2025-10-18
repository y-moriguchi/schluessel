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

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.List;

import net.morilib.lisp.CompiledCode.Builder;
import net.morilib.lisp.LispCompiler.MiscInfo;
import net.morilib.lisp.subr.UnaryArgs;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class SynUse extends Syntax {

	//
	private static class Aux2 extends UnaryArgs {

		//
		private static final Aux2 INSTANCE = new Aux2();

		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof InputPort) {
				InputPort ipt = (InputPort)c1a;

				ipt.close();
				return Undef.UNDEF;
			} else {
				throw mesg.getError("err.require.iport");
			}
		}

	}

	//
	private InputStream searchPath(
			String fn, Datum pth, LispMessage mesg) {
		Class<?> cls = SynUse.class;

		for(Datum p = pth; p instanceof Cons;) {
			InputStream ins;
			Cons c = (Cons)p;

			if(c.getCar() instanceof Symbol) {
				String p0 = ((Symbol)c.getCar()).getName();
				String resn2;

				resn2 = ("/" + p0.replace('.', '/') + "/" +
						 fn.replace('.', '/') + ".scm");
				ins = cls.getResourceAsStream(resn2);
				if(ins != null) {
					return ins;
				}
			}
			p = c.getCdr();
		}

		InputStream ins = cls.getResourceAsStream(
				"/" + fn.replace('.', '/') + ".scm");
		if(ins != null) {
			return ins;
		} else {
			throw mesg.getError("err.use.library.notfound", fn);
		}
	}

	//
	@Override
	/*package*/ void compile(Datum body, Environment env,
			LispCompiler comp, Builder build, boolean toplevel,
			Cons callsym, boolean istail, LispMessage mesg,
			List<Cons> symlist, CodeExecutor exec, IntStack memento,
			MiscInfo syncased) {
		Datum pth = env.findDatum(Symbol.getSymbol("*use-path*"));

		if(pth == null) {
			throw mesg.getError("err.unbound", "*use-path*");
		}

		if(body instanceof Cons) {
			Cons c1 = (Cons)body;

			if(c1.getCdr() == Nil.NIL) {
				if(c1.getCar() instanceof Symbol) {
					String resn = ((Symbol)c1.getCar()).getName();
					InputStream ins = null;
					Reader rd = null;
					InputPort p;

					ins = searchPath(resn, pth, mesg);
					rd  = new BufferedReader(new InputStreamReader(ins));
					p   = new InputPort(rd, mesg);

					build.addPush(p);
					build.addLoadCode();
					build.addPush(Aux2.INSTANCE);   // close port
					build.addBeginList();
					build.addPush(p);
					build.addAppendList();
					build.addEndList();
					build.addCall();
					build.addPop();
					return;
				}
			}
		}
		throw mesg.getError("err.use.malform");
	}

	//
	@Override
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv,
			LispMessage mesg, boolean toplv, int ttype) {
		return body;
	}

}
