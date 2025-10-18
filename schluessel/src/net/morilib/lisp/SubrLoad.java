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
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.io.Reader;

import net.morilib.lisp.subr.UnaryArgs;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class SubrLoad extends Subr {

	//
	private static File loadFile;

	//
	private static class Aux1 extends UnaryArgs {

		//
		private static final Aux1 INSTANCE = new Aux1();

		/*private File findScm0(String fn, LispMessage mesg) {
			File ff = new File(fn);

			if(!ff.isFile()) {
				ff = new File(fn + ".scm");
			}
			if(!ff.isFile()) {
				throw mesg.getError("err.load.filenotfound", fn);
			}
			return ff;
		}*/

		private File searchPath(
				String fn, Datum pth, String sep, LispMessage mesg) {
			File   ff;

			// search current directory
			if((ff = new File(fn)).isFile()) {
				return ff;
			} else if((ff = new File(fn + ".scm")).isFile()) {
				return ff;
			}

			// search path
			for(Datum p = pth; p instanceof Cons;) {
				Cons   c  = (Cons)p;

				if(c.getCar() instanceof LispString) {
					String p0 = ((LispString)c.getCar()).getString();

					ff = new File(p0 + sep + fn);
					if(ff.isFile()) {
						return ff;
					}

					ff = new File(p0 + sep + fn + ".scm");
					if(ff.isFile()) {
						return ff;
					}
				}
				p = c.getCdr();
			}
			throw mesg.getError("err.load.filenotfound", fn);
		}

		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			Datum pth = env.findDatum(Symbol.getSymbol("*load-path*"));

			if(pth == null) {
				throw mesg.getError("err.unbound", "*load-path*");
			}

			if(c1a instanceof LispString) {
				String fn = ((LispString)c1a).getString();
				String sp = System.getProperty("file.separator");
				File   ff;
				Reader rd;

				/*if(fn.length() == 0) {
					throw mesg.getError("err.load.filenotfound", fn);
				} else if(sp.equals("/")) {      // UNIX/Linux/Mac OS X
					if(fn.charAt(0) == '/') {
						ff = findScm0(fn, mesg);
					} else {
						ff = searchPath(fn, pth, sp, mesg);
					}
				} else if(sp.equals("\\")) {     // Windows
					if(fn.matches("^([A-Za-z]:)?\\\\")) {
						ff = findScm0(fn, mesg);
					} else {
						ff = searchPath(fn, pth, sp, mesg);
					}
				} else {                         // Other OS
					ff = findScm0(fn, mesg);
				}*/
				try {
					loadFile = ff = searchPath(fn, pth, sp, mesg);
					rd = new BufferedReader(new InputStreamReader(
							new FileInputStream(ff)));
					return new InputPort(rd, mesg);
				} catch (FileNotFoundException e) {
					throw new RuntimeException(e);
				}
			} else {
				throw mesg.getError("err.reqired.string", c1a);
			}
		}

	}

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

	/**
	 * @return the loadFile
	 */
	public static File getLoadFile() {
		return loadFile;
	}

	//
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		throw new RuntimeException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#getClosureClass(net.morilib.lisp.Environment)
	 */
	@Override
	/*package*/ ClosureClass createClosureClass(Environment env) {
		CompiledCode.Builder bld = new CompiledCode.Builder();
		ClosureClass cl1 = new ClosureClass();
		Symbol f = Symbol.getSymbol("f");
		Symbol p = Symbol.getSymbol("p");

		bld.addPush(Aux1.INSTANCE);   // search file
		bld.addBeginList();
		bld.addReferSymbol(f);
		bld.addAppendList();
		bld.addEndList();
		bld.addCall();
		bld.addBind(p);               // bind port
		bld.addReferSymbol(p);        // load
		bld.addOverrideLoadCode();
		bld.addPush(Aux2.INSTANCE);   // close port
		bld.addBeginList();
		bld.addReferSymbol(p);
		bld.addAppendList();
		bld.addEndList();
		bld.addCall();
		bld.addPop();                 // set #<undef>
		bld.addPush(Undef.UNDEF);
		bld.addReturnOp();

		cl1.setParameterList(new Cons(f, Nil.NIL));
		cl1.setCode(bld.getCodeRef());
		return cl1;
	}

}
