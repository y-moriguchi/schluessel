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

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.jar.Attributes;
import java.util.jar.JarFile;
import java.util.jar.Manifest;

import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/08
 */
public class LoadPlagin extends UnaryArgs {

	//
	private static final Datum PLUGIN_PATH =
		Symbol.getSymbol("*plugin-path*");

	/**
	 * 
	 * @param fname
	 * @param env
	 * @throws IOException
	 */
	public static void loadPlugin(File fil, JarFile jar,
			Environment env, LispMessage mesg) throws IOException {
		Manifest    mf  = jar.getManifest();
		Attributes  atr = mf.getMainAttributes();
		URL         url = fil.getCanonicalFile().toURI().toURL();
		ClassLoader lrd = new URLClassLoader(new URL[] { url });
		String subrs = atr.getValue("Procedures");
		String pkg   = atr.getValue("Base-Package");
		String vars  = atr.getValue("Variables");
		String init  = atr.getValue("Init-Script");
		String mes0  = atr.getValue("Message-Bundle");

		if(mes0 != null) {
			LispMessage.addPlugin(mes0, lrd);
		}
		if(subrs != null) {
			subrs = pkg.replace('.', '/') + "/" + subrs;
			if(!InitSubrLoader.load1(pkg, subrs, env, lrd)) {
				throw mesg.getError("err.plugin.notfound.subrs");
			}
		}
		if(vars != null) {
			vars = pkg.replace('.', '/') + "/" + vars;
			if(!InitVarLoader.load1(pkg, vars, env, lrd)) {
				throw mesg.getError("err.plugin.notfound.vars");
			}
		}
		if(init != null) {
			init = pkg.replace('.', '/') + "/" + init;
			if(!InitLispLoader.load1(init, env, mesg, lrd)) {
				throw mesg.getError("err.plugin.notfound.init");
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		String s = SubrUtils.getString(c1a, mesg);
		Datum  d = env.findDatum(PLUGIN_PATH);

		try {
			File f1 = null;
			JarFile jf = null;

			try {
				f1 = new File(s);
				jf = new JarFile(f1);
			} catch(IOException e1) {
				ConsIterator itr =
					new ConsIterator((d == null) ? Nil.NIL : d);

				while(itr.hasNext()) {
					String p = SubrUtils.getString(itr.next(), mesg);
					String l = System.getProperty("file.separator");

					p  = p.replaceFirst(l + "+$", "");
					try {
						jf = null;
						f1 = new File(p + l + s);
						jf = new JarFile(f1);
						break;
					} catch(IOException e2) {
						//t = e2;
					}
				}

				if(jf == null) {
					throw mesg.getError("err.io");
				}
			}
			loadPlugin(f1, jf, env, mesg);
			return Undef.UNDEF;
		} catch (IOException e) {
			throw mesg.getError("err.io");
		}
	}

}
