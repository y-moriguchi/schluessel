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

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import net.morilib.lisp.sos.SOS;
import net.morilib.util.IOs;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class InitVarLoader {

	//
	private static final String INIT_XML =
		"/net/morilib/lisp/init/initvar.xml";

	//
	private static Properties initProp = null;
	private static Map<String, InitVarLoader> subf =
		Collections.synchronizedMap(
				new HashMap<String, InitVarLoader>());

	//
	private Properties props = new Properties();

	//
	private InitVarLoader(InputStream ins) throws IOException {
		props.loadFromXML(ins);
	}

	/**
	 * 
	 * @param env
	 */
	public static void load(Environment env) {
		if(initProp == null) {
			synchronized(InitVarLoader.class) {
				InputStream ins = null;

				// 初期化一覧リストを検索する
				try {
					initProp = new Properties();
					ins = InitVarLoader.class.getResourceAsStream(
							INIT_XML);
					initProp.loadFromXML(ins);
				} catch (IOException e) {
					throw new RuntimeException(
							"Cannot load propfile", e);
				} finally {
					IOs.close(ins);
				}

				// 各初期化ファイルを読み込む
				for(Map.Entry<Object, Object> e
						: initProp.entrySet()) {
					String k = (String)e.getKey();
					String v = (String)e.getValue();
					String fn;

					k = k.replaceFirst("#[0-9a-zA-Z]+$", "");
					fn = "/" + k.replaceAll("\\.", "/") + "/" + v;
					subf.put(k, loadInstance(k, fn));
				}
			}
		}

		// 初期環境(subroutine, syntax)を構築する
		for(InitVarLoader s : subf.values()) {
			s.load1(env, null);
		}

		// initialize object system
		initSOS(env);
	}

	//
	private static void putz(
			Environment env, String sy, NamableDatum cls) {
		cls.setName(sy);
		env.bindDatum(Symbol.getSymbol(sy), cls);
	}

	//
	private static void initSOS(Environment env) {
		SOS s = SOS.getInstance();

		putz(env, "<top>",        s.cTop);
		putz(env, "<collection>", s.cCollection);
		putz(env, "<sequence>",   s.cSequence);
		putz(env, "<vector>",     s.cVector);
		putz(env, "<list>",       s.cList);
		putz(env, "<string>",     s.cString);
		putz(env, "<null>",       s.cNull);
		putz(env, "<pair>",       s.cPair);
		putz(env, "<char>",       s.cChar);
		putz(env, "<number>",     s.cNumber);
		putz(env, "<complex>",    s.cComplex);
		putz(env, "<real>",       s.cReal);
		putz(env, "<rational>",   s.cRational);
		putz(env, "<integer>",    s.cInteger);
		putz(env, "<boolean>",    s.cBoolean);
		putz(env, "<symbol>",     s.cSymbol);
		putz(env, "<object>",     s.cObject);
		putz(env, "<class>",      s.cClass);
		putz(env, "<generic>",    s.cGeneric);
		putz(env, "<keyword>",    s.cKeyword);
		putz(env, "<regexp>",     s.cRegexp);
	}

	/**
	 * 
	 * @param env
	 * @param ver
	 */
	public static void loadNullEnv(Environment env, int ver) {
		// do nothing
	}

	/**
	 * 
	 * @param env
	 * @param ver
	 */
	public static void loadRnRSEnv(Environment env, int ver) {
		// do nothing
	}

	//
	private static InitVarLoader loadInstance(String pkg, String fname,
			ClassLoader loader) {
		InitVarLoader res;
		InputStream ins = null;

		try {
			if((ins = loader.getResourceAsStream(fname)) == null) {
				return null;
			}
			res = new InitVarLoader(ins);
			//subf.put(pkg, res);
		} catch (IOException e) {
			throw new RuntimeException("Cannot load propfile", e);
		} finally {
			IOs.close(ins);
		}
		return res;
	}

	//
	private static InitVarLoader loadInstance(String pkg, String fn) {
//		return loadInstance(pkg, fn,
//				InitVarLoader.class.getClassLoader());
		InitVarLoader res;
		InputStream ins = null;

		try {
			ins = InitVarLoader.class.getResourceAsStream(fn);
			res = new InitVarLoader(ins);
			//subf.put(pkg, res);
		} catch (IOException e) {
			throw new RuntimeException("Cannot load propfile", e);
		} finally {
			IOs.close(ins);
		}
		return res;
	}

	/**
	 * 
	 * @param pkg
	 * @param fname
	 * @param env
	 * @param loader
	 */
	public static boolean load1(String pkg, String fname, Environment env,
			ClassLoader loader) {
		InitVarLoader r = loadInstance(pkg, fname, loader);

		if(r == null) {
			return false;
		}
		r.load1(env, loader);
		return true;
	}

	//
	private void load1(Environment env, ClassLoader ldr) {
		for(Map.Entry<Object, Object> i : props.entrySet()) {
			String k = (String)i.getKey();
			String v = (String)i.getValue();

			v = v.trim();
			try {
				String cln, fln;
				int    spl = v.lastIndexOf('.');

				if(spl < 0) {
					throw new RuntimeException("illegal field");
				}
				cln = v.substring(0, spl);
				fln = v.substring(spl + 1, v.length());

				Class<?> cls = (ldr == null) ?
						Class.forName(cln) : ldr.loadClass(cln);
				Field    fld = cls.getField(fln);
				Datum    ins = (Datum)fld.get(null);

				env.bindDatum(Symbol.getSymbol(k), ins);
			} catch (ClassNotFoundException e) {
				throw new RuntimeException(e);
			} catch (IllegalAccessException e) {
				throw new RuntimeException(e);
			} catch (SecurityException e) {
				throw new RuntimeException(e);
			} catch (NoSuchFieldException e) {
				throw new RuntimeException(e);
			}
		}
	}

}
