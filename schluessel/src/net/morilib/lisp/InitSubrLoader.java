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
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import net.morilib.util.IOs;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class InitSubrLoader {

	//
	static final String SUBRS_INIT =
//			"/net/morilib/lisp/subr/subrs.properties";
			"/net/morilib/lisp/subr/subrs.xml";

	//
	static {
		SYNTAX_LD = loadInstance(
				"net.morilib.lisp",
				"/net/morilib/lisp/syntaxes.xml");
		R5RS_SUBR_LD = loadInstance(
				"net.morilib.lisp.subr", SUBRS_INIT);
	}

	//
	private static final String LOAD_PATH = "net/morilib/lisp/init/";
	private static final String ALL_LOAD_FILE = "all";
	private static final InitSubrLoader SYNTAX_LD;
	private static final InitSubrLoader R5RS_SUBR_LD;

//	private static Properties initProp = null;

//	private static Map<String, InitSubrLoader> subf =
//		Collections.synchronizedMap(
//				new HashMap<String, InitSubrLoader>());

	//
//	private Properties props = new Properties();
	private Properties props;
	private String base;

	//
	private InitSubrLoader(String base, Properties prop
			) throws IOException {
		this.base = base;
//		props.loadFromXML(ins);
		this.props = prop;
	}

	/**
	 * 
	 * @param initfile
	 * @param env
	 * @param loader
	 */
	public static void load(String initfile, Environment env,
			ClassLoader loader) throws InitLoadException {
		Map<String, InitSubrLoader> subf =
				new HashMap<String, InitSubrLoader>();
		Properties initProp = null;

		InputStream ins = null;

		// 初期化一覧リストを検索する
		try {
			initProp = new Properties();
			ins = loader.getResourceAsStream(
					LOAD_PATH + initfile + ".properties");
			if(ins == null) {
				throw new InitLoadException(
						"Cannot find library: " + initfile);
			}
//					initProp.loadFromXML(ins);
			initProp.load(ins);
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
//			fn = "/" + k.replaceAll("\\.", "/") + "/" + v;
			fn = "/" + k.replace('.', '/') + "/" + v;
			subf.put(k, loadInstance(k, fn));
		}

		// 初期環境(subroutine, syntax)を構築する
		for(InitSubrLoader s : subf.values()) {
			s.load1(env);
		}
	}

	/**
	 * 
	 * @param initfile
	 * @param env
	 * @throws InitLoadException 
	 */
	public static void load(String initfile,
			Environment env) throws InitLoadException {
		load(initfile, env, InitSubrLoader.class.getClassLoader());
	}

	/**
	 * 
	 * @param initfile
	 * @param env
	 */
	public static void load(Environment env) {
		try {
			load(ALL_LOAD_FILE, env);
		} catch (InitLoadException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * 
	 * @param env
	 * @param ver
	 */
	public static void loadNullEnv(Environment env, int ver) {
		switch(ver) {
		case 5:
			SYNTAX_LD.load1(env);
			break;
		}
	}

	/**
	 * 
	 * @param env
	 * @param ver
	 */
	public static void loadRnRSEnv(Environment env, int ver) {
		switch(ver) {
		case 5:
			SYNTAX_LD.load1(env);
			R5RS_SUBR_LD.load1(env);
			break;
		}
	}

	/**
	 * 
	 * @param pkg
	 * @param fname
	 * @param loader
	 * @return
	 */
	public static InitSubrLoader loadInstance(String pkg, String fname,
			ClassLoader loader) {
		InitSubrLoader res;
		InputStream ins = null;
		Properties prop = new Properties();

		try {
			if((ins = loader.getResourceAsStream(fname)) == null) {
				return null;
			}

			if(fname.lastIndexOf(".properties") >= 0) {
				prop.load(ins);
			} else {
				prop.loadFromXML(ins);
			}
			res = new InitSubrLoader(pkg, prop);
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
	 * @param fn
	 * @return
	 */
	public static InitSubrLoader loadInstance(String pkg, String fn) {
//		return loadInstance(pkg, fn,
//				InitSubrLoader.class.getClassLoader());
		InitSubrLoader res;
		InputStream ins = null;
		Properties prop = new Properties();

		try {
			ins = InitSubrLoader.class.getResourceAsStream(fn);
			if(fn.lastIndexOf(".properties") >= 0) {
				prop.load(ins);
			} else {
				prop.loadFromXML(ins);
			}
			res = new InitSubrLoader(pkg, prop);
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
		InitSubrLoader r = loadInstance(pkg, fname, loader);

		if(r == null) {
			return false;
		}
		r.load1(env, loader);
		return true;
	}

	//
	private void load1(Environment env) {
		for(Map.Entry<Object, Object> i : props.entrySet()) {
			String k = (String)i.getKey();
			String v = (String)i.getValue();

			v = v.trim();
			if(v.indexOf('.') < 0) {
				v = base + "." + v;
			}
			IntLispUtils.loadJavaSubr(env, Symbol.getSymbol(k), v);
		}
	}

	//
	private void load1(Environment env, ClassLoader loader) {
		for(Map.Entry<Object, Object> i : props.entrySet()) {
			String k = (String)i.getKey();
			String v = (String)i.getValue();

			v = v.trim();
			if(v.indexOf('.') < 0) {
				v = base + "." + v;
			}
			IntLispUtils.loadJavaSubr(env, Symbol.getSymbol(k), v,
					loader);
		}
	}

}
