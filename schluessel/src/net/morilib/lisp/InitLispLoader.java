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
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Properties;
import java.util.TreeMap;

import net.morilib.lisp.util.LispHandler;
import net.morilib.util.IOs;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class InitLispLoader {

	//
	private static final String INIT_XML =
		"/net/morilib/lisp/init/initlisp.xml";

	//
	private static Properties initProp = null;

	/**
	 * 
	 * @param lsp
	 */
	public static void load(Scheme lsp) {
		if(initProp == null) {
			synchronized(InitLispLoader.class) {
				InputStream ins = null;

				// 初期化一覧リストを検索する
				try {
					initProp = new Properties();
					ins = InitLispLoader.class.getResourceAsStream(
							INIT_XML);
					initProp.loadFromXML(ins);
				} catch (IOException e) {
					throw new RuntimeException(
							"Cannot load propfile", e);
				} finally {
					IOs.close(ins);
				}
			}
		}

		// 初期環境(subroutine, syntax)を構築する
		TreeMap<?, ?> mp2 = new TreeMap<Object, Object>(initProp);
		for(Object s : mp2.values()) {
			load1(lsp, (String)s);
		}
	}

	/**
	 * 
	 * @param lsp
	 * @param ver
	 */
	public static void loadNullEnv(Scheme lsp, int ver) {
		// do nothing
	}

	/**
	 * 
	 * @param lsp
	 * @param ver
	 */
	public static void loadRnRSEnv(Scheme lsp, int ver) {
		switch(ver) {
		case 5:
			load1(lsp, "/net/morilib/lisp/init/r5rs.scm");
			break;
		}
	}

	/**
	 * 
	 * @param fname
	 * @param env
	 * @param mesg
	 * @param loader
	 */
	public static boolean load1(String fname, Environment env,
			LispMessage mesg, ClassLoader loader) {
		return load1(new Scheme(env, mesg), fname, loader);
	}

	//
	private static void load1(Scheme lsp, String fn) {
//		load1(lsp, fn, InitLispLoader.class.getClassLoader());
		InputStream ins = null;

		try {
			Reader rd;

			LispHandler.setLoggable2(false);
			ins = InitLispLoader.class.getResourceAsStream(fn);
			rd = new InputStreamReader(ins);
			lsp.readFile(rd);
			LispHandler.setLoggable2(true);
		} catch (IOException e) {
			throw new RuntimeException(
					"Cannot load initfile", e);
		} finally {
			IOs.close(ins);
		}
	}

	//
	private static boolean load1(Scheme lsp, String fn,
			ClassLoader loader) {
		InputStream ins = null;

		try {
			Reader rd;

			LispHandler.setLoggable2(false);
			if((ins = loader.getResourceAsStream(fn)) == null) {
				return false;
			}
			rd = new InputStreamReader(ins);
			lsp.readFile(rd);
			LispHandler.setLoggable2(true);
			return true;
		} catch (IOException e) {
			throw new RuntimeException(
					"Cannot load initfile", e);
		} finally {
			IOs.close(ins);
		}
	}

}
