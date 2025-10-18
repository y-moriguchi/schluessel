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
package net.morilib.lang.system;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/24
 */
public final class System2 {

	//
	private System2() {}

	/**
	 * 
	 * @return
	 */
	public static Reader consoleReader() {
		Class<?> cls = System.class;
		Method mth;
		Object o1;

		try {
			mth = cls.getMethod("console");
			if((o1 = mth.invoke(null)) == null) {
				return null;
			}

			cls = Class.forName("java.io.Console");
			mth = cls.getMethod("reader");
			return (Reader)mth.invoke(o1);
		} catch(SecurityException e) {
		} catch(NoSuchMethodException e) {
		} catch(IllegalArgumentException e) {
		} catch(IllegalAccessException e) {
		} catch(InvocationTargetException e) {
		} catch(ClassNotFoundException e) {
		} catch(ClassCastException e) {
		}
		return null;
	}

	/**
	 * 
	 * @return
	 */
	public static String getShell() {
		String s = System.getenv("SHELL");

		return (s != null) ? s : OSInfo.OS.getStandardShell();
	}

	/**
	 * 
	 * @param s
	 * @return
	 * @throws IOException 
	 */
	public static int system(String s,
			OutputStream ous) throws IOException {
		OutputStream bos = null;
		InputStream ins;
		String[] args = new String[3];
		byte[] b = new byte[1024];
		Process p;
		int l;

		if(!OSInfo.OS.hasShell()) {
			return Integer.MIN_VALUE;
		} else if(ous != null) {
			bos = new BufferedOutputStream(ous);
		}

		try {
			args[0] = getShell();
			args[1] = OSInfo.OS.getOptionString() + "c";
			args[2] = s;
			p = new ProcessBuilder(args).redirectErrorStream(
					true).start();

			ins = new BufferedInputStream(p.getInputStream());
			if(bos != null) {
				while((l = ins.read(b)) >= 0) {
					bos.write(b, 0, l);
					bos.flush();
				}
			}
			return p.waitFor();
		} catch (InterruptedException e) {
			return Integer.MIN_VALUE;
		}
	}

}
