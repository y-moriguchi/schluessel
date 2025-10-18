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
package net.morilib.util.io;

import java.io.File;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/31
 */
public final class Files {

	//
	private Files() {}

	/**
	 * 
	 * @param f
	 * @param s
	 * @return
	 */
	public static boolean chmod(File f, String s) {
		char[] cs;
		boolean own, val;

		if(s == null)        throw new NullPointerException();
		if(s.length() != 3)  throw new IllegalArgumentException();
		cs = s.toCharArray();

		switch(cs[0]) {
		case 'u':  own = true;  break;
		case 'a':  own = false;  break;
		default:   throw new IllegalArgumentException();
		}

		switch(cs[1]) {
		case '+':  val = true;  break;
		case '-':  val = false;  break;
		default:   throw new IllegalArgumentException();
		}

		switch(cs[2]) {
		case 'r':  return f.setReadable(val, own);
		case 'w':  return f.setWritable(val, own);
		case 'x':  return f.setExecutable(val, own);
		default:   throw new IllegalArgumentException();
		}
	}

	//
	private static boolean _chmoda(File f,
			boolean x, boolean w, boolean r) {
		boolean z;

		z =  f.setReadable(r, false);
		z &= f.setWritable(w, false);
		z &= f.setExecutable(x, false);
		return z;
	}

	//
	private static boolean _chmodu(File f,
			boolean x, boolean w, boolean r) {
		boolean z;

		z =  f.setReadable(r, true);
		z &= f.setWritable(w, true);
		z &= f.setExecutable(x, true);
		return z;
	}

	/**
	 * 
	 * @param f
	 * @param v
	 * @return
	 */
	public static boolean chmod(File f, int v) {
		boolean r;

		if(v < 0 || v > 77) {
			throw new IllegalArgumentException();
		}

		switch(v % 10) {
		case 0:  r = _chmoda(f, false, false, false);  break;
		case 1:  r = _chmoda(f,  true, false, false);  break;
		case 2:  r = _chmoda(f, false,  true, false);  break;
		case 3:  r = _chmoda(f,  true,  true, false);  break;
		case 4:  r = _chmoda(f, false, false,  true);  break;
		case 5:  r = _chmoda(f,  true, false,  true);  break;
		case 6:  r = _chmoda(f, false,  true,  true);  break;
		case 7:  r = _chmoda(f,  true,  true,  true);  break;
		default: throw new IllegalArgumentException();
		}

		switch(v / 10) {
		case 0:  return _chmodu(f, false, false, false) & r;
		case 1:  return _chmodu(f,  true, false, false) & r;
		case 2:  return _chmodu(f, false,  true, false) & r;
		case 3:  return _chmodu(f,  true,  true, false) & r;
		case 4:  return _chmodu(f, false, false,  true) & r;
		case 5:  return _chmodu(f,  true, false,  true) & r;
		case 6:  return _chmodu(f, false,  true,  true) & r;
		case 7:  return _chmodu(f,  true,  true,  true) & r;
		default: throw new IllegalArgumentException();
		}
	}

}
