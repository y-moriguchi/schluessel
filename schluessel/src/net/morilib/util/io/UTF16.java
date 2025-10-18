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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/01
 */
public final class UTF16 {

	/**
	 * 
	 */
	public static final boolean LITTLE_ENDIAN = false;

	/**
	 * 
	 */
	public static final boolean BIG_ENDIAN = true;

	//
	private UTF16() {}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static int[] getInts(String s) {
		int[] r;
		int len = 0, c;

		if(s == null) {
			throw new NullPointerException();
		}

		for(int i = 0; i < s.length();) {
			c = s.codePointAt(i);
			i += (c > Character.MAX_VALUE) ? 2 : 1;
			len++;
		}

		r = new int[len];
		len = 0;
		for(int i = 0; i < s.length();) {
			c = s.codePointAt(i);
			r[len++] = s.codePointAt(i);
			i += (c > Character.MAX_VALUE) ? 2 : 1;
		}
		return r;
	}

	/**
	 * 
	 * @param cd
	 * @param off
	 * @param len
	 * @param endian
	 * @return
	 */
	public static String toString(int[] cd, int off, int len) {
		StringBuilder b = new StringBuilder();

		for(int i = off; i < len; i++) {
			b.appendCodePoint(cd[i]);
		}
		return b.toString();
	}

	/**
	 * 
	 * @param cd
	 * @return
	 */
	public static String toString(int[] cd) {
		return toString(cd, 0, cd.length);
	}

	/**
	 * 
	 * @param ins
	 * @param endian
	 * @return
	 * @throws IOException
	 */
	public static int read(InputStream ins,
			boolean endian) throws IOException {
		int state = 0, c, r = 0, s = 0;

		while((c = ins.read()) >= 0) {
			switch(state) {
			case 0:
				r  = endian ? (c << 8) : c;
				state = 1;
				break;
			case 1:
				r |= endian ? c : (c << 8);
				if((r & 0xfc00) == 0xd800) {
					s = (((r & 0x03c0) + 0x40) | (r & 0x3f)) << 10;
					state = 2;
				} else {
					state = 0;
					return r;
				}
				break;
			case 2:
				r  = endian ? (c << 8) : c;
				state = 3;
				break;
			case 3:
				r |= endian ? c : (c << 8);
				if((r & 0xfc00) == 0xdc00) {
					s |= (r & 0x3ff);
					state = 0;
					return s;
				} else {
					throw new IllegalUTFException();
				}
			}
		}

		if(state != 0) {
			throw new IllegalUTFException();
		}
		return c;
	}

	/**
	 * 
	 * @param ous
	 * @param c
	 * @param endian
	 * @throws IOException
	 */
	public static void write(OutputStream ous, int c,
			boolean endian) throws IOException {
		int s, t;

		if(c < 0 || c > 0x10FFFF) {
			throw new IllegalUTFException();
		} else if(c < 0x10000) {
			ous.write((endian ? (c >> 8) : c) & 0xff);
			ous.write((endian ? c : (c >> 8)) & 0xff);
		} else {
			s = ((c >> 10) - 0x40) | 0xd800;
			t = (c & 0x03ff) | 0xdc00;
			ous.write((endian ? (s >> 8) : s) & 0xff);
			ous.write((endian ? s : (s >> 8)) & 0xff);
			ous.write((endian ? (t >> 8) : t) & 0xff);
			ous.write((endian ? t : (t >> 8)) & 0xff);
		}
	}

	/**
	 * 
	 * @param ins
	 * @return
	 * @throws IOException
	 */
	public static boolean readBOM(InputStream ins) throws IOException {
		int c;

		if((c = ins.read()) == 0xfe) {
			if((c = ins.read()) == 0xff) {
				return BIG_ENDIAN;
			} else {
				throw new IllegalUTFException();
			}
		} else if(c == 0xff) {
			if((c = ins.read()) == 0xfe) {
				return LITTLE_ENDIAN;
			} else {
				throw new IllegalUTFException();
			}
		} else {
			throw new IllegalUTFException();
		}
	}

}
