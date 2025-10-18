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
package net.morilib.lisp.charset;

import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/16
 */
/*package*/ class CharSetPropertyParser {

	//
	private static int read1(InputStream p) throws IOException {
		int r = p.read();

		if(r < 0) {
			throw new RuntimeException("Illegal code");
		}
		return r;
	}

	/**
	 * 
	 * @param ins
	 * @param h
	 * @throws IOException
	 */
	public static void parse(
			InputStream ins, CharSetPropertyHandler h) throws IOException {
		PushbackInputStream p = new PushbackInputStream(ins);
		StringBuilder b = new StringBuilder();
		int st = 0;
		int c, cb = -1;

		while((c = p.read()) >= 0) {
			if(c > 127) {
				throw new RuntimeException("Illegal character");
			} else if(st == 0) {
				if(c == '=') {
					h.charsetName(b.toString());
					b.delete(0, b.length());
					st = 1;
				} else if(c == '\r' || c == '\n') {
					st = 8;
				} else {
					b.append((char)c);
				}
			} else if(st == 1) {
				if(c == '\\') {
					st = 2;
				} else if(c == '\r' || c == '\n') {
					st = 8;
				} else {
					cb = c;
					st = 4;
				}
			} else if(st == 2) {
				if(c == 'x' || c == 'u') {
					st = 3;
				} else {
					throw new RuntimeException("Illegal character");
				}
			} else if(st == 3) {
				b.append((char)c);
				b.append((char)read1(p));
				b.append((char)read1(p));
				b.append((char)read1(p));
				try {
					cb = Integer.parseInt(b.toString(), 16);
					b.delete(0, b.length());
					st = 4;
				} catch(NumberFormatException e) {
					throw new RuntimeException("Illegal code");
				}
			} else if(st == 4) {
				if(c == '\\') {
					h.singleChar(cb);
					st = 2;
				} else if(c == '-') {
					st = 5;
				} else if(c == '\r' || c == '\n') {
					st = 8;
				} else {
					h.singleChar(cb);
					cb = c;
					st = 4;
				}
			} else if(st == 5) {
				if(c == '\\') {
					st = 6;
				} else if(c == '\r' || c == '\n') {
					h.singleChar('-');
					st = 8;
				} else {
					h.rangedChar(cb, c);
					st = 1;
				}
			} else if(st == 6) {
				if(c == 'x' || c == 'u') {
					st = 7;
				} else {
					throw new RuntimeException("Illegal character");
				}
			} else if(st == 7) {
				b.append((char)c);
				b.append((char)read1(p));
				b.append((char)read1(p));
				b.append((char)read1(p));
				try {
					h.rangedChar(
							cb, Integer.parseInt(b.toString(), 16));
					b.delete(0, b.length());
					st = 1;
				} catch(NumberFormatException e) {
					throw new RuntimeException("Illegal code");
				}
			} else if(st == 8) {
				if(c != '\n') {
					p.unread(c);
				}
				st = 0;
			}
		}
	}

}
