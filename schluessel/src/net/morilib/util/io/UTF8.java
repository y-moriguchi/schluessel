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
public final class UTF8 {

	//
	private UTF8() {}

	//
	private static void chk2(int c) throws IOException {
		if(c < 0x80) {
			throw new IllegalUTFException();
		}
	}

	//
	private static int chk3(int c) throws IOException {
		if((c & 0xc0) != 0x80) {
			throw new IllegalUTFException();
		}
		return c & 0x3f;
	}

	/**
	 * 
	 * @param ins
	 * @param mode
	 * @return
	 * @throws IOException
	 */
	public static int read(InputStream ins) throws IOException {
		int r = -1, c, state = 0;

		outer: while((c = ins.read()) >= 0) {
			if(state > 0) {
				chk2(c);
			}

			switch(state) {
			case 0:
				r = 0;
				if((c & 0x80) == 0) {
					r = c;
					break outer;
				} else if((c & 0xe0) == 0xc0) {
					r = (c & 0x1f) << 6;
					if(r < 0x80) {
						throw new IllegalUTFException();
					}
					state = 10;
				} else if((c & 0xf0) == 0xe0) {
					r = (c & 0x0f) << 12;
					state = 20;
				} else if((c & 0xf8) == 0xf0) {
					r = (c & 0x07) << 16;
					state = 30;
				} else if((c & 0xfc) == 0xf8) {
					r = (c & 0x03) << 24;
					state = 40;
				} else if((c & 0xfe) == 0xfc) {
					r = (c & 0x01) << 30;
					state = 50;
				} else {
					throw new IllegalUTFException();
				}
				break;
			case 10:
				r = r | chk3(c);
				state = 0;
				break outer;
			case 20:
				r = r | (chk3(c) << 6);
				state = 21;
				break;
			case 21:
				r = r | chk3(c);
				if(r < 0x0800 ||
						(r >= 0xd800 && r <= 0xd8ff) ||
						(r >= 0xdc00 && r <= 0xdfff)) {
					throw new IllegalUTFException();
				}
				state = 0;
				break outer;
			case 30:
				r = r | (chk3(c) << 12);
				state = 31;
				break;
			case 31:
				r = r | (chk3(c) << 6);
				state = 32;
				break;
			case 32:
				r = r | chk3(c);
				if(r < 0x010000) {
					throw new IllegalUTFException();
				}
				state = 0;
				break outer;
			case 40:
				r = r | (chk3(c) << 18);
				state = 41;
				break;
			case 41:
				r = r | (chk3(c) << 12);
				state = 42;
				break;
			case 42:
				r = r | (chk3(c) << 6);
				state = 43;
				break;
			case 43:
				r = r | chk3(c);
				if(r < 0x0200000) {
					throw new IllegalUTFException();
				}
				state = 0;
				break outer;
			case 50:
				r = r | (chk3(c) << 24);
				state = 51;
				break;
			case 51:
				r = r | (chk3(c) << 18);
				state = 52;
				break;
			case 52:
				r = r | (chk3(c) << 12);
				state = 53;
				break;
			case 53:
				r = r | (chk3(c) << 6);
				state = 54;
				break;
			case 54:
				r = r | chk3(c);
				if(r < 0x04000000) {
					throw new IllegalUTFException();
				}
				state = 0;
				break outer;
			}
		}

		if(state != 0) {
			throw new IllegalUTFException();
		}
		return r;
	}

	/**
	 * 
	 * @param ous
	 * @param c
	 * @throws IOException
	 */
	public static void write(OutputStream ous,
			int c) throws IOException {
		if(c < 0 || (c >= 0xd800 && c <= 0xd8ff) ||
				(c >= 0xdc00 && c <= 0xdfff)) {
			throw new IllegalUTFException();
		} else if(c < 0x80) {
			ous.write(c);
		} else if(c < 0x800) {
			ous.write(0xc0 | ((c >>  6) & 0x3f));
			ous.write(0x80 | ((c      ) & 0x3f));
		} else if(c < 0x10000) {
			ous.write(0xe0 | ((c >> 12) & 0x3f));
			ous.write(0x80 | ((c >>  6) & 0x3f));
			ous.write(0x80 | ((c      ) & 0x3f));
		} else if(c < 0x200000) {
			ous.write(0xf0 | ((c >> 18) & 0x3f));
			ous.write(0x80 | ((c >> 12) & 0x3f));
			ous.write(0x80 | ((c >>  6) & 0x3f));
			ous.write(0x80 | ((c      ) & 0x3f));
		} else if(c < 0x4000000) {
			ous.write(0xf8 | ((c >> 24) & 0x3f));
			ous.write(0x80 | ((c >> 18) & 0x3f));
			ous.write(0x80 | ((c >> 12) & 0x3f));
			ous.write(0x80 | ((c >>  6) & 0x3f));
			ous.write(0x80 | ((c      ) & 0x3f));
		} else {
			ous.write(0xfc | ((c >> 30) & 0x3f));
			ous.write(0x80 | ((c >> 24) & 0x3f));
			ous.write(0x80 | ((c >> 18) & 0x3f));
			ous.write(0x80 | ((c >> 12) & 0x3f));
			ous.write(0x80 | ((c >>  6) & 0x3f));
			ous.write(0x80 | ((c      ) & 0x3f));
		}
	}

}
