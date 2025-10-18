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
import java.io.OutputStream;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/23
 */
public class UTF8StringBuilderOutputStream extends OutputStream {

	//
	private int b1, b2, stat = 0;
	private StringBuilder buf = new StringBuilder();

	/* (non-Javadoc)
	 * @see java.io.OutputStream#write(int)
	 */
	@Override
	public void write(int b) throws IOException {
		switch(stat) {
		case 0:
			if((b & 0x80) == 0) {
				buf.append((char)(b & 0x7f));
			} else if((b & 0xe0) == 0xc0) {
				b1 = b & 0x3f;
				stat = 10;
			} else if((b & 0xf0) == 0xe0) {
				b1 = b & 0x0f;
				stat = 20;
			} else if((b & 0xf8) == 0xf0) {
				b1 = b & 0x07;
				stat = 30;
			}
			break;
		case 10:
			if((b & 0xc0) == 0x80) {
				int c = (b1 << 6) | (b & 0x3f);

				if(c >= 0x80) {
					buf.append((char)c);
				}
			}
			stat = 0;
			break;
		case 20:
			if((b & 0xc0) == 0x80) {
				b2 = b & 0x3f;
				stat = 21;
			} else {
				stat = 0;
			}
			break;
		case 21:
			if((b & 0xc0) == 0x80) {
				int c = (b1 << 12) | (b2 << 6) | (b & 0x3f);

				if(c >= 0x800) {
					buf.append((char)c);
				}
			}
			stat = 0;
			break;
		case 30:
			if((b & 0xc0) == 0x80) {
				b2 = b & 0x3f;
				stat = 31;
			} else {
				stat = 0;
			}
			break;
		case 31:
			if((b & 0xc0) == 0x80) {
				stat = 32;
			} else {
				stat = 0;
			}
			break;
		case 32:
			stat = 0;
			break;
		}
	}

	/**
	 * 
	 */
	public void clear() {
		buf = new StringBuilder();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return buf.toString();
	}

}
