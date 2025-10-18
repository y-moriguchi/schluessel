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
package net.morilib.util.codec;

import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/23
 */
public class MIME {

	/**
	 * 
	 * @param src
	 * @return
	 * @throws MIMEException
	 */
	public static String decode(String src) throws MIMEException {
		int c, p = -1, state = 0;
		StringBuilder b = null;
		StringBuilder r = new StringBuilder();
		StringBuilder x = new StringBuilder();
		Decoder dec = null;

		while(++p < src.length()) {
			c = src.charAt(p);
			if(c < 0 || c > 127) {
				throw new MIMEException();
			}

			switch(state) {
			case 0:
				if(c == '=') {
					state = 1;
				}
				break;
			case 1:
				state = (c == '?') ? 2 : 0;
				b = new StringBuilder();
				break;
			case 2:
				if(c == '?') {
					state = 3;
				} else {
					b.append((char)c);
				}
				break;
			case 3:
				switch(c) {
				case 'Q':  dec = QuotedPrintable.STANDARD;  break;
				case 'B':  dec = Base64Decoder.STANDARD;  break;
				default:   throw new MIMEException();
				}
				state = 4;
				break;
			case 4:
				if(c == '?') {
					state = 5;
				} else {
					throw new MIMEException();
				}
				break;
			case 5:
				if(c == '?') {
					state = 6;
				} else {
					x.append((char)c);
				}
				break;
			case 6:
				if(c == '=') {
					try {
						r.append(new String(dec.decode(x.toString()),
								b.toString()));
					} catch (UnsupportedEncodingException e) {
						throw new MIMEException(e);
					} catch (DecodeException e) {
						throw new MIMEException(e);
					}
					x = new StringBuilder();
					state = 7;
				} else {
					throw new MIMEException();
				}
				break;
			case 7:
				if(c == '\r') {
					state = 8;
				} else {
					throw new MIMEException();
				}
				break;
			case 8:
				if(c == '\n') {
					state = 0;
				} else {
					throw new MIMEException();
				}
				break;
			}
		}

		if(state != 7 && state != 0) {
			throw new MIMEException();
		} else {
			return r.toString();
		}
	}

	//
	private static final int _MAXLENGTH = 76;

	/**
	 * 
	 * @param src
	 * @param header
	 * @param encoding
	 * @return
	 * @throws UnsupportedEncodingException
	 */
	public static String encodeBase64(String src, String header,
			String encoding) throws UnsupportedEncodingException {
		char[] s0 = new char[1], ss = new char[2];
		byte[] b = null, z;
		int c, p = 0, l, m = 0;
		ByteArrayOutputStream r = new ByteArrayOutputStream();
		ByteArrayOutputStream t = new ByteArrayOutputStream();
		boolean jis = false, dir = false, ini = false;

		if(header != null) {
			z = header.getBytes();
			l = header.length();
			r.write(z, 0, z.length);
		} else {
			l = 0;
		}
		l += 4;

		while(p < src.length()) {
			c = src.codePointAt(p);
			if(c > Character.MAX_VALUE) {
				int x1 = c & 0x3ff;
				int x2 = (c >> 10) & 0x3f;
				int x3 = (((c >> 16) & 0x1f) - 1) & 0xf;

				ss[0] = (char)(0xd800 | (x3 << 6) | x2);
				ss[1] = (char)(0xdc00 | x1);
				b = new String(ss).getBytes(encoding);
				p += 2;
			} else {
				s0[0] = (char)(c & 0xffff);
				b = new String(s0).getBytes(encoding);
				p++;
			}

			if(!dir) {
				if(ini) {
					r.write('\r');  r.write('\n');
				}
				z = (" =?" + encoding + "?B?").getBytes();
				r.write(z, 0, z.length);
				l += encoding.length() + 6;
				ini = true;
			}

			if(!encoding.equalsIgnoreCase("ISO-2022-JP")) {
				t.write(b, 0, b.length);
				m += b.length;
			} else if(b.length == 1) {
				if(jis) {
					t.write(0x1b); t.write(0x28); t.write(0x42);
					jis = false;
					m += 3;
				}
				t.write(b, 0, b.length);
				m += b.length;
			} else {
				if(!jis) {
					t.write(0x1b); t.write(0x24); t.write(0x42);
					jis = true;
					m += 3;
				}
				t.write(b, 3, b.length - 6);
				m += b.length - 6;
			}
			l += (m / 3) * 4;
			m  = m % 3;

			if(l + (jis ? 10 : 6) > _MAXLENGTH) {
				if(jis) {
					t.write(0x1b); t.write(0x28); t.write(0x42);
					jis = false;
					m += 3;
				}
				z = Base64Encoder.STANDARD.encode(
						t.toByteArray()).getBytes();
				r.write(z, 0, z.length);
				r.write('?');   r.write('=');
				l = 0;
				t = new ByteArrayOutputStream();
				dir = false;
			} else {
				dir = true;
			}
		}

		if(dir) {
			if(jis) {
				t.write(0x1b);  t.write(0x28); t.write(0x42);
			}
			z = Base64Encoder.STANDARD.encode(
					t.toByteArray()).getBytes();
			r.write(z, 0, z.length);
			r.write('?');   r.write('=');
		}
		return new String(r.toByteArray());
	}

}
