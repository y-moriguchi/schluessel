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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/18
 */
public class Base64Encoder implements Encoder {

	//
	/*package*/ static final byte[] STANDARD_B64 = new byte[] {
		'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
		'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
		'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
		'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
		'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
		'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
		'w', 'x', 'y', 'z', '0', '1', '2', '3',
		'4', '5', '6', '7', '8', '9', '+', '/'
	};

	/*package*/ static final byte[] FILE_B64 = new byte[] {
		'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
		'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
		'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
		'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
		'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
		'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
		'w', 'x', 'y', 'z', '0', '1', '2', '3',
		'4', '5', '6', '7', '8', '9', '+', '-'
	};

	/*package*/ static final byte[] URL_B64 = new byte[] {
		'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
		'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
		'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
		'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
		'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
		'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
		'w', 'x', 'y', 'z', '0', '1', '2', '3',
		'4', '5', '6', '7', '8', '9', '-', '_'
	};

	/*package*/ static final byte[] NMTOKEN_B64 = new byte[] {
		'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
		'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
		'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
		'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
		'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
		'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
		'w', 'x', 'y', 'z', '0', '1', '2', '3',
		'4', '5', '6', '7', '8', '9', '.', '-'
	};

	/*package*/ static final byte[] RE_B64 = new byte[] {
		'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
		'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
		'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
		'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
		'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
		'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
		'w', 'x', 'y', 'z', '0', '1', '2', '3',
		'4', '5', '6', '7', '8', '9', '!', '-'
	};

	//
	private static final byte[] _CRLF = new byte[] { '\r', '\n' };
	private static final byte[] _CR   = new byte[] { '\r' };
	private static final byte[] _LF   = new byte[] { '\n' };
	private static final byte[] _NONE = null;

	//
	private static final byte[][] _NEWLS = new byte[][] {
		_CRLF, _CR, _LF, _NONE
	};

	/**
	 * 
	 */
	public static final int CRLF = 0;

	/**
	 * 
	 */
	public static final int CR = 1;

	/**
	 * 
	 */
	public static final int LF = 2;

	/**
	 * 
	 */
	public static final int NONE = 3;

	/**
	 * 
	 */
	public static final Base64Encoder STANDARD = new Base64Encoder(
			STANDARD_B64, '=', 64, CRLF);

	/**
	 * 
	 */
	public static final Base64Encoder UTF7 = new Base64Encoder(
			STANDARD_B64, -1, -1, NONE);

	/**
	 * 
	 */
	public static final Base64Encoder FILE = new Base64Encoder(
			FILE_B64, -1, -1, NONE);

	/**
	 * 
	 */
	public static final Base64Encoder URL = new Base64Encoder(
			URL_B64, -1, -1, NONE);

	/**
	 * 
	 */
	public static final Base64Encoder NMTOKEN = new Base64Encoder(
			NMTOKEN_B64, -1, -1, NONE);

	/**
	 * 
	 */
	public static final Base64Encoder RE = new Base64Encoder(
			RE_B64, -1, -1, NONE);

	//
	private byte[]  chars;
	private int     padding;
	private int     maxlength;
	private byte[]  newline;

	//
	private static byte[] _copy(byte[] s) {
		byte[] r = new byte[s.length];

		System.arraycopy(s, 0, r, 0, s.length);
		return r;
	}

	/**
	 * 
	 * @param chars
	 * @param padding
	 * @param maxlength
	 * @param newline
	 * @param extrachar
	 * @param header
	 * @param encoding
	 * @param trailer
	 * @return
	 */
	public static Base64Encoder getInstance(byte[] chars, int padding,
			int maxlength, int newline) {
		return new Base64Encoder(_copy(chars), padding, maxlength,
				newline);
	}

	//
	/*package*/ Base64Encoder(byte[] chars, int padding, int maxlength,
			int newline) {
		if(newline < 0 || newline > NONE) {
			throw new IllegalArgumentException();
		} else if(chars == null) {
			throw new NullPointerException();
		} else if(chars.length != 64) {
			throw new IllegalArgumentException();
		}

		this.chars     = chars;
		this.padding   = padding;
		this.maxlength = maxlength;
		this.newline   = _NEWLS[newline];
	}

	/**
	 * 
	 * @param wr
	 * @param ins
	 * @throws IOException
	 */
	public void encode(OutputStream wr,
			InputStream ins) throws IOException {
		int c, r = 0, l = 0, w, o = 0, n = 0;

		while((c = ins.read()) >= 0) {
			r = (r << 8) + c;
			l = l + 8;
			for(int x = l; x >= 6; x -= 6) {
				w = (r >> (x - 6)) & 0x3f;
				wr.write(chars[w]);
				if(maxlength > 0) {
					n++;
					if(n >= maxlength) {
						wr.write(newline);
						n = n % maxlength;
					}
				}
			}
			o = (o + (l / 6)) % 4;
			l = l % 6;
			r = r & ((1 << l) - 1);
		}

		if(l > 0) {
			w = r << (6 - l);
			wr.write(chars[w]);
			o = (o + 1) % 4;
		}

		if(padding >= 0 && o > 0) {
			for(; o < 4; o++) {
				wr.write(padding);
			}
		}
	}

	/**
	 * 
	 * @param a
	 * @return
	 */
	public String encode(byte[] a) {
		ByteArrayOutputStream ous = new ByteArrayOutputStream();
		ByteArrayInputStream  ins = new ByteArrayInputStream(a);

		try {
			encode(ous, ins);
			return new String(ous.toByteArray());
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

}
