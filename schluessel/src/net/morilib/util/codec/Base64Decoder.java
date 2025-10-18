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
import java.util.Arrays;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/23
 */
public class Base64Decoder implements Decoder {

	//
	private static final byte INV = -1;

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
	public static final Base64Decoder STANDARD = new Base64Decoder(
			Base64Encoder.STANDARD_B64, '=', CRLF);

	/**
	 * 
	 */
	public static final Base64Decoder UTF7 = new Base64Decoder(
			Base64Encoder.STANDARD_B64, -1, NONE);

	/**
	 * 
	 */
	public static final Base64Decoder FILE = new Base64Decoder(
			Base64Encoder.FILE_B64, -1, NONE);

	/**
	 * 
	 */
	public static final Base64Decoder URL = new Base64Decoder(
			Base64Encoder.URL_B64, -1, NONE);

	/**
	 * 
	 */
	public static final Base64Decoder NMTOKEN = new Base64Decoder(
			Base64Encoder.NMTOKEN_B64, -1, NONE);

	/**
	 * 
	 */
	public static final Base64Decoder RE = new Base64Decoder(
			Base64Encoder.RE_B64, -1, NONE);

	//
	private byte[]  charinv;
	private int     padding;
	private byte[]  newline;

	/**
	 * 
	 * @param maxlength
	 * @return
	 */
	public static Base64Decoder newMime() {
		return new Base64Decoder(Base64Encoder.STANDARD_B64, '=',
				CRLF);
	}

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
	 * @param newline
	 * @param extrachar
	 * @return
	 */
	public static Base64Decoder getInstance(byte[] chars, int padding,
			int newline) {
		return new Base64Decoder(_copy(chars), padding, newline);
	}

	//
	/*package*/ Base64Decoder(byte[] chars, int padding, int newline) {
		if(newline < 0 || newline > NONE) {
			throw new IllegalArgumentException();
		} else if(chars == null) {
			throw new NullPointerException();
		} else if(chars.length != 64) {
			throw new IllegalArgumentException();
		}

		this.charinv   = new byte[128 - 32];
		this.padding   = padding;
		this.newline   = _NEWLS[newline];

		Arrays.fill(this.charinv, INV);
		for(int i = 0; i < chars.length; i++) {
			if(chars[i] < 32) {
				throw new IllegalArgumentException();
			} else {
				charinv[chars[i] - 32] = (byte)i;
			}
		}
	}

	/**
	 * 
	 * @param wr
	 * @param ins
	 * @throws IOException
	 * @throws Base64Exception
	 */
	public void decode(OutputStream wr,
			InputStream ins) throws IOException, DecodeException {
		int c, state = 0, r = 0, l = 0, x = 0;

		while((c = ins.read()) >= 0) {
			if(state == 2) {
				if(c != newline[x++]) {
					throw new DecodeException();
				} else if(newline.length == x) {
					state = 0;
				}
			} else if(c == padding) {
				state = 1;
			} else if(state == 1) {
				throw new DecodeException();
			} else if(newline != null && c == newline[0]) {
				x = 1;
				state = 2;
			} else if(c > 127 || c < 32) {
				throw new DecodeException();
			} else if(charinv[c - 32] == INV) {
				throw new DecodeException();
			} else {
				r  = (r << 6) + charinv[c - 32];
				l += 6;
				if(l >= 8) {
					wr.write((r >> (l - 8)) & 0xff);
					l -= 8;
					r &= ((1 << l) - 1);
				}
			}
		}
	}

	/**
	 * 
	 * @param a
	 * @return
	 * @throws Base64Exception 
	 */
	public byte[] decode(String s) throws DecodeException {
		ByteArrayOutputStream ous = new ByteArrayOutputStream();
		ByteArrayInputStream  ins =
			new ByteArrayInputStream(s.getBytes());

		try {
			decode(ous, ins);
			return ous.toByteArray();
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

}
