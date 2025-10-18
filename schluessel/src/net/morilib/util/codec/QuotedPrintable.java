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
 * @author MORIGUCHI, Yuichiro 2011/12/23
 */
public class QuotedPrintable implements Encoder, Decoder {

	/**
	 * 
	 */
	public static final QuotedPrintable STANDARD =
		new QuotedPrintable('=', false, null);

	/**
	 * 
	 */
	public static final QuotedPrintable STANDARD_NEWLINE =
		new QuotedPrintable('=', true, null);

	//
	private int metachar;
	private boolean newline;
	private ASCIIFilter asciiexcept;

	/**
	 * 
	 * @param metachar
	 * @param newline
	 * @param asciiexcept
	 */
	public QuotedPrintable(int metachar, boolean newline,
			ASCIIFilter asciiexcept) {
		this.metachar    = metachar;
		this.newline     = newline;
		this.asciiexcept = (asciiexcept == null) ?
				ASCIIFilter.NULL : asciiexcept;
	}

	//
	private boolean _except(int c) {
		return (c == metachar || asciiexcept.test((byte)c));
	}

	//
	private int _hexe(int n) {
		if(n < 0) {
			throw new RuntimeException();
		} else if(n < 10) {
			return n + '0';
		} else if(n < 16) {
			return n + 'A';
		} else {
			throw new RuntimeException();
		}
	}

	//
	private int _hexd(int n) throws DecodeException {
		if(n >= '0' && n <= '9') {
			return n - '0';
		} else if(n >= 'A' && n <= 'F') {
			return n - 'A';
		} else {
			throw new DecodeException();
		}
	}

	//
	private void _crlf(OutputStream ous) throws IOException {
		ous.write(metachar);
		ous.write(_hexe('\r' >> 4));
		ous.write(_hexe('\r' & 0x0f));
		ous.write(metachar);
		ous.write(_hexe('\n' >> 4));
		ous.write(_hexe('\n' & 0x0f));
	}

	//
	private void _putc(OutputStream ous, int c) throws IOException {
		if(c < 33 || c > 126 || _except(c)) {
			ous.write(metachar);
			ous.write(_hexe(c >> 4));
			ous.write(_hexe(c & 0x0f));
		} else {
			ous.write(c);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.codec.Encoder#encode(java.io.OutputStream, java.io.InputStream)
	 */
	public void encode(OutputStream ous,
			InputStream ins) throws IOException {
		int state = 0, c;

		while((c = ins.read()) >= 0) {
			switch(state) {
			case 0:
				if(newline && c == '\r') {
					state = 1;
				} else if(newline && c == '\n') {
					_crlf(ous);
				} else {
					_putc(ous, c);
				}
				break;
			case 1:
				if(c == '\n') {
					_crlf(ous);
				} else {
					_putc(ous, '\r');
					_putc(ous, c);
				}
				state = 0;
				break;
			default:
				throw new RuntimeException();
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.codec.Encoder#encode(byte[])
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

	/* (non-Javadoc)
	 * @see net.morilib.util.codec.Decoder#decode(java.io.OutputStream, java.io.InputStream)
	 */
	public void decode(OutputStream ous, InputStream ins
			) throws IOException, DecodeException {
		int state = 0, c, d = 0;

		while((c = ins.read()) >= 0) {
			switch(state) {
			case 0:
				if(c == metachar) {
					d = 0;
					state = 1;
				} else {
					ous.write(c);
				}
				break;
			case 1:
			case 2:
				d = (d << 4) + _hexd(c);
				if(++state == 3) {
					ous.write(d);
					state = 0;
				}
				break;
			default:
				throw new RuntimeException();
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.codec.Decoder#decode(java.lang.String)
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
