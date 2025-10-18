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
package net.morilib.lisp.r6rs.io;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.uvector.LispU8Vector;
import net.morilib.util.Bytes;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/03
 */
public class LispCustomBinaryInputPort extends LispCustomInputPort
implements ILispBinaryInputPort {

	//
	private Procedure read;

	/**
	 * @param id
	 * @param rd
	 * @param ps
	 * @param sp
	 * @param cl
	 */
	public LispCustomBinaryInputPort(Environment env, LispMessage mes,
			String id, Procedure rd, Procedure ps, Procedure sp,
			Procedure cl) {
		super(env, mes, id, ps, sp, cl);
		this.read = rd;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getByte()
	 */
	public int getByte() throws IOException {
		if(lookahead < 0) {
			return -1;
		} else {
			LispU8Vector b = new LispU8Vector(new byte[1]);
			Datum d = Scheme.callva(read, env, mesg,
					b, LispInteger.ZERO, LispInteger.ONE);
			int bts = SubrUtils.getSmallInt(d, mesg);
			int r = lookahead;

			if(bts > 0) {
				lookahead = b.get(0).getExactSmallInt();
				return r;
			} else {
				return lookahead = -1;
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#lookaheadByte()
	 */
	public int lookaheadByte() throws IOException {
		return lookahead;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getBytes(int)
	 */
	public byte[] getBytes(int n) throws IOException {
		byte[] r = new byte[n];

		return (getBytes(r, 0, n) >= 0) ? r : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getBytes(byte[], int, int)
	 */
	public int getBytes(byte[] buf,
			int start, int end) throws IOException {
		if(lookahead < 0) {
			return -1;
		} else {
			LispU8Vector b = new LispU8Vector(buf);
			Datum d = Scheme.callva(read, env, mesg,
					b, LispInteger.valueOf(start),
					LispInteger.valueOf(end - start));
			int bts = SubrUtils.getSmallInt(d, mesg);
			byte[] c;

			if(bts > 0) {
				c = b.toBytes();
				System.arraycopy(c, start,
						buf, start + 1, bts - 1);
				buf[start] = (byte)lookahead;
				lookahead = Bytes.ubyteToInt(c[start + bts - 1]);
				return bts;
			} else {
				buf[start] = (byte)lookahead;
				lookahead = -1;
				return 1;
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getByteSome()
	 */
	public byte[] getBytesSome() throws IOException {
		return getBytes(1024);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getByteAll()
	 */
	public byte[] getBytesAll() throws IOException {
		ByteArrayOutputStream b = new ByteArrayOutputStream();
		byte[] c = new byte[1024];
		int l;

		while((l = getBytes(c, 0, 1024)) >= 0) {
			b.write(c, 0, l);
		}
		return b.toByteArray();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryInputPort#getStream()
	 */
	public InputStream getInputStream() {
		return new SchlushBinaryPortInputStream(this);
	}

}
