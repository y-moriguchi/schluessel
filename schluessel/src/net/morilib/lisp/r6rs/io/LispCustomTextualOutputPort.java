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

import java.io.IOException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.util.io.UTF16;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/03
 */
public class LispCustomTextualOutputPort extends LispCustomOutputPort
implements ILispTextualOutputPort {

	//
	private Procedure write;

	/**
	 * @param id
	 * @param rd
	 * @param ps
	 * @param sp
	 * @param cl
	 */
	public LispCustomTextualOutputPort(Environment env, LispMessage mes,
			String id, Procedure wr, Procedure ps, Procedure sp,
			Procedure cl) {
		super(env, mes, id, ps, sp, cl);
		this.write = wr;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#putByte(int)
	 */
	public void putChar(int b) throws IOException {
		int[] c = new int[1];

		c[0] = b;
		putString(UTF16.toString(c), 0, 1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#putBytes(byte[])
	 */
	public void putString(String src) throws IOException {
		putString(src, 0, src.length());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispBinaryOutputPort#putBytes(byte[], int)
	 */
	public void putString(String src, int start) throws IOException {
		putString(src, start, src.length());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putString(java.lang.String, int, int)
	 */
	public void putString(String s, int start, int end) throws IOException {
		LispString b = new LispString(s);

		Scheme.callva(write, env, mesg, b, LispInteger.valueOf(start),
				LispInteger.valueOf(end - start));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispTextualOutputPort#putDatum(net.morilib.lisp.Datum)
	 */
	public void putDatum(Datum d) throws IOException {
		putString(LispUtils.print(d));
	}

}
