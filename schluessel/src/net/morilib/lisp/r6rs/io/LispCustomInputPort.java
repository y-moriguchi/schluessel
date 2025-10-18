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
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNotSupportedException;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.r6rs.io.transcd.ILispTranscoder;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/03
 */
public abstract class LispCustomInputPort extends Datum2
implements ILispR6RSInputPort {

	//
	private String id;
	/*package*/ Environment env;
	/*package*/ LispMessage mesg;
	private Procedure pos, spos, close;
	/*package*/ int lookahead;

	/**
	 * @param id2
	 * @param ps
	 * @param sp
	 * @param cl
	 */
	public LispCustomInputPort(Environment env, LispMessage mesg,
			String id, Procedure ps, Procedure sp, Procedure cl) {
		this.env = env;
		this.mesg = mesg;
		this.id = id;
		this.pos = ps;
		this.spos = sp;
		this.close = cl;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#getTranscoder()
	 */
	public ILispTranscoder getTranscoder() {
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#hasPortPosition()
	 */
	public boolean hasPortPosition() {
		return pos != null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#getPortPosition()
	 */
	public Datum getPortPosition() {
		if(pos != null) {
			return Scheme.callva(pos, env, mesg);
		} else {
			throw new LispNotSupportedException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#hasSetPortPosition()
	 */
	public boolean hasSetPortPosition() {
		return spos != null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispPort#setPortPosition(net.morilib.lisp.Datum)
	 */
	public void setPortPosition(Datum pos) {
		if(spos != null) {
			Scheme.callva(spos, env, mesg, pos);
		} else {
			throw new LispNotSupportedException();
		}
	}

	/* (non-Javadoc)
	 * @see java.io.Closeable#close()
	 */
	public void close() throws IOException {
		if(close != null) {
			Scheme.callva(close, env, mesg);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.r6rs.io.ILispR6RSInputPort#isPortEof()
	 */
	public boolean isPortEof() {
		return lookahead < 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<custom-binary-input-port ");
		buf.append(id).append(">");
	}

}
