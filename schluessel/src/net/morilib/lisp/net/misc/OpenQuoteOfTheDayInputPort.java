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
package net.morilib.lisp.net.misc;

import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.net.UnknownHostException;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.net.SubrNetUtils;
import net.morilib.lisp.r6rs.io.LispFileOptions;
import net.morilib.lisp.r6rs.io.LispInputStreamPort;
import net.morilib.lisp.r6rs.io.LispTextualInputPort;
import net.morilib.lisp.r6rs.io.transcd.ILispTranscoder;
import net.morilib.lisp.r6rs.io.transcd.LispBufferMode;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.net.misc.TCPServiceInputStream;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/09
 */
public class OpenQuoteOfTheDayInputPort extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		try {
			ConsIterator itr = new ConsIterator(body);
			InetAddress ip = SubrNetUtils.nextAddress(itr, body, mesg);
			int   pt = 17;
			Datum op = Iterators.nextIf(itr, LispFileOptions.DEFAULT);
			Datum bm = Iterators.nextIf(itr, LispBufferMode.BLOCK);
			Datum tr = Iterators.nextIf(itr, LispBoolean.FALSE);
			InputStream ins;

			SubrUtils.checkTerminated(itr, body, mesg);
			if(!(op instanceof LispFileOptions)) {
				throw mesg.getError("err.io.require.fileoptions", op);
			} else if(!(bm instanceof LispBufferMode)) {
				throw mesg.getError("err.io.require.buffermode", bm);
			} else if(tr.isTrue() &&
					!(tr instanceof ILispTranscoder)) {
				throw mesg.getError("err.io.require.transcoder", tr);
			} else {
				ins = new TCPServiceInputStream(ip, pt);
				if(tr.isTrue()) {
					return new LispTextualInputPort(
							((ILispTranscoder)tr).newInput(ins),
							(ILispTranscoder)tr);
				} else {
					return new LispInputStreamPort(ins);
				}
			}
		} catch (UnknownHostException e) {
			throw mesg.getError("err.net.unknownhost");
		} catch (IOException e) {
			throw mesg.getError("err.io", e.getMessage());
		}
	}

}
