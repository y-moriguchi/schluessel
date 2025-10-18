/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp.net;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.OutputPort;
import net.morilib.lisp.Subr;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class SocketOutputPort extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);

		try {
			if(l.size() == 1) {
				Datum c1a = l.get(0);

				if(c1a instanceof LispSocket) {
					LispSocket lcs = (LispSocket)c1a;
					OutputStream ins = lcs.socket.getOutputStream();

					return new OutputPort(ins, mesg);
				} else {
					throw mesg.getError(
							"err.net.require.clientsocket", c1a);
				}
			} else if(l.size() == 2) {
				Datum c1a = l.get(0);
				Datum c2a = l.get(1);
				LispSocket lcs = (LispSocket)c1a;
				OutputStream ins = lcs.socket.getOutputStream();

				if(!(c1a instanceof LispSocket)) {
					throw mesg.getError(
							"err.net.require.clientsocket", c1a);
				} else if(!(c2a instanceof LispString)) {
					throw mesg.getError("err.require.string", c2a);
				} else {
					String enc = c2a.getString();

					try {
						return new OutputPort(
								new PrintStream(ins, false, enc),
								mesg);
					} catch (UnsupportedEncodingException e) {
						throw mesg.getError(
								"err.unsupportedencoding", enc);
					}
				}
			} else {
				throw mesg.getError("err.argument", body);
			}
		} catch(IOException e) {
			throw mesg.getError("err.io");
		}
	}

}
