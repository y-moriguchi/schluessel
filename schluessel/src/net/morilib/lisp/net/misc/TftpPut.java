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

import java.io.BufferedInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.net.InetAddress;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.net.SubrNetUtils;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.net.misc.TFTP;
import net.morilib.util.IOs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/21
 */
public class TftpPut extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum d2;
		BufferedInputStream ins = null;
		InetAddress addr;
		String mode, fn, ofn;
		int port = 69;

		try {
			addr = SubrNetUtils.nextAddress(itr, body, mesg);
			d2   = SubrUtils.nextIf(itr, mesg, body);
			if(d2 instanceof LispSmallInt) {
				port = SubrUtils.getSmallInt(d2, mesg);
				mode = SubrUtils.nextSymbolName(itr, mesg, body);
			} else {
				mode = SubrUtils.getSymbolName(d2, mesg);
			}

			fn  = SubrUtils.nextString(itr, mesg, body);
			ofn = SubrUtils.nextString(itr, mesg, body);
			ins = new BufferedInputStream(new FileInputStream(ofn));
			TFTP.put(addr, port, fn, mode, ins);
			return Undef.UNDEF;
		} catch(IOException e) {
			throw mesg.getError("err.io", e.getMessage());
		} finally {
			IOs.close(ins);
		}
	}

}
