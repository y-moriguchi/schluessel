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

import java.io.BufferedOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.InetAddress;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.Subr;
import net.morilib.lisp.net.SubrNetUtils;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.net.misc.TFTP;
import net.morilib.util.IOs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/21
 */
public class TftpGet extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum d2;
		BufferedOutputStream ous = null;
		InetAddress addr;
		String mode, fn, ofn;
		int port = 69;
		long l;

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
			ous = new BufferedOutputStream(new FileOutputStream(ofn));
			l = TFTP.get(addr, port, fn, mode, ous);
			ous.flush();
			return LispInteger.valueOf(l);
		} catch(IOException e) {
			throw mesg.getError("err.io", e.getMessage());
		} finally {
			IOs.close(ous);
		}
	}

}
