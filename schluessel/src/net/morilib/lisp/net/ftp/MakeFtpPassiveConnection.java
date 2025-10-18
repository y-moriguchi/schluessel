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
package net.morilib.lisp.net.ftp;

import java.io.IOException;
import java.net.InetAddress;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Subr;
import net.morilib.lisp.net.LispInetAddress;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.net.ftp.FTPPassiveClient;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/14
 */
public class MakeFtpPassiveConnection extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum d1 = SubrUtils.nextIf(itr, mesg, body);
		Datum d2 = SubrUtils.nextIf(itr, mesg, body);
		int port = 21;
		String user, pass;
		InetAddress addr;

		try {
			if(d2 instanceof LispSmallInt) {
				port = SubrUtils.getSmallInt(d2, mesg);
				user = SubrUtils.nextString(itr, mesg, body);
			} else {
				user = SubrUtils.getString(d2, mesg);
			}
			pass = SubrUtils.nextString(itr, mesg, body);
			SubrUtils.checkTerminated(itr, body, mesg);

			if(d1 instanceof LispInetAddress) {
				addr = ((LispInetAddress)d1).getAddress();
			} else if(d1 instanceof LispString) {
				addr = InetAddress.getByName(d1.getString());
			} else {
				throw mesg.getError(
						"err.net.require.hostnameoraddress", d1);
			}
			return new LispFTPConnection(new FTPPassiveClient(
					addr, port, user, pass));
		} catch(IOException e) {
			throw mesg.getError("err.io", e.getMessage());
		}
	}

}
