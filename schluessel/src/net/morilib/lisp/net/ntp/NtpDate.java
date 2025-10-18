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
package net.morilib.lisp.net.ntp;

import java.io.IOException;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispIOException;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Subr;
import net.morilib.lisp.datetime.LispDate;
import net.morilib.lisp.net.LispInetAddress;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.net.ntp.NTP;
import net.morilib.net.ntp.NTPException;
import net.morilib.net.ntp.NTPTimestamp;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/06
 */
public class NtpDate extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum d1 = SubrUtils.nextIf(itr, mesg, body);
		int pt = SubrUtils.nextSmallInt(itr, 123, mesg);
		NTPTimestamp t1;

		try {
			SubrUtils.checkTerminated(itr, body, mesg);
			if(d1 instanceof LispInetAddress) {
				t1 = NTP.receiveTime(
						((LispInetAddress)d1).getAddress(), pt);
			} else if(d1 instanceof LispString) {
				t1 = NTP.receiveTime(d1.getString(), pt);
			} else {
				throw mesg.getError("err.net.require.hostnameoraddress",
						d1);
			}
			return new LispDate(t1.getTime());
		} catch(NTPException e) {
			throw mesg.getError("err.net.ntp");
		} catch (IOException e) {
			throw new LispIOException(e);
		}
	}

}
