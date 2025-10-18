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
package net.morilib.lisp.net.dhcp;

import java.io.IOException;
import java.net.InetAddress;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.Subr;
import net.morilib.lisp.net.LispHardwareAddress;
import net.morilib.lisp.net.LispInetAddress;
import net.morilib.lisp.net.SubrNetUtils;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.uvector.LispU8Vector;
import net.morilib.net.dhcp.BOOTP;
import net.morilib.net.dhcp.BOOTPResult;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/05/19
 */
public class GetIpAddressByBootp extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		InetAddress adr, cad;
		Datum d1;
		int sec;
		BOOTPResult res;
		byte[] hwaddr;

		try {
			adr = SubrNetUtils.nextAddress(itr, body, mesg);
			cad = SubrNetUtils.nextAddress(itr, body, mesg);
			d1  = SubrUtils.nextIf(itr, mesg, body);
			sec = SubrUtils.nextSmallInt(itr, 0, mesg);

			if(d1 instanceof LispHardwareAddress) {
				hwaddr = ((LispHardwareAddress)d1).getAddress();
			} else if(d1 instanceof LispU8Vector) {
				hwaddr = ((LispU8Vector)d1).toArray();
			} else {
				return LispBoolean.FALSE;
			}

			if(cad.getAddress().length != 4) {
				return LispBoolean.FALSE;
			}

			if(hwaddr.length != 6)  return LispBoolean.FALSE;
			sec = (sec > Short.MAX_VALUE) ? Short.MAX_VALUE : sec;
			res = BOOTP.request(adr, cad, hwaddr, (short)sec);
			return MultiValues.newValues(
					new LispInetAddress(res.getMyAddress()),
					new LispInetAddress(res.getServerAddress()),
					new LispString(res.getBootFilePath()));
		} catch (IOException e) {
			throw mesg.getError("err.io", e.getMessage());
		}
	}

}
