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
package net.morilib.lisp.net;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/17
 */
public class SubrInetAddress extends Subr {

	//
	/*package*/ static InetAddress listToInetAddress(
			Datum body, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);
		byte[] adr;

		if(l.size() != 4 && l.size() != 16) {
			throw mesg.getError("err.argument", body);
		} else {
			adr = new byte[l.size()];
			for(int i = 0; i < l.size(); i++) {
				int x = SubrUtils.getSmallInt(l.get(i), mesg);

				if(x < 0 || x > 255) {
					throw mesg.getError(
							"err.uvector.outofrange.u8", l.get(i));
				}
				adr[i] = (byte)x;
			}

			try {
				return InetAddress.getByAddress(adr);
			} catch (UnknownHostException e) {
				throw mesg.getError("err.net.host.unknown");
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		return new LispInetAddress(listToInetAddress(body, mesg));
	}

}
