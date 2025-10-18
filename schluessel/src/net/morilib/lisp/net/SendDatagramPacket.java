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

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.uvector.LispS8Vector;
import net.morilib.lisp.uvector.LispU8Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/17
 */
public class SendDatagramPacket extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum c1a = SubrUtils.nextIf(itr, mesg, body);
		Datum c2a = SubrUtils.nextIf(itr, mesg, body);
		int   prt = SubrUtils.nextSmallInt(itr, mesg, body);
		Datum c4a = SubrUtils.nextIf(itr, mesg, body);
		InetAddress adr;
		DatagramPacket pkt;
		byte[] buf;
		int   s, e;

		if(!(c1a instanceof LispDatagramSocket)) {
			throw mesg.getError("err.net.require.datagramsocket", c1a);
		} else if(c2a instanceof LispInetAddress) {
			adr = ((LispInetAddress)c2a).address;
		} else if(c2a instanceof Cons) {
			adr = SubrInetAddress.listToInetAddress(c2a, mesg);
		} else {
			throw mesg.getError("err.net.require.address", c2a);
		}

		if(c4a instanceof LispS8Vector) {
			buf = ((LispS8Vector)c4a).toArray();
		} else if(c4a instanceof LispU8Vector) {
			buf = ((LispU8Vector)c4a).toArray();
		} else {
			throw mesg.getError("err.uvector.require.8", c4a);
		}

		try {
			s = SubrUtils.nextSmallInt(itr, 0, mesg);
			e = SubrUtils.nextSmallInt(itr, buf.length, mesg);
			if(s < 0 || s >= buf.length) {
				throw mesg.getError("err.range.invalid",
						Integer.toString(s));
			} else if(e < 0 || e > buf.length) {
				throw mesg.getError("err.range.invalid",
						Integer.toString(e));
			}
			SubrUtils.checkTerminated(itr, body, mesg);
			pkt = new DatagramPacket(buf, s, e, adr, prt);
			((LispDatagramSocket)c1a).socket.send(pkt);
		} catch (IOException e1) {
			throw mesg.getError("err.io");
		}
		return Undef.UNDEF;
	}

}
