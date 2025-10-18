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
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.uvector.LispU8Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/17
 */
public class ReceiveDatagramPacketU8 extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum c1a = SubrUtils.nextIf(itr, mesg, body);
		Datum c2a = SubrUtils.nextIf(itr, mesg, body);
		int   prt = -1;
		int   len;
		InetAddress adr;
		DatagramPacket pkt;
		byte[] buf;

		if(!(c1a instanceof LispDatagramSocket)) {
			throw mesg.getError("err.net.require.datagramsocket", c1a);
		} else if(c2a instanceof LispInetAddress) {
			adr = ((LispInetAddress)c2a).address;
			prt = SubrUtils.nextSmallInt(itr, mesg, body);
			len = SubrUtils.nextSmallInt(itr, mesg, body);
		} else if(c2a instanceof Cons) {
			adr = SubrInetAddress.listToInetAddress(c2a, mesg);
			prt = SubrUtils.nextSmallInt(itr, mesg, body);
			len = SubrUtils.nextSmallInt(itr, mesg, body);
		} else {
			adr = null;
			len = SubrUtils.getSmallInt(c2a, mesg);
		}

		try {
			if(len < 0) {
				throw mesg.getError("err.require.int.nonnegative",
						Integer.toString(len));
			}
			SubrUtils.checkTerminated(itr, body, mesg);
			buf = new byte[len];
			if(adr == null) {
				pkt = new DatagramPacket(buf, len);
			} else {
				pkt = new DatagramPacket(buf, len, adr, prt);
			}
			((LispDatagramSocket)c1a).socket.receive(pkt);
			return new LispU8Vector(buf);
		} catch (IOException e1) {
			throw mesg.getError("err.io");
		}
	}

}
