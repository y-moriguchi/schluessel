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
package net.morilib.lisp.net.snmp;

import java.io.IOException;
import java.net.InetAddress;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.Subr;
import net.morilib.lisp.net.SubrNetUtils;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.net.snmp.ASN1Exception;
import net.morilib.net.snmp.ObjectIdentifier;
import net.morilib.net.snmp.SNMP;
import net.morilib.net.snmp.SNMPPDU;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/15
 */
public class SnmpWalk extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum d1 = SubrUtils.nextIf(itr, mesg, body);
		Datum d2 = SubrUtils.nextIf(itr, mesg, body), d3;
		int port = 161;
		String comm;
		ObjectIdentifier oid, oid2;
		InetAddress addr;
		SNMPPDU pdu;
		ConsListBuilder b;

		try {
			addr = SubrNetUtils.getAddress(d1, mesg);
			if(d2 instanceof LispSmallInt) {
				port = SubrUtils.getSmallInt(d2, mesg);
				comm = SubrUtils.nextString(itr, mesg, body);
			} else {
				comm = SubrUtils.getString(d2, mesg);
			}
			d3   = SubrUtils.nextIf(itr, mesg, body);
			SubrUtils.checkTerminated(itr, body, mesg);

			if(d3 instanceof LispOID) {
				oid = oid2 = ((LispOID)d3).oid;
			} else {
				throw mesg.getError("err.net.require.oid", d3);
			}

			b = new ConsListBuilder();
			while(true) {
				pdu = SNMP.getNext(addr, port, comm, oid2);
				if(pdu.getErrorCode() != 0) {
					return b.get();
				} else if(pdu.getVariables().isEmpty()) {
					return b.get();
				} else if(pdu.getVariables().get(0).getA().isSubTreeOf(
						oid)) {
					b.append(SnmpGet.tupletodtm(
							pdu.getVariables().get(0)));
					oid2 = pdu.getVariables().get(0).getA();
				} else {
					return b.get();
				}
			}
		} catch(IOException e) {
			throw mesg.getError("err.io", e.getMessage());
		} catch (ASN1Exception e) {
			throw mesg.getError("err.net.asn1.invalid");
		}
	}

}
