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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.Subr;
import net.morilib.lisp.datetime.LispDate;
import net.morilib.lisp.net.SubrNetUtils;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.net.snmp.ASN1Exception;
import net.morilib.net.snmp.ObjectIdentifier;
import net.morilib.net.snmp.SNMP;
import net.morilib.net.snmp.SNMPPDU;
import net.morilib.net.snmp.TimeTicks;
import net.morilib.util.Tuple2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/15
 */
public class SnmpGet extends Subr {

	//
	private static Datum asn1todtm(Object a) {
		ConsListBuilder b;

		if(a == null) {
			return LispBoolean.FALSE;
		} else if(a instanceof ObjectIdentifier) {
			return new LispOID((ObjectIdentifier)a);
		} else if(a instanceof java.util.Date) {
			return new LispDate((java.util.Date)a);
		} else if(a instanceof TimeTicks) {
			return new LispTimeTicks(((TimeTicks)a).toMilliseconds());
		} else if(a instanceof Collection) {
			b = new ConsListBuilder();
			for(Object o : (Collection<?>)a) {
				b.append(asn1todtm(o));
			}
			return b.get();
		} else {
			return LispUtils.toDatum(a);
		}
	}

	//
	/*package*/ static Datum tupletodtm(
			Tuple2<ObjectIdentifier, Object> t) {
		return new Cons(
				asn1todtm(t.getA()), asn1todtm(t.getB()));
	}

	//
	private static Datum parseVariables(
			List<Tuple2<ObjectIdentifier, Object>> variables) {
		ConsListBuilder b = new ConsListBuilder();

		for(Tuple2<ObjectIdentifier, Object> t : variables) {
			b.append(tupletodtm(t));
		}
		return b.get();
	}

	//
	/*package*/ static Datum parse(SNMPPDU req) {
		List<Datum> b = new ArrayList<Datum>();

		b.add(LispInteger.valueOf(req.getVersion() + 1));
		b.add(new LispString(req.getCommunity()));
		b.add(LispInteger.valueOf(req.getId()));
		b.add(LispInteger.valueOf(req.getErrorCode()));
		b.add(LispInteger.valueOf(req.getErrorIndex()));
		b.add(parseVariables(req.getVariables()));
		return MultiValues.newValues(b);
	}

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
		ObjectIdentifier oid;
		InetAddress addr;

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
				oid = ((LispOID)d3).oid;
			} else {
				throw mesg.getError("err.net.require.oid", d3);
			}
			return parse(SNMP.getRequest(addr, port, comm, oid));
		} catch(IOException e) {
			throw mesg.getError("err.io", e.getMessage());
		} catch (ASN1Exception e) {
			throw mesg.getError("err.net.asn1.invalid");
		}
	}

}
