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
package net.morilib.net;

import net.morilib.net.snmp.ObjectIdentifier;
import net.morilib.net.snmp.SNMP;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/11
 */
public class SNMPDrv {

	public static void main(String[] args) throws Exception {
		ObjectIdentifier oid;
		Object o;

		oid = ObjectIdentifier.parse("1.3.6.1.2.1.1.1.0");
		o = SNMP.getRequest("192.168.0.10", "public", oid);
		System.out.println(o);

		oid = ObjectIdentifier.parse("1.3.6.1.2.1.1.1");
		o = SNMP.getNext("192.168.0.10", "public", oid);
		System.out.println(o);

		oid = ObjectIdentifier.parse("1.3.6.1.2.1.1.1.0");
		o = SNMP.getNext("192.168.0.10", "public", oid);
		System.out.println(o);
		System.exit(0);
	}

}
