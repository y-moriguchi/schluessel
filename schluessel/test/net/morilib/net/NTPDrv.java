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

import java.text.SimpleDateFormat;

import net.morilib.net.ntp.NTP;
import net.morilib.net.ntp.NTPInfo;
import net.morilib.net.ntp.NTPTimestamp;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/06
 */
public class NTPDrv {

	public static void main(String[] args) throws Exception {
		NTPTimestamp t;
		NTPInfo ri;
		SimpleDateFormat df = new SimpleDateFormat("yyyyMMdd");

		long x1 = df.parse("19000101").getTime();
		long x2 = df.parse("19700101").getTime();
		System.out.println(x2 - x1);
		System.out.println(NTPTimestamp.localCurrentTime().getDate());

		t = NTP.receiveTime("localhost");
		System.out.println(t.getDate());

		ri = NTP.receive("localhost");
		System.out.println(ri.getLeapIndicator());
		System.out.println(ri.getVersion());
		System.out.println(ri.getMode());
		System.out.println(ri.getReferenceInfo().getLayer());
		System.out.println(ri.getPrecision());
		System.out.println(ri.getOriginateTime().getDate());
		System.out.println(ri.getReceiveTime().getDate());
		System.out.println(new String(ri.getMessageDigest()));
		System.exit(0);
	}

}
