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
package net.morilib.net.snmp;

import java.text.SimpleDateFormat;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/11
 */
public class ASN1TagUTCTime extends ASN1Tag {

	//
	private static final SimpleDateFormat FMT =
		new SimpleDateFormat("yyMMddHHmmssZ");

	//
	ASN1TagUTCTime() {
		super(0, false, (byte)0x17);
	}

	/* (non-Javadoc)
	 * @see net.morilib.net.snmp.ASN1Tag#analyse(byte[])
	 */
	@Override
	protected Object analyse(byte[] bts) throws ASN1Exception {
		String s = new String(bts);

		try {
			if(s.endsWith("Z")) {
				s = s.replaceFirst("Z$", "") + "+0000";
			}
			return FMT.format(s);
		} catch(IllegalArgumentException e) {
			throw new ASN1Exception();
		}
	}

}
