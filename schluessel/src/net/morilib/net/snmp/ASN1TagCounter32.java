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

import net.morilib.util.Bytes;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/15
 */
public class ASN1TagCounter32 extends ASN1Tag {

	//
	ASN1TagCounter32() {
		super(1, false, (byte)0x01);
	}

	/* (non-Javadoc)
	 * @see net.morilib.net.snmp.ASN1Tag#analyse(byte[])
	 */
	@Override
	protected Object analyse(byte[] bts) throws ASN1Exception {
		long l = 0;

		for(int i = 0; i < bts.length; i++) {
			l = (l << 8) + Bytes.ubyteToInt(bts[i]);
		}
		return Long.valueOf(l);
	}

}
