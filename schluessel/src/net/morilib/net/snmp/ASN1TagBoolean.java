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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/11
 */
public class ASN1TagBoolean extends ASN1Tag {

	//
	ASN1TagBoolean() {
		super(0, false, (byte)0x01);
	}

	/* (non-Javadoc)
	 * @see net.morilib.net.snmp.ASN1Tag#analyse(byte[])
	 */
	@Override
	protected Object analyse(byte[] bts) throws ASN1Exception {
		if(bts.length > 0) {
			return bts[0] != 0;
		} else {
			throw new ASN1Exception();
		}
	}

}
