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

import java.util.ArrayList;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/11
 */
public class ASN1TagOID extends ASN1Tag {

	ASN1TagOID() {
		super(0, false, (byte)0x06);
	}

	@Override
	protected Object analyse(byte[] bts) throws ASN1Exception {
		List<Integer> l = new ArrayList<Integer>();
		int[] r;
		int x = 0, cnt = 0;

		// error check
		if(bts == null) {
			throw new NullPointerException();
		} else if(bts.length == 0) {
			throw new ASN1Exception();
		}

		// step 1
		l.add(bts[0] / 40);
		l.add(bts[0] % 40);

		// step 2
		for(int i = 1; i < bts.length; i++) {
			if(cnt == 0) {
				if((bts[i] & 0x80) == 0) {
					l.add(bts[i] & 0x7f);
				} else {
					cnt = 1;
					x = bts[i] & 0x7f;
				}
			} else if(cnt > 3) {
				throw new ASN1Exception();
			} else {
				x = (x << 7) | bts[i] & 0x7f;
				if((bts[i] & 0x80) == 0) {
					l.add(x);
					x = cnt = 0;
				} else {
					cnt++;
				}
			}
		}

		// step 3
		if(cnt != 0) {
			throw new ASN1Exception();
		}

		// step 4
		r = new int[l.size()];
		for(int i = 0; i < l.size(); i++) {
			r[i] = l.get(i).intValue();
		}

		// step 5
		try {
			return new ObjectIdentifier(r, false);
		} catch(IllegalArgumentException e) {
			throw new ASN1Exception("illegal OID");
		}
	}

}
