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

import java.io.ByteArrayOutputStream;
import java.math.BigInteger;
import java.util.Arrays;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/11
 */
public class ObjectIdentifier {

	//
	private int[] oid;

	/**
	 * 
	 * @param o
	 */
	public ObjectIdentifier(int[] o) {
		oid = new int[o.length];
		System.arraycopy(o, 0, oid, 0, o.length);
	}

	//
	ObjectIdentifier(int[] o, boolean f) {
		oid = o;
	}

	/**
	 * 
	 * @return
	 */
	public byte[] toBytes() {
		ByteArrayOutputStream b = new ByteArrayOutputStream();
		int c = 0;

		b.write(oid[0] * 40 + oid[1]);
		for(int i = 2; i < oid.length; i++) {
			if(c == 0 && (c = oid[i]) == 0) {
				b.write(0);
			} else if((c & 0xf0000000) != 0) {
				b.write((c >>> 28) | 0x80);
			} else if((c & 0x0fe00000) != 0) {
				b.write((c >>> 21) | 0x80);
			} else if((c & 0x001fc000) != 0) {
				b.write((c >>> 14) | 0x80);
			} else if((c & 0x00003f80) != 0) {
				b.write((c >>>  7) | 0x80);
			} else {
				b.write(c & 0x7f);
			}
			c >>>= 7;
		}
		return b.toByteArray();
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static ObjectIdentifier parse(String s) {
		String[] ss = s.split("\\.");
		int[] rr = new int[ss.length];
		BigInteger x;

		for(int i = 0; i < ss.length; i++) {
			x = new BigInteger(ss[i]);
			if(x.signum() < 0 && x.bitLength() > 31) {
				throw new IllegalArgumentException();
			} else {
				rr[i] = x.intValue();
			}
		}
		return new ObjectIdentifier(rr, false);
	}

	/**
	 * 
	 * @param oid1
	 * @return
	 */
	public boolean isSubTreeOf(ObjectIdentifier oid1) {
		if(oid.length > oid1.oid.length) {
			for(int i = 0; i < oid1.oid.length; i++) {
				if(oid[i] != oid1.oid[i]) {
					return false;
				}
			}
			return true;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return Arrays.hashCode(oid);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof ObjectIdentifier) {
			return Arrays.equals(oid, ((ObjectIdentifier)o).oid);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder b = new StringBuilder();
		String dlm = "";

		for(int i = 0; i < oid.length; i++) {
			b.append(dlm).append(oid[i]);
			dlm = ".";
		}
		return b.toString();
	}

}
