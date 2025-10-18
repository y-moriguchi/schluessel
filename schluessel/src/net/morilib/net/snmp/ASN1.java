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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInput;
import java.io.DataInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/11
 */
public class ASN1 {

	//
	static ASN1Tag readTag(DataInput din) throws IOException {
		ByteArrayOutputStream b = new ByteArrayOutputStream();
		int c, d;

		c = din.readUnsignedByte();
		if((c & 0x1f) != 0x1f) {
			return ASN1Tag.getInstance(c >> 6, (c & 0x20) != 0,
					c & 0x1f);
		} else {
			while(((d = din.readByte()) & 0x80) != 0) {
				b.write(d);
			}
			return new ASN1Tag(c >> 6, (c & 0x20) != 0,
					b.toByteArray());
		}
	}

	//
	static int readLength(
			DataInput din) throws IOException, ASN1Exception {
		int l = 0;
		int c;

		c = din.readUnsignedByte();
		if((c & 0x80) == 0) {
			return c & 0x7f;
		} else if((c & 0x7f) > 4) {
			throw new ASN1Exception();
		} else {
			for(int i = 0; i < (c & 0x7f); i++) {
				l = (l << 8) + din.readUnsignedByte();
			}

			if(l < 0) {
				throw new ASN1Exception();
			}
			return l;
		}
	}

	//
	static List<Object> readSequence(byte[] b) throws ASN1Exception {
		List<Object> r = new ArrayList<Object>();
		ByteArrayInputStream bis = new ByteArrayInputStream(b);
		DataInputStream din = new DataInputStream(bis);
		ASN1Tag t;
		int l;
		byte[] f;

		try {
			for(int i = 0; i < b.length;) {
				t = readTag(din);
				l = readLength(din);
				f = new byte[l];
				din.readFully(f);
				r.add(t.analyse(f));
				i += 1 + SNMP.lengthToBytes(l).length + f.length;
			}
			return r;
		} catch(IOException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * 
	 * @param din
	 * @return
	 * @throws IOException
	 * @throws ASN1Exception
	 */
	public static Object read(
			DataInput din) throws IOException, ASN1Exception {
		ASN1Tag t = readTag(din);
		int l = readLength(din);
		byte[] b = new byte[l];

		din.readFully(b);
		return t.analyse(b);
	}

}
