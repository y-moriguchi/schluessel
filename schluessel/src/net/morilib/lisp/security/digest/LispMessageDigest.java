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
package net.morilib.lisp.security.digest;

import net.morilib.lisp.Datum2;
import net.morilib.util.Bytes;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/09
 */
public class LispMessageDigest extends Datum2
implements java.io.Serializable {

	//
	byte[] digest;
	String algorithm;

	//
	LispMessageDigest(byte[] digest, String algorithm) {
		this.digest = digest;
		this.algorithm = algorithm;
	}

	/**
	 * 
	 * @param digest
	 * @return
	 */
	public static String hexify(byte[] digest) {
		StringBuilder b = new StringBuilder();

		for(int i = 0; i < digest.length; i++) {
			b.append(Integer.toString(
					Bytes.ubyteToInt(digest[i]), 16));
		}
		return b.toString();
	}

	/**
	 * 
	 * @return
	 */
	public String hexify() {
		StringBuilder b = new StringBuilder();

		for(int i = 0; i < digest.length; i++) {
			b.append(Integer.toString(
					Bytes.ubyteToInt(digest[i]), 16));
		}
		return b.toString();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<message-digest").append(hexify()).append(">");
	}

}
