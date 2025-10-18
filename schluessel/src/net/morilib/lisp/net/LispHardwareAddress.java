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
package net.morilib.lisp.net;

import java.util.Arrays;

import net.morilib.lisp.Datum2;
import net.morilib.util.Bytes;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/16
 */
public class LispHardwareAddress extends Datum2 {

	//
	private byte[] addrs;

	/**
	 * 
	 * @param addrs
	 */
	public LispHardwareAddress(byte[] addrs) {
		this.addrs = new byte[addrs.length];
		System.arraycopy(addrs, 0, this.addrs, 0, addrs.length);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return Arrays.hashCode(addrs);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof LispHardwareAddress) {
			return Arrays.equals(
					addrs, ((LispHardwareAddress)o).addrs);
		}
		return false;
	}

	/**
	 * 
	 * @param mac
	 * @return
	 */
	public static String toString(LispHardwareAddress mac) {
		String dlm = "";
		StringBuilder buf = new StringBuilder();

		for(byte b : mac.addrs) {
			buf.append(dlm);
			buf.append(String.format("%02X", Bytes.ubyteToInt(b)));
			dlm = ":";
		}
		return buf.toString();
	}

	/**
	 * 
	 * @return
	 */
	public String toPrintString() {
		return toString(this);
	}

	/**
	 * 
	 * @return
	 */
	public byte[] getAddress() {
		return Arrays.copyOf(addrs, addrs.length);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<hardware-address ");
		buf.append(toString(this)).append(">");
	}

}
