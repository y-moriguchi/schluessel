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
 * @author MORIGUCHI, Yuichiro 2012/01/15
 */
public enum SNMPPDUType {

	GET(0xa0, "get"),
	GET_NEXT(0xa1, "get-next"),
	GET_RESPONSE(0xa2, "get-response"),
	SET(0xa3, "set"),
	TRAP(0xa4, "trap"),
	UNKNOWN(0xff, "unknown");

	//
	private byte code;
	private String type;

	//
	private SNMPPDUType(int code, String type) {
		this.code = (byte)code;
		this.type = type;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static SNMPPDUType getInstance(int x) {
		switch(x) {
		case 0xa0:  return GET;
		case 0xa1:  return GET_NEXT;
		case 0xa2:  return GET_RESPONSE;
		case 0xa3:  return SET;
		case 0xa4:  return TRAP;
		default:    return UNKNOWN;
		}
	}

	/**
	 * 
	 * @return
	 */
	public byte getCode() {
		return code;
	}

	/* (non-Javadoc)
	 * @see java.lang.Enum#toString()
	 */
	public String toString() {
		return type;
	}

}
