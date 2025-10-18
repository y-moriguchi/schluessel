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
package net.morilib.net.ntp;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/06
 */
public enum NTPMode {

	RESERVED(0), SYMMETRIC_ACTIVE(1), SYMMETRIC_PASSIVE(2),
	CLIANT(3), SERVER(4), BROADCAST(5), NTP_MESSAGE(6), PRIVATE(7);

	//
	private int cd;

	//
	private NTPMode(int cd) {
		this.cd = cd;
	}

	/**
	 * 
	 * @param bt
	 * @return
	 */
	public static NTPMode getFromNTPBits(int bt) {
		switch(bt & 7) {
		case 0:   return RESERVED;
		case 1:   return SYMMETRIC_ACTIVE;
		case 2:   return SYMMETRIC_PASSIVE;
		case 3:   return CLIANT;
		case 4:   return SERVER;
		case 5:   return BROADCAST;
		case 6:   return NTP_MESSAGE;
		case 7:   return PRIVATE;
		default:  throw new IllegalArgumentException();
		}
	}

	/**
	 * 
	 * @return
	 */
	public int getCode() {
		return cd;
	}

}
