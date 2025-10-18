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
public enum NTPLeapIndicator {

	NORMAL(0),
	LAST_MINUTE_HAS_61SECONDS(1),
	LAST_MINUTE_HAS_59SECONDS(2),
	WARNING(3);

	//
	private int cd;

	//
	private NTPLeapIndicator(int cd) {
		this.cd = cd;
	}

	/**
	 * 
	 * @param bt
	 * @return
	 */
	public static NTPLeapIndicator getFromNTPBits(int bt) {
		switch((bt >> 6) & 3) {
		case 0:   return NORMAL;
		case 1:   return LAST_MINUTE_HAS_61SECONDS;
		case 2:   return LAST_MINUTE_HAS_59SECONDS;
		case 3:   return WARNING;
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
