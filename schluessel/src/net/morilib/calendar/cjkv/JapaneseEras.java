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
package net.morilib.calendar.cjkv;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/28
 */
public final class JapaneseEras {

	//
	private JapaneseEras() {}

	//
//	static final CJKVEra 
//	SHORYAKU = new CJKVOldEra("正暦",
//			"990/01/30", "990/11/26", "995/03/24",
//			0, 2, 0, 10, 0, 0);

	//
	static final CJKVEra
	KEIO = new CJKVOldEra("慶応",
			"1865/01/27", "1865/05/01", "1868/10/23",
			new int[] { 5,  0x1256 },
			new int[] { 0,  0x013a },
			new int[] { 0,  0x0175 },
			new int[] { 4,  0x12d9 });

	/**
	 * 
	 */
	public static final CJKVEra MEIJI = new MeijiEra();

	/**
	 * 
	 */
	public static final CJKVEra
	TAISHO = new CJKVGregorianEra("大正", "T",
			"1912/01/01", "1912/07/30", "1926/12/24");

	/**
	 * 
	 */
	public static final CJKVEra
	SHOWA  = new CJKVGregorianEra("昭和", "S",
			"1926/01/01", "1926/12/25", "1989/01/07");

	/**
	 * 
	 */
	public static final CJKVEra
	HEISEI = new CJKVGregorianEra("平成", "H",
			"1989/01/01", "1989/01/07", null);

	//
	static CJKVEra[] ERAS = new CJKVEra[] {
		HEISEI, SHOWA, TAISHO, MEIJI
	};

	//
	static CJKVEra[] ANCIENT_ERAS = new CJKVEra[] {
		KEIO,
//		SHORYAKU,
	};

	/**
	 * 
	 */
	public static final List<CJKVEra> SUPPORTED_ERAS =
		Collections.unmodifiableList(Arrays.asList(ERAS));

	/**
	 * 
	 */
	public static final List<CJKVEra> SUPPORTED_ANCIENT_ERAS =
		Collections.unmodifiableList(Arrays.asList(ANCIENT_ERAS));

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static CJKVEra getEra(java.util.Date d) {
		for(int i = 0; i < ERAS.length; i++) {
			if(ERAS[i].isEra(d)) {
				return ERAS[i];
			}
		}
		return null;
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static CJKVEra getEraFromDescription(String e) {
		for(int i = 0; i < ERAS.length; i++) {
			if(ERAS[i].toString().equals(e)) {
				return ERAS[i];
			}
		}
		return null;
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static CJKVEra getEraFromShortDescription(String e) {
		for(int i = 0; i < ERAS.length; i++) {
			if(ERAS[i].getShortDescription().equals(e)) {
				return ERAS[i];
			}
		}
		return null;
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static CJKVEra getEraFromInitialLetter(String e) {
		for(int i = 0; i < ERAS.length; i++) {
			if(ERAS[i].getInitialLetter().equals(e)) {
				return ERAS[i];
			}
		}
		return null;
	}

	/**
	 * 
	 * @param d
	 * @return
	 */
	public static CJKVEra getAncientEra(java.util.Date d) {
		if(MEIJI.before(d)) {
			return getEra(d);
		} else {
			for(int i = 0; i < ANCIENT_ERAS.length; i++) {
				if(ANCIENT_ERAS[i].isEra(d)) {
					return ANCIENT_ERAS[i];
				}
			}
			return null;
		}
	}

}
