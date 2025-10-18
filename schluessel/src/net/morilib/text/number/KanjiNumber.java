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
package net.morilib.text.number;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/16
 */
public final class KanjiNumber {

	//
	private KanjiNumber() {}

	//
	private static final String PTN_KANS =
		"([0-9]*)" +
		"(([0-9]{1,4})\u7121\u91CF\u5927\u6570)?" +
		"(([0-9]{1,4})\u4E0D\u53EF\u601D\u8B70)?" +
		"(([0-9]{1,4})\u90A3\u7531\u4ED6)?" +
		"(([0-9]{1,4})\u963F\u50E7\u7947)?" +
		"(([0-9]{1,4})\u6052\u6CB3\u6C99)?" +
		"(([0-9]{1,4})\u6975)?" +
		"(([0-9]{1,4})\u8F09)?" +
		"(([0-9]{1,4})\u6B63)?" +
		"(([0-9]{1,4})\u6F97)?" +
		"(([0-9]{1,4})\u6E9D)?" +
		"(([0-9]{1,4})\u7A63)?" +
		"(([0-9]{1,4})[\u30B7\u79ED])?" +
		"(([0-9]{1,4})\u5793)?" +
		"(([0-9]{1,4})\u4EAC)?" +
		"(([0-9]{1,4})\u5146)?" +
		"(([0-9]{1,4})\u5104)?" +
		"(([0-9]{1,4})\u4E07)?" +
		"([0-9]{1,4})?";
	private static final Pattern PTN_KAN = Pattern.compile(PTN_KANS);
	private static final int MAX_KETA = 71;
	private static String[] SU = new String[] {
		"\u7121\u91CF\u5927\u6570", "\u4E0D\u53EF\u601D\u8B70",
		"\u90A3\u7531\u4ED6", "\u963F\u50E7\u7947",
		"\u6052\u6CB3\u6C99", "\u6975", "\u8F09", "\u6B63",
		"\u6F97", "\u6E9D", "\u7A63", "\u79ED", "\u5793", "\u4EAC",
		"\u5146", "\u5104", "\u4E07", "",
	};
	private static final BigInteger ICHIMAN =
		BigInteger.valueOf(10000);

	//
	private static void copystr(String s, char[] cs, int off) {
		if(s != null) {
			System.arraycopy(s.toCharArray(), 0, cs,
					MAX_KETA - off - s.length(),
					s.length());
		}
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static BigInteger parseNumber(String s) {
		Matcher mt;
		String ss;
		char[] cs = new char[71];

		if(s == null) {
			throw new NullPointerException();
		} else if(s.equals("")) {
			throw new NumberFormatException(s);
		} else if(!(mt = PTN_KAN.matcher(s)).matches()) {
			throw new NumberFormatException(s);
		}
		Arrays.fill(cs, '0');
		copystr(mt.group( 3), cs, 68);  // muryotaisu
		copystr(mt.group( 5), cs, 64);  // fukashigi
		copystr(mt.group( 7), cs, 60);  // nayuta
		copystr(mt.group( 9), cs, 56);  // asogi
		copystr(mt.group(11), cs, 52);  // gogasha
		copystr(mt.group(13), cs, 48);  // goku
		copystr(mt.group(15), cs, 44);  // sai
		copystr(mt.group(17), cs, 40);  // sei
		copystr(mt.group(19), cs, 36);  // kan
		copystr(mt.group(21), cs, 32);  // ko
		copystr(mt.group(23), cs, 28);  // jo
		copystr(mt.group(25), cs, 24);  // shi
		copystr(mt.group(27), cs, 20);  // gai
		copystr(mt.group(29), cs, 16);  // kei
		copystr(mt.group(31), cs, 12);  // cho
		copystr(mt.group(33), cs,  8);  // oku
		copystr(mt.group(35), cs,  4);  // man
		copystr(mt.group(36), cs,  0);  // ichi
		ss = new String(cs);
		ss = ss.replaceFirst("^0+", "");
		return new BigInteger(mt.group(1) + ss);
	}

	/**
	 * 
	 * @param b
	 * @return
	 */
	public static String toString(BigInteger b) {
		BigInteger[] d;
		int i = SU.length - 1;
		String r = "", sgn = "";

		if(b.signum() == 0) {
			return "0";
		} else if(b.signum() < 0) {
			sgn = "-";
		}
		d = new BigInteger[] { b.abs() };
		for(; d[0].signum() > 0 && i > 0; i--) {
			d = d[0].divideAndRemainder(ICHIMAN);
			if(d[1].signum() > 0) {
				r = d[1] + SU[i] + r;
			}
		}
		return sgn + ((i > 0) ? "" : d[0] + SU[i]) + r;
	}

}
