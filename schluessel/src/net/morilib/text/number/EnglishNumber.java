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
import java.util.HashMap;
import java.util.Map;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/16
 */
public final class EnglishNumber {

	//
	private static final String[] NUMERAL1 = new String[] {
		"zero", "one", "two",   "three", "four",
		"five", "six", "seven", "eight", "nine"
	};

	//
	private static final String[] NUMERALTEEN = new String[] {
		"ten",     "eleven",  "twelve",    "thirteen", "fourteen",
		"fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
	};

	//
	private static final String[] NUMERAL10 = new String[] {
		null,    "ten",   "twenty",  "thirty", "forty",
		"fifty", "sixty", "seventy", "eighty", "ninety"
	};

	//
	private static final String[] NUMERAL1000 = new String[] {
		"",          " thousand",    " million",     " billion",
		" trillion", " quadrillion", " quintillion", " sextillion",
		" septillion"
	};

	//
	static final Map<String, BigInteger> DICTIONARY9;
	static final Map<String, BigInteger> DICTIONARY99;
	static final Map<String, BigInteger> DICTIONARY1000;
	static final BigInteger HUNDRED = BigInteger.valueOf(100);

	//
	static {
		DICTIONARY9    = init9();
		DICTIONARY99   = init99();
		DICTIONARY1000 = init1000();
	}

	//
	static void putnum(Map<String, BigInteger> m, String s, int i) {
		m.put(s, BigInteger.valueOf(i));
	}

	//
	static Map<String, BigInteger> init9() {
		Map<String, BigInteger> m;

		m = new HashMap<String, BigInteger>();
		putnum(m, "one",   1);  putnum(m, "two",   2);
		putnum(m, "three", 3);  putnum(m, "four",  4);
		putnum(m, "five",  5);  putnum(m, "six",   6);
		putnum(m, "seven", 7);  putnum(m, "eight", 8);
		putnum(m, "nine",  9);
		return m;
	}

	//
	static Map<String, BigInteger> init99() {
		Map<String, BigInteger> m, n, r;

		m = init9();
		n = new HashMap<String, BigInteger>();
		r = new HashMap<String, BigInteger>();
		putnum(n, "twenty", 20);  putnum(n, "thirty",  30);
		putnum(n, "forty",  40);  putnum(n, "fifty",   50);
		putnum(n, "sixty",  60);  putnum(n, "seventy", 70);
		putnum(n, "eighty", 80);  putnum(n, "ninety",  90);

		for(Map.Entry<String, BigInteger> em : m.entrySet()) {
			for(Map.Entry<String, BigInteger> en : n.entrySet()) {
				r.put(en.getKey() + "-" + em.getKey(),
						en.getValue().add(em.getValue()));
			}
		}
		r.putAll(m);  r.putAll(n);
		putnum(r, "ten",      10);  putnum(r, "eleven",    11);
		putnum(r, "twelve",   12);  putnum(r, "thirteen",  13);
		putnum(r, "fourteen", 14);  putnum(r, "fifteen",   15);
		putnum(r, "sixteen",  16);  putnum(r, "seventeen", 17);
		putnum(r, "eighteen", 18);  putnum(r, "nineteen",  19);
		return r;
	}

	//
	static Map<String, BigInteger> init1000() {
		Map<String, BigInteger> m;

		m = new HashMap<String, BigInteger>();
		m.put("thousand",    new BigInteger("1000"));
		m.put("million",     new BigInteger("1000000"));
		m.put("billion",     new BigInteger("1000000000"));
		m.put("trillion",    new BigInteger("1000000000000"));
		m.put("quadrillion", new BigInteger("1000000000000000"));
		m.put("quintillion", new BigInteger("1000000000000000000"));
		m.put("sextillion",  new BigInteger("1000000000000000000000"));
		m.put("septillion",
				new BigInteger("1000000000000000000000000"));
		return m;
	}

	//
	static int toDigit(char c) {
		if(c < '0' || c > '9') {
			throw new NumberFormatException();
		}
		return c - '0';
	}

	//
	static boolean describeBelowThousand(StringBuilder buf,
			String s, int b) {
		int c1 = toDigit(s.charAt(b));
		int c2 = toDigit(s.charAt(b + 1));
		int c3 = toDigit(s.charAt(b + 2));
		String dlm = "";

		if(c1 == 0 && c2 == 0 && c3 == 0) {
			return false;
		} else {
			if(c1 > 0) {
				buf.append(NUMERAL1[c1]).append(" hundred");
				dlm = " ";
			}

			if(c2 >= 2) {
				buf.append(dlm).append(NUMERAL10[c2]);
				if(c3 > 0)  buf.append("-").append(NUMERAL1[c3]);
			} else if(c2 == 1) {
				buf.append(dlm).append(NUMERALTEEN[c3]);
			} else {
				if(c3 > 0)  buf.append(dlm).append(NUMERAL1[c3]);
			}
			return true;
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static String toString(BigInteger n) {
		StringBuilder buf = new StringBuilder();
		String s0, dlm = "";

		if(n.signum() < 0) {
			buf.append("minus ");
			s0 = n.negate().toString();
		} else if(n.signum() == 0) {
			return "zero";
		} else {
			s0 = n.toString();
		}

		switch(s0.length() % 3) {
		case 1:  s0 = "00" + s0;  break;
		case 2:  s0 = "0"  + s0;  break;
		}

		for(int i = 0; i < s0.length(); i += 3) {
			buf.append(dlm);
			if(describeBelowThousand(buf, s0, i)) {
				buf.append(NUMERAL1000[(s0.length() - i) / 3 - 1]);
				dlm = " ";
			}
		}
		return buf.toString();
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static String toString(long n) {
		return toString(BigInteger.valueOf(n));
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static BigInteger parseNumber(String s) {
		String[] sp;
		BigInteger bf = null, bi = null, bj, br = BigInteger.ZERO;
		int stat = 100;

		if(s == null) {
			throw new NullPointerException();
		} else if((sp = s.split("[ \t]+")).length == 0) {
			throw new NumberFormatException();
		} else if(s.equals("zero")) {
			return BigInteger.ZERO;
		}

		for(int i = 0; true; i++) {
			switch(stat) {
			case 100:
				if(i == sp.length) {
					return br;
				} else if((bi = DICTIONARY9.get(sp[i])) != null) {
					stat = 111;
				} else if((bi = DICTIONARY99.get(sp[i])) != null) {
					stat = 131;
				} else {
					throw new NumberFormatException();
				}
				break;
			case 111:
				if(i == sp.length) {
					return br.add(bi);
				} else if(sp[i].equals("hundred")) {
					bi = bi.multiply(HUNDRED);
					stat = 121;
				} else if((bj = DICTIONARY1000.get(sp[i])) != null) {
					if(bf != null && bf.compareTo(bj) < 0) {
						throw new NumberFormatException();
					}
					br = br.add(bi.multiply(bj));
					bf = bj;
					stat = 100;
				} else {
					throw new NumberFormatException();
				}
				break;
			case 121:
				if(i == sp.length) {
					return br.add(bi);
				} else if((bj = DICTIONARY99.get(sp[i])) != null) {
					bi = bi.add(bj);
					stat = 131;
				} else if((bj = DICTIONARY1000.get(sp[i])) != null) {
					if(bf != null && bf.compareTo(bj) < 0) {
						throw new NumberFormatException();
					}
					br = br.add(bi.multiply(bj));
					bf = bj;
					stat = 100;
				} else {
					throw new NumberFormatException();
				}
				break;
			case 131:
				if(i == sp.length) {
					return br.add(bi);
				} else if((bj = DICTIONARY1000.get(sp[i])) != null) {
					if(bf != null && bf.compareTo(bj) < 0) {
						throw new NumberFormatException();
					}
					br = br.add(bi.multiply(bj));
					bf = bj;
					stat = 100;
				} else {
					throw new NumberFormatException();
				}
				break;
			}
		}
	}

}
