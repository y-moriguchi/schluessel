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
package net.morilib.lingua.numeral;

import java.math.BigInteger;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/05/24
 */
public class GermanNumeral {

	//
	private static final String[] NUMERAL1 = new String[] {
		null,   "ein",   "zwei",   "drei", "vier",
		"fünf", "sechs", "sieben", "acht", "neun"
	};

	//
	private static final String[] NUMERAL20 = new String[] {
		"zehn",     "elf",      "zwölf",    "dreizehn", "vierzehn",
		"fünfzehn", "sechzehn", "siebzehn", "achtzehn", "neunzehn"
	};

	//
	private static final String[] NUMERAL10 = new String[] {
		null,      "zehn",    "zwanzig", "dreißig", "vierzig",
		"fünfzig", "sechzig", "siebzig", "achtzig", "neunzig"
	};

	//
	private static final String[] NUMERAL1000 = new String[] {
		"",            "tausend",
		"Million",     "Milliarde",
		"Billion",     "Billiarde",
		"Trillion",    "Trilliarde",
		"Quadrillion", "Quadrilliarde"
	};

	//
	static final BigInteger TAUSEND = BigInteger.valueOf(1000);

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

		if(c1 == 0 && c2 == 0 && c3 == 0) {
			return false;
		} else {
			if(c1 > 0) {
				if(c1 > 1)  buf.append(NUMERAL1[c1]);
				buf.append("hundert");
			}

			if(c2 >= 2) {
				buf.append(NUMERAL1[c3]);
				if(c3 > 0)  buf.append("und").append(NUMERAL10[c2]);
			} else if(c2 == 1) {
				buf.append(NUMERAL20[c3]);
			} else {
				if(c3 > 0)  buf.append(NUMERAL1[c3]);
			}
			return true;
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static String describeNumber(BigInteger n) {
		StringBuilder buf = new StringBuilder();
		String s0;
		int l;

		if(n.signum() < 0) {
//			buf.append("minus ");
//			s0 = n.negate().toString();
			return null;
		} else if(n.signum() == 0) {
			return "null";
		} else if(n.equals(BigInteger.ONE)) {
			return "eins";
		} else {
			s0 = n.toString();
		}

		switch(s0.length() % 3) {
		case 1:  s0 = "00" + s0;  break;
		case 2:  s0 = "0"  + s0;  break;
		}

		for(int i = 0; i < s0.length(); i += 3) {
			if((l = (s0.length() - i) / 3 - 1) >= NUMERAL1000.length) {
				return null;
			} else if(l > 0 &&
					s0.charAt(i) == '0' &&
					s0.charAt(i + 1) == '0' &&
					s0.charAt(i + 2) == '1') {
				if(l == 1) {
					buf.append("tausend");
				} else {
					buf.append("eine ").append(NUMERAL1000[l]);
				}
			} else {
				if(l > 0 && buf.length() > 0)  buf.append(" ");
				if(describeBelowThousand(buf, s0, i)) {
					if(l > 1) {
						buf.append(" ");
						buf.append(NUMERAL1000[l]).append("en");
					} else {
						buf.append(NUMERAL1000[l]);
					}
				}
			}
		}
		return buf.toString();
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static String describeNumber(long n) {
		return describeNumber(BigInteger.valueOf(n));
	}

	//
	private static boolean match(String s, String d, int p) {
		if(s.length() - p < d.length()) {
			return false;
		} else {
			for(int i = 0; i < d.length(); i++) {
				if(s.charAt(i + p) != d.charAt(i))  return false;
			}
			return true;
		}
	}

	//
	private static BigInteger matchMillion(String s) {
		BigInteger r = BigInteger.ONE;

		for(int i = 2; i < NUMERAL1000.length; i++) {
			if(s.equals(NUMERAL1000[i])) {
				for(int j = 0; j < i; j++) {
					r = r.multiply(TAUSEND);
				}
				return r;
			}
		}
		return null;
	}

	//
	private static BigInteger matchMillionPlural(String s) {
		BigInteger r = BigInteger.ONE;
		String t;

		for(int i = 2; i < NUMERAL1000.length; i++) {
			t = NUMERAL1000[i];
			if(((i & 1) == 0 && s.equals(t + "en")) ||
					(((i & 1) != 0 && s.equals(t + "n")))) {
				for(int j = 0; j < i; j++) {
					r = r.multiply(TAUSEND);
				}
				return r;
			}
		}
		return null;
	}

	//
	private static boolean isEnd(String s, int p) {
		return (p + 1 >= s.length() ||
				Character.isWhitespace(s.charAt(p + 1)));
	}

	//
	static int parse99(String s, int[] p) {
		int r = 0;

		for(int i = 0; i < 10; i++) {
			if(match(s, NUMERAL20[i], p[0])) {
				p[0] += NUMERAL20[i].length();
				return i + 10;
			}
		}

		for(int i = 1; i < 10; i++) {
			if(match(s, NUMERAL1[i], p[0])) {
				r = i;
				p[0] += NUMERAL1[i].length();
				if(p[0] >= s.length()) {
					return r;
				} else if(!match(s, "und", p[0])) {
					throw new NumberFormatException();
				} else {
					p[0] += 3;
				}
				break;
			}
		}

		for(int i = 2; i < 10; i++) {
			if(match(s, NUMERAL10[i], p[0])) {
				p[0] += NUMERAL10[i].length();
				return r + i * 10;
			}
		}
		throw new NumberFormatException();
//		return r;
	}

	//
	static int parse999(String s, int[] p) {
		int r = 0;

		if(match(s, "hundert", p[0])) {
			p[0] += 7;
			if(isEnd(s, p[0])) {
				return 100;
			} else {
				return 100 + parse99(s, p);
			}
		}

		for(int i = 2; i < 10; i++) {
			if(match(s, NUMERAL1[i], p[0])) {
				r = i;
				p[0] += NUMERAL1[i].length();
				if(isEnd(s, p[0])) {
					p[0] -= NUMERAL1[i].length();
					break;
				} else if(match(s, "hundert", p[0])) {
					p[0] += 7;
					if(isEnd(s, p[0])) {
						return r * 100;
					} else {
						return r * 100 + parse99(s, p);
					}
				} else {
					p[0] -= NUMERAL1[i].length();
					break;
				}
			}
		}
		return parse99(s, p);
	}

	//
	static int parse999999(String s, int[] p) {
		int r;

		if(match(s, "tausend", p[0])) {
			p[0] += 7;
			return 1000 + parse999(s, p);
		} else {
			r = parse999(s, p);
			if(isEnd(s, p[0])) {
				return r;
			} else if(r > 1 && match(s, "tausend", p[0])) {
				p[0] += 7;
				if(isEnd(s, p[0])) {
					return r * 1000;
				} else {
					return r * 1000 + parse999(s, p);
				}
			} else {
				throw new NumberFormatException();
			}
		}
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static BigInteger parseNumber(String s) {
		BigInteger r = BigInteger.ZERO, t, b = null;
		String[] sp;
		int[] p = new int[1];
		int v;

		if(s == null) {
			throw new NullPointerException();
		} else if((sp = s.split("[ \t\n]+")).length == 0) {
			throw new NumberFormatException();
		} else if(s.equals("null")) {
			return BigInteger.ZERO;
		} else if(s.equals("eins")) {
			return BigInteger.ONE;
		}

		for(int i = 0;;) {
			if(i >= sp.length) {
				if(i == 0)  throw new NumberFormatException();
				return r;
			} else if(sp[i].equals("eine")) {
				if(i + 1 >= sp.length) {
					throw new NumberFormatException();
				} else if((t = matchMillion(sp[i + 1])) != null) {
					if(b != null && b.compareTo(t) <= 0) {
						throw new NumberFormatException();
					}
					b = t;
					r = r.add(t);
					i += 2;
				} else {
					throw new NumberFormatException();
				}
			} else if(i + 1 >= sp.length) {
				p[0] = 0;
				r = r.add(BigInteger.valueOf(parse999999(sp[i], p)));
				return r;
			} else if((t = matchMillionPlural(sp[i + 1])) != null) {
				if(b != null && b.compareTo(t) <= 0) {
					throw new NumberFormatException();
				}
				b = t;
				p[0] = 0;
				v = parse999(sp[i], p);
				if(v == 1) {
					// such as "ein Million" is invalid
					throw new NumberFormatException();
				}
				r = r.add(t.multiply(BigInteger.valueOf(v)));
				i += 2;
			} else {
				throw new NumberFormatException();
			}
		}
	}

}
