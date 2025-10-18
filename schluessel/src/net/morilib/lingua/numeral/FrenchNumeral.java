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
import java.util.HashMap;
import java.util.Map;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/05/22
 */
public final class FrenchNumeral {

	//
	private FrenchNumeral() {}

	//
	private static String[] NUMERAL20 = new String[] {
		"zéro",   "un",    "deux",     "trois",    "quatre",
		"cinq",   "six",   "sept",     "huit",     "neuf",
		"dix",    "onze",  "douze",    "treize",   "quatorze",
		"quinze", "seize", "dix-sept", "dix-huit", "dix-neuf"
	};

	//
	private static String[] NUMERAL60 = new String[] {
		"", "", "vingt", "trente", "quarante", "cinquante", "soixante"
	};

	//
	private static String[] NUMERAL1000 = new String[] {
		"", "mille",
		"million", "milliard",
		"billion", "billiard",
		"trillion", "trilliard",
		"quadrillion", "quadrilliard",
		"quintillion", "quintilliard",
		"sextillion", "sextilliard",
		"septillion", "septilliard",
		"octillion", "octilliard",
		"nonillion", "nonilliard",
		"décillion", "décilliard",
	};

	//
	static final BigInteger MILLE = BigInteger.valueOf(1000);
	static final BigInteger CENT  = BigInteger.valueOf(100);

	//
	static final Map<String, BigInteger> DICTIONARY9;
	static final Map<String, BigInteger> DICTIONARY99;
	static final Map<String, BigInteger> DICTIONARY1000;

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
		for(int i = 1; i < 10; i++) {
			putnum(m, NUMERAL20[i], i);
		}
		return m;
	}

	//
	static Map<String, BigInteger> init99() {
		Map<String, BigInteger> m, n, r;

		m = init9();

		// 20, 30, 40, 50, 60
		n = new HashMap<String, BigInteger>();
		for(int i = 2; i < NUMERAL60.length; i++) {
			putnum(n, NUMERAL60[i], i * 10);
		}

		// 20-69
		r = new HashMap<String, BigInteger>();
		for(Map.Entry<String, BigInteger> em : m.entrySet()) {
			for(Map.Entry<String, BigInteger> en : n.entrySet()) {
				if(!em.getValue().equals(BigInteger.ONE)) {
					// except like "vingt et un"
					r.put(en.getKey() + "-" + em.getKey(),
							en.getValue().add(em.getValue()));
				}
			}
		}
		r.putAll(m);  r.putAll(n);

		// 70, 72-79
		putnum(r, "soixante-dix", 70);
		for(int i = 12; i < 20; i++) {
			putnum(r, "soixante-" + NUMERAL20[i], i + 60);
		}

		// 80-99
		putnum(r, "quatre-vingts", 80);
		for(int i = 1; i < 20; i++) {
			putnum(r, "quatre-vingt-" + NUMERAL20[i], i + 80);
		}

		// 10-19
		for(int i = 10; i < 20; i++) {
			putnum(r, NUMERAL20[i], i);
		}
		return r;
	}

	//
	static Map<String, BigInteger> init1000() {
		Map<String, BigInteger> m;
		BigInteger j = BigInteger.ONE;

		m = new HashMap<String, BigInteger>();
		for(int i = 1; i < NUMERAL1000.length; i++) {
			j = j.multiply(MILLE);
			m.put(NUMERAL1000[i], j);
		}
		return m;
	}

	//
	static String describe100(int n) {
		String s;

		if(n == 0) {
			return "";
		} else if(n < 20) {
			return NUMERAL20[n];
		} else if(n < 70) {
			s = NUMERAL60[n / 10];
			if(n % 10 == 0) {
				return s;
			} else if(n % 10 == 1) {
				return s + " et un";
			} else {
				return s + "-" + NUMERAL20[n % 10];
			}
		} else if(n == 71) {
			return "soixante et onze";
		} else if(n < 80) {
			return "soixante-" + NUMERAL20[n - 60];
		} else if(n == 80) {
			return "quatre-vingts";
		} else if(n < 100) {
			return "quatre-vingt-" + NUMERAL20[n - 80];
		} else {
			throw new RuntimeException();
		}
	}

	//
	static String describe1000(int n) {
		if(n < 100) {
			return describe100(n);
		} else if(n == 100) {
			return "cent";
		} else if(n < 200) {
			return "cent " + describe100(n % 100);
		} else if(n % 100 == 0) {
			return NUMERAL20[n / 100] + " cents";
		} else {
			return NUMERAL20[n / 100] + " cent " + describe100(n % 100);
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static String describeNumber(BigInteger n) {
		StringBuffer r = new StringBuffer();
		BigInteger[] b;
		BigInteger x = n;
		int[] ms = new int[NUMERAL1000.length];
		int i = 0;

		if(n.signum() < 0) {
			return null;
		} else if(n.signum() == 0) {
			return "zéro";
		}

		do {
			b = x.divideAndRemainder(MILLE);
			ms[i++] = b[1].intValue();
		} while((x = b[0]).signum() != 0);

		for(int j = i - 1; j >= 0; j--) {
			if(j >= NUMERAL1000.length)  return null;
			if(j < i - 1)  r.append(" ");
			if(j != 1 || ms[j] > 1) {
				r.append(describe1000(ms[j]));
				if(j > 0)  r.append(" ");
			}
			if(j > 0)  r.append(NUMERAL1000[j]);
		}
		return r.toString();
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
	static BigInteger milleplural(String s) {
		String x;

		if(s.charAt(s.length() - 1) == 's') {
			x = s.substring(0, s.length() - 1);
			return DICTIONARY1000.get(x);
		} else {
			return null;
		}
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
		} else if((sp = s.split("[ \t\n]+")).length == 0) {
			throw new NumberFormatException();
		} else if(s.equals("zéro")) {
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
					stat = 113;
				} else if(sp[i].equals("cent")) {
					bi = CENT;
					stat = 121;
				} else if(sp[i].equals("mille")) {
					br = br.add(MILLE);
					bf = MILLE;
				} else {
					throw new NumberFormatException();
				}
				break;
			case 111:
				if(i == sp.length) {
					return br.add(bi);
				} else if(sp[i].equals("cents")) {
					if(bi.equals(BigInteger.ONE)) {
						// "un cents" is invalid
						throw new NumberFormatException();
					}
					bi = bi.multiply(CENT);
					stat = 131;
				} else if(sp[i].equals("cent")) {
					bi = bi.multiply(CENT);
					stat = 121;
				} else if(sp[i].equals("et")) {
					switch(bi.intValue()) {
					case 20: case 30: case 40: case 50: case 60:
						break;
					default:  throw new NumberFormatException();
					}
					stat = 112;
				} else {
					i--;  stat = 131;
				}
				break;
			case 112:
				if(i == sp.length) {
					throw new NumberFormatException();
				} else if(sp[i].equals("un")) {
					bi = bi.add(BigInteger.ONE);
					stat = 131;
				} else if(sp[i].equals("onze") &&
						bi.intValue() == 60) {
					bi = bi.add(BigInteger.valueOf(11));
					stat = 131;
				} else {
					throw new NumberFormatException();
				}
				break;
			case 113:
				if(i == sp.length) {
					i--;  stat = 131;
				} else if(sp[i].equals("et")) {
					switch(bi.intValue()) {
					case 20: case 30: case 40: case 50: case 60:
						break;
					default:  throw new NumberFormatException();
					}
					stat = 112;
				} else {
					i--;  stat = 131;
				}
				break;
			case 121:
				if(i == sp.length) {
					return br.add(bi);
				} else if((bj = DICTIONARY99.get(sp[i])) != null) {
					bi = bi.add(bj);
					stat = 131;
				} else if((bj = DICTIONARY1000.get(sp[i])) != null) {
					if(bf != null && bf.compareTo(bj) <= 0) {
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
				} else if(bi.intValue() == 1 &&
						(bj = DICTIONARY1000.get(sp[i])) != null) {
					if(bf != null && bf.compareTo(bj) <= 0) {
						throw new NumberFormatException();
					}
					br = br.add(bi.multiply(bj));
					bf = bj;
					stat = 100;
				} else if(sp[i].equals("mille")) {
					if(bf != null && bf.compareTo(MILLE) <= 0) {
						throw new NumberFormatException();
					}
					br = br.add(bi.multiply(MILLE));
					bf = MILLE;
					stat = 100;
				} else if(bi.intValue() > 1 &&
						(bj = milleplural(sp[i])) != null) {
					if(bf != null && bf.compareTo(bj) <= 0) {
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
