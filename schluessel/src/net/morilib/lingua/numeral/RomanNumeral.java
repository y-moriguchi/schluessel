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
 * @author MORIGUCHI, Yuichiro 2012/05/09
 */
public final class RomanNumeral {

	//
	private RomanNumeral() {}

	//
	static String[] MILLE = new String[] {
		"", "M", "MM", "MMM"
	};

	//
	static String[] CENT = new String[] {
		"", "C", "CC", "CCC", "CD", "D", "DC", "DCC", "DCCC", "CM"
	};

	//
	static String[] DECA = new String[] {
		"", "X", "XX", "XXX", "XL", "L", "LX", "LXX", "LXXX", "XC"
	};

	//
	static String[] UNUS = new String[] {
		"", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"
	};

	//
	private static final BigInteger X = BigInteger.valueOf(10);
	private static final BigInteger L = BigInteger.valueOf(50);
	private static final BigInteger C = BigInteger.valueOf(100);
	private static final BigInteger D = BigInteger.valueOf(500);

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static String toRoman(int n) {
		int m, c, d, u;

		if(n <= 0 || n >= 4000)  return null;
		m = n / 1000;
		c = n / 100 % 10;
		d = n / 10  % 10;
		u = n % 10;
		return MILLE[m] + CENT[c] + DECA[d] + UNUS[u];
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static int parse(String roman) {
		int r = 0, w, m, c, d, u;
		String s = roman.toUpperCase();

		for(int p = 0;; p++) {
			w = p < s.length() ? s.charAt(p) : -1;
			if((u = r % 10) == 1) {
				switch(w) {
				case 'I':  r += 1;  break;
				case 'V':  r += 3;  break;
				case 'X':  r += 8;  break;
				case -1:   return r;
				default:   return -1;
				}
			} else if(u == 2 || u == 5 || u == 6 || u == 7) {
				switch(w) {
				case 'I':  r += 1;  break;
				case -1:   return r;
				default:   return -1;
				}
			} else if(u == 3 || u == 4 || u == 8 || u == 9) {
				switch(w) {
				case -1:   return r;
				default:   return -1;
				}
			} else if((d = (r / 10) % 10) == 1) {
				switch(w) {
				case 'I':  r += 1;   break;
				case 'V':  r += 5;   break;
				case 'X':  r += 10;  break;
				case 'L':  r += 30;  break;
				case 'C':  r += 80;  break;
				case -1:   return r;
				default:   return -1;
				}
			} else if(d == 2 || d == 5 || d == 6 || d == 7) {
				switch(w) {
				case 'I':  r += 1;   break;
				case 'V':  r += 5;   break;
				case 'X':  r += 10;  break;
				case -1:   return r;
				default:   return -1;
				}
			} else if(d == 3 || d == 4 || d == 8 || d == 9) {
				switch(w) {
				case 'I':  r += 1;     break;
				case 'V':  r += 5;     break;
				case -1:   return r;
				default:   return -1;
				}
			} else if((c = r / 100 % 10) == 1) {
				switch(w) {
				case 'I':  r += 1;    break;
				case 'V':  r += 5;    break;
				case 'X':  r += 10;   break;
				case 'L':  r += 50;   break;
				case 'C':  r += 100;  break;
				case 'D':  r += 300;  break;
				case 'M':  r += 800;  break;
				case -1:   return r;
				default:   return -1;
				}
			} else if(c == 2 || c == 5 || c == 6 || c == 7) {
				switch(w) {
				case 'I':  r += 1;    break;
				case 'V':  r += 5;    break;
				case 'X':  r += 10;   break;
				case 'L':  r += 50;   break;
				case 'C':  r += 100;  break;
				case -1:   return r;
				default:   return -1;
				}
			} else if(c == 3 || c == 4 || c == 8 || c == 9) {
				switch(w) {
				case 'I':  r += 1;     break;
				case 'V':  r += 5;     break;
				case 'X':  r += 10;    break;
				case 'L':  r += 50;    break;
				case -1:   return r;
				default:   return -1;
				}
			} else if(r == 0 || (m = r / 1000) == 1 || m == 2) {
				switch(w) {
				case 'I':  r += 1;     break;
				case 'V':  r += 5;     break;
				case 'X':  r += 10;    break;
				case 'L':  r += 50;    break;
				case 'C':  r += 100;   break;
				case 'D':  r += 500;   break;
				case 'M':  r += 1000;  break;
				case -1:   return (r > 0) ? r : -1;
				default:   return -1;
				}
			} else if(m == 3) {
				switch(w) {
				case 'I':  r += 1;    break;
				case 'V':  r += 5;    break;
				case 'X':  r += 10;   break;
				case 'L':  r += 50;   break;
				case 'C':  r += 100;  break;
				case 'D':  r += 500;  break;
				case -1:   return r;
				default:   return -1;
				}
			}
		}
	}

	//
	static void putNum1(StringBuffer r, int n, int d) {
		for(int j = 0; j < n; j++) {
			if(d == 3) {
				r.append("C");
			} else {
				for(int i = 0; i < d - 3; i++)  r.append("C");
				r.append("I");
				for(int i = 0; i < d - 3; i++)  r.append("\u2183");
			}
		}
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static String toBigRoman(BigInteger n) {
		StringBuffer r = new StringBuffer();
		boolean a = true;
		String m;
		int d, q;

		if(n == null) {
			throw new NullPointerException();
		} else if(n.signum() <= 0) {
			throw new IllegalArgumentException();
		} else if(n.compareTo(C) < 0) {
			return toRoman(n.intValue());
		}

		q = n.remainder(C).intValue();
		m = n.toString();
		for(int i = 0; i + 2 < m.length(); i++) {
			d = m.charAt(i) - '0';
			if(d == 0) {
				a = true;
			} else if(d < 5) {
				putNum1(r, d, m.length() - i);
				a = false;
			} else if(d == 5) {
				if(a)  r.append("I");
				for(int j = 0; j < m.length() - i - 2; j++) {
					r.append("\u2183");
				}
				a = true;
			} else {
				if(a)  r.append("I");
				for(int j = 0; j < m.length() - i - 2; j++) {
					r.append("\u2183");
				}
				putNum1(r, d - 5, m.length() - i);
				a = false;
			}
		}

		if(q > 0)  r.append(toRoman(q));
		return r.toString();
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static String toBigRoman(long n) {
		return toBigRoman(BigInteger.valueOf(n));
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static BigInteger parseBigRoman(String roman) {
		BigInteger rr = BigInteger.ZERO;
		int stat = 1000, r = 0, r1 = 0, r2 = 0, r3 = -1, r4 = 0;
		int w, d, u;
		String s;

		s = roman.toUpperCase();
		s = s.replace('\u216D', 'C');
		s = s.replaceAll("\u2180", "CI\u2183");
		s = s.replaceAll("\u2181", "I\u2183\u2183");
		s = s.replaceAll("\u2182", "CCI\u2183\u2183");
		for(int p = 0;; p++) {
			w = p < s.length() ? s.charAt(p) : -1;
			switch(stat) {
			case 1000:
				switch(w) {
				case 'I':  stat = 1010;  break;
				case 'C':  stat = 1020;  r1 = 2;  break;
				case 'X':  stat = 2000;  r = 10;  break;
				case 'L':  stat = 2000;  r = 50;  break;
				case 'V':  stat = 2000;  r = 5;  break;
				default:  throw new NumberFormatException();
				}
				break;
			case 1010:
				switch(w) {
				case 'I':       stat = 2000;  r = 2;  break;
				case 'V':       stat = 2000;  r = 4;  break;
				case 'X':       stat = 2000;  r = 9;  break;
				case '\u2183':  stat = 1030;  r2 = 1;  break;
				case -1:        return BigInteger.ONE;
				default:  throw new NumberFormatException();
				}
				break;
			case 1020:
				switch(w) {
				case 'C':  r1 += 2;  break;
				case 'I':  stat = 1040;  break;
				case 'X':  stat = 2000;  r = 50 * r1 + 10;  break;
				case 'L':  stat = 2000;  r = 50 * r1 + 50;  break;
				case 'V':  stat = 2000;  r = 50 * r1 + 5;  break;
				case -1:
					if(r1 > 8)  throw new NumberFormatException();
					return rr.add(BigInteger.valueOf(50 * r1));
				default:  throw new NumberFormatException();
				}
				break;
			case 1030:
				if(w == '\u2183') {
					r2 += 2;
				} else {
					if(r3 >= 0 && r2 >= r3) {
						throw new NumberFormatException();
					}
					rr = rr.add(D.multiply(X.pow((r3 = r2) / 2)));
					r2 = 0;
					switch(w) {
					case 'I':  stat = 1010;  break;
					case 'C':  stat = 1020;  r1 = 2;  break;
					case 'X':  stat = 2000;  r = 10;  break;
					case 'L':  stat = 2000;  r = 50;  break;
					case 'V':  stat = 2000;  r = 5;  break;
					case -1:   return rr;
					default:  throw new NumberFormatException();
					}
				}
				break;
			case 1040:
				if(w == '\u2183') {
					r2 += 2;
				} else {
					if(r3 < 0 || r1 < r3) {
						r4 = 1;  r3 = r1;
					} else if(r1 > r3) {
						throw new NumberFormatException();
					} else if(r4++ >= 4) {
						throw new NumberFormatException();
					}

					if(r2 < r1) {
						throw new NumberFormatException();
					} else if(r2 == r1) {
						rr = rr.add(C.multiply(X.pow(r1 / 2)));
					} else if(r2 <= r1 * 2) {
						rr = rr.add(C.multiply(X.pow(r1 / 2)));
						rr = rr.add(L.multiply(X.pow((r2 - r1) / 2)));
					} else {
						throw new NumberFormatException();
					}

					r2 = 0;
					switch(w) {
					case 'I':  stat = 1010;  break;
					case 'C':  stat = 1020;  r1 = 2;  break;
					case 'X':  stat = 2000;  r = 10;  break;
					case 'L':  stat = 2000;  r = 50;  break;
					case 'V':  stat = 2000;  r = 5;  break;
					case -1:   return rr;
					default:  throw new NumberFormatException();
					}
				}
				break;
			case 2000:
				if((u = r % 10) == 1) {
					switch(w) {
					case 'I':  r += 1;  break;
					case 'V':  r += 3;  break;
					case 'X':  r += 8;  break;
					case -1:   return rr.add(BigInteger.valueOf(r));
					default:   throw new NumberFormatException();
					}
				} else if(u == 2 || u == 5 || u == 6 || u == 7) {
					switch(w) {
					case 'I':  r += 1;  break;
					case -1:   return rr.add(BigInteger.valueOf(r));
					default:   throw new NumberFormatException();
					}
				} else if(u == 3 || u == 4 || u == 8 || u == 9) {
					switch(w) {
					case -1:   return rr.add(BigInteger.valueOf(r));
					default:   throw new NumberFormatException();
					}
				} else if((d = (r / 10) % 10) == 1) {
					switch(w) {
					case 'I':  r += 1;   break;
					case 'V':  r += 5;   break;
					case 'X':  r += 10;  break;
					case 'L':  r += 30;  break;
					case 'C':  r += 80;  break;
					case -1:   return rr.add(BigInteger.valueOf(r));
					default:   throw new NumberFormatException();
					}
				} else if(d == 2 || d == 5 || d == 6 || d == 7) {
					switch(w) {
					case 'I':  r += 1;   break;
					case 'V':  r += 5;   break;
					case 'X':  r += 10;  break;
					case -1:   return rr.add(BigInteger.valueOf(r));
					default:   throw new NumberFormatException();
					}
				} else if(d == 3 || d == 4 || d == 8 || d == 9) {
					switch(w) {
					case 'I':  r += 1;     break;
					case 'V':  r += 5;     break;
					case -1:   return rr.add(BigInteger.valueOf(r));
					default:   throw new NumberFormatException();
					}
				}
				break;
			default:  throw new RuntimeException();
			}
		}
	}

}
