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
package net.morilib.phys.unit;

import java.util.HashMap;
import java.util.Map;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/05
 */
public final class Units {

	//
	private static final String PREFIXIES = "YZEPTGMkhcmuμnpfazy";

	//
	private Units() {}

	//
	static void multiply(Map<String, Integer> r,
			Map<String, Integer> s) {
		int i;

		for(String x : s.keySet()) {
			i = r.containsKey(x) ? r.get(x) + s.get(x) : s.get(x);
			if(i == 0) {
				r.remove(x);
			} else {
				r.put(x, i);
			}
		}
	}

	//
	static void divide(Map<String, Integer> r,
			Map<String, Integer> s) {
		int i;

		for(String x : s.keySet()) {
			i = r.containsKey(x) ? r.get(x) - s.get(x) : -s.get(x);
			if(i == 0) {
				r.remove(x);
			} else {
				r.put(x, i);
			}
		}
	}

	//
	static void pow(Map<String, Integer> r, int n) {
		for(String x : r.keySet()) {
			r.put(x, r.get(x) * n);
		}
	}

	/**
	 * 
	 * @param sys
	 * @param s
	 * @return
	 */
	public static Quantity parse(UnitSystem sys, String s) {
		Map<String, Integer> r = new HashMap<String, Integer>();
		Quantity q;
		String sx = null;
		double qty = 1.0;
		char c;
		int state = 100, p = 0, inv = 1, bx1 = 0, n;

		while(true) {
			c = p < s.length() ? s.charAt(p++) : 0;
			switch(state) {
			case 100:
				if(c == '/') {
					inv = -inv;
					state = 1001;
				} else if(c == 0) {
					return new Quantity(qty, new Unit(r));
				} else {
					bx1 = p - 1;
					state = 101;
				}
				break;
			case 1001:
				if(c == '/' || c == '*' || c == '^') {
					return null;
				}
				state = 101;
				break;
			case 101:
				if(c == '/' || c == '*' || c == 0) {
					sx = s.substring(bx1, p - ((c > 0) ? 1 : 0));
					if((q = getSIprefix(sys, sx)) == null) {
						return null;
					}
					qty *= q.getValue();
					if(inv > 0) {
						multiply(r, q.getUnit().getIndices());
					} else {
						divide(r, q.getUnit().getIndices());
					}
				}

				if(c == '/') {
					inv = -inv;
					bx1 = p;  state = 1001;
				} else if(c == '*') {
					bx1 = p;  state = 1001;
				} else if(c == 0) {
					return new Quantity(qty, new Unit(r));
				} else if(c == '^') {
					sx = s.substring(bx1, p - 1);
					bx1 = p;
					state = 102;
				}
				break;
			case 102:
				if(c == '-') {
					state = 103;
				} else if(c < '0' || c > '9') {
					return null;
				} else {
					state = 104;
				}
				break;
			case 103:
				if(c < '0' || c > '9')  return null;
				state = 104;
				break;
			case 104:
				if(c < '0' || c > '9') {
					n = Integer.parseInt(s.substring(
							bx1, p - ((c > 0) ? 1 : 0)));
					if((q = getSIprefix(sys, sx)) == null) {
						return null;
					}
					q = q.pow(n);
					multiply(r, q.getUnit().getIndices());
					qty *= q.getValue();
				}

				if(c == 0) {
					return new Quantity(qty, new Unit(r));
				} else if(c >= '0' && c <= '9') {
					// stay
				} else if(c == '*') {
					bx1 = p;  state = 1001;
				} else if(c == '/') {
					inv = -inv;
					bx1 = p;  state = 1001;
				}
				break;
			}
		}
	}

	/**
	 * 
	 * @param sys
	 * @param u
	 * @return
	 */
	public static Quantity getSIprefix(UnitSystem sys, String u) {
		Quantity q;
		double x;

		if(u == null) {
			throw new NullPointerException();
		} else if(u.length() == 0) {
			throw new IllegalArgumentException();
		} else if(u.length() == 1) {
			return sys.getQuantity(u);
		} else if(PREFIXIES.indexOf(u.charAt(0)) < 0) {
			return sys.getQuantity(u);
		} else if((q = sys.getQuantity(u.substring(1))) == null) {
			return sys.getQuantity(u);
		} else {
			switch(u.charAt(0)) {
			case 'Y':  x = 1e24;  break;
			case 'Z':  x = 1e21;  break;
			case 'E':  x = 1e18;  break;
			case 'P':  x = 1e15;  break;
			case 'T':  x = 1e12;  break;
			case 'G':  x = 1e09;  break;
			case 'M':  x = 1e06;  break;
			case 'k':  x = 1e03;  break;
			case 'h':  x = 1e02;  break;
			case 'c':  x = 1e-02;  break;
			case 'm':  x = 1e-03;  break;
			case 'μ':
			case 'u':  x = 1e-06;  break;
			case 'n':  x = 1e-09;  break;
			case 'p':  x = 1e-12;  break;
			case 'f':  x = 1e-15;  break;
			case 'a':  x = 1e-18;  break;
			case 'z':  x = 1e-21;  break;
			case 'y':  x = 1e-24;  break;
			default:  throw new RuntimeException();
			}
			return q.multiply(x);
		}
	}

}
