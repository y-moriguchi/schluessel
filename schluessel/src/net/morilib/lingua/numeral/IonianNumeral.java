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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/05/10
 */
public final class IonianNumeral {

	//
	private IonianNumeral() {}

	//
	private static String CENT =
		"\0\u03c1\u03c3\u03c4\u03c5\u03c6\u03c7\u03c8\u03c9\u03e1";
	private static String DECA =
		"\0\u03b9\u03ba\u03bb\u03bc\u03bd\u03be\u03bf\u03c0\u03d9";
	private static String UNUS =
		"\0\u03b1\u03b2\u03b3\u03b4\u03b5\u03db\u03b6\u03b7\u03b8";

	/**
	 * 
	 * @param n
	 * @return
	 */
	public static String toGreek(int n) {
		int m, c, d, u;
		StringBuffer r = new StringBuffer();

		if(n <= 0 || n >= 10000)  return null;
		m = n / 1000;
		c = n / 100 % 10;
		d = n / 10  % 10;
		u = n % 10;
		if(m > 0)  r.append(',').append(UNUS.charAt(m));
		if(c > 0)  r.append(CENT.charAt(c));
		if(d > 0)  r.append(DECA.charAt(d));
		if(u > 0)  r.append(UNUS.charAt(u));
		return r.append('\'').toString();
	}

	/**
	 * 
	 * @param greek
	 * @return
	 */
	public static int parse(String greek) {
		String s = greek.toLowerCase();
		int r = 0, i, w;

		for(int p = 0;; p++) {
			w = p < s.length() ? s.charAt(p) : -1;
			if(r == 0 && w == ',') {
				r = -10000;
			} else if(r == -10000 && (i = UNUS.indexOf(w)) > 0) {
				r = 1000 * i;
			} else if(r < 0) {
				return (w == -1) ? -r : -1;
			} else if(r % 1000 == 0) {
				if((i = CENT.indexOf(w)) > 0) {
					r += 100 * i;
				} else if((i = DECA.indexOf(w)) > 0) {
					r += 10 * i;
				} else if((i = UNUS.indexOf(w)) > 0) {
					r += i;
				} else if(w == '\'') {
					r = -r;
				} else {
					return -1;
				}
			} else if(r % 100 == 0) {
				if((i = DECA.indexOf(w)) > 0) {
					r += 10 * i;
				} else if((i = UNUS.indexOf(w)) > 0) {
					r += i;
				} else if(w == '\'') {
					r = -r;
				} else {
					return -1;
				}
			} else if(r % 10 == 0) {
				if((i = UNUS.indexOf(w)) > 0) {
					r += i;
				} else if(w == '\'') {
					r = -r;
				} else {
					return -1;
				}
			} else if(w == '\'') {
				r = -r;
			} else {
				return -1;
			}
		}
	}

}
