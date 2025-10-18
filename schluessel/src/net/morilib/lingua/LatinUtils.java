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
package net.morilib.lingua;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/09/05
 */
public final class LatinUtils {

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static String normalizeClassic(String s) {
		String t = s;

		t = t.replace('u', 'V');
		t = t.replace('j', 'I');
		t = t.replace('U', 'V');
		t = t.replace('J', 'I');
		t = t.replaceAll("W", "VV");
//		t = t.replaceAll("VV([a-z])", "Vv$1");
//		t = t.replaceFirst(
//				"^([A-Za-z]*[a-z][A-Za-z]*)VV$",
//				"$1Vv");
		t = t.replaceAll("w", "VV");
		return t.toUpperCase();
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static String normalizeModern(String s) {
		String t = s;

		t = t.replace('i', 'j');
		t = t.replace('I', 'J');
		t = t.replace('u', 'v');
		t = t.replace('U', 'V');
		t = t.replaceAll("W", "VV");
		t = t.replaceAll("VV([a-z])", "Vv$1");
		t = t.replaceFirst(
				"^([A-Za-z]*[a-z][A-Za-z]*)VV$",
				"$1Vv");
		t = t.replaceAll("w", "vv");

		t = t.replaceFirst("^([aejovAEJOV])v", "$1u");
		t = t.replaceFirst("^([AEJOV])V", "$1U");
		t = t.replaceFirst("^([aejouAEJOU])j", "$1i");
		t = t.replaceFirst("^([AEJOU])J", "$1I");
		t = t.replaceAll("([^a-zA-Z][aejovAEJOV])v", "$1u");
		t = t.replaceAll("([^A-Z][AEJOV])V", "$1U");
		t = t.replaceAll("([^a-zA-Z][aejouAEJOU])j", "$1i");
		t = t.replaceAll("([^A-Z][AEJOU])J", "$1I");
		t = t.replaceAll(
				"([bcdfghkjlmnpqrstvxyzBCDFGHJKLMNPQRSTVXYZ])v",
				"$1u");
		t = t.replaceAll("([BCDFGHJKLMNPQRSTVXYZ])V", "$1U");
		t = t.replaceAll(
				"([bcdfghkjlmnpqrstvxyzBCDFGHJKLMNPQRSTVXYZ])j",
				"$1i");
		t = t.replaceAll("([BCDFGHJKLMNPQRSTVXYZ])J", "$1I");
		t = t.replaceAll("v([bcdfghkjlmnpqrstvxyz])", "u$1");
		t = t.replaceAll("V([BCDFGHJKLMNPQRSTVXYZ])", "U$1");
		return t;
	}

	//
	static String normalizeClassicWithCase(String s) {
		String t = s;

		t = t.replace('u', 'v');
		t = t.replace('j', 'i');
		t = t.replace('U', 'V');
		t = t.replace('J', 'I');
		t = t.replaceAll("W", "VV");
		t = t.replaceAll("VV([a-z])", "Vv$1");
		t = t.replaceFirst(
				"^([A-Za-z]*[a-z][A-Za-z]*)VV$",
				"$1Vv");
		t = t.replaceAll("w", "vv");
		return t;
	}

	/**
	 * 
	 * @param s
	 * @param t
	 * @return
	 */
	public static boolean equalsAsLatin(String s, String t) {
		return (s != null && t != null &&
				normalizeClassicWithCase(s).equals(
						normalizeClassicWithCase(t)));
	}

	/**
	 * 
	 * @param s
	 * @param t
	 * @return
	 */
	public static boolean equalsIgnoreCaseAsLatin(String s, String t) {
		String a = s.toUpperCase(), b = t.toUpperCase();

		if(a == null || b == null)  return false;
		a = a.replace('U', 'V');
		a = a.replace('J', 'I');
		a = a.replaceAll("W", "VV");
		b = b.replace('U', 'V');
		b = b.replace('J', 'I');
		b = b.replaceAll("W", "VV");
		return a.equals(b);
	}

}
