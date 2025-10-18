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

import java.util.HashMap;
import java.util.Map;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/02/16
 */
public class Language {

	private static final Map<String, Language> INSTANCES =
		new HashMap<String, Language>();

	private int code;

	private Language(int c) {
		code = c;
	}

	public static Language getInstance(String s) {
		Language l;
		String x;

		if(s == null) {
			throw new NullPointerException();
		} else if((l = INSTANCES.get(x = s.toLowerCase())) != null) {
			return l;
		} else if(x.length() == 3) {
			l = new Language(LanguageTDB.encodeISO639_3(x) / 26 / 26);
		} else if(x.length() == 2 &&
				(x = LanguageTDB.findISO639_1(x)) != null) {
			l = getInstance(x);
		} else {
			throw new IllegalArgumentException();
		}
		INSTANCES.put(s, l);
		return l;
	}

	public boolean isISO639_3() {
		return LanguageTDB.find(code) != null;
	}

	public String getISO639_1() {
		String s = LanguageTDB.find(code);

		return (s != null && !s.equals("")) ? s : null;
	}

	public LanguageTDBRecord getProperty() {
		return LanguageTDB.findRecord(code);
	}

	public String getName() {
		LanguageTDBRecord r = getProperty();

		return (r != null) ? r.getDescription() : null;
	}

	public String toString() {
		char[] a = new char[3];

		a[0] = (char)(code / 26 / 26 + 'a');
		a[1] = (char)((code % (26 * 26)) / 26 + 'a');
		a[2] = (char)(code % 26 + 'a');
		return new String(a);
	}

}
