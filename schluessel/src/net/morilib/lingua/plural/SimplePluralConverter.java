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
package net.morilib.lingua.plural;

import java.util.Locale;
import java.util.Map;
import java.util.WeakHashMap;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/03
 */
public final class SimplePluralConverter {

	//
	private static final String LOCATION =
		"net.morilib.lingua.plural.SimplePluralConverter";
	private static Map<Locale, PluralConverter> pool =
		new WeakHashMap<Locale, PluralConverter>();

	/**
	 * 
	 */
	public static final PluralConverter ENGLISH =
		SimplePluralConverter.getInstance(Locale.ENGLISH);

	//
	private SimplePluralConverter() { }

	//
	private static PluralConverter load(
			String name) throws ClassNotFoundException {
		PluralConverter res;

		try {
			Class<?> cl = Class.forName(name);

			res = (PluralConverter)cl.newInstance();
		} catch (InstantiationException e) {
			throw new UnsupportedLocaleException(e);
		} catch (IllegalAccessException e) {
			throw new UnsupportedLocaleException(e);
		}
		return res;
	}

	//
	private static PluralConverter loadInstance(String cn, Locale lc) {
		// language + country + variant
		try {
			return load(cn +
					"_" + lc.getLanguage() +
					"_" + lc.getCountry()  +
					"_" + lc.getVariant());
		} catch (ClassNotFoundException e) {
			// ignore
		}

		// language + country
		try {
			return load(cn +
					"_" + lc.getLanguage() +
					"_" + lc.getCountry());
		} catch (ClassNotFoundException e) {
			// ignore
		}

		// language
		try {
			return load(cn + "_" + lc.getLanguage());
		} catch (ClassNotFoundException e) {
			throw new UnsupportedLocaleException(e);
		}

		// base
		/*try {
			return load(cn);
		} catch (ClassNotFoundException e) {
			throw new UnsupportedLocaleException(e);
		}*/
	}

	/**
	 * 
	 * @param lc
	 * @return
	 */
	public static PluralConverter getInstance(Locale lc) {
		PluralConverter res;

		if((res = pool.get(lc)) == null) {
			synchronized(SimplePluralConverter.class) {
				res = loadInstance(LOCATION, lc);
				pool.put(lc, res);
			}
		}
		return res;
	}

}
