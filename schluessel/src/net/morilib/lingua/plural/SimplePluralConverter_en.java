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

import java.io.IOException;
import java.io.InputStream;
import java.util.Collections;
import java.util.Set;
import java.util.regex.Pattern;

import net.morilib.parser.NewPropertiesParser;
import net.morilib.util.mapset.ManyToManySet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/03
 */
public class SimplePluralConverter_en implements PluralConverter {

	//
	private Pattern _ES   = Pattern.compile("^.*(s|z|sh|ch|o)$");
	private Pattern _IES  = Pattern.compile("^.*[^aeiou]y$");

	//
	private ManyToManySet<String, String> irregulars;

	//
	/*package*/ SimplePluralConverter_en() {
		Class<?>    cl  = SimplePluralConverter_en.class;
		InputStream ins = null;
		NewPropertiesParser p = new NewPropertiesParser();

		try {
			ins = cl.getResourceAsStream(
					"/" +
					cl.getPackage().getName().replace('.', '/') +
					"/simpleirregular_en.properties");
			irregulars = p.parse(ins);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	//
	private String getRegularPlural(String word) {
		if(_ES.matcher(word).matches()) {
			return word + "es";
		} else if(_IES.matcher(word).matches()) {
			return word.substring(0, word.length() - 1) + "ies";
		} else {
			return word + "s";
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.locale.WordConverter#toPlurals(java.lang.String)
	 */
	@Override
	public Set<String> toPlurals(String word) {
		if(irregulars.containsKey(word)) {
			return irregulars.getValues(word);
		} else {
			return Collections.singleton(getRegularPlural(word));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.locale.WordConverter#isPlural(java.lang.String, java.lang.String)
	 */
	@Override
	public boolean isPlural(String singular, String plural) {
		if(singular == null || plural == null) {
			throw new NullPointerException();
		} else if(irregulars.containsKey(singular)) {
			return irregulars.contains(singular, plural);
		} else {
			return plural.equals(getRegularPlural(singular));
		}
	}

}
