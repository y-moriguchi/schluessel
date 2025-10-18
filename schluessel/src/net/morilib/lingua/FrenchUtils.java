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

import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/02/16
 */
public class FrenchUtils {

	//
	private static final String IRREGULAR_NOUNS_FILE =
			"/net/morilib/lingua/french_irregular_nouns.properties";
	private static final Properties IRREGULAR_NOUNS;

	static {
		InputStream ins;
		Properties p;

		ins = FrenchUtils.class.getResourceAsStream(
				IRREGULAR_NOUNS_FILE);
		p = new Properties();
		try {
			p.load(ins);
			IRREGULAR_NOUNS = p;
		} catch (IOException e) {
			throw new RuntimeException(e);
		} finally {
			try {
				ins.close();
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
	}

	/**
	 * 
	 * @param word
	 * @return
	 */
	public static String toPlural(String word) {
		String s;

		if(word == null) {
			throw new NullPointerException();
		} else if(word.equals("")) {
			return word;
		} else if((s = IRREGULAR_NOUNS.getProperty(word)) != null) {
			return s;
		} else if(word.endsWith("s") ||
				word.endsWith("x") || word.endsWith("z")) {
			return word;
		} else if(word.endsWith("au") ||
				word.endsWith("eu") || word.endsWith("ou")) {
			return word + "x";
		} else if(word.endsWith("al")) {
			return word.replaceFirst("al$", "aux");
		} else {
			return word + "s";
		}
	}

}
