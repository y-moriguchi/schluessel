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

import net.morilib.lingua.numeral.FrenchNumeral;
import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/05/22
 */
public class FrenchNumeralTest extends TC {

	public void testDescribeNumeral() {
		eq(FrenchNumeral.describeNumber(1), "un");
		eq(FrenchNumeral.describeNumber(41), "quarante et un");
		eq(FrenchNumeral.describeNumber(42), "quarante-deux");
		eq(FrenchNumeral.describeNumber(71), "soixante et onze");
		eq(FrenchNumeral.describeNumber(72), "soixante-douze");
		eq(FrenchNumeral.describeNumber(100), "cent");
		eq(FrenchNumeral.describeNumber(165), "cent soixante-cinq");
		eq(FrenchNumeral.describeNumber(700), "sept cents");
		eq(FrenchNumeral.describeNumber(765), "sept cent soixante-cinq");
		eq(FrenchNumeral.describeNumber(1234), "mille deux cent trente-quatre");
		eq(FrenchNumeral.describeNumber(1798123094), "un milliard" +
				" sept cent quatre-vingt-dix-huit million" +
				" cent vingt-trois mille quatre-vingt-quatorze");
	}

	public void testParseNumber() {
		eq(FrenchNumeral.parseNumber("un").intValue(), 1);
		eq(FrenchNumeral.parseNumber("quarante et un").intValue(), 41);
		eq(FrenchNumeral.parseNumber("quarante-deux").intValue(), 42);
		eq(FrenchNumeral.parseNumber("soixante et onze").intValue(), 71);
		eq(FrenchNumeral.parseNumber("soixante-douze").intValue(), 72);
		eq(FrenchNumeral.parseNumber("cent").intValue(), 100);
		eq(FrenchNumeral.parseNumber("cent soixante-cinq").intValue(), 165);
		eq(FrenchNumeral.parseNumber("sept cents").intValue(), 700);
		eq(FrenchNumeral.parseNumber("sept cent soixante-cinq").intValue(), 765);
		eq(FrenchNumeral.parseNumber("mille deux cent trente-quatre").intValue(),
				1234);
		eq(FrenchNumeral.parseNumber("un milliard" +
				" sept cent quatre-vingt-dix-huit millions" +
				" cent vingt-trois mille quatre-vingt-quatorze").intValue(),
				1798123094);
	}

	static void parseerrtest(String s) {
		try {
			FrenchNumeral.parseNumber(s);  fail();
		} catch(Exception e) {}
	}

	public static void testParseNumber2() {
		parseerrtest("un miile un million");
		parseerrtest("un mille un mille");
		parseerrtest("dix mille un mille");
		parseerrtest("cent mille un mille");
		parseerrtest("un cent cent");
		parseerrtest("dix vingt mille");
		parseerrtest("deux vingt mille");

		parseerrtest("un millions");
		parseerrtest("deux million");
		parseerrtest("quarante et deux");
		parseerrtest("sept cents soixante-cinq");
	}

}
