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

import net.morilib.lingua.numeral.EnglishNumeral;
import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/05/21
 */
public class EnglishNumeralTest extends TC {

	public void testDecribeNumber() {
		eq(EnglishNumeral.describeNumber(1000), "one thousand");
		eq(EnglishNumeral.describeNumber(1729),
				"one thousand seven hundred twenty-nine");
		eq(EnglishNumeral.describeNumber(72), "seventy-two");
		eq(EnglishNumeral.describeNumber(9308),
				"nine thousand three hundred eight");
		eq(EnglishNumeral.describeNumber(2025),
				"two thousand twenty-five");
		eq(EnglishNumeral.describeNumber(765),
				"seven hundred sixty-five");
		eq(EnglishNumeral.describeNumber(212),
				"two hundred twelve");
		eq(EnglishNumeral.describeNumber(4315),
				"four thousand three hundred fifteen");

		eq(EnglishNumeral.describeNumber(10000),
				"ten thousand");
		eq(EnglishNumeral.describeNumber(51425),
				"fifty-one thousand four hundred" +
				" twenty-five");
		eq(EnglishNumeral.describeNumber(193002),
				"one hundred ninety-three thousand two");
		eq(EnglishNumeral.describeNumber(6800000),
				"six million eight hundred thousand");
		eq(EnglishNumeral.describeNumber(1798123094), "one billion" +
				" seven hundred ninety-eight million" +
				" one hundred twenty-three thousand" +
				" ninety-four");
		eq(EnglishNumeral.describeNumber(2000000000000l), "two trillion");
	}

	public void testParseNumber() {
		eq(EnglishNumeral.parseNumber("one thousand").intValue(), 1000);
		eq(EnglishNumeral.parseNumber(
				"one thousand seven hundred twenty-nine").intValue(),
				1729);
		eq(EnglishNumeral.parseNumber("seventy-two").intValue(), 72);
		eq(EnglishNumeral.parseNumber(
				"nine thousand three hundred eight").intValue(), 9308);
		eq(EnglishNumeral.parseNumber(
				"two thousand twenty-five").intValue(), 2025);
		eq(EnglishNumeral.parseNumber(
				"seven hundred sixty-five").intValue(), 765);
		eq(EnglishNumeral.parseNumber(
				"two hundred twelve").intValue(), 212);
		eq(EnglishNumeral.parseNumber(
				"four thousand three hundred fifteen").intValue(), 4315);

		eq(EnglishNumeral.parseNumber(
				"ten thousand").intValue(), 10000);
		eq(EnglishNumeral.parseNumber(
				"fifty-one thousand four hundred" +
				" twenty-five").intValue(), 51425);
		eq(EnglishNumeral.parseNumber(
				"one hundred ninety-three thousand two").intValue(),
				193002);
		eq(EnglishNumeral.parseNumber(
				"six million eight hundred thousand").intValue(),
				6800000);
		eq(EnglishNumeral.parseNumber("one billion" +
				" seven hundred ninety-eight million" +
				" one hundred twenty-three thousand" +
				" ninety-four").intValue(), 1798123094);
		eq(EnglishNumeral.parseNumber("two trillion").longValue(),
				2000000000000l);
	}

	static void parseerrtest(String s) {
		try {
			EnglishNumeral.parseNumber(s);  fail();
		} catch(Exception e) {}
	}

	public static void testParseNumber2() {
		parseerrtest("one thousand one million");
		parseerrtest("one thousand one thousand");
		parseerrtest("ten thousand one thousand");
		parseerrtest("one hundred thousand one thousand");
		parseerrtest("one hundred hundred");
		parseerrtest("ten twenty thousand");
		parseerrtest("two twenty thousand");
	}

}
