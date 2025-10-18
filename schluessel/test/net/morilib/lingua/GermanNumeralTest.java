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

import net.morilib.lingua.numeral.GermanNumeral;
import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/05/22
 */
public class GermanNumeralTest extends TC {

	public void testDescribeNumeral() {
		eq(GermanNumeral.describeNumber(0), "null");
		eq(GermanNumeral.describeNumber(1), "eins");
		eq(GermanNumeral.describeNumber(41), "einundvierzig");
		eq(GermanNumeral.describeNumber(42), "zweiundvierzig");
		eq(GermanNumeral.describeNumber(71), "einundsiebzig");
		eq(GermanNumeral.describeNumber(72), "zweiundsiebzig");
		eq(GermanNumeral.describeNumber(100), "hundert");
		eq(GermanNumeral.describeNumber(165), "hundertfünfundsechzig");
		eq(GermanNumeral.describeNumber(700), "siebenhundert");
		eq(GermanNumeral.describeNumber(765), "siebenhundertfünfundsechzig");
		eq(GermanNumeral.describeNumber(1234), "tausendzweihundertvierunddreißig");
		eq(GermanNumeral.describeNumber(765876),
				"siebenhundertfünfundsechzigtausendachthundertsechsundsiebzig");
		eq(GermanNumeral.describeNumber(1798123094), "eine Milliarde" +
				" siebenhundertachtundneunzig Millionen" +
				" hundertdreiundzwanzigtausendvierundneunzig");
	}

	public void testParseNumber() {
		eq(GermanNumeral.parseNumber("null").intValue(), 0);
		eq(GermanNumeral.parseNumber("eins").intValue(), 1);
		eq(GermanNumeral.parseNumber("einundvierzig").intValue(), 41);
		eq(GermanNumeral.parseNumber("zweiundvierzig").intValue(), 42);
		eq(GermanNumeral.parseNumber("einundsiebzig").intValue(), 71);
		eq(GermanNumeral.parseNumber("zweiundsiebzig").intValue(), 72);
		eq(GermanNumeral.parseNumber("hundert").intValue(), 100);
		eq(GermanNumeral.parseNumber("hundertfünfundsechzig").intValue(), 165);
		eq(GermanNumeral.parseNumber("siebenhundert").intValue(), 700);
		eq(GermanNumeral.parseNumber("siebenhundertfünfundsechzig").intValue(), 765);
		eq(GermanNumeral.parseNumber("tausendzweihundertvierunddreißig").intValue(),
				1234);
		eq(GermanNumeral.parseNumber(
				"siebenhundertfünfundsechzigtausendachthundertsechsundsiebzig").intValue(),
				765876);
		eq(GermanNumeral.parseNumber("eine Milliarde").intValue(),
				1000000000);
		eq(GermanNumeral.parseNumber("eine Milliarde" +
				" siebenhundertachtundneunzig Millionen").intValue(),
				1798000000);
		eq(GermanNumeral.parseNumber("eine Milliarde" +
				" siebenhundertachtundneunzig Millionen" +
				" hundertdreiundzwanzigtausendvierundneunzig").intValue(),
				1798123094);
		eq(GermanNumeral.parseNumber("zwei Milliarden" +
				" siebenhundertachtundneunzig Millionen" +
				" hundertdreiundzwanzigtausendvierundneunzig").longValue(),
				2798123094l);
	}

	static void parseerrtest(String s) {
		try {
			GermanNumeral.parseNumber(s);  fail();
		} catch(Exception e) {}
	}

	public static void testParseNumber2() {
		parseerrtest("tausend eine Million");
		parseerrtest("eine Million eine Milliarde");
		parseerrtest("zwei Millionen eine Milliarde");
		parseerrtest("eine Million zwei Milliarden");
		parseerrtest("tausendtausend");
		parseerrtest("zehntausendtausend");
		parseerrtest("hunderttausendtausend");
		parseerrtest("hunderthundert");
		parseerrtest("zehnzwanzigtausend");
		parseerrtest("zweizwanzigtausend");

		parseerrtest("zweiunddrei");
		parseerrtest("zweiundzehn");
		parseerrtest("hundertundzwanzig");
		parseerrtest("eine Millionen");
		parseerrtest("eine Milliarden");
		parseerrtest("zwei Million");
		parseerrtest("zwei Milliarde");
	}

}
