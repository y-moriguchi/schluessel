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

import java.math.BigInteger;

import net.morilib.lingua.numeral.KanjiNumeral;
import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/05/21
 */
public class KanjiNumeralTest extends TC {

	public void testGroupBy10000() {
		eq(KanjiNumeral.groupBy10000(
				new BigInteger(
						"99999" +
						"0002" +
						"0003" +
						"0004" +
						"0005" +
						"0006" +
						"0007" +
						"0008" +
						"0009" +
						"0010" +
						"0011" +
						"0012" +
						"0013" +
						"0014" +
						"0015" +
						"0016" +
						"0017" +
						"0018")),
				"99999無量大数" +
				"2不可思議" +
				"3那由他" +
				"4阿僧祇" +
				"5恒河沙" +
				"6極" +
				"7載" +
				"8正" +
				"9澗" +
				"10溝" +
				"11穣" +
				"12秭" +
				"13垓" +
				"14京" +
				"15兆" +
				"16億" +
				"17万" +
				"18");
		eq(KanjiNumeral.groupBy10000(
				new BigInteger(
						"99999" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000")), "99999無量大数");
		eq(KanjiNumeral.groupBy10000(
				new BigInteger(
						"99999" +
						"0002" +
						"0003" +
						"0004" +
						"0005" +
						"0006" +
						"0007" +
						"0008" +
						"0009" +
						"0010" +
						"0011" +
						"0012" +
						"0013" +
						"0014" +
						"0015" +
						"0016" +
						"0017" +
						"0018"), true),
				"99999無量大数" +
				"0002不可思議" +
				"0003那由他" +
				"0004阿僧祇" +
				"0005恒河沙" +
				"0006極" +
				"0007載" +
				"0008正" +
				"0009澗" +
				"0010溝" +
				"0011穣" +
				"0012秭" +
				"0013垓" +
				"0014京" +
				"0015兆" +
				"0016億" +
				"0017万" +
				"0018");
	}

	public void testParseNumeralGroupedBy10000() {
		eq(KanjiNumeral.parseNumeralGroupedBy10000(
				"99999無量大数" +
				"0002不可思議" +
				"0003那由他" +
				"0004阿僧祇" +
				"0005恒河沙" +
				"0006極" +
				"0007載" +
				"0008正" +
				"0009澗" +
				"0010溝" +
				"0011穣" +
				"0012秭" +
				"0013垓" +
				"0014京" +
				"0015兆" +
				"0016億" +
				"0017万" +
				"0018"),
				new BigInteger(
						"99999" +
						"0002" +
						"0003" +
						"0004" +
						"0005" +
						"0006" +
						"0007" +
						"0008" +
						"0009" +
						"0010" +
						"0011" +
						"0012" +
						"0013" +
						"0014" +
						"0015" +
						"0016" +
						"0017" +
						"0018"));
		eq(KanjiNumeral.parseNumeralGroupedBy10000("99999無量大数"),
				new BigInteger(
						"99999" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000" +
						"0000"));
	}

}
