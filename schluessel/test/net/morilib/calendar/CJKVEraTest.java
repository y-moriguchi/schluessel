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
package net.morilib.calendar;

import java.text.ParseException;
import java.text.SimpleDateFormat;

import net.morilib.calendar.cjkv.CJKVEra;
import net.morilib.calendar.cjkv.JapaneseEras;
import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/28
 */
public class CJKVEraTest extends TC {

	//
	private static final SimpleDateFormat FMT =
		new SimpleDateFormat("yyyy/MM/dd");

	//
	private static java.util.Date _p(String ymd) {
		try {
			return (ymd == null) ? null : FMT.parse(ymd);
		} catch (ParseException e) {
			throw new RuntimeException(e);
		}
	}

	public void testHeisei() {
		CJKVEra e = JapaneseEras.HEISEI;

		eq(e.getYears(), Integer.MAX_VALUE);
		eq(e.getMonths(20), 12);
		eq(e.getLeapMonth(20), 0);
		ok(e.isEra(_p("2012/01/28")));
		ok(e.isEra(_p("1989/01/08")));
		ng(e.isEra(_p("1989/01/06")));
		eq(e.getDaysOfMonth(24, 1), 31);
		eq(e.getDaysOfMonth(23, 2), 28);
		eq(e.getDaysOfMonth(24, 2), 29);
		eq(e.getYear(_p("2012/01/28")), 24);
		eq(e.getYear(_p("1989/01/08")), 1);
		eq(e.getYear(_p("1989/01/06")), -1);
		eq(e.getDaysFromNewYearEpoch(_p("1990/01/01")), 365);
		eq(e.getDaysFromEpoch(_p("1990/01/07")), 365);
		ng(e.before(_p("2012/01/28")));
		ok(e.after(_p("1989/01/06")));
		ng(e.after(_p("2012/01/28")));
	}

	public void testShowa() {
		CJKVEra e = JapaneseEras.SHOWA;

		eq(e.getYears(), 64);
		eq(e.getMonths(20), 12);
		eq(e.getLeapMonth(20), 0);
		ng(e.isEra(_p("2012/01/28")));
		ng(e.isEra(_p("1989/01/08")));
		ok(e.isEra(_p("1989/01/06")));
		ok(e.isEra(_p("1926/12/25")));
		ng(e.isEra(_p("1926/12/24")));
		eq(e.getDaysOfMonth(24, 1), 31);
		eq(e.getYear(_p("1989/01/06")), 64);
		eq(e.getYear(_p("1926/12/25")), 1);
		eq(e.getYear(_p("1926/12/24")), -1);
		ok(e.before(_p("2012/01/28")));
		ok(e.before(_p("1989/01/08")));
		ng(e.before(_p("1989/01/06")));
		ng(e.after(_p("1926/12/25")));
		ok(e.after(_p("1926/12/24")));
	}

	public void testMeiji() {
		CJKVEra e = JapaneseEras.MEIJI;

		eq(e.getYears(), 45);
		eq(e.getMonths(6), 12);
		eq(e.getMonths(5), 12);
		eq(e.getMonths(1), 13);
		eq(e.getMonths(3), 13);
		eq(e.getLeapMonth(6), 0);
		eq(e.getLeapMonth(5), 0);
		eq(e.getLeapMonth(1), 4);
		eq(e.getLeapMonth(3), 10);
		ng(e.isEra(_p("1912/07/31")));
		ok(e.isEra(_p("1912/07/29")));
		ok(e.isEra(_p("1873/01/01")));
		ok(e.isEra(_p("1872/12/31")));
		ok(e.isEra(_p("1868/01/25")));
		ng(e.isEra(_p("1968/01/24")));
		eq(e.getDaysOfMonth(6, 1), 31);
		eq(e.getDaysOfMonth(5, 12), 2);
		eq(e.getDaysOfMonth(5, 11), 29);
		eq(e.getDaysOfMonth(1, 1), 29);
		eq(e.getDaysOfMonth(1, 2), 30);
		eq(e.getDaysOfYear(6), 365);
		eq(e.getDaysOfYear(5), 332);
		eq(e.getDaysOfYear(4), 355);
		eq(e.getDaysOfYear(1), 383);
		eq(e.getDaysFromNewYearEpoch(_p("1868/01/25")), 0);
		eq(e.getDaysFromNewYearEpoch(_p("1869/02/11")), 383);
		eq(e.getYear(_p("1912/07/29")), 45);
		eq(e.getYear(_p("1873/01/01")), 6);
		eq(e.getYear(_p("1872/12/31")), 5);
		eq(e.getYear(_p("1872/02/09")), 5);
		eq(e.getYear(_p("1872/02/08")), 4);
		eq(e.getYear(_p("1871/02/19")), 4);
		eq(e.getYear(_p("1871/02/18")), 3);
		eq(e.getYear(_p("1868/01/25")), 1);
		eq(e.getYear(_p("1868/01/24")), -1);
		ok(e.before(_p("1912/07/31")));
		ng(e.before(_p("1912/07/29")));
		ng(e.after(_p("1868/01/25")));
		ok(e.after(_p("1868/01/24")));
	}

	public void testGetEra() {
		eq(JapaneseEras.getEra(_p("2012/01/28")).toString(), "平成");
		eq(JapaneseEras.getEra(_p("1989/01/08")).toString(), "平成");
		eq(JapaneseEras.getEra(_p("1989/01/06")).toString(), "昭和");
		eq(JapaneseEras.getEra(_p("1926/12/25")).toString(), "昭和");
		eq(JapaneseEras.getEra(_p("1926/12/24")).toString(), "大正");
		eq(JapaneseEras.getEra(_p("1912/07/31")).toString(), "大正");
		eq(JapaneseEras.getEra(_p("1912/07/29")).toString(), "明治");
		eq(JapaneseEras.getEra(_p("1868/01/25")).toString(), "明治");
		nil(JapaneseEras.getEra(_p("1868/01/24")));
	}

}
