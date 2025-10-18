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

import junit.framework.TestCase;

public class SimpleGregorianCalendarTest extends TestCase {

	//
	static SimpleGregorianCalendar c1(int y, int m, int d) {
		return new SimpleGregorianCalendar(y, m, d);
	}

	//
	static SimpleGregorianCalendar c2(long t) {
		return SimpleGregorianCalendar.getInstaceFromTime(t);
	}

	//
	static void e1(int y, int m, int d) {
		try {
			new SimpleGregorianCalendar(y, m, d);
			fail();
		} catch(IllegalArgumentException e) {}
	}

	public void testCalendarAD20110101() {
		SimpleGregorianCalendar k = c1(2011, 1, 1);

		assertEquals(k.getYear(), 2011);
		assertEquals(k.getDayOfYear(), 1);
		assertEquals(k.getMaximumDaysOfYear(), 365);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
		assertEquals(k.getMonth(), 1);
		assertEquals(k.getDayOfWeek(), 6);
	}

	public void testCalendarAD20110228() {
		SimpleGregorianCalendar k = c1(2011, 2, 28);

		assertEquals(k.getYear(), 2011);
		assertEquals(k.getDayOfYear(), 59);
		assertEquals(k.getMaximumDaysOfYear(), 365);
		assertEquals(k.getDayOfMonth(), 28);
		assertEquals(k.getMaximumDaysOfMonth(), 28);
		assertEquals(k.getMonth(), 2);
		assertEquals(k.getDayOfWeek(), 1);
	}

	public void testCalendarAD20110301() {
		SimpleGregorianCalendar k = c1(2011, 3, 1);

		assertEquals(k.getYear(), 2011);
		assertEquals(k.getDayOfYear(), 60);
		assertEquals(k.getMaximumDaysOfYear(), 365);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
		assertEquals(k.getMonth(), 3);
		assertEquals(k.getDayOfWeek(), 2);
	}

	public void testCalendarAD20120101() {
		SimpleGregorianCalendar k = c1(2012, 1, 1);

		assertEquals(k.getYear(), 2012);
		assertEquals(k.getDayOfYear(), 1);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
		assertEquals(k.getMonth(), 1);
		assertEquals(k.getDayOfWeek(), 0);
	}

	public void testCalendarAD20120201() {
		SimpleGregorianCalendar k = c1(2012, 2, 1);

		assertEquals(k.getYear(), 2012);
		assertEquals(k.getDayOfYear(), 32);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 29);
		assertEquals(k.getMonth(), 2);
		assertEquals(k.getDayOfWeek(), 3);
	}

	public void testCalendarAD20120229() {
		SimpleGregorianCalendar k = c1(2012, 2, 29);

		assertEquals(k.getYear(), 2012);
		assertEquals(k.getDayOfYear(), 60);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 29);
		assertEquals(k.getMaximumDaysOfMonth(), 29);
		assertEquals(k.getMonth(), 2);
		assertEquals(k.getDayOfWeek(), 3);
	}

	public void testCalendarAD20120301() {
		SimpleGregorianCalendar k = c1(2012, 3, 1);

		assertEquals(k.getYear(), 2012);
		assertEquals(k.getDayOfYear(), 61);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
		assertEquals(k.getMonth(), 3);
		assertEquals(k.getDayOfWeek(), 4);
	}

	public void testCalendarAD20121222() {
		SimpleGregorianCalendar k = c1(2012, 12, 22);

		assertEquals(k.getYear(), 2012);
		assertEquals(k.getDayOfYear(), 357);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 22);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
		assertEquals(k.getMonth(), 12);
		assertEquals(k.getDayOfWeek(), 6);
	}

	public void testCalendarAD20121231() {
		SimpleGregorianCalendar k = c1(2012, 12, 31);

		assertEquals(k.getYear(), 2012);
		assertEquals(k.getDayOfYear(), 366);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 31);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
		assertEquals(k.getMonth(), 12);
		assertEquals(k.getDayOfWeek(), 1);
	}

	public void testCalendarAD20000101() {
		SimpleGregorianCalendar k = c1(2000, 1, 1);

		assertEquals(k.getYear(), 2000);
		assertEquals(k.getDayOfYear(), 1);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
		assertEquals(k.getMonth(), 1);
	}

	public void testCalendarAD19991231() {
		SimpleGregorianCalendar k = c1(1999, 12, 31);

		assertEquals(k.getYear(), 1999);
		assertEquals(k.getDayOfYear(), 365);
		assertEquals(k.getMaximumDaysOfYear(), 365);
		assertEquals(k.getDayOfMonth(), 31);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
		assertEquals(k.getMonth(), 12);
	}

	public void testCalendarAD19000101() {
		SimpleGregorianCalendar k = c1(1900, 1, 1);

		assertEquals(k.getYear(), 1900);
		assertEquals(k.getDayOfYear(), 1);
		assertEquals(k.getMaximumDaysOfYear(), 365);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
		assertEquals(k.getMonth(), 1);
	}

	public void testCalendarAD00010101() {
		SimpleGregorianCalendar k = c1(1, 1, 1);

		assertEquals(k.getDaysFromEpoch(), 0);
		assertEquals(k.getYear(), 1);
		assertEquals(k.getDayOfYear(), 1);
		assertEquals(k.getMaximumDaysOfYear(), 365);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
		assertEquals(k.getMonth(), 1);
	}

	public void testCalendarAD19700101() {
		SimpleGregorianCalendar k = c1(1970, 1, 1);

		assertEquals(k.getDate(), 0);
	}

	public void testAD15821015() {
		SimpleGregorianCalendar k = c1(1582, 10, 15);

		assertEquals(k.getLilianDay(), 0);
	}

	public void testCalendarAD19700101_2() {
		SimpleGregorianCalendar k = c2(0);

		assertEquals(k.getDate(), 0);
	}

	public void testAD17520915After() {
		SimpleGregorianCalendar k = c1(1752, 9, 15);
		SimpleGregorianCalendar m = c1(1752, 9, 14);

		assertEquals(m.getDaysFromUnixEpoch() + 1, k.getDaysFromUnixEpoch());
		assertEquals(m.getDaysFromEpoch() + 1, k.getDaysFromEpoch());
		assertEquals(m.after(0).getDaysFromEpoch(), m.getDaysFromEpoch());
		assertEquals(m.after(1).getDaysFromEpoch(), k.getDaysFromEpoch());
		assertEquals(k.after(-1).getDaysFromEpoch(), m.getDaysFromEpoch());
	}

	public void testAD17520914() {
		SimpleGregorianCalendar k = c1(1752, 9, 14);

		assertEquals(k.getJulianDay(), 2361222);
	}

	public void testCalendarErrors() {
		e1(0, 1, 1);
		e1(2011, 2, 0);  e1(2011, 2, 29);
	}

}
