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

public class SimpleJulianCalendarTest extends TestCase {

	//
	static SimpleJulianCalendar c1(int y, int m, int d) {
		return new SimpleJulianCalendar(y, m, d);
	}

	//
	static SimpleJulianCalendar c2(long t) {
		return SimpleJulianCalendar.getInstaceFromTime(t);
	}

	//
	static void e1(int y, int m, int d) {
		try {
			new SimpleJulianCalendar(y, m, d);
			fail();
		} catch(IllegalArgumentException e) {}
	}

	public void testKalendaeBC450101() {
		SimpleJulianCalendar k = c1(-45, 1, 1);

		assertEquals(k.getDaysFromEpoch(), 0);
		assertEquals(k.getYear(), -45);
		assertEquals(k.getDayOfYear(), 1);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
		assertEquals(k.getMonth(), 1);
		assertEquals(k.getDayOfWeek(), 5);
	}

	public void testKalendaeBC450131() {
		SimpleJulianCalendar k = c1(-45, 1, 31);

		assertEquals(k.getDaysFromEpoch(), 30);
		assertEquals(k.getYear(), -45);
		assertEquals(k.getDayOfYear(), 31);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 31);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
		assertEquals(k.getMonth(), 1);
		assertEquals(k.getDayOfWeek(), 0);
	}

	public void testKalendaeBC450201() {
		SimpleJulianCalendar k = c1(-45, 2, 1);

		assertEquals(k.getDaysFromEpoch(), 31);
		assertEquals(k.getYear(), -45);
		assertEquals(k.getDayOfYear(), 32);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 29);
		assertEquals(k.getMonth(), 2);
		assertEquals(k.getDayOfWeek(), 1);
	}

	public void testKalendaeBC450229() {
		SimpleJulianCalendar k = c1(-45, 2, 29);

		assertEquals(k.getDaysFromEpoch(), 59);
		assertEquals(k.getYear(), -45);
		assertEquals(k.getDayOfYear(), 60);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 29);
		assertEquals(k.getMaximumDaysOfMonth(), 29);
		assertEquals(k.getDayOfWeek(), 1);
	}

	public void testKalendaeBC450301() {
		SimpleJulianCalendar k = c1(-45, 3, 1);

		assertEquals(k.getDaysFromEpoch(), 60);
		assertEquals(k.getYear(), -45);
		assertEquals(k.getDayOfYear(), 61);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMonth(), 3);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
	}

	public void testKalendaeBC450401() {
		SimpleJulianCalendar k = c1(-45, 4, 1);

		assertEquals(k.getDaysFromEpoch(), 91);
		assertEquals(k.getYear(), -45);
		assertEquals(k.getDayOfYear(), 92);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMonth(), 4);
		assertEquals(k.getMaximumDaysOfMonth(), 30);
	}

	public void testKalendaeBC450501() {
		SimpleJulianCalendar k = c1(-45, 5, 1);

		assertEquals(k.getDaysFromEpoch(), 121);
		assertEquals(k.getYear(), -45);
		assertEquals(k.getDayOfYear(), 122);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
	}

	public void testKalendaeBC450601() {
		SimpleJulianCalendar k = c1(-45, 6, 1);

		assertEquals(k.getDaysFromEpoch(), 152);
		assertEquals(k.getYear(), -45);
		assertEquals(k.getDayOfYear(), 153);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 30);
	}

	public void testKalendaeBC450701() {
		SimpleJulianCalendar k = c1(-45, 7, 1);

		assertEquals(k.getDaysFromEpoch(), 182);
		assertEquals(k.getYear(), -45);
		assertEquals(k.getDayOfYear(), 183);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
	}

	public void testKalendaeBC450801() {
		SimpleJulianCalendar k = c1(-45, 8, 1);

		assertEquals(k.getDaysFromEpoch(), 213);
		assertEquals(k.getYear(), -45);
		assertEquals(k.getDayOfYear(), 214);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
	}

	public void testKalendaeBC450901() {
		SimpleJulianCalendar k = c1(-45, 9, 1);

		assertEquals(k.getDaysFromEpoch(), 244);
		assertEquals(k.getYear(), -45);
		assertEquals(k.getDayOfYear(), 245);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 30);
	}

	public void testKalendaeBC451001() {
		SimpleJulianCalendar k = c1(-45, 10, 1);

		assertEquals(k.getDaysFromEpoch(), 274);
		assertEquals(k.getYear(), -45);
		assertEquals(k.getDayOfYear(), 275);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
	}

	public void testKalendaeBC451101() {
		SimpleJulianCalendar k = c1(-45, 11, 1);

		assertEquals(k.getDaysFromEpoch(), 305);
		assertEquals(k.getYear(), -45);
		assertEquals(k.getDayOfYear(), 306);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 30);
	}

	public void testKalendaeBC451201() {
		SimpleJulianCalendar k = c1(-45, 12, 1);

		assertEquals(k.getDaysFromEpoch(), 335);
		assertEquals(k.getYear(), -45);
		assertEquals(k.getDayOfYear(), 336);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMonth(), 12);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
	}

	public void testKalendaeBC451231() {
		SimpleJulianCalendar k = c1(-45, 12, 31);

		assertEquals(k.getDaysFromEpoch(), 365);
		assertEquals(k.getYear(), -45);
		assertEquals(k.getDayOfYear(), 366);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 31);
		assertEquals(k.getMonth(), 12);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
	}

	public void testKalendaeBC440228() {
		SimpleJulianCalendar k = c1(-44, 2, 28);

		assertEquals(k.getYear(), -44);
		assertEquals(k.getDayOfYear(), 59);
		assertEquals(k.getMaximumDaysOfYear(), 365);
		assertEquals(k.getDayOfMonth(), 28);
		assertEquals(k.getMonth(), 2);
		assertEquals(k.getMaximumDaysOfMonth(), 28);
	}

	public void testKalendaeBC440301() {
		SimpleJulianCalendar k = c1(-44, 3, 1);

		assertEquals(k.getYear(), -44);
		assertEquals(k.getDayOfYear(), 60);
		assertEquals(k.getMaximumDaysOfYear(), 365);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
	}

	public void testKalendaeBC461231() {
		SimpleJulianCalendar k = c1(-46, 12, 31);

		assertEquals(k.getDaysFromEpoch(), -1);
		assertEquals(k.getYear(), -46);
		assertEquals(k.getDayOfYear(), 365);
		assertEquals(k.getMaximumDaysOfYear(), 365);
		assertEquals(k.getDayOfMonth(), 31);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
		assertEquals(k.getDayOfWeek(), 4);
	}

	public void testKalendaeBC490229() {
		SimpleJulianCalendar k = c1(-49, 2, 29);

		assertEquals(k.getYear(), -49);
		assertEquals(k.getDayOfYear(), 60);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 29);
		assertEquals(k.getMaximumDaysOfMonth(), 29);
	}

	public void testKalendaeBC410229() {
		SimpleJulianCalendar k = c1(-41, 2, 29);

		assertEquals(k.getYear(), -41);
		assertEquals(k.getDayOfYear(), 60);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 29);
		assertEquals(k.getMaximumDaysOfMonth(), 29);
	}

	public void testKalendaeBC011231() {
		SimpleJulianCalendar k = c1(-1, 12, 31);

		assertEquals(k.getDaysFromEpoch(), 16436);
		assertEquals(k.getYear(), -1);
		assertEquals(k.getDayOfYear(), 366);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 31);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
	}

	public void testKalendaeAD010101() {
		SimpleJulianCalendar k = c1(1, 1, 1);

		assertEquals(k.getDaysFromEpoch(), 16437);
		assertEquals(k.getYear(), 1);
		assertEquals(k.getDayOfYear(), 1);
		assertEquals(k.getMaximumDaysOfYear(), 365);
		assertEquals(k.getDayOfMonth(), 1);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
		assertEquals(k.getDayOfWeek(), 6);
	}

	public void testKalendaeAD19700101_2() {
		SimpleJulianCalendar k = c2(0);

		assertEquals(k.getDate(), 0);
	}

	public void testAD17520914() {
		SimpleJulianCalendar k = c1(1752, 9, 2);

		assertEquals(k.getJulianDay(), 2361221);
	}

	public void testAD17520915After() {
		SimpleJulianCalendar k = c1(1752, 9, 15);
		SimpleJulianCalendar m = c1(1752, 9, 14);

		assertEquals(m.getDaysFromUnixEpoch() + 1, k.getDaysFromUnixEpoch());
		assertEquals(m.getDaysFromEpoch() + 1, k.getDaysFromEpoch());
		assertEquals(m.after(0).getDaysFromEpoch(), m.getDaysFromEpoch());
		assertEquals(m.after(1).getDaysFromEpoch(), k.getDaysFromEpoch());
		assertEquals(k.after(-1).getDaysFromEpoch(), m.getDaysFromEpoch());
	}

	public void testKalendaeErrors() {
		e1(-45, 1, 0);  e1(-45, 1, 32);
		e1(-45, 2, 0);  e1(-45, 2, 30);
		e1(-45, 3, 0);  e1(-45, 3, 32);
		e1(-45, 4, 0);  e1(-45, 4, 31);
		e1(-45, 5, 0);  e1(-45, 5, 32);
		e1(-45, 6, 0);  e1(-45, 6, 31);
		e1(-45, 7, 0);  e1(-45, 7, 32);
		e1(-45, 8, 0);  e1(-45, 8, 32);
		e1(-45, 9, 0);  e1(-45, 9, 31);
		e1(-45, 10, 0);  e1(-45, 10, 32);
		e1(-45, 11, 0);  e1(-45, 11, 31);
		e1(-45, 12, 0);  e1(-45, 12, 32);
		e1(-49, 2, 0);  e1(-49, 2, 30);
		e1(-48, 2, 0);  e1(-48, 2, 29);
		e1(-47, 2, 0);  e1(-47, 2, 29);
		e1(-46, 2, 0);  e1(-46, 2, 29);
		e1(-44, 2, 0);  e1(-44, 2, 29);
		e1(-43, 2, 0);  e1(-43, 2, 29);
		e1(-42, 2, 0);  e1(-42, 2, 29);
		e1(-41, 2, 0);  e1(-41, 2, 30);
		e1(0, 1, 1);
	}

}
