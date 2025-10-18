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

import java.util.GregorianCalendar;

import junit.framework.TestCase;

public class AnnoDominiTest extends TestCase {

	//
	static AnnoDomini c1(int y, int m, int d) {
		return new AnnoDomini(y, m, d);
	}

	//
	static AnnoDomini c2(long t) {
		return AnnoDomini.getInstaceFromTime(t);
	}

	//
	static void e1(int y, int m, int d) {
		try {
			new AnnoDomini(y, m, d);
			fail();
		} catch(IllegalArgumentException e) {}
	}

	public void testCalendarAD20121222() {
		AnnoDomini k = c1(2012, 12, 22);

		assertEquals(k.getYear(), 2012);
		assertEquals(k.getDayOfYear(), 357);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 22);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
		assertEquals(k.getMonth(), 12);
		assertEquals(k.getDayOfWeek(), 6);
	}

	public void testAD17520914() {
		AnnoDomini k = c1(1752, 9, 14);

		assertEquals(k.getJulianDay(), 2361222);
		assertEquals(k.getDaysFromEpoch(), 639796);
		assertEquals(k.getYear(), 1752);
		assertEquals(k.getDayOfYear(), 258);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 14);
		assertEquals(k.getMaximumDaysOfMonth(), 30);
		assertEquals(k.getMonth(), 9);
		assertEquals(k.getDayOfWeek(), 4);
	}

	public void testAD17520902() {
		AnnoDomini k = c1(1752, 9, 2);

		assertEquals(k.getDaysFromEpoch(), 639795);
		assertEquals(k.getYear(), 1752);
		assertEquals(k.getDayOfYear(), 246);
		assertEquals(k.getMaximumDaysOfYear(), 366);
		assertEquals(k.getDayOfMonth(), 2);
		assertEquals(k.getMaximumDaysOfMonth(), 30);
		assertEquals(k.getMonth(), 9);
		assertEquals(k.getDayOfWeek(), 3);
	}

	public void testAD17520915After() {
		AnnoDomini k = c1(1752, 9, 15);
		AnnoDomini m = c1(1752, 9, 14);

		assertEquals(m.getDaysFromUnixEpoch() + 1, k.getDaysFromUnixEpoch());
		assertEquals(m.getDaysFromEpoch() + 1, k.getDaysFromEpoch());
		assertEquals(m.after(0).getDaysFromEpoch(), m.getDaysFromEpoch());
		assertEquals(m.after(1).getDaysFromEpoch(), k.getDaysFromEpoch());
		assertEquals(k.after(-1).getDaysFromEpoch(), m.getDaysFromEpoch());
	}

	public void testAD17520914After() {
		AnnoDomini k = c1(1752, 9, 14);
		AnnoDomini m = c1(1752, 9, 2);

		assertEquals(m.getDaysFromUnixEpoch() + 1, k.getDaysFromUnixEpoch());
		assertEquals(m.getDaysFromEpoch() + 1, k.getDaysFromEpoch());
		assertEquals(m.after(1).getDaysFromEpoch(), k.getDaysFromEpoch());
		assertEquals(k.after(-1).getDaysFromEpoch(), m.getDaysFromEpoch());
	}

	public void testAD17520902After() {
		AnnoDomini k = c1(1752, 9, 2);
		AnnoDomini m = c1(1752, 9, 1);

		assertEquals(m.getDaysFromUnixEpoch() + 1, k.getDaysFromUnixEpoch());
		assertEquals(m.getDaysFromEpoch() + 1, k.getDaysFromEpoch());
		assertEquals(m.after(1).getDaysFromEpoch(), k.getDaysFromEpoch());
		assertEquals(k.after(-1).getDaysFromEpoch(), m.getDaysFromEpoch());
	}

	public void testInterval() {
		AnnoDomini k = c1(1752, 9, 2);
		AnnoDomini m = c1(1752, 9, 1);

		assertEquals(k.interval(m), 1);
		assertEquals(m.interval(k), -1);
	}

	public void testInterval2() {
		AnnoDomini k = c1(1752, 9, 14);
		AnnoDomini m = c1(1752, 9, 2);

		assertEquals(k.interval(m), 1);
		assertEquals(m.interval(k), -1);
	}

	public void testAD00010101() {
		AnnoDomini k = c1(1, 1, 3);

		assertEquals(k.getDaysFromEpoch(), 0);
		assertEquals(k.getYear(), 1);
		assertEquals(k.getDayOfYear(), 3);
		assertEquals(k.getMaximumDaysOfYear(), 365);
		assertEquals(k.getDayOfMonth(), 3);
		assertEquals(k.getMaximumDaysOfMonth(), 31);
		assertEquals(k.getMonth(), 1);
		assertEquals(k.getDayOfWeek(), 1);
	}

	public void testBC47130101() {
		AnnoDomini k = c1(-4713, 1, 1);

		assertEquals(k.getJulianDay(), 0);
	}

	public void testAD18581117() {
		AnnoDomini k = c1(1858, 11, 17);

		assertEquals(k.getModifiedJulianDay(), 0);
	}

	public void testAD19700101_2() {
		AnnoDomini k = c2(0);

		assertEquals(k.getDate(), 0);
	}

	public void testAD00010101_2() {
		AnnoDomini m = c2(new GregorianCalendar(
				1, 0, 1, 0, 0, 0).getTimeInMillis());
		AnnoDomini k = c1(1, 1, 1);

		assertEquals(k.getDaysFromUnixEpoch(), m.getDaysFromUnixEpoch());
	}

	public void testCalendarErrors() {
		e1(0, 1, 1);
		e1(1752, 9, 3);  e1(1752, 9, 13);
	}

}
