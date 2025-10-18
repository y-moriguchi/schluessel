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

public class MesoamericanLongCountCalendarTest extends TestCase {

	//
	static MesoamericanLongCountCalendar c1(int y, int m, int d) {
		return new MesoamericanLongCountCalendar(
				new GregorianCalendar(y, m - 1, d, 0, 0, 0)
				.getTimeInMillis());
	}

	//
	static void e1(int a, int b, int y, int m, int d) {
		try {
			new MesoamericanLongCountCalendar(a, b, y, m, d);
			fail();
		} catch(IllegalArgumentException e) {}
	}

	public void testBC31140906() {
		MesoamericanLongCountCalendar x = c1(-3113, 9, 7);

		assertEquals(x.getDaysFromEpoch(), 0);
		assertEquals(x.toString(), "0.0.0.0.0");
	}

	public void testAD19700101() {
		MesoamericanLongCountCalendar x = c1(1970, 1, 1);

		assertEquals(x.getDate(), 0);
	}

	public void testAD20121221() {
		MesoamericanLongCountCalendar x = c1(2012, 12, 21);

		assertEquals(x.getBaktun(), 12);
		assertEquals(x.getKatun(),  19);
		assertEquals(x.getTun(),    19);
		assertEquals(x.getWinal(),  17);
		assertEquals(x.getKin(),    19);
		assertEquals(x.toString(), "12.19.19.17.19");
	}

	public void testAD20121222() {
		MesoamericanLongCountCalendar x = c1(2012, 12, 22);

		assertEquals(x.toString(), "0.0.0.0.0");
	}

	public void testAD17520915After() {
		MesoamericanLongCountCalendar k = c1(1752, 9, 15);
		MesoamericanLongCountCalendar m = c1(1752, 9, 14);

		assertEquals(m.getDaysFromUnixEpoch() + 1, k.getDaysFromUnixEpoch());
		assertEquals(m.getDaysFromEpoch() + 1, k.getDaysFromEpoch());
		assertEquals(m.after(0).getDaysFromEpoch(), m.getDaysFromEpoch());
		assertEquals(m.after(1).getDaysFromEpoch(), k.getDaysFromEpoch());
		assertEquals(k.after(-1).getDaysFromEpoch(), m.getDaysFromEpoch());
	}

	public void testError() {
		e1(14,  0,  0,  0,  0);  e1(-1,  0,  0,  0,  0);
		e1( 0, 20,  0,  0,  0);  e1( 0, -1,  0,  0,  0);
		e1( 0,  0, 20,  0,  0);  e1( 0,  0, -1,  0,  0);
		e1( 0,  0,  0, 18,  0);  e1( 0,  0,  0, -1,  0);
		e1( 0,  0,  0,  0, 20);  e1( 0,  0,  0,  0, -1);
	}

}
