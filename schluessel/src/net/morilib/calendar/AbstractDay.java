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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/22
 */
public abstract class AbstractDay implements Day {

	//
	private static final long DAY_MILLISECS = 24 * 60 * 60 * 1000;

	/* (non-Javadoc)
	 * @see net.morilib.calendar.Calendar2#getDaysFromUnixEpoch()
	 */
	@Override
	public int getDaysFromUnixEpoch() {
		return (int)SimpleGregorianCalendar.div(
				getDate(), DAY_MILLISECS);
	}

	/* (non-Javadoc)
	 * @see net.morilib.calendar.Calendar2#interval(net.morilib.calendar.Calendar2)
	 */
	@Override
	public int interval(Day c) {
		return getDaysFromUnixEpoch() - c.getDaysFromUnixEpoch();
	}

	@Override
	public int compareTo(Day o) {
		long x = getDate(), y = o.getDate();

		return (x < y) ? -1 : (x > y) ? 1 : 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.calendar.Calendar2#getJulianDate()
	 */
	@Override
	public int getJulianDay() {
		return getDaysFromUnixEpoch() + 2440588;
	}

	/* (non-Javadoc)
	 * @see net.morilib.calendar.Calendar2#getJulianDate()
	 */
	@Override
	public int getModifiedJulianDay() {
		return getDaysFromUnixEpoch() + 40587;
	}

	/* (non-Javadoc)
	 * @see net.morilib.calendar.Calendar2#getJulianDate()
	 */
	@Override
	public int getLilianDay() {
		return getDaysFromUnixEpoch() + 2440588 - 2299161;
	}

}
