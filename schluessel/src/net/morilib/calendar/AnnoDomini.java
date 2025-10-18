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
 *
 * @author MORIGUCHI, Yuichiro 2012/12/22
 */
public class AnnoDomini extends AbstractDay
implements WeekSolarCalendar {

	//
	private static final int DAYS_TO_UNIX_EPOCH = 719162;
	private static final int AD010101_JULIAN = 16439;
	private static final long DAY_MILLISECS = 24 * 60 * 60 * 1000;
	private static final SimpleGregorianCalendar GREGORIAN_EPOCH =
		new SimpleGregorianCalendar(1752, 9, 14);
	private static final int GREGORIAN_EPOCH_DAYS =
		GREGORIAN_EPOCH.getDaysFromEpoch();
	private static final int LAST_JULIAN_DAYS =
		GREGORIAN_EPOCH_DAYS - 1;
	private static final SimpleJulianCalendar LAST_JULIAN =
		new SimpleJulianCalendar(LAST_JULIAN_DAYS + AD010101_JULIAN);

	//
	private WeekSolarCalendar cal;

	/**
	 * 
	 * @param days
	 */
	public AnnoDomini(int days) {
		if(days < GREGORIAN_EPOCH_DAYS) {
			cal = new SimpleJulianCalendar(days + AD010101_JULIAN);
		} else {
			cal = new SimpleGregorianCalendar(days);
		}
	}

	/**
	 * 
	 * @param time
	 * @return
	 */
	public static AnnoDomini getInstaceFromTime(long time) {
		int t = (int)SimpleGregorianCalendar.div(time, DAY_MILLISECS);

		return new AnnoDomini(t + DAYS_TO_UNIX_EPOCH);
	}

	/**
	 * 
	 * @param year
	 * @param month
	 * @param day
	 * @return
	 */
	public AnnoDomini(int year, int month, int day) {
		if(year < GREGORIAN_EPOCH.getYear()) {
			cal = new SimpleJulianCalendar(year, month, day);
		} else if(year > GREGORIAN_EPOCH.getYear()) {
			cal = new SimpleGregorianCalendar(year, month, day);
		} else if(month < GREGORIAN_EPOCH.getMonth()) {
			cal = new SimpleJulianCalendar(year, month, day);
		} else if(month > GREGORIAN_EPOCH.getMonth()) {
			cal = new SimpleGregorianCalendar(year, month, day);
		} else if(day <= LAST_JULIAN.getDayOfMonth()) {
			cal = new SimpleJulianCalendar(year, month, day);
		} else if(day >= GREGORIAN_EPOCH.getDayOfMonth()) {
			cal = new SimpleGregorianCalendar(year, month, day);
		} else {
			throw new IllegalArgumentException();
		}
	}

	@Override
	public WeekSolarCalendar after(int days) {
		return new AnnoDomini(getDaysFromEpoch() + days);
	}

	@Override
	public int getDayOfWeek() {
		return cal.getDayOfWeek();
	}

	@Override
	public int getDayOfMonth() {
		return cal.getDayOfMonth();
	}

	@Override
	public int getDayOfYear() {
		return cal.getDayOfYear();
	}

	@Override
	public int getMaximumDaysOfMonth() {
		if(cal.getYear() == GREGORIAN_EPOCH.getYear() &&
				cal.getMonth() == GREGORIAN_EPOCH.getMonth()) {
			return GREGORIAN_EPOCH.getMaximumDaysOfMonth();
		} else {
			return cal.getMaximumDaysOfMonth();
		}
	}

	@Override
	public int getMaximumDaysOfYear() {
		return cal.getMaximumDaysOfYear();
	}

	@Override
	public int getMonth() {
		return cal.getMonth();
	}

	@Override
	public int getYear() {
		return cal.getYear();
	}

	@Override
	public boolean isLeapYear() {
		return cal.isLeapYear();
	}

	@Override
	public long getDate() {
		return cal.getDate();
	}

	@Override
	public int getDaysFromEpoch() {
		return getDaysFromUnixEpoch() + DAYS_TO_UNIX_EPOCH;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer b = new StringBuffer();
		int y = getYear();

		if(y < 0) {
			y = -y;
			b.append("BC");
		}
		b.append(y).append("/").append(getMonth());
		b.append("/").append(getDayOfMonth());
		return b.toString();
	}

}
