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
public class SimpleGregorianCalendar extends AbstractDay
implements WeekSolarCalendar {

	//
	private static final int DAYS_400YEARS = 146097;
	private static final int DAYS_100YEARS = 36524;
	private static final int DAYS_4YEARS = 366 + 365 * 3;
	private static final int DAYS_TO_UNIX_EPOCH = 719162;
	private static final long DAY_MILLISECS = 24 * 60 * 60 * 1000;

	private static final int[] LMAX_DAYS_OF_MONTH = new int[] {
		31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
	};

	private static final int[] LMAX_DAYS_OF_MONTH_L = new int[] {
		31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
	};

	private static final int[] ACC_LMAX_DAYS_OF_MONTH = new int[] {
		31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365
	};

	private static final int[] ACC_LMAX_DAYS_OF_MONTH_L = new int[] {
		31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366
	};

	// days from AD1/1/1
	private int days;

	/**
	 * 
	 * @param year
	 * @param month
	 * @param day
	 */
	public SimpleGregorianCalendar(int year, int month, int day) {
		int[] p, a;
		int y, my, ny, oy, py, qy, ry;

		if(year == 0)  throw new IllegalArgumentException();
		y  = (year < 0) ? year : year - 1;
		my = div(y,  400);
		ny = mod(y,  400);
		oy = div(ny, 100);
		py = mod(ny, 100);
		qy = div(py, 4);
		ry = mod(py, 4);
		p  = isLeapNormalizedYear(y + 1) ?
				LMAX_DAYS_OF_MONTH_L : LMAX_DAYS_OF_MONTH;
		a  = isLeapNormalizedYear(y + 1) ?
				ACC_LMAX_DAYS_OF_MONTH_L : ACC_LMAX_DAYS_OF_MONTH;

		if(month < 1 || month > 12) {
			throw new IllegalArgumentException();
		} else if(day < 1 || day > p[month - 1]) {
			throw new IllegalArgumentException();
		}
		days  = my * DAYS_400YEARS;
		days += oy * DAYS_100YEARS;
		days += qy * DAYS_4YEARS;
		days += ry * 365;
		days += (month < 2) ? 0 : a[month - 2];
		days += day - 1;
	}

	/**
	 * 
	 * @param days
	 */
	public SimpleGregorianCalendar(int days) {
		this.days = days;
	}

	/**
	 * 
	 * @param time
	 * @return
	 */
	public static SimpleGregorianCalendar getInstaceFromTime(
			long time) {
		int t = (int)div(time, DAY_MILLISECS);

		return new SimpleGregorianCalendar(t + DAYS_TO_UNIX_EPOCH);
	}

	@Override
	public int getDaysFromEpoch() {
		return days;
	}

	@Override
	public long getDate() {
		return (days - DAYS_TO_UNIX_EPOCH) * DAY_MILLISECS;
	}

	static int div(int x, int y) {
		return (x < 0) ? (x - y + 1) / y : x / y;
	}

	static long div(long x, long y) {
		return (x < 0) ? (x - y + 1) / y : x / y;
	}

	static int mod(int x, int y) {
		int r;

		r = x % y;
		return (r < 0) ? r + y : r;
	}

	private static int getYear100(int d) {
		int x, y;

		x = div(d, DAYS_4YEARS);
		y = mod(d, DAYS_4YEARS);
		if(y < 365) {
			return x * 4 + 0;
		} else if(y < 365 * 2) {
			return x * 4 + 1;
		} else if(y < 365 * 3) {
			return x * 4 + 2;
		} else {
			return x * 4 + 3;
		}
	}

	/**
	 * 
	 * @return
	 */
	private int getNormalizedYear() {
		int x, y, z, w;

		x = div(days, DAYS_400YEARS);
		y = mod(days, DAYS_400YEARS);
		z = div(y,    DAYS_100YEARS);
		w = mod(y,    DAYS_100YEARS);
		if(z < 4) {
			return x * 400 + z * 100 + getYear100(w) + 1;
		} else {
			return x * 400 + 400;
		}
	}

	
	public int getYear() {
		int r = getNormalizedYear();

		return (r > 0) ? r : r - 1;
	}

	private static int getDayOfYear100(int d) {
		int y;

		y = mod(d, DAYS_4YEARS);
		return ((y < 365 * 3) ? mod(y, 365) : y - 365 * 3) + 1;
	}

	/**
	 * 
	 * @return
	 */
	public int getDayOfYear() {
		int y, z, w;

		y = mod(days, DAYS_400YEARS);
		z = div(y,    DAYS_100YEARS);
		w = mod(y,    DAYS_100YEARS);
		return (z < 4) ? getDayOfYear100(w) : 365;
	}

	/**
	 * 
	 * @return
	 */
	public int getMaximumDaysOfYear() {
		return isLeapYear() ? 366 : 365;
	}

	//
	private static boolean isLeapNormalizedYear(int y) {
		return (y % 400 == 0) || (y % 4 == 0 && y % 100 != 0);
	}

	/**
	 * 
	 * @return
	 */
	public boolean isLeapYear() {
		return isLeapNormalizedYear(getNormalizedYear());
	}

	/**
	 * 
	 * @return
	 */
	public int getDayOfMonth() {
		int[] l = isLeapYear() ? 
				LMAX_DAYS_OF_MONTH_L : LMAX_DAYS_OF_MONTH;
		int d = getDayOfYear() - 1;

		for(int i = 0; i < l.length; i++) {
			if(d < l[i])  return d + 1;
			d -= l[i];
		}
		throw new RuntimeException();
	}

	/**
	 * 
	 * @return
	 */
	public int getMaximumDaysOfMonth() {
		int[] l = isLeapYear() ?
				ACC_LMAX_DAYS_OF_MONTH_L : ACC_LMAX_DAYS_OF_MONTH;
		int[] r = isLeapYear() ?
				LMAX_DAYS_OF_MONTH_L : LMAX_DAYS_OF_MONTH;
		int d = getDayOfYear() - 1;

		for(int i = 0; i < l.length; i++) {
			if(d < l[i])  return r[i];
		}
		throw new RuntimeException();
	}

	public int getMonth() {
		int[] l = isLeapYear() ? 
				LMAX_DAYS_OF_MONTH_L : LMAX_DAYS_OF_MONTH;
		int d = getDayOfYear() - 1;

		for(int i = 0; i < l.length; i++) {
			if(d < l[i])  return i + 1;
			d -= l[i];
		}
		throw new RuntimeException();
	}

	/**
	 * 
	 * @return
	 */
	public int getDayOfWeek() {
		int r = (days + 1) % 7;   // BC45/1/1 is Friday

		return (r < 0) ? r + 7 : r;
	}

	/* (non-Javadoc)
	 * @see test.cal.WeekSolarCalendar#after(int)
	 */
	@Override
	public SimpleGregorianCalendar after(int days) {
		return new SimpleGregorianCalendar(this.days + days);
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
		b.append(" (Gregorian)");
		return b.toString();
	}

}
