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
public class SimpleJulianCalendar extends AbstractDay
implements WeekSolarCalendar {

	//
	private static final int DAYS_4YEARS = 366 + 365 * 3;
	private static final int AD_1 = 44;
	private static final int DAYS_TO_UNIX_EPOCH = 735601;
	private static final long DAY_MILLISECS = 24 * 60 * 60 * 1000;

	private static final int[] DAYS_YEARS = new int[] {
		0, 366, 366 + 365, 366 + 365 * 2
	};

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

	/**
	 * 
	 * @param year
	 * @param month
	 * @param day
	 */
	public SimpleJulianCalendar(int year, int month, int day) {
		int[] p, a;
		int y, my, ay;

		if(year == 0)  throw new IllegalArgumentException();
		y  = (year < 0) ? AD_1 + year + 1 : year + AD_1;
		my = y % 4;
		my = (my < 0) ? my + 4 : my;
		ay = (y < 0) ? (y - 3) / 4 : y / 4;
		p  = (my > 0) ? LMAX_DAYS_OF_MONTH : LMAX_DAYS_OF_MONTH_L;
		a  = (my > 0) ?
				ACC_LMAX_DAYS_OF_MONTH : ACC_LMAX_DAYS_OF_MONTH_L;

		if(month < 1 || month > 12) {
			throw new IllegalArgumentException();
		} else if(day < 1 || day > p[month - 1]) {
			throw new IllegalArgumentException();
		}
		days  = ay * DAYS_4YEARS + DAYS_YEARS[my];
		days += (month < 2) ? 0 : a[month - 2];
		days += day - 1;
	}

	/**
	 * 
	 * @param days
	 */
	public SimpleJulianCalendar(int days) {
		this.days = days;
	}

	/**
	 * 
	 * @param time
	 * @return
	 */
	public static SimpleJulianCalendar getInstaceFromTime(long time) {
		int t = (int)SimpleGregorianCalendar.div(time, DAY_MILLISECS);

		return new SimpleJulianCalendar(t + DAYS_TO_UNIX_EPOCH);
	}

	// days from BC45/1/1
	private int days;

	@Override
	public int getDaysFromEpoch() {
		return days;
	}

	@Override
	public long getDate() {
		return (days - DAYS_TO_UNIX_EPOCH) * DAY_MILLISECS;
	}

	private static int toyear(int y) {
		return (y > AD_1) ? y - AD_1 : y - AD_1 - 1;
	}

	/**
	 * 
	 * @return
	 */
	public int getYear() {
		int x = days / DAYS_4YEARS, y;

		x = (days < 0) ?
				(days - DAYS_4YEARS - 1) / DAYS_4YEARS :
					days / DAYS_4YEARS;
		y = days % DAYS_4YEARS;
		y = (y < 0) ? y + DAYS_4YEARS : y;
		if(y < 366) {
			return toyear(x * 4);
		} else if(y < 366 + 365) {
			return toyear(x * 4 + 1);
		} else if(y < 366 + 365 * 2) {
			return toyear(x * 4 + 2);
		} else {
			return toyear(x * 4 + 3);
		}
	}

	/**
	 * 
	 * @return
	 */
	public int getDayOfYear() {
		int y;

		y = days % DAYS_4YEARS;
		y = (y < 0) ? y + DAYS_4YEARS : y;
		if(y < 366) {
			return y + 1;
		} else if(y < 366 + 365) {
			return y - 366 + 1;
		} else if(y < 366 + 365 * 2) {
			return y - 366 - 365 + 1;
		} else {
			return y - 366 - 365 * 2 + 1;
		}
	}

	/**
	 * 
	 * @return
	 */
	public int getMaximumDaysOfYear() {
		int d = days % DAYS_4YEARS;

		d = (d < 0) ? d + DAYS_4YEARS : d;
		return (d < 366) ? 366 : 365;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isLeapYear() {
		return getMaximumDaysOfYear() == 366;
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
		int r = (days + 5) % 7;   // BC45/1/1 is Friday

		return (r < 0) ? r + 7 : r;
	}

	/* (non-Javadoc)
	 * @see test.cal.WeekSolarCalendar#after(int)
	 */
	@Override
	public SimpleJulianCalendar after(int days) {
		return new SimpleJulianCalendar(this.days + days);
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
		b.append(" (Julian)");
		return b.toString();
	}

}
