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
package net.morilib.lisp.datetime;

import java.util.Calendar;
import java.util.GregorianCalendar;

import net.morilib.lisp.Datum;
import net.morilib.lisp.JavaObjective;
import net.morilib.util.datetime.JulianDay;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/09
 */
public class LispDate extends Datum
implements JavaObjective, java.io.Serializable {

	//
	private Calendar calendar;

	/**
	 * 
	 */
	public LispDate() {
		calendar = new GregorianCalendar();
	}

	/**
	 * 
	 * @param offsetSecond
	 */
	public LispDate(int offsetSecond) {
		calendar = new GregorianCalendar();
		calendar.set(Calendar.ZONE_OFFSET, offsetSecond * 1000);
	}

	/**
	 * 
	 * @param offsetSecond
	 */
	public LispDate(java.util.Date d) {
		calendar = new GregorianCalendar();
		calendar.setTime(d);
	}

	/**
	 * 
	 * @param time
	 */
	public LispDate(long time) {
		calendar = new GregorianCalendar();
		calendar.setTimeInMillis(time);
	}

	/**
	 * 
	 * @param time
	 */
	public LispDate(long time, int offsetSecond) {
		calendar = new GregorianCalendar();
		calendar.setTimeInMillis(time);
		calendar.set(Calendar.ZONE_OFFSET, offsetSecond * 1000);
	}

	//
	/*package*/ LispDate(LispTime t) {
		this(t.time);
	}

	//
	/*package*/ LispDate(LispTime t, int offsetSecond) {
		this(t.toMonotonicTime().time, offsetSecond);
	}

	/**
	 * 
	 * @param offsetSecond
	 */
	public LispDate(java.util.Date d, int offsetSecond) {
		calendar = new GregorianCalendar();
		calendar.setTime(d);
		calendar.set(Calendar.ZONE_OFFSET, offsetSecond * 1000);
	}

	/**
	 * 
	 * @param nanosecond
	 * @param second
	 * @param minute
	 * @param hour
	 * @param day
	 * @param month
	 * @param year
	 * @param zoneOffset
	 */
	public LispDate(
			int nanosecond,
			int second,
			int minute,
			int hour,
			int day,
			int month,
			int year,
			int zoneOffset) {
		calendar = new GregorianCalendar();
		calendar.set(Calendar.MILLISECOND, nanosecond / 1000000);
		calendar.set(Calendar.SECOND, second);
		calendar.set(Calendar.MINUTE, minute);
		calendar.set(Calendar.HOUR_OF_DAY, hour);
		calendar.set(Calendar.DAY_OF_MONTH, day);
		calendar.set(Calendar.MONTH, month - 1);
		calendar.set(Calendar.YEAR, year);
		calendar.set(Calendar.ZONE_OFFSET, zoneOffset * 1000);
	}

	/**
	 * 
	 * @return
	 */
	public JulianDay getJulianDay() {
		return new JulianDay(calendar.getTimeInMillis());
	}

	/**
	 * 
	 * @return
	 */
	public int getNanosecond() {
		return calendar.get(Calendar.MILLISECOND) * 1000000;
	}

	/**
	 * 
	 * @return
	 */
	public int getSecond() {
		return calendar.get(Calendar.SECOND);
	}

	/**
	 * 
	 * @return
	 */
	public int getMinute() {
		return calendar.get(Calendar.MINUTE);
	}

	/**
	 * 
	 * @return
	 */
	public int getHour() {
		return calendar.get(Calendar.HOUR_OF_DAY);
	}

	/**
	 * 
	 * @return
	 */
	public int getDay() {
		return calendar.get(Calendar.DAY_OF_MONTH);
	}

	/**
	 * 
	 * @return
	 */
	public int getMonth() {
		return calendar.get(Calendar.MONTH) + 1;
	}

	/**
	 * 
	 * @return
	 */
	public int getYear() {
		return calendar.get(Calendar.YEAR);
	}

	/**
	 * 
	 * @return
	 */
	public int getZoneOffset() {
		return calendar.get(Calendar.ZONE_OFFSET) / 1000;
	}

	/**
	 * 
	 * @return
	 */
	public int getDayOfYear() {
		return calendar.get(Calendar.DAY_OF_YEAR);
	}

	/**
	 * 
	 * @return
	 */
	public int getDayOfWeek() {
		return calendar.get(Calendar.DAY_OF_WEEK) - 1;
	}

	/**
	 * 
	 * @return
	 */
	public int getWeekOfYear(int firstDay) {
		Calendar cal = (Calendar)calendar.clone();

		cal.setFirstDayOfWeek(firstDay + 1);
		cal.setMinimalDaysInFirstWeek(7);
		return cal.get(Calendar.WEEK_OF_YEAR);
	}

	/**
	 * 
	 * @return
	 */
	public java.util.Date getDate() {
		return new java.util.Date(calendar.getTimeInMillis());
	}

	/**
	 * 
	 * @return
	 */
	public long getTime() {
		return calendar.getTimeInMillis();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.JavaObjective#toObject()
	 */
	public Object toObject() {
		return getDate();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<date ").append(calendar.getTime().toString())
		.append(">");
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		return (int)calendar.getTimeInMillis();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof LispDate) {
			return getTime() == ((LispDate)obj).getTime();
		}
		return false;
	}

}
