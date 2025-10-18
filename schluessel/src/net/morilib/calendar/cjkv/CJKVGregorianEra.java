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
package net.morilib.calendar.cjkv;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.GregorianCalendar;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/28
 */
public class CJKVGregorianEra implements CJKVEra {

	//
	private static final SimpleDateFormat FMT =
		new SimpleDateFormat("yyyy/MM/dd");

	//
	private Calendar newYearEpoch, epoch, end;
	private String   desc, init;

	/**
	 * 
	 * @param newYearEpoch
	 * @param epoch
	 * @param end
	 */
	public CJKVGregorianEra(String desc,
			String letter, java.util.Date newYearEpoch,
			java.util.Date epoch, java.util.Date end) {
		if(newYearEpoch == null || epoch == null || desc == null) {
			throw new NullPointerException();
		}

		this.newYearEpoch = new GregorianCalendar();
		this.epoch = new GregorianCalendar();
		this.newYearEpoch.setTime(newYearEpoch);
		this.epoch.setTime(epoch);
		if(end != null) {
			this.end = new GregorianCalendar();
			this.end.setTime(end);
		} else {
			this.end = null;
		}
		this.desc = desc;
		this.init = letter;
	}

	public CJKVGregorianEra(String desc,
			String letter, String newYearEpoch,
			String epoch, String end) {
		this(desc, letter, _p(newYearEpoch), _p(epoch), _p(end));
	}

	//
	private static java.util.Date _p(String ymd) {
		try {
			return (ymd == null) ? null : FMT.parse(ymd);
		} catch (ParseException e) {
			throw new RuntimeException(e);
		}
	}

	public java.util.Date getEnd() {
		return (end == null) ? null : end.getTime();
	}

	public java.util.Date getEpoch() {
		return epoch.getTime();
	}

	public int getLeapMonth(int year) {
		return 0;
	}

	public java.util.Date getNewYearEpoch() {
		return newYearEpoch.getTime();
	}

	public int getYears() {
		if(end == null) {
			return Integer.MAX_VALUE;
		}
		return end.get(Calendar.YEAR) - epoch.get(Calendar.YEAR) + 1;
	}

	public boolean isEra(java.util.Date d) {
		if(end == null) {
			return !epoch.getTime().after(d);
		}
		return !(epoch.getTime().after(d) || end.getTime().before(d));
	}

	public int getMonths(int year) {
		if(year < 1) {
			throw new IllegalArgumentException();
		}
		return 12;
	}

	public int getDaysOfMonth(int year, int month) {
		Calendar cal = (Calendar)newYearEpoch.clone();

		cal.add(Calendar.YEAR, year - 1);
		cal.set(Calendar.MONTH, month - 1);
		return cal.getActualMaximum(Calendar.DAY_OF_MONTH);
	}

	/* (non-Javadoc)
	 * @see net.morilib.calendar.cjkv.CJKVEra#getDaysOfYear(int)
	 */
	public int getDaysOfYear(int year) {
		Calendar cal;

		if(year > 1 && year < getYears()) {
			cal = (Calendar)newYearEpoch.clone();
			cal.add(Calendar.YEAR, year - 1);
			return cal.getActualMaximum(Calendar.DAY_OF_YEAR);
		} else if(year == 1) {
			return (epoch.getActualMaximum(Calendar.DAY_OF_YEAR) -
					epoch.get(Calendar.DAY_OF_YEAR) + 1);
		} else if(year == getYears()) {
			return end.get(Calendar.DAY_OF_YEAR);
		} else {
			return -1;
		}
	}

	public int getYear(java.util.Date d) {
		Calendar cal = new GregorianCalendar();
		int y;

		cal.setTime(d);
		y = cal.get(Calendar.YEAR) - epoch.get(Calendar.YEAR);
		return isEra(d) ? y + 1 : -1;
	}

	public int getDaysFromNewYearEpoch(java.util.Date d) {
		Calendar cal = (Calendar)newYearEpoch.clone();
		Calendar c2  = new GregorianCalendar();
		final int Y = Calendar.YEAR;
		int r = 0;

		if(isEra(d)) {
			c2.setTime(d);
			for(int i = cal.get(Y); i < c2.get(Y); i++) {
				r += cal.getActualMaximum(Calendar.DAY_OF_YEAR);
			}
			return r + c2.get(Calendar.DAY_OF_YEAR) - 1;
		} else {
			return -1;
		}
	}

	public int getDaysFromEpoch(java.util.Date d) {
		if(isEra(d)) {
			return (getDaysFromNewYearEpoch(d) -
					epoch.get(Calendar.DAY_OF_YEAR) + 1);
		} else {
			return -1;
		}
	}

	public boolean after(java.util.Date d) {
		return getEpoch().after(d);
	}

	public boolean before(java.util.Date d) {
		return end != null && getEnd().before(d);
	}

	public String getInitialLetter() {
		return init;
	}

	public String getShortDescription() {
		return desc.substring(0, 1);
	}

	public String toString() {
		return desc;
	}

}
