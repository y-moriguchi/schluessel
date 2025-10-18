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

import net.morilib.util.BitUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/28
 */
public class CJKVOldEra implements CJKVEra {

	//
	private static final SimpleDateFormat FMT =
		new SimpleDateFormat("yyyy/MM/dd");
	private static final long DAY_IN_MILLS = 1000 * 60 * 60 * 24;

	//
	private long newYearEpoch, epoch, end;
	private byte[]  intercalary;
	private short[] shonotsuki;
	private String desc;

	/**
	 * 
	 * @param newYearEpoch
	 * @param epoch
	 * @param end
	 * @param leapMonths
	 */
	public CJKVOldEra(String desc,
			long newYearEpoch, long epoch, long end,
			int[]... intercalary) {
		if(newYearEpoch > epoch) {
			throw new IllegalArgumentException();
		} else if(epoch > end) {
			throw new IllegalArgumentException();
		}

		this.newYearEpoch = newYearEpoch;
		this.epoch = epoch;
		this.end = end;
		this.intercalary = new byte[intercalary.length];
		this.shonotsuki  = new short[intercalary.length];

		for(int i = 0; i < intercalary.length; i++) {
			this.intercalary[i] = (byte)intercalary[i][0];
			this.shonotsuki[i]  = (short)intercalary[i][1];
		}
	}

	/**
	 * 
	 * @param newYearEpoch
	 * @param epoch
	 * @param end
	 * @param leapMonths
	 */
	public CJKVOldEra(String desc,
			java.util.Date newYearEpoch,
			java.util.Date epoch,
			java.util.Date end,
			int[]... intercalary) {
		this(desc, newYearEpoch.getTime(), epoch.getTime(),
				end.getTime(), intercalary);
	}

	/**
	 * 
	 * @param newYearEpoch
	 * @param epoch
	 * @param end
	 * @param leapMonths
	 */
	public CJKVOldEra(String desc,
			String newYearEpoch, String epoch, String end,
			int[]... intercalary) {
		this(desc, _p(newYearEpoch), _p(epoch), _p(end), intercalary);
	}

	//
	private static long _p(String ymd) {
		try {
			if(ymd == null) {
				throw new NullPointerException();
			}
			return FMT.parse(ymd).getTime();
		} catch (ParseException e) {
			throw new RuntimeException(e);
		}
	}

	public java.util.Date getNewYearEpoch() {
		return new java.util.Date(newYearEpoch);
	}

	public java.util.Date getEpoch() {
		return new java.util.Date(epoch);
	}

	public java.util.Date getEnd() {
		return new java.util.Date(end);
	}

	public int getYears() {
		return intercalary.length;
	}

	public int getMonths(int year) {
		if(year < 1 || year > getYears()) {
			throw new IllegalArgumentException();
		}
		return intercalary[year - 1] == 0 ? 12 : 13;
	}

	/* (non-Javadoc)
	 * @see net.morilib.calendar.cjkv.CJKVEra#getDaysOfYear(int)
	 */
	public int getDaysOfYear(int year) {
		if(year < 1 || year > getYears()) {
			throw new IllegalArgumentException(year + "");
		}
		return (((intercalary[year - 1] == 0) ? 360 : 390) -
				BitUtils.countBit(shonotsuki[year - 1]));
	}

	public int getLeapMonth(int year) {
		if(year < 1 || year > getYears()) {
			throw new IllegalArgumentException();
		}
		return intercalary[year - 1];
	}

	public boolean isEra(java.util.Date d) {
		return epoch <= d.getTime() && end >= d.getTime();
	}

	public int getDaysOfMonth(int year, int month) {
		if(year < 1 || year > getYears() ||
				month < 1 || month > getMonths(year)) {
			throw new IllegalArgumentException();
		}
		return (shonotsuki[year - 1] & (1 << (month - 1))) == 0 ?
				30 : 29;
	}

	public int getYear(java.util.Date d) {
		int dy = getDaysFromNewYearEpoch(d);
		int i = 0;

		for(; dy >= 0; dy -= getDaysOfYear(++i));
		return i;
	}

	public int getDaysFromNewYearEpoch(java.util.Date d) {
		if(isEra(d)) {
			return (int)((d.getTime() - newYearEpoch) / DAY_IN_MILLS);
		} else {
			return -1;
		}
	}

	public int getDaysFromEpoch(java.util.Date d) {
		if(isEra(d)) {
			return (int)((d.getTime() - epoch) / DAY_IN_MILLS);
		} else {
			return -1;
		}
	}

	public boolean after(java.util.Date d) {
		return getEpoch().after(d);
	}

	public boolean before(java.util.Date d) {
		return getEnd().before(d);
	}

	public String getInitialLetter() {
		return null;
	}

	public String getShortDescription() {
		return null;
	}

	public String toString() {
		return desc;
	}

}
