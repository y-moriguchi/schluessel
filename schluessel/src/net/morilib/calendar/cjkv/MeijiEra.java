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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/28
 */
/*package*/ final class MeijiEra implements CJKVEra {

	//
	private static final SimpleDateFormat FMT =
		new SimpleDateFormat("yyyy/MM/dd");
	private static final CJKVOldEra
	OLD_MEIJI = new CJKVOldEra("明治",
			"1868/01/25", "1868/01/25", "1872/12/31",
			new int[] { 4,  0x12d9 },
			new int[] { 0,  0x02d8 },
			new int[] { 10, 0x16a9 },
			new int[] { 0,  0x06a4 },
			new int[] { 0,  0x0525 });
	private static final CJKVGregorianEra
	NEW_MEIJI = new CJKVGregorianEra("明治", "M",
			"1868/01/01", "1868/01/25", "1912/07/30");
	private static final java.util.Date CHANGE_GREGORIAN_EPOCH =
		_p("1873/01/01");

	//
	MeijiEra() {}

	//
	private static java.util.Date _p(String ymd) {
		try {
			return (ymd == null) ? null : FMT.parse(ymd);
		} catch (ParseException e) {
			throw new RuntimeException(e);
		}
	}

	public java.util.Date getEnd() {
		return NEW_MEIJI.getEnd();
	}

	public java.util.Date getEpoch() {
		return OLD_MEIJI.getEpoch();
	}

	public int getLeapMonth(int year) {
		if(year < 1) {
			throw new IllegalArgumentException();
		} else if(year <= 5) {
			return OLD_MEIJI.getLeapMonth(year);
		} else {
			return 0;
		}
	}

	public java.util.Date getNewYearEpoch() {
		return OLD_MEIJI.getNewYearEpoch();
	}

	public int getYears() {
		return 45;
	}

	public boolean isEra(java.util.Date d) {
		return NEW_MEIJI.isEra(d);
	}

	public int getMonths(int year) {
		if(year < 1) {
			throw new IllegalArgumentException();
		} else if(year <= 5) {
			return OLD_MEIJI.getMonths(year);
		} else {
			return 12;
		}
	}

	public int getDaysOfMonth(int year, int month) {
		if(year < 1 || month < 1 || month > 12) {
			throw new IllegalArgumentException();
		} else if(year == 5 && month == 12) {
			return 2;
		} else if(year <= 5) {
			return OLD_MEIJI.getDaysOfMonth(year, month);
		} else {
			return NEW_MEIJI.getDaysOfMonth(year, month);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.calendar.cjkv.CJKVEra#getDaysOfYear(int)
	 */
	public int getDaysOfYear(int year) {
		if(year < 1) {
			throw new IllegalArgumentException();
		} else if(year < 5) {
			return OLD_MEIJI.getDaysOfYear(year);
		} else if(year == 5) {
			return 332;
		} else {
			return NEW_MEIJI.getDaysOfYear(year);
		}
	}

	public int getDaysFromNewYearEpoch(java.util.Date d) {
		if(CHANGE_GREGORIAN_EPOCH.after(d)) {
			return OLD_MEIJI.getDaysFromNewYearEpoch(d);
		} else {
			return NEW_MEIJI.getDaysFromEpoch(d);
		}
	}

	public int getDaysFromEpoch(java.util.Date d) {
		return NEW_MEIJI.getDaysFromEpoch(d);
	}

	public int getYear(java.util.Date d) {
		if(!isEra(d)) {
			return -1;
		} else if(CHANGE_GREGORIAN_EPOCH.after(d)) {
			return OLD_MEIJI.getYear(d);
		} else {
			return NEW_MEIJI.getYear(d);
		}
	}

	public boolean after(java.util.Date d) {
		return NEW_MEIJI.after(d);
	}

	public boolean before(java.util.Date d) {
		return NEW_MEIJI.before(d);
	}

	public String getInitialLetter() {
		return "M";
	}

	public String getShortDescription() {
		return "明";
	}

	public String toString() {
		return "明治";
	}

}
