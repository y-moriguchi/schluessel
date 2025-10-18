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
public class MesoamericanLongCountCalendar extends AbstractDay {

	//
	private static final int TUN_PER_WINALS = 18;
	private static final int KATUN_PER_TUNS = 20;
	private static final int BAKTUN_PER_KATUNS = 20;
	private static final int PERIOD_PER_BAKTUNS = 13;
	private static final int WINAL = 20;
	private static final int TUN = WINAL * TUN_PER_WINALS;
	private static final int KATUN = TUN * KATUN_PER_TUNS;
	private static final int BAKTUN = KATUN * BAKTUN_PER_KATUNS;
	private static final int PERIOD = BAKTUN * PERIOD_PER_BAKTUNS;
	private static final int DAYS_TO_UNIX_EPOCH = 1856304;
	private static final long DAY_MILLISECS = 24 * 60 * 60 * 1000;

	// days from September 6, 3114 BC (Julian)
	private int days;

	/**
	 * 
	 * @param date
	 */
	public MesoamericanLongCountCalendar(long date) {
		days = (int)(SimpleGregorianCalendar.div(
				date, DAY_MILLISECS)) + DAYS_TO_UNIX_EPOCH;
	}

	/**
	 * 
	 * @param time
	 * @return
	 */
	public static MesoamericanLongCountCalendar getInstaceFromTime(
			long time) {
		return new MesoamericanLongCountCalendar(time);
	}

	/**
	 * 
	 * @param baktun
	 * @param katun
	 * @param tun
	 * @param winal
	 * @param kin
	 */
	public MesoamericanLongCountCalendar(int baktun, int katun,
			int tun, int winal, int kin) {
		if(baktun < 0 || baktun >= PERIOD_PER_BAKTUNS) {
			throw new IllegalArgumentException(baktun + "");
		} else if(katun < 0 || katun >= BAKTUN_PER_KATUNS) {
			throw new IllegalArgumentException(katun + "");
		} else if(tun < 0 || tun >= KATUN_PER_TUNS) {
			throw new IllegalArgumentException(tun + "");
		} else if(winal < 0 || winal >= TUN_PER_WINALS) {
			throw new IllegalArgumentException(winal + "");
		} else if(kin < 0 || kin >= WINAL) {
			throw new IllegalArgumentException(kin + "");
		}
		days  = kin;
		days += winal * WINAL;
		days += tun * TUN;
		days += katun * KATUN;
		days += baktun * BAKTUN;
	}

	@Override
	public long getDate() {
		return (days - DAYS_TO_UNIX_EPOCH) * DAY_MILLISECS;
	}

	@Override
	public int getDaysFromEpoch() {
		return days;
	}

	public int getKin() {
		return SimpleGregorianCalendar.mod(days, WINAL);
	}

	public int getWinal() {
		return SimpleGregorianCalendar.mod(days, TUN) / WINAL;
	}

	public int getTun() {
		return SimpleGregorianCalendar.mod(days, KATUN) / TUN;
	}

	public int getKatun() {
		return SimpleGregorianCalendar.mod(days, BAKTUN) / KATUN;
	}

	public int getBaktun() {
		return SimpleGregorianCalendar.mod(days, PERIOD) / BAKTUN;
	}

	@Override
	public MesoamericanLongCountCalendar after(int days) {
		return new MesoamericanLongCountCalendar(
				(this.days + days - DAYS_TO_UNIX_EPOCH) *
				DAY_MILLISECS);
	}

	public String toString() {
		int b, a, t, w, k = days;

		k %= PERIOD;
		b  = k / BAKTUN;  k %= BAKTUN;
		a  = k / KATUN;   k %= KATUN;
		t  = k / TUN;     k %= TUN;
		w  = k / WINAL;   k %= WINAL;
		return b + "." + a + "." + t + "." + w + "." + k;
	}

}
