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
package net.morilib.lisp.calendar;

import net.morilib.calendar.Day;
import net.morilib.calendar.SolarCalendar;
import net.morilib.calendar.WeekCalendar;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.math.algebra.ILispNumberEqual;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/22
 */
public class LispDay extends Datum2
implements ILispNumberEqual<LispDay>, Comparable<LispDay> {

	//
	Day calendar;

	//
	static Day getDay(Datum d, LispMessage mesg) {
		if(d instanceof LispDay) {
			return ((LispDay)d).calendar;
		} else {
			throw mesg.getError("err.calendar.require.day");
		}
	}

	//
	static SolarCalendar getSolarCalendar(Datum d, LispMessage mesg) {
		Day x;

		if(d instanceof LispDay) {
			x = ((LispDay)d).calendar;
			if(x instanceof SolarCalendar) {
				return (SolarCalendar)x;
			}
		}
		throw mesg.getError("err.calendar.require.solarcalendar");
	}

	//
	static WeekCalendar getWeekCalendar(Datum d, LispMessage mesg) {
		Day x;

		if(d instanceof LispDay) {
			x = ((LispDay)d).calendar;
			if(x instanceof WeekCalendar) {
				return (WeekCalendar)x;
			}
		}
		throw mesg.getError("err.calendar.require.weekcalendar");
	}

	/**
	 * 
	 * @param d
	 */
	public LispDay(Day d) {
		this.calendar = d;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append(calendar.toString());
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(LispDay o) {
		long a = calendar.getDate();
		long b = o.calendar.getDate();

		return (a < b) ? -1 : (a > b) ? 1 : 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispNumberEqual#isEqualTo(java.lang.Object)
	 */
	@Override
	public boolean isEqualTo(LispDay x) {
		return calendar.getDate() == x.calendar.getDate();
	}

}
