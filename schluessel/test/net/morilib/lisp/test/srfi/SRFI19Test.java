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
package net.morilib.lisp.test.srfi;

import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import net.morilib.lang.number.Rational;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.datetime.LispDate;
import net.morilib.lisp.datetime.LispJulianDay;
import net.morilib.lisp.datetime.LispModifiedJulianDay;
import net.morilib.lisp.datetime.LispTime;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/15
 */
public class SRFI19Test extends TCSubr {

	public void testMakeTime() {
		Scheme l = Scheme.newInstance();
		LispTime t;

		t = (LispTime)l.exec("(make-time 'time-utc 0 0)");
		eq(t.getTimeMillis(), 0);
		eq(t.getTimeType(), LispTime.TimeType.TIME_UTC);
		t = (LispTime)l.exec("(make-time 'time-utc 0 1)");
		eq(t.getTimeMillis(), 1000);
		t = (LispTime)l.exec("(make-time 'time-utc 1000000 0)");
		eq(t.getTimeMillis(), 1);
		t = (LispTime)l.exec("(make-time 'time-utc 1000000 1)");
		eq(t.getTimeMillis(), 1001);

		lperr(l,"(make-time)");
		lperr(l,"(make-time 'aaaa 0 0)");
		lperr(l,"(make-time 'time-utc 1.1 0)");
		lperr(l,"(make-time 'time-utc 0 1.1)");
	}

	public void testIsTime() {
		Scheme l = Scheme.newInstance();

		eq   (l,"(time? (current-time))", LispBoolean.TRUE);
		eq   (l,"(time? 1)", LispBoolean.FALSE);

		lperr(l,"(time?)");
	}

	public void testTimeType() {
		Scheme l = Scheme.newInstance();

		eq   (l,"(time-type (current-time))",
				Symbol.getSymbol("time-utc"));
		eq   (l,"(time-type (make-time 'time-duration 0 0))",
				Symbol.getSymbol("time-duration"));

		lperr(l,"(time-type)");
		lperr(l,"(time-type 1)");
	}

	public void testTimeNanosecond() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(time-nanosecond (make-time 'time-utc 1000000 1))",
				1000000);

		lperr(l,"(time-nanosecond)");
		lperr(l,"(time-nanosecond 1)");
	}

	public void testTimeSecond() {
		Scheme l = Scheme.newInstance();

		eqi  (l,"(time-second (make-time 'time-utc 1000000 1))",
				1);

		lperr(l,"(time-second)");
		lperr(l,"(time-second 1)");
	}

	public void testSetTimeTypeS() {
		Scheme l = Scheme.newInstance();
		LispTime t;

		l.exec("(define t (make-time 'time-utc 0 0))");
		l.exec("(set-time-type! t 'time-duration)");
		t = (LispTime)l.get("t");
		eq(t.getTimeType(), LispTime.TimeType.TIME_DURATION);

		lperr(l,"(set-time-type!)");
		lperr(l,"(set-time-type! 1 'time-tai)");
		lperr(l,"(set-time-type! t 'time-aaa)");
	}

	public void testSetTimeNanosecondS() {
		Scheme l = Scheme.newInstance();
		LispTime t;

		l.exec("(define t (make-time 'time-utc 0 0))");
		l.exec("(set-time-nanosecond! t 1000000)");
		t = (LispTime)l.get("t");
		eq(t.getTimeMillis(), 1);

		lperr(l,"(set-time-nanosecond!)");
		lperr(l,"(set-time-nanosecond! 1 1)");
		lperr(l,"(set-time-nanosecond! t 1.2)");
	}

	public void testSetTimeSecondS() {
		Scheme l = Scheme.newInstance();
		LispTime t;

		l.exec("(define t (make-time 'time-utc 0 0))");
		l.exec("(set-time-second! t 1)");
		t = (LispTime)l.get("t");
		eq(t.getTimeMillis(), 1000);

		lperr(l,"(set-time-second!)");
		lperr(l,"(set-time-second! 1 1)");
		lperr(l,"(set-time-second! t 1.2)");
	}

	public void testCopyTime() {
		Scheme l = Scheme.newInstance();
		LispTime t;

		l.exec("(define t (make-time 'time-utc 0 0))");
		t = (LispTime)l.exec("(copy-time t)");
		l.exec("(set-time-second! t 1)");
		eq(t.getTimeMillis(), 0);

		lperr(l,"(copy-time)");
		lperr(l,"(cpoy-time 1)");
	}

	public void testTimeLe() {
		Scheme l = Scheme.newInstance();

		l.exec("(define t1 (make-time 'time-utc 0 0))");
		l.exec("(define t2 (make-time 'time-utc 0 1))");
		l.exec("(define t3 (make-time 'time-utc 0 2))");
		l.exec("(define t4 (make-time 'time-utc 0 1))");
		l.exec("(define t5 (make-time 'time-tai 0 1))");
		eq(l,"(time<=? t2 t3)", LispBoolean.TRUE);
		eq(l,"(time<=? t2 t2)", LispBoolean.TRUE);
		eq(l,"(time<=? t2 t1)", LispBoolean.FALSE);

		lperr(l,"(time<=?)");
		lperr(l,"(time<=? 1 t2)");
		lperr(l,"(time<=? t2 1)");
		lperr(l,"(time<=? t2 t5)");
	}

	public void testTimeLt() {
		Scheme l = Scheme.newInstance();

		l.exec("(define t1 (make-time 'time-utc 0 0))");
		l.exec("(define t2 (make-time 'time-utc 0 1))");
		l.exec("(define t3 (make-time 'time-utc 0 2))");
		l.exec("(define t4 (make-time 'time-utc 0 1))");
		l.exec("(define t5 (make-time 'time-tai 0 1))");
		eq(l,"(time<? t2 t3)", LispBoolean.TRUE);
		eq(l,"(time<? t2 t2)", LispBoolean.FALSE);
		eq(l,"(time<? t2 t1)", LispBoolean.FALSE);

		lperr(l,"(time<?)");
		lperr(l,"(time<? 1 t2)");
		lperr(l,"(time<? t2 1)");
		lperr(l,"(time<? t2 t5)");
	}

	public void testTimeEq() {
		Scheme l = Scheme.newInstance();

		l.exec("(define t1 (make-time 'time-utc 0 0))");
		l.exec("(define t2 (make-time 'time-utc 0 1))");
		l.exec("(define t3 (make-time 'time-utc 0 2))");
		l.exec("(define t4 (make-time 'time-utc 0 1))");
		l.exec("(define t5 (make-time 'time-tai 0 1))");
		eq(l,"(time=? t2 t3)", LispBoolean.FALSE);
		eq(l,"(time=? t2 t2)", LispBoolean.TRUE);
		eq(l,"(time=? t2 t1)", LispBoolean.FALSE);

		lperr(l,"(time=?)");
		lperr(l,"(time=? 1 t2)");
		lperr(l,"(time=? t2 1)");
		lperr(l,"(time=? t2 t5)");
	}

	public void testTimeGe() {
		Scheme l = Scheme.newInstance();

		l.exec("(define t1 (make-time 'time-utc 0 0))");
		l.exec("(define t2 (make-time 'time-utc 0 1))");
		l.exec("(define t3 (make-time 'time-utc 0 2))");
		l.exec("(define t4 (make-time 'time-utc 0 1))");
		l.exec("(define t5 (make-time 'time-tai 0 1))");
		eq(l,"(time>=? t2 t3)", LispBoolean.FALSE);
		eq(l,"(time>=? t2 t2)", LispBoolean.TRUE);
		eq(l,"(time>=? t2 t1)", LispBoolean.TRUE);

		lperr(l,"(time>=?)");
		lperr(l,"(time>=? 1 t2)");
		lperr(l,"(time>=? t2 1)");
		lperr(l,"(time>=? t2 t5)");
	}

	public void testTimeGt() {
		Scheme l = Scheme.newInstance();

		l.exec("(define t1 (make-time 'time-utc 0 0))");
		l.exec("(define t2 (make-time 'time-utc 0 1))");
		l.exec("(define t3 (make-time 'time-utc 0 2))");
		l.exec("(define t4 (make-time 'time-utc 0 1))");
		l.exec("(define t5 (make-time 'time-tai 0 1))");
		eq(l,"(time>? t2 t3)", LispBoolean.FALSE);
		eq(l,"(time>? t2 t2)", LispBoolean.FALSE);
		eq(l,"(time>? t2 t1)", LispBoolean.TRUE);

		lperr(l,"(time>?)");
		lperr(l,"(time>? 1 t2)");
		lperr(l,"(time>? t2 1)");
		lperr(l,"(time>? t2 t5)");
	}

	public void testTimeDifference() {
		Scheme l = Scheme.newInstance();
		LispTime t;

		l.exec("(define t1 (make-time 'time-utc 0 9))");
		l.exec("(define t2 (make-time 'time-utc 0 0))");
		t = (LispTime)l.exec("(time-difference t1 t2)");
		eq(t.getTimeMillis(), 9000);
		eq(t.getTimeType(), LispTime.TimeType.TIME_DURATION);

		lperr(l,"(time-difference)");
		lperr(l,"(time-difference 1 t2)");
		lperr(l,"(time-difference t2 1)");
		lperr(l,"(time-difference t2 (make-time 'time-tai 0 0))");
	}

	public void testTimeDifferenceS() {
		Scheme l = Scheme.newInstance();
		LispTime t;

		l.exec("(define t1 (make-time 'time-utc 0 9))");
		l.exec("(define t2 (make-time 'time-utc 0 0))");
		t = (LispTime)l.exec("(time-difference! t1 t2)");
		eq(t.getTimeMillis(), 9000);
		eq(t.getTimeType(), LispTime.TimeType.TIME_DURATION);

		lperr(l,"(time-difference!)");
		lperr(l,"(time-difference! 1 t2)");
		lperr(l,"(time-difference! t2 1)");
		lperr(l,"(time-difference! t2 (make-time 'time-tai 0 0))");
	}

	public void testAddDuration() {
		Scheme l = Scheme.newInstance();
		LispTime t;

		l.exec("(define t1 (make-time 'time-utc 0 9))");
		l.exec("(define t2 (make-time 'time-duration 0 1))");
		t = (LispTime)l.exec("(add-duration t1 t2)");
		eq(t.getTimeMillis(), 10000);
		eq(t.getTimeType(), LispTime.TimeType.TIME_UTC);

		lperr(l,"(add-duration)");
		lperr(l,"(add-duration 1 t2)");
		lperr(l,"(add-duration t2 1)");
		lperr(l,"(add-duration t2 (make-time 'time-tai 0 0))");
	}

	public void testAddDurationS() {
		Scheme l = Scheme.newInstance();
		LispTime t;

		l.exec("(define t1 (make-time 'time-utc 0 9))");
		l.exec("(define t2 (make-time 'time-duration 0 1))");
		t = (LispTime)l.exec("(add-duration! t1 t2)");
		eq(t.getTimeMillis(), 10000);
		eq(t.getTimeType(), LispTime.TimeType.TIME_UTC);

		lperr(l,"(add-duration!)");
		lperr(l,"(add-duration! 1 t2)");
		lperr(l,"(add-duration! t2 1)");
		lperr(l,"(add-duration! t2 (make-time 'time-tai 0 0))");
	}

	public void testSubtractDuration() {
		Scheme l = Scheme.newInstance();
		LispTime t;

		l.exec("(define t1 (make-time 'time-utc 0 9))");
		l.exec("(define t2 (make-time 'time-duration 0 1))");
		t = (LispTime)l.exec("(subtract-duration t1 t2)");
		eq(t.getTimeMillis(), 8000);
		eq(t.getTimeType(), LispTime.TimeType.TIME_UTC);

		lperr(l,"(subtract-duration)");
		lperr(l,"(subtract-duration 1 t2)");
		lperr(l,"(subtract-duration t2 1)");
		lperr(l,"(subtract-duration t2 (make-time 'time-tai 0 0))");
	}

	public void testSubtractDurationS() {
		Scheme l = Scheme.newInstance();
		LispTime t;

		l.exec("(define t1 (make-time 'time-utc 0 9))");
		l.exec("(define t2 (make-time 'time-duration 0 1))");
		t = (LispTime)l.exec("(subtract-duration! t1 t2)");
		eq(t.getTimeMillis(), 8000);
		eq(t.getTimeType(), LispTime.TimeType.TIME_UTC);

		lperr(l,"(subtract-duration!)");
		lperr(l,"(subtract-duration! 1 t2)");
		lperr(l,"(subtract-duration! t2 1)");
		lperr(l,"(subtract-duration! t2 (make-time 'time-tai 0 0))");
	}

	public void testMakeDate() throws ParseException {
		Scheme l = Scheme.newInstance();
		LispDate t;
		SimpleDateFormat f =
			new SimpleDateFormat("yyyy/MM/dd HH:mm:ss SSS Z");

		t = (LispDate)l.exec(
				"(make-date 1000000 2 3 4 5 6 2007 32400)");
		eq(t.getDate(),
				f.parse("2007/06/05 04:03:02 001 +0900"));

		lperr(l,"(make-date)");
		lperr(l,"(make-date 1.1 2 3 4 5 6 2007 32400)");
		lperr(l,"(make-date 1 2.1 3 4 5 6 2007 32400)");
		lperr(l,"(make-date 1 2 3.1 4 5 6 2007 32400)");
		lperr(l,"(make-date 1 2 3 4.1 5 6 2007 32400)");
		lperr(l,"(make-date 1 2 3 4 5.1 6 2007 32400)");
		lperr(l,"(make-date 1 2 3 4 5 6.1 2007 32400)");
		lperr(l,"(make-date 1 2 3 4 5 6 2007.1 32400)");
		lperr(l,"(make-date 1 2 3 4 5 6 2007 32400.1)");
	}

	public void testIsDate() {
		Scheme l = Scheme.newInstance();

		l.exec("(define d (make-date 1000000 2 3 4 5 6 2007 32400))");
		eq(l,"(date? d)", LispBoolean.TRUE);
		eq(l,"(date? 1)", LispBoolean.FALSE);

		lperr(l,"(date?)");
	}

	public void testDateNanosecond() {
		Scheme l = Scheme.newInstance();

		l.exec("(define d (make-date 1000000 1 1 1 1 1 2007 32400))");
		eqi(l,"(date-nanosecond d)", 1000000);

		lperr(l,"(date-nanosecond)");
		lperr(l,"(date-nanosecond 1)");
	}

	public void testDateSecond() {
		Scheme l = Scheme.newInstance();

		l.exec("(define d (make-date 1000000 1 1 1 1 1 2007 32400))");
		eqi(l,"(date-second d)", 1);

		lperr(l,"(date-second)");
		lperr(l,"(date-second 1)");
	}

	public void testDateMinute() {
		Scheme l = Scheme.newInstance();

		l.exec("(define d (make-date 1000000 1 1 1 1 1 2007 32400))");
		eqi(l,"(date-minute d)", 1);

		lperr(l,"(date-minute)");
		lperr(l,"(date-minute 1)");
	}

	public void testDateHour() {
		Scheme l = Scheme.newInstance();

		l.exec("(define d (make-date 1000000 1 1 1 1 1 2007 32400))");
		eqi(l,"(date-hour d)", 1);

		lperr(l,"(date-hour)");
		lperr(l,"(date-hour 1)");
	}

	public void testDateDay() {
		Scheme l = Scheme.newInstance();

		l.exec("(define d (make-date 1000000 1 1 1 1 1 2007 32400))");
		eqi(l,"(date-day d)", 1);

		lperr(l,"(date-day)");
		lperr(l,"(date-day 1)");
	}

	public void testDateMonth() {
		Scheme l = Scheme.newInstance();

		l.exec("(define d (make-date 1000000 1 1 1 1 1 2007 32400))");
		eqi(l,"(date-month d)", 1);

		lperr(l,"(date-month)");
		lperr(l,"(date-month 1)");
	}

	public void testDateYear() {
		Scheme l = Scheme.newInstance();

		l.exec("(define d (make-date 1000000 1 1 1 1 1 2007 32400))");
		eqi(l,"(date-year d)", 2007);

		lperr(l,"(date-year)");
		lperr(l,"(date-year 1)");
	}

	public void testDateZoneOffset() {
		Scheme l = Scheme.newInstance();

		l.exec("(define d (make-date 1000000 1 1 1 1 1 2007 32400))");
		eqi(l,"(date-zone-offset d)", 32400);

		lperr(l,"(date-zone-offset)");
		lperr(l,"(date-zone-offset 1)");
	}

	public void testDateYearDay() {
		Scheme l = Scheme.newInstance();

		l.exec("(define d (make-date 0 0 0 0 31 12 2010 32400))");
		eqi(l,"(date-year-day d)", 365);

		lperr(l,"(date-year-day)");
		lperr(l,"(date-year-day 1)");
	}

	public void testDateWeekDay() {
		Scheme l = Scheme.newInstance();

		l.exec("(define d (make-date 0 0 0 0 31 12 2010 32400))");
		eqi(l,"(date-week-day d)", 5);

		lperr(l,"(date-week-day)");
		lperr(l,"(date-week-day 1)");
	}

	public void testDateWeekNumber() {
		Scheme l = Scheme.newInstance();

		l.exec("(define d (make-date 0 0 0 0 16 1 2007 32400))");
		eqi(l,"(date-week-number d 0)", 2);
		eqi(l,"(date-week-number d 1)", 3);

		lperr(l,"(date-week-number)");
		lperr(l,"(date-week-number 1)");
	}

	public void testDateToJulianDay() {
		Scheme l = Scheme.newInstance();
		LispJulianDay d;

		l.exec("(define d (make-date 0 0 0 21 14 1 2011 32400))");
		d = (LispJulianDay)l.exec("(date->julian-day d)");
		eq(Rational.toBigDecimal(d.getJulianDay().getJulianDay()),
				new BigDecimal("2455576"));

		lperr(l,"(date->julian-day)");
		lperr(l,"(date->julian-day 1)");
	}

	public void testDateToModifiedJulianDay() {
		Scheme l = Scheme.newInstance();
		LispModifiedJulianDay d;

		l.exec("(define d (make-date 0 0 0 9 15 1 2011 32400))");
		d = (LispModifiedJulianDay)l.exec(
				"(date->modified-julian-day d)");
		eq(Rational.toBigDecimal(
				d.getJulianDay().getModifiedJulianDay()),
				new BigDecimal("55576"));

		lperr(l,"(date->modified-julian-day)");
		lperr(l,"(date->modified-julian-day 1)");
	}

	public void testDateToTimeMonotonic() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec("(date->time-monotonic d)");
		eq(d.getTimeMillis(), 0);

		lperr(l,"(date->time-monotonic)");
		lperr(l,"(date->time-monotonic 1)");
	}

	public void testDateToTimeTAI() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec("(date->time-tai d)");
		eq(d.getTimeMillis(), 34000);

		lperr(l,"(date->time-tai)");
		lperr(l,"(date->time-tai 1)");
	}

	public void testDateToTimeUTC() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec("(date->time-utc d)");
		eq(d.getTimeMillis(), 0);

		lperr(l,"(date->time-utc)");
		lperr(l,"(date->time-utc 1)");
	}

	public void testJulianDayToDate() throws ParseException {
		Scheme l = Scheme.newInstance();
		LispDate t;
		SimpleDateFormat f =
			new SimpleDateFormat("yyyy/MM/dd HH:mm:ss SSS Z");

		t = (LispDate)l.exec(
				"(julian-day->date" +
				"  (date->julian-day" +
				"    (make-date 1000000 2 3 4 5 6 2007 32400)))");
		eq(t.getDate(),
				f.parse("2007/06/05 04:03:02 001 +0900"));

		lperr(l,"(julian-day->date)");
		lperr(l,"(julian-day->date 1)");
	}

	public void testJulianDayToTimeMonotonic() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(julian-day->time-monotonic" +
				"  (date->julian-day d))");
		eq(d.getTimeMillis(), 0);

		lperr(l,"(julian-day->time-monotonic)");
		lperr(l,"(julian-day->time-monotonic 1)");
	}

	public void testJulianDayToTimeTAI() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(julian-day->time-tai" +
				"  (date->julian-day d))");
		eq(d.getTimeMillis(), 34000);

		lperr(l,"(julian-day->time-tai)");
		lperr(l,"(julian-day->time-tai 1)");
	}

	public void testJulianDayToTimeUTC() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(julian-day->time-utc" +
				"  (date->julian-day d))");
		eq(d.getTimeMillis(), 0);

		lperr(l,"(julian-day->time-utc)");
		lperr(l,"(julian-day->time-utc 1)");
	}

	public void testModifiedJulianDayToDate() throws ParseException {
		Scheme l = Scheme.newInstance();
		LispDate t;
		SimpleDateFormat f =
			new SimpleDateFormat("yyyy/MM/dd HH:mm:ss SSS Z");

		t = (LispDate)l.exec(
				"(modified-julian-day->date" +
				"  (date->modified-julian-day" +
				"    (make-date 1000000 2 3 4 5 6 2007 32400)))");
		eq(t.getDate(),
				f.parse("2007/06/05 04:03:02 001 +0900"));

		lperr(l,"(modified-julian-day->date)");
		lperr(l,"(modified-julian-day->date 1)");
	}

	public void testModifiedJulianDayToTimeMonotonic() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(modified-julian-day->time-monotonic" +
				"  (date->modified-julian-day d))");
		eq(d.getTimeMillis(), 0);

		lperr(l,"(modified-julian-day->time-monotonic)");
		lperr(l,"(modified-julian-day->time-monotonic 1)");
	}

	public void testModifiedJulianDayToTimeTAI() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(modified-julian-day->time-tai" +
				"  (date->modified-julian-day d))");
		eq(d.getTimeMillis(), 34000);

		lperr(l,"(modified-julian-day->time-tai)");
		lperr(l,"(modified-julian-day->time-tai 1)");
	}

	public void testModifiedJulianDayToTimeUTC() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(modified-julian-day->time-utc" +
				"  (date->modified-julian-day d))");
		eq(d.getTimeMillis(), 0);

		lperr(l,"(modified-julian-day->time-utc)");
		lperr(l,"(modified-julian-day->time-utc 1)");
	}

//	public void testTimeMonotonicToDate() throws ParseException {
//		Scheme l = Scheme.newInstance();
//		LispDate t;
//		SimpleDateFormat f =
//			new SimpleDateFormat("yyyy/MM/dd HH:mm:ss SSS Z");
//		long lt;
//
//		lt = f.parse("2007/06/05 04:03:02 000 +0900").getTime();
//		t = (LispDate)l.exec(
//				"(time-monotonic->date " +
//				"  (make-time 'time-monotonic 0 " + (lt / 1000) + ")" +
//				"  32400)");
//		eq(t.getDate(), new java.util.Date(lt));
//	}

	public void testTimeMonotonicToJulianDay() throws ParseException {
		Scheme l = Scheme.newInstance();
		LispJulianDay d;
		SimpleDateFormat f =
			new SimpleDateFormat("yyyy/MM/dd HH:mm:ss SSS Z");
		long lt;

		lt = f.parse("2011/01/14 21:00:00 000 +0900").getTime();
		d = (LispJulianDay)l.exec(
				"(time-monotonic->julian-day " +
				"  (make-time 'time-monotonic 0 " + (lt / 1000) + "))");
		eq(Rational.toBigDecimal(d.getJulianDay().getJulianDay()),
				new BigDecimal("2455576"));
	}

	public void testTimeMonotonicToModifiedJulianDay() throws ParseException {
		Scheme l = Scheme.newInstance();
		LispModifiedJulianDay d;
		SimpleDateFormat f =
			new SimpleDateFormat("yyyy/MM/dd HH:mm:ss SSS Z");
		long lt;

		lt = f.parse("2011/01/15 09:00:00 000 +0900").getTime();
		d = (LispModifiedJulianDay)l.exec(
				"(time-monotonic->modified-julian-day " +
				"  (make-time 'time-monotonic 0 " + (lt / 1000) + "))");
		eq(Rational.toBigDecimal(d.getJulianDay().getModifiedJulianDay()),
				new BigDecimal("55576"));
	}

	public void testTimeMonotonicToTimeTAI() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(time-monotonic->time-tai" +
				"  (date->time-monotonic d))");
		eq(d.getTimeMillis(), 34000);

		lperr(l,"(time-monotonic->time-tai)");
		lperr(l,"(time-monotonic->time-tai 1)");
	}

	public void testTimeMonotonicToTimeUTC() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(time-monotonic->time-utc" +
				"  (date->time-monotonic d))");
		eq(d.getTimeMillis(), 0);

		lperr(l,"(time-monotonic->time-utc)");
		lperr(l,"(time-monotonic->time-utc 1)");
	}

	public void testTimeMonotonicToTimeTAIS() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(time-monotonic->time-tai!" +
				"  (date->time-monotonic d))");
		eq(d.getTimeMillis(), 34000);

		lperr(l,"(time-monotonic->time-tai!)");
		lperr(l,"(time-monotonic->time-tai! 1)");
	}

	public void testTimeMonotonicToTimeUTCS() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(time-monotonic->time-utc!" +
				"  (date->time-monotonic d))");
		eq(d.getTimeMillis(), 0);

		lperr(l,"(time-monotonic->time-utc!)");
		lperr(l,"(time-monotonic->time-utc! 1)");
	}

//	public void testTimeTAIToDate() throws ParseException {
//		Scheme l = Scheme.newInstance();
//		LispDate t;
//		SimpleDateFormat f =
//			new SimpleDateFormat("yyyy/MM/dd HH:mm:ss SSS Z");
//		java.util.Date dt;
//		long lt;
//
//		dt = f.parse("2007/06/05 04:03:02 000 +0900");
//		lt = dt.getTime();
//		t = (LispDate)l.exec(
//				"(time-tai->date " +
//				"  (make-time 'time-tai 0 " + (lt / 1000 + 34) + ")" +
//				"  32400)");
//		eq(t.getDate(), dt);
//	}

	public void testTimeTAIToJulianDay() throws ParseException {
		Scheme l = Scheme.newInstance();
		LispJulianDay d;
		SimpleDateFormat f =
			new SimpleDateFormat("yyyy/MM/dd HH:mm:ss SSS Z");
		long lt;

		lt = f.parse("2011/01/14 21:00:00 000 +0900").getTime();
		d = (LispJulianDay)l.exec(
				"(time-tai->julian-day " +
				"  (make-time 'time-tai 0 " + (lt / 1000 + 34) + "))");
		eq(Rational.toBigDecimal(d.getJulianDay().getJulianDay()),
				new BigDecimal("2455576"));
	}

	public void testTimeTAIToModifiedJulianDay() throws ParseException {
		Scheme l = Scheme.newInstance();
		LispModifiedJulianDay d;
		SimpleDateFormat f =
			new SimpleDateFormat("yyyy/MM/dd HH:mm:ss SSS Z");
		long lt;

		lt = f.parse("2011/01/15 09:00:00 000 +0900").getTime();
		d = (LispModifiedJulianDay)l.exec(
				"(time-tai->modified-julian-day " +
				"  (make-time 'time-tai 0 " + (lt / 1000 + 34) + "))");
		eq(Rational.toBigDecimal(d.getJulianDay().getModifiedJulianDay()),
				new BigDecimal("55576"));
	}

	public void testTimeTAIToTimeMonotonic() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(time-tai->time-monotonic" +
				"  (date->time-tai d))");
		eq(d.getTimeMillis(), 0);

		lperr(l,"(time-tai->time-monotonic)");
		lperr(l,"(time-tai->time-monotonic 1)");
	}

	public void testTimeTAIToTimeUTC() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(time-tai->time-utc" +
				"  (date->time-tai d))");
		eq(d.getTimeMillis(), 0);

		lperr(l,"(time-tai->time-utc)");
		lperr(l,"(time-tai->time-utc 1)");
	}

	public void testTimeTAIToTimeMonotonicS() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(time-tai->time-monotonic!" +
				"  (date->time-tai d))");
		eq(d.getTimeMillis(), 0);

		lperr(l,"(time-tai->time-monotonic)");
		lperr(l,"(time-tai->time-monotonic 1)");
	}

	public void testTimeTAIToTimeUTCS() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(time-tai->time-utc!" +
				"  (date->time-tai d))");
		eq(d.getTimeMillis(), 0);

		lperr(l,"(time-tai->time-utc)");
		lperr(l,"(time-tai->time-utc 1)");
	}

//	public void testTimeUTCToDate() throws ParseException {
//		Scheme l = Scheme.newInstance();
//		LispDate t;
//		SimpleDateFormat f =
//			new SimpleDateFormat("yyyy/MM/dd HH:mm:ss SSS Z");
//		long lt;
//
//		lt = f.parse("2007/06/05 04:03:02 000 +0900").getTime();
//		t = (LispDate)l.exec(
//				"(time-utc->date " +
//				"  (make-time 'time-utc 0 " + (lt / 1000) + ")" +
//				"  32400)");
//		eq(t.getDate(), new java.util.Date(lt));
//	}

	public void testTimeUTCToJulianDay() throws ParseException {
		Scheme l = Scheme.newInstance();
		LispJulianDay d;
		SimpleDateFormat f =
			new SimpleDateFormat("yyyy/MM/dd HH:mm:ss SSS Z");
		long lt;

		lt = f.parse("2011/01/14 21:00:00 000 +0900").getTime();
		d = (LispJulianDay)l.exec(
				"(time-utc->julian-day " +
				"  (make-time 'time-utc 0 " + (lt / 1000) + "))");
		eq(Rational.toBigDecimal(d.getJulianDay().getJulianDay()),
				new BigDecimal("2455576"));
	}

	public void testTimeUTCToModifiedJulianDay() throws ParseException {
		Scheme l = Scheme.newInstance();
		LispModifiedJulianDay d;
		SimpleDateFormat f =
			new SimpleDateFormat("yyyy/MM/dd HH:mm:ss SSS Z");
		long lt;

		lt = f.parse("2011/01/15 09:00:00 000 +0900").getTime();
		d = (LispModifiedJulianDay)l.exec(
				"(time-utc->modified-julian-day " +
				"  (make-time 'time-utc 0 " + (lt / 1000) + "))");
		eq(Rational.toBigDecimal(d.getJulianDay().getModifiedJulianDay()),
				new BigDecimal("55576"));
	}

	public void testTimeUTCToTimeMonotonic() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(time-utc->time-monotonic" +
				"  (date->time-utc d))");
		eq(d.getTimeMillis(), 0);

		lperr(l,"(time-tai->time-monotonic)");
		lperr(l,"(time-tai->time-monotonic 1)");
	}

	public void testTimeUTCToTimeTAI() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(time-utc->time-tai" +
				"  (date->time-utc d))");
		eq(d.getTimeMillis(), 34000);

		lperr(l,"(time-monotonic->time-tai)");
		lperr(l,"(time-monotonic->time-tai 1)");
	}

	public void testTimeUTCToTimeMonotonicS() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(time-utc->time-monotonic!" +
				"  (date->time-utc d))");
		eq(d.getTimeMillis(), 0);

		lperr(l,"(time-tai->time-monotonic)");
		lperr(l,"(time-tai->time-monotonic 1)");
	}

	public void testTimeUTCToTimeTAIS() {
		Scheme l = Scheme.newInstance();
		LispTime d;

		l.exec("(define d (make-date 0 0 0 0 1 1 1970 0))");
		d = (LispTime)l.exec(
				"(time-utc->time-tai!" +
				"  (date->time-utc d))");
		eq(d.getTimeMillis(), 34000);

		lperr(l,"(time-monotonic->time-tai!)");
		lperr(l,"(time-monotonic->time-tai! 1)");
	}

}
