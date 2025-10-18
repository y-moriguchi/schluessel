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
package net.morilib.util.datetime;

import java.math.BigDecimal;
import java.util.Date;

import net.morilib.lang.number.Integer2;
import net.morilib.lang.number.Rational;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/08
 */
public class JulianDay extends Date {

	//
	private static final long serialVersionUID = -2229179274862330981L;

	//
	private static final long HOURS = 24;
	private static final long MINUTES = 60;
	private static final long SECONDS = 60;
	private static final long MILLISEC = 1000;
	private static final long DAY_MILLISEC =
		HOURS * MINUTES * SECONDS * MILLISEC;
	private static final Integer2 DAY_MILLISEC_2 =
		Integer2.valueOf(DAY_MILLISEC);
	private static final Rational DAY_MILLISEC_R =
		Rational.valueOf(DAY_MILLISEC_2, Integer2.ONE);
	private static final long MAX_DAYS =
		Long.MAX_VALUE / DAY_MILLISEC;
	private static final long MIN_DAYS =
		Long.MIN_VALUE / DAY_MILLISEC;
	private static final Rational MAX_DAYS_R =
		Rational.valueOf(Integer2.valueOf(MAX_DAYS), Integer2.ONE);
	private static final Rational MIN_DAYS_R =
		Rational.valueOf(Integer2.valueOf(MIN_DAYS), Integer2.ONE);
	private static final int JULIAN_DAY_OF_UNIX_EPOCH = 2440587;
	private static final Rational JULIAN_DAY_OF_UNIX_EPOCH_R =
		Rational.valueOf(2440587 * 2 + 1, 2);
	private static final long JULIAN_TIME_OF_UNIX_EPOCH =
		JULIAN_DAY_OF_UNIX_EPOCH * DAY_MILLISEC + DAY_MILLISEC / 2;
	private static final Integer2 JULIAN_TIME_OF_UNIX_EPOCH_2 =
		Integer2.valueOf(JULIAN_TIME_OF_UNIX_EPOCH);
	private static final Rational MODIFIED_JULIAN_DAY_EPOCH_R =
		Rational.valueOf(2400000 * 2 + 1, 2);

	/**
	 * 
	 * @param date
	 */
	public JulianDay(long time) {
		super(time);
	}

	/**
	 * 
	 */
	public JulianDay() {
		super();
	}

	/**
	 * 
	 * @param day
	 * @return
	 */
	public static JulianDay day(int day) {
		long d = (day - JULIAN_DAY_OF_UNIX_EPOCH) * DAY_MILLISEC;

		return new JulianDay(d + DAY_MILLISEC / 2);
	}

	//
	private static long toTime(Rational day) {
		Rational d2;

		d2 = day.subtract(JULIAN_DAY_OF_UNIX_EPOCH_R);
		if(d2.compareTo(MAX_DAYS_R) > 0) {
			return Long.MAX_VALUE;
		} else if(d2.compareTo(MIN_DAYS_R) < 0) {
			return Long.MIN_VALUE;
		} else {
			d2 = d2.multiply(DAY_MILLISEC_R);
			return d2.longFloor();
		}
	}

	/**
	 * 
	 * @param day
	 * @return
	 */
	public static JulianDay day(BigDecimal day) {
		return new JulianDay(toTime(Rational.valueOf(day)));
	}

	/**
	 * 
	 * @param r
	 * @return
	 */
	public static JulianDay day(Rational r) {
		return new JulianDay(toTime(r));
	}

	/**
	 * 
	 * @param day
	 * @return
	 */
	public static JulianDay day(double day) {
		return new JulianDay(toTime(Rational.valueOf(day)));
	}

	/**
	 * 
	 * @return
	 */
	public Rational getJulianDay() {
		Integer2 l = Integer2.valueOf(getTime());

		l = l.add(JULIAN_TIME_OF_UNIX_EPOCH_2);
		return Rational.valueOf(l, DAY_MILLISEC_2);
	}

	/**
	 * 
	 * @param day
	 * @return
	 */
	public void setJulianDay(BigDecimal day) {
		setTime(toTime(Rational.valueOf(day)));
	}

	/**
	 * 
	 * @param r
	 * @return
	 */
	public void setJulianDay(Rational r) {
		setTime(toTime(r));
	}

	/**
	 * 
	 * @param day
	 * @return
	 */
	public void setJulianDay(double day) {
		setTime(toTime(Rational.valueOf(day)));
	}

	/**
	 * 
	 * @return
	 */
	public Rational getModifiedJulianDay() {
		return getJulianDay().subtract(MODIFIED_JULIAN_DAY_EPOCH_R);
	}

	/**
	 * 
	 * @param day
	 * @return
	 */
	public void setModifiedJulianDay(BigDecimal day) {
		setModifiedJulianDay(Rational.valueOf(day));
	}

	/**
	 * 
	 * @param r
	 * @return
	 */
	public void setModifiedJulianDay(Rational r) {
		setJulianDay(r.add(MODIFIED_JULIAN_DAY_EPOCH_R));
	}

	/**
	 * 
	 * @param day
	 * @return
	 */
	public void setModifiedJulianDay(double day) {
		setModifiedJulianDay(Rational.valueOf(day));
	}

}
