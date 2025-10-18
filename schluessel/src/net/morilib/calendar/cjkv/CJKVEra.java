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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/28
 */
public interface CJKVEra {

	/**
	 * 
	 * @return
	 */
	public java.util.Date getNewYearEpoch();

	/**
	 * 
	 * @return
	 */
	public java.util.Date getEpoch();

	/**
	 * 
	 * @return
	 */
	public java.util.Date getEnd();

	/**
	 * 
	 * @return
	 */
	public int getYears();

	/**
	 * 
	 * @param year
	 * @return
	 */
	public int getMonths(int year);

	/**
	 * 
	 * @param year
	 * @return
	 */
	public int getLeapMonth(int year);

	/**
	 * 
	 * @param d
	 * @return
	 */
	public boolean isEra(java.util.Date d);

	/**
	 * 
	 * @param year
	 * @param month
	 * @return
	 */
	public int getDaysOfMonth(int year, int month);

	/**
	 * 
	 * @param year
	 * @param month
	 * @return
	 */
	public int getDaysOfYear(int year);

	/**
	 * 
	 * @param d
	 * @return
	 */
	public int getYear(java.util.Date d);

	/**
	 * 
	 * @param d
	 * @return
	 */
	public int getDaysFromNewYearEpoch(java.util.Date d);

	/**
	 * 
	 * @param d
	 * @return
	 */
	public int getDaysFromEpoch(java.util.Date d);

	/**
	 * 
	 * @param d
	 * @return
	 */
	public boolean before(java.util.Date d);

	/**
	 * 
	 * @param d
	 * @return
	 */
	public boolean after(java.util.Date d);

	/**
	 * 
	 * @return
	 */
	public String getShortDescription();

	/**
	 * 
	 * @return
	 */
	public String getInitialLetter();

}
