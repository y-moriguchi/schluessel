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
public interface Day extends Comparable<Day> {

	/**
	 * 
	 * @return
	 */
	public int getDaysFromEpoch();

	/**
	 * 
	 * @return
	 */
	public long getDate();

	/**
	 * 
	 * @return
	 */
	public int getDaysFromUnixEpoch();

	/**
	 * 
	 * @param days
	 * @return
	 */
	public Day after(int days);

	/**
	 * 
	 * @param c
	 * @return
	 */
	public int interval(Day c);

	/**
	 * 
	 * @return
	 */
	public int getJulianDay();

	/**
	 * 
	 * @return
	 */
	public int getModifiedJulianDay();

	/**
	 * 
	 * @return
	 */
	public int getLilianDay();

}
