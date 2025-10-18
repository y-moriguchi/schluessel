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
package net.morilib.net.snmp;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/15
 */
public class TimeTicks implements java.io.Serializable {

	//
	private long ticks;

	/**
	 * 
	 * @param ticks
	 */
	public TimeTicks(long ticks) {
		if(ticks < 0) {
			throw new IllegalArgumentException();
		}
		this.ticks = ticks;
	}

	/**
	 * 
	 * @param ticks2
	 * @return
	 */
	public TimeTicks add(TimeTicks ticks2) {
		return new TimeTicks(ticks + ticks2.ticks);
	}

	/**
	 * 
	 * @param ticks2
	 * @return
	 */
	public TimeTicks sub(TimeTicks ticks2) {
		long t = ticks - ticks2.ticks;

		if(t < 0) {
			throw new ArithmeticException();
		}
		return new TimeTicks(t);
	}

	/**
	 * 
	 * @return
	 */
	public long toMilliseconds() {
		return ticks * 10;
	}

	/**
	 * 
	 * @return
	 */
	public double toSeconds() {
		return (double)ticks / 100.0;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return (int)ticks;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof TimeTicks) {
			return ticks == ((TimeTicks)o).ticks;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return String.format("%d.%02dsec", ticks / 100, ticks % 100);
	}

}
