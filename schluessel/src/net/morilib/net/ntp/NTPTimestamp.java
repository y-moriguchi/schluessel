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
package net.morilib.net.ntp;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/06
 */
public class NTPTimestamp {

	//
	private static final long OFFSET_1970_MS = 2208988800000l;
	private static final long OFFSET_1970_S  = 2208988800l;

	//
	private long seconds;
	private long fraction;

	/**
	 * 
	 * @param seconds
	 * @param fraction
	 */
	public NTPTimestamp(long seconds, long fraction) {
		this.seconds  = seconds;
		this.fraction = fraction;
	}

	/**
	 * 
	 * @param time
	 * @return
	 */
	public static NTPTimestamp getInstance(long time) {
		long s, f = 0, t = time;
		int  g;
		double k = 500.0;

		s = (int)(t / 1000);
		g = (int)(t % 1000);
		for(int i = 31; k >= 1.0; k /= 2, i--) {
			if(g > i) {
				f |= (1l << i);
				g -= (int)k;
			}
		}
		return new NTPTimestamp(s + OFFSET_1970_S, f);
	}

	/**
	 * 
	 * @return
	 */
	public static NTPTimestamp localCurrentTime() {
		return getInstance(System.currentTimeMillis());
	}

	/**
	 * 
	 * @return
	 */
	public long getTime() {
		long r = 0;
		double k = 500;

		for(int i = 31; k > 0; k /= 2, i--) {
			if((fraction & (1l << i)) != 0) {
				r += (int)k;
			}
		}
		r += seconds * 1000;
		return r - OFFSET_1970_MS;
	}

	/**
	 * 
	 * @return
	 */
	public java.util.Date getDate() {
		return new java.util.Date(getTime());
	}

	/**
	 * 
	 * @return
	 */
	public long getSeconds() {
		return seconds;
	}

	/**
	 * 
	 * @return
	 */
	public long getFraction() {
		return fraction;
	}

	/**
	 * 
	 * @param t
	 * @return
	 */
	public NTPTimestamp add(long l) {
		long s, f, c;

		f = fraction + (l & 0xffffffffl);
		c = f >> 32;
		f = f & 0xffffffffl;
		s = seconds + (l >> 32) + c;
		return new NTPTimestamp(s, f);
	}

	/**
	 * 
	 * @param t
	 * @return
	 */
	public long subtract(NTPTimestamp t) {
		long s, f, c;

		f = fraction - t.fraction;
		c = f >> 32;
		f = f & 0xffffffffl;
		s = seconds - t.seconds + c;
		return (s << 32) | f;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return (int)fraction;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof NTPTimestamp) {
			NTPTimestamp n = (NTPTimestamp)o;

			return seconds == n.seconds && fraction == n.fraction;
		}
		return false;
	}

}
