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
package net.morilib.lisp.datetime;

import java.math.BigInteger;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import net.morilib.lang.Hashes;
import net.morilib.lang.algebra.Subtractable;
import net.morilib.lisp.Datum;
import net.morilib.lisp.LispExactReal;
import net.morilib.lisp.LispRational;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.Symbol;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/07
 */
public class LispTime extends Datum
implements Cloneable, Comparable<LispTime>, Subtractable<LispTime>,
java.io.Serializable {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/07
	 */
	public static enum TimeType implements java.io.Serializable {
		TIME_UTC(Symbol.getSymbol("time-utc")),
		TIME_TAI(Symbol.getSymbol("time-tai")),
		TIME_MONOTONIC(Symbol.getSymbol("time-monotonic")),
		TIME_DURATION(Symbol.getSymbol("time-duration")),
		TIME_PROCESS(Symbol.getSymbol("time-process")),
		TIME_THREAD(Symbol.getSymbol("time-thread"));

		//
		private Datum symbol;

		private TimeType(Datum d) {
			this.symbol = d;
		}

		/**
		 * 
		 * @return
		 */
		public Datum getSymbol() {
			return symbol;
		}

	}

	//
	private static final long UTC_TO_TAI = -34000;
	private static final BigInteger THOUSAND =
		BigInteger.valueOf(1000);
	/*package*/ static final BigInteger MAX_LONG =
		BigInteger.valueOf(Long.MAX_VALUE);
	/*package*/ static final BigInteger MIN_LONG =
		BigInteger.valueOf(Long.MIN_VALUE);

	//
	private static long processEpoch;
	private static ThreadLocal<Long>
	threadEpoch = new ThreadLocal<Long>() {

		@Override
		protected Long initialValue() {
			return System.currentTimeMillis();
		}

	};

	//
	/*package*/ static final Map<Datum, TimeType> SYM_TO_TYPE;

	static {
		Map<Datum, TimeType> symToType =
			new HashMap<Datum, TimeType>();

		symToType.put(
				Symbol.getSymbol("time-utc"),
				TimeType.TIME_UTC);
		symToType.put(
				Symbol.getSymbol("time-tai"),
				TimeType.TIME_TAI);
		symToType.put(
				Symbol.getSymbol("time-monotonic"),
				TimeType.TIME_MONOTONIC);
		symToType.put(
				Symbol.getSymbol("time-duration"),
				TimeType.TIME_DURATION);
		symToType.put(
				Symbol.getSymbol("time-process"),
				TimeType.TIME_PROCESS);
		symToType.put(
				Symbol.getSymbol("time-thread"),
				TimeType.TIME_THREAD);
		SYM_TO_TYPE = Collections.unmodifiableMap(symToType);
	}

	//
	/*package*/ long time;
	private TimeType type;

	/**
	 * 
	 * @param type
	 * @param time
	 */
	public LispTime(TimeType type, long time) {
		this.type = type;
		this.time = time;

		if(this.time > Long.MAX_VALUE / 1000) {
			this.time = Long.MAX_VALUE;
		} else if(this.time < Long.MIN_VALUE / 1000) {
			this.time = Long.MIN_VALUE;
		}
	}

	/**
	 * 
	 * @param type
	 * @param sec
	 */
	public LispTime(TimeType type, LispReal sec) {
		this.type = type;
		setSecond2(sec);
	}

	/**
	 * 
	 * @param type
	 * @param time
	 * @param nanos
	 */
	public LispTime(TimeType type, long sec, int nanos) {
		this(type, sec * 1000 + nanos / 1000000);
	}

	/**
	 * 
	 */
	public static void initilizeProcessEpoch() {
		processEpoch = System.currentTimeMillis();
	}

	/**
	 * 
	 * @param time
	 * @return
	 */
	public static LispTime currentUTCTime() {
		return new LispTime(
				TimeType.TIME_UTC,
				System.currentTimeMillis());
	}

	/**
	 * 
	 * @return
	 */
	public static LispTime currentTAITime() {
		return new LispTime(
				TimeType.TIME_TAI,
				System.currentTimeMillis() + UTC_TO_TAI);
	}

	/**
	 * 
	 * @return
	 */
	public static LispTime currentMonotonicTime() {
		return currentUTCTime();
	}

	/**
	 * 
	 * @return
	 */
	public static LispTime currentProcessTime() {
		return new LispTime(
				TimeType.TIME_TAI,
				System.currentTimeMillis() - processEpoch);
	}

	/**
	 * 
	 * @return
	 */
	public static LispTime currentThreadTime() {
		return new LispTime(
				TimeType.TIME_TAI,
				System.currentTimeMillis() - threadEpoch.get());
	}

	/**
	 * 
	 * @param time
	 * @return
	 */
	public static LispTime toUTCTime(java.util.Date d) {
		return new LispTime(TimeType.TIME_UTC, d.getTime());
	}

	/**
	 * 
	 * @return
	 */
	public static LispTime toTAITime(java.util.Date d) {
		return new LispTime(
				TimeType.TIME_TAI,
				d.getTime() - UTC_TO_TAI);
	}

	/**
	 * 
	 * @return
	 */
	public static LispTime toMonotonicTime(java.util.Date d) {
		return new LispTime(
				TimeType.TIME_MONOTONIC, d.getTime());
	}

	/**
	 * 
	 * @return
	 */
	public static LispTime toProcessTime(java.util.Date d) {
		return new LispTime(
				TimeType.TIME_TAI,
				d.getTime() - processEpoch);
	}

	/**
	 * 
	 * @return
	 */
	public static LispTime toThreadTime(java.util.Date d) {
		return new LispTime(
				TimeType.TIME_TAI,
				d.getTime() - threadEpoch.get());
	}

	/**
	 * 
	 * @param time
	 * @return
	 */
	public LispTime toUTCTime() {
		switch(type) {
		case TIME_UTC: case TIME_MONOTONIC:
			return clone();
		case TIME_TAI:
			return new LispTime(
					TimeType.TIME_UTC, time + UTC_TO_TAI);
		case TIME_PROCESS:
			return new LispTime(
					TimeType.TIME_UTC, time + processEpoch);
		case TIME_THREAD:
			return new LispTime(
					TimeType.TIME_UTC, time + threadEpoch.get());
		case TIME_DURATION:
			throw new UnsupportedOperationException();
		default:
			throw new IllegalStateException();
		}
	}

	/**
	 * 
	 * @return
	 */
	public LispTime toTAITime() {
		switch(type) {
		case TIME_UTC: case TIME_MONOTONIC:
			return new LispTime(TimeType.TIME_TAI, time - UTC_TO_TAI);
		case TIME_TAI:
			return clone();
		case TIME_PROCESS:
			return new LispTime(
					TimeType.TIME_TAI,
					time + processEpoch - UTC_TO_TAI);
		case TIME_THREAD:
			return new LispTime(
					TimeType.TIME_TAI,
					time + threadEpoch.get() - UTC_TO_TAI);
		case TIME_DURATION:
			throw new UnsupportedOperationException();
		default:
			throw new IllegalStateException();
		}
	}

	/**
	 * 
	 * @return
	 */
	public LispTime toMonotonicTime() {
		switch(type) {
		case TIME_UTC: case TIME_MONOTONIC:
			return clone();
		case TIME_TAI:
			return new LispTime(
					TimeType.TIME_MONOTONIC, time + UTC_TO_TAI);
		case TIME_PROCESS:
			return new LispTime(
					TimeType.TIME_MONOTONIC, time + processEpoch);
		case TIME_THREAD:
			return new LispTime(
					TimeType.TIME_MONOTONIC, time + threadEpoch.get());
		case TIME_DURATION:
			throw new UnsupportedOperationException();
		default:
			throw new IllegalStateException();
		}
	}

	/**
	 * 
	 * @param time
	 * @return
	 */
	public void toUTCTimeSet() {
		switch(type) {
		case TIME_UTC: case TIME_MONOTONIC:
			break;
		case TIME_TAI:
			time = time + UTC_TO_TAI;
			break;
		case TIME_PROCESS:
			time = time + processEpoch;
			break;
		case TIME_THREAD:
			time = time + threadEpoch.get();
			break;
		case TIME_DURATION:
			throw new UnsupportedOperationException();
		default:
			throw new IllegalStateException();
		}
		type = TimeType.TIME_UTC;
	}

	/**
	 * 
	 * @return
	 */
	public void toTAITimeSet() {
		switch(type) {
		case TIME_UTC: case TIME_MONOTONIC:
			time = time - UTC_TO_TAI;
			break;
		case TIME_TAI:
			break;
		case TIME_PROCESS:
			time = time + processEpoch - UTC_TO_TAI;
			break;
		case TIME_THREAD:
			time = time + threadEpoch.get() - UTC_TO_TAI;
			break;
		case TIME_DURATION:
			throw new UnsupportedOperationException();
		default:
			throw new IllegalStateException();
		}
		type = TimeType.TIME_TAI;
	}

	/**
	 * 
	 * @return
	 */
	public void toMonotonicTimeSet() {
		switch(type) {
		case TIME_UTC: case TIME_MONOTONIC:
			break;
		case TIME_TAI:
			time = time + UTC_TO_TAI;
			break;
		case TIME_PROCESS:
			time = time + processEpoch;
			break;
		case TIME_THREAD:
			time = time + threadEpoch.get();
			break;
		case TIME_DURATION:
			throw new UnsupportedOperationException();
		default:
			throw new IllegalStateException();
		}
		type = TimeType.TIME_MONOTONIC;
	}

	/**
	 * 
	 * @return
	 */
	public TimeType getTimeType() {
		return type;
	}

	/**
	 * 
	 * @return
	 */
	public int getNanosecond() {
		return (int)(time % 1000) * 1000000;
	}

	/**
	 * 
	 * @return
	 */
	public long getSecond() {
		return time / 1000;
	}

	/**
	 * 
	 * @param type
	 */
	public void setTimeType(TimeType type) {
		this.type = type;
	}

	/**
	 * 
	 * @param nanos
	 */
	public void setNanosecond(int nanos) {
		time += nanos / 1000000;
	}

	/**
	 * 
	 * @param sec
	 */
	public void setSecond(long sec) {
		if(sec > Long.MAX_VALUE / 1000) {
			time = Long.MAX_VALUE;
		} else if(sec < Long.MIN_VALUE / 1000) {
			time = Long.MIN_VALUE;
		} else {
			time += sec * 1000;
		}
	}

	/**
	 * 
	 * @return
	 */
	public LispExactReal getSecond2() {
		return LispRational.newRational(
				BigInteger.valueOf(time), THOUSAND);
	}

	/**
	 * 
	 * @param sec
	 */
	public void setSecond2(LispReal sec) {
		LispExactReal r;
		BigInteger b;

		r = !sec.isExact() ? sec.toExact() : (LispExactReal)sec;
		b = r.getBigInteger().multiply(THOUSAND);
		if(b.compareTo(MAX_LONG) > 0) {
			time = Long.MAX_VALUE;
		} else if(b.compareTo(MIN_LONG) < 0) {
			time = Long.MIN_VALUE;
		} else {
			time = r.getLong();
		}
	}

	/**
	 * 
	 * @return
	 */
	public long getTimeMillis() {
		return time;
	}

	/**
	 * 
	 * @param type
	 * @return
	 */
	public static int getTimeResolution(TimeType type) {
		return 1000000/*nanosecond*/;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#add(java.lang.Object)
	 */
	public LispTime add(LispTime x) {
		if(!x.type.equals(TimeType.TIME_DURATION)) {
			throw new IllegalArgumentException();
		}
		return new LispTime(type, time + x.time);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public LispTime multiply(int n) {
		if(!type.equals(TimeType.TIME_DURATION)) {
			throw new IllegalArgumentException();
		}
		return new LispTime(type, time * n);
	}

	/**
	 * 
	 * @param t
	 * @return
	 */
	public LispTime subtract(LispTime t) {
		if(type.equals(t.type)) {
			return new LispTime(
					TimeType.TIME_DURATION, time - t.time);
		} else if(t.type.equals(TimeType.TIME_DURATION)) {
			return new LispTime(type, time - t.time);
		} else {
			throw new IllegalArgumentException();
		}
	}

	/**
	 * 
	 * @param t
	 * @return
	 */
	public void addAssign(LispTime x) {
		if(!x.type.equals(TimeType.TIME_DURATION)) {
			throw new IllegalArgumentException();
		}
		time += x.time;
	}

	/**
	 * 
	 * @param t
	 * @return
	 */
	public void subtractAssign(LispTime t) {
		if(type.equals(t.type)) {
			type  = TimeType.TIME_DURATION;
			time -= t.time;
		} else if(t.type.equals(TimeType.TIME_DURATION)) {
			time -= t.time;
		} else {
			throw new IllegalArgumentException();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#clone()
	 */
	public LispTime clone() {
		return new LispTime(type, time);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<time ").append(time).append(">");
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		int r = Hashes.INIT;

		r = Hashes.A * (r + type.hashCode());
		r = Hashes.A * (r + (int)time);
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if(obj instanceof LispTime) {
			LispTime o = (LispTime)obj;

			return type.equals(o.type) && time == o.time;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(LispTime o) {
		if(type.equals(o.type)) {
			return (time < o.time) ? -1 : ((time > o.time) ? 1 : 0);
		} else {
			throw new IllegalArgumentException();
		}
	}

}
