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
package net.morilib.geography;

import net.morilib.arith.NonnegativeDegree;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/09/30
 */
public class Longitude extends Number {

	//
	private int PERIOD = 360 * 60 * 60 * 100;
	private int MAXSEC = 360 * 60 * 60;
	private int HALFPERIOD = 180 * 60 * 60 * 100;
	private int HALFSEC = 180 * 60 * 60;
	private static NonnegativeDegree PERIOD_DEG =
			new NonnegativeDegree(360, 0, 0, 0);
	private static NonnegativeDegree PRIME_MERIDIAN =
			new NonnegativeDegree(180, 0, 0, 0);
	private static NonnegativeDegree MAX_D =
			new NonnegativeDegree(180, 0, 0, 0);

	//
	private NonnegativeDegree degree;

	//
	Longitude(int x) {
		this.degree = new NonnegativeDegree(x);
	}

	//
	Longitude(NonnegativeDegree degree) {
		this.degree = degree;
	}

	/**
	 * 
	 * @param degree
	 * @param minutes
	 * @param seconds
	 * @param undersec
	 */
	public Longitude(int degree, int minutes, int seconds,
			int undersec) {
		if(degree < -180 || degree > 180) {
			throw new IllegalArgumentException();
		} else if(degree > 0) {
			this.degree = new NonnegativeDegree(
					degree + 180, minutes, seconds, undersec);
		} else if(degree < 0) {
			this.degree = PRIME_MERIDIAN.subtract(
					new NonnegativeDegree(
							-degree, minutes, seconds, undersec));
		} else if(minutes > 0) {
			this.degree = new NonnegativeDegree(
					180, minutes, seconds, undersec);
		} else if(minutes < 0) {
			this.degree = PRIME_MERIDIAN.subtract(
					new NonnegativeDegree(
							0, -minutes, seconds, undersec));
		} else if(seconds > 0) {
			this.degree = new NonnegativeDegree(
					180, 0, seconds, undersec);
		} else if(seconds < 0) {
			this.degree = PRIME_MERIDIAN.subtract(
					new NonnegativeDegree(
							0, 0, -seconds, undersec));
		} else if(undersec > 0) {
			this.degree = new NonnegativeDegree(180, 0, 0, undersec);
		} else if(undersec < 0) {
			this.degree = PRIME_MERIDIAN.subtract(
					new NonnegativeDegree(0, 0, 0, -undersec));
		} else {
			this.degree = PRIME_MERIDIAN;
		}
	}

	/**
	 * 
	 * @param string
	 * @return
	 */
	public static Longitude parse(String string) {
		NonnegativeDegree d;

		if(string == null) {
			throw new NullPointerException();
		} else if(string.length() == 0) {
			throw new NumberFormatException();
		}

		switch(string.charAt(string.length() - 1)) {
		case 'E':
			d = NonnegativeDegree.parse(
					string.substring(0, string.length() - 1));
			if(d.compareTo(MAX_D) > 0) {
				throw new NumberFormatException();
			}
			return new Longitude(d.add(PRIME_MERIDIAN));
		case 'W':
			d = NonnegativeDegree.parse(
					string.substring(0, string.length() - 1));
			if(d.compareTo(MAX_D) > 0) {
				throw new NumberFormatException();
			}
			return new Longitude(PRIME_MERIDIAN.subtract(d));
		default:
			throw new NumberFormatException();
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Longitude add(Longitude x) {
		return new Longitude(
				degree.add(x.degree).add(MAX_D).remainder(PERIOD_DEG));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Longitude subtract(Longitude x) {
		if(degree.compareTo(x.degree) >= 0) {
			return new Longitude(degree.subtract(x.degree).add(MAX_D));
		} else {
			return new Longitude(PERIOD_DEG.subtract(
					x.degree.subtract(degree)).add(MAX_D));
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Longitude multiply(int x) {
		long a = degree.toCentiSecond() - HALFPERIOD;

		a = (a * x + HALFPERIOD) % PERIOD;
		return new Longitude(new NonnegativeDegree((int)a));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Longitude multiply(double x) {
		double r;

		r = (degree.doubleValue() - HALFSEC) * x + HALFSEC;
		r = Math.IEEEremainder(r, MAXSEC);
		r = (r < 0) ? MAXSEC + r : r;
		return new Longitude(new NonnegativeDegree(r));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Longitude divide(int x) {
		long a;

		a = (((degree.toCentiSecond() - HALFPERIOD)
				/ Math.abs(x)) + HALFPERIOD) % PERIOD;
		a = (x < 0) ? PERIOD - a : a;
		return new Longitude(new NonnegativeDegree((int)a));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Longitude divide(double x) {
		double r;

		r = ((degree.doubleValue() - HALFSEC) / x) + HALFSEC;
		r = Math.IEEEremainder(r, MAXSEC);
		r = (r < 0) ? MAXSEC + r : r;
		return new Longitude(new NonnegativeDegree(r));
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#intValue()
	 */
	@Override
	public int intValue() {
		return degree.intValue();
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#longValue()
	 */
	@Override
	public long longValue() {
		return degree.longValue();
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#floatValue()
	 */
	@Override
	public float floatValue() {
		return degree.floatValue();
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#doubleValue()
	 */
	@Override
	public double doubleValue() {
		return degree.doubleValue();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return degree.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof Longitude) {
			return degree.equals(((Longitude)o).degree);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		if(degree.compareTo(PRIME_MERIDIAN) > 0) {
			return degree.subtract(PRIME_MERIDIAN).toString() + "E";
		} else if(degree.compareTo(PRIME_MERIDIAN) < 0) {
			return PRIME_MERIDIAN.subtract(degree).toString() + "W";
		} else {
			return NonnegativeDegree.ZERO.toString();
		}
	}

}
