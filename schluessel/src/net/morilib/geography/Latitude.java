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
public class Latitude extends Number {

	//
	private int PERIOD = 180 * 60 * 60 * 100;
	private int MAXSEC = 180 * 60 * 60;
	private int HALFPERIOD = 90 * 60 * 60 * 100;
	private int HALFSEC = 90 * 60 * 60;
	private static NonnegativeDegree PERIOD_DEG =
			new NonnegativeDegree(180, 0, 0, 0);
	private static NonnegativeDegree EQUATOR =
			new NonnegativeDegree(90, 0, 0, 0);
	private static NonnegativeDegree MAX_D =
			new NonnegativeDegree(90, 0, 0, 0);

	//
	private NonnegativeDegree degree;

	//
	Latitude(int x) {
		this.degree = new NonnegativeDegree(x);
	}

	//
	Latitude(NonnegativeDegree degree) {
		this.degree = degree;
	}

	/**
	 * 
	 * @param degree
	 * @param minutes
	 * @param seconds
	 * @param undersec
	 */
	public Latitude(int degree, int minutes, int seconds,
			int undersec) {
		if(degree < -90 || degree > 90) {
			throw new IllegalArgumentException();
		} else if(degree > 0) {
			this.degree = new NonnegativeDegree(
					degree + 90, minutes, seconds, undersec);
		} else if(degree < 0) {
			this.degree = EQUATOR.subtract(new NonnegativeDegree(
					-degree, minutes, seconds, undersec));
		} else if(minutes > 0) {
			this.degree = new NonnegativeDegree(
					90, minutes, seconds, undersec);
		} else if(minutes < 0) {
			this.degree = EQUATOR.subtract(new NonnegativeDegree(
					0, -minutes, seconds, undersec));
		} else if(seconds > 0) {
			this.degree = new NonnegativeDegree(
					90, 0, seconds, undersec);
		} else if(seconds < 0) {
			this.degree = EQUATOR.subtract(new NonnegativeDegree(
					0, 0, -seconds, undersec));
		} else if(undersec > 0) {
			this.degree = new NonnegativeDegree(90, 0, 0, undersec);
		} else if(undersec < 0) {
			this.degree = EQUATOR.subtract(
					new NonnegativeDegree(0, 0, 0, -undersec));
		} else {
			this.degree = EQUATOR;
		}
	}

	/**
	 * 
	 * @param string
	 * @return
	 */
	public static Latitude parse(String string) {
		NonnegativeDegree d;

		if(string == null) {
			throw new NullPointerException();
		} else if(string.length() == 0) {
			throw new NumberFormatException();
		}

		switch(string.charAt(string.length() - 1)) {
		case 'N':
			d = NonnegativeDegree.parse(
					string.substring(0, string.length() - 1));
			if(d.compareTo(MAX_D) > 0) {
				throw new NumberFormatException();
			}
			return new Latitude(d.add(EQUATOR));
		case 'S':
			d = NonnegativeDegree.parse(
					string.substring(0, string.length() - 1));
			if(d.compareTo(MAX_D) > 0) {
				throw new NumberFormatException();
			}
			return new Latitude(EQUATOR.subtract(d));
		default:
			throw new NumberFormatException();
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Latitude add(Latitude x) {
		return new Latitude(
				degree.add(x.degree).add(MAX_D).remainder(PERIOD_DEG));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Latitude subtract(Latitude x) {
		if(degree.compareTo(x.degree) >= 0) {
			return new Latitude(degree.subtract(x.degree).add(MAX_D));
		} else {
			return new Latitude(PERIOD_DEG.subtract(
					x.degree.subtract(degree)).add(MAX_D));
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Latitude multiply(int x) {
		long a = degree.toCentiSecond() - HALFPERIOD;

		a = (a * x + HALFPERIOD) % PERIOD;
		return new Latitude(new NonnegativeDegree((int)a));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Latitude multiply(double x) {
		double r;

		r = (degree.doubleValue() - HALFSEC) * x + HALFSEC;
		r = Math.IEEEremainder(r, MAXSEC);
		r = (r < 0) ? MAXSEC + r : r;
		return new Latitude(new NonnegativeDegree(r));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Latitude divide(int x) {
		long a;

		a = (((degree.toCentiSecond() - HALFPERIOD)
				/ Math.abs(x)) + HALFPERIOD) % PERIOD;
		a = (x < 0) ? PERIOD - a : a;
		return new Latitude(new NonnegativeDegree((int)a));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Latitude divide(double x) {
		double r;

		r = ((degree.doubleValue() - HALFSEC) / x) + HALFSEC;
		r = Math.IEEEremainder(r, MAXSEC);
		r = (r < 0) ? MAXSEC + r : r;
		return new Latitude(new NonnegativeDegree(r));
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
		if(o instanceof Latitude) {
			return degree.equals(((Latitude)o).degree);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		if(degree.compareTo(EQUATOR) > 0) {
			return degree.subtract(EQUATOR).toString() + "N";
		} else if(degree.compareTo(EQUATOR) < 0) {
			return EQUATOR.subtract(degree).toString() + "S";
		} else {
			return NonnegativeDegree.ZERO.toString();
		}
	}

}
