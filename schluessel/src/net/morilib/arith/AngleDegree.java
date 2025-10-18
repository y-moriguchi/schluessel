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
package net.morilib.arith;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/09/30
 */
public class AngleDegree extends Number {

	//
	private int MAX360 = 360 * 60 * 60 * 100;
	private static NonnegativeDegree DEGREE360 =
			new NonnegativeDegree(360, 0, 0, 0);

	//
	private NonnegativeDegree degree;

	//
	AngleDegree(int x) {
		this.degree = new NonnegativeDegree(x);
	}

	//
	AngleDegree(NonnegativeDegree degree) {
		this.degree = degree;
	}

	/**
	 * 
	 * @param degree
	 * @param minutes
	 * @param seconds
	 * @param undersec
	 */
	public AngleDegree(int degree, int minutes, int seconds,
			int undersec) {
		if(degree > 0) {
			this.degree = new NonnegativeDegree(
					degree % 360, minutes, seconds, undersec);
		} else if(degree < 0) {
			this.degree = DEGREE360.subtract(new NonnegativeDegree(
					-(degree % 360), minutes, seconds, undersec));
		} else if(minutes > 0) {
			this.degree = new NonnegativeDegree(
					0, minutes, seconds, undersec);
		} else if(minutes < 0) {
			this.degree = DEGREE360.subtract(new NonnegativeDegree(
					0, -minutes, seconds, undersec));
		} else if(seconds > 0) {
			this.degree = new NonnegativeDegree(
					0, 0, seconds, undersec);
		} else if(seconds < 0) {
			this.degree = DEGREE360.subtract(new NonnegativeDegree(
					0, 0, -seconds, undersec));
		} else if(undersec > 0) {
			this.degree = new NonnegativeDegree(0, 0, 0, undersec);
		} else if(undersec < 0) {
			this.degree = DEGREE360.subtract(
					new NonnegativeDegree(0, 0, 0, -undersec));
		} else {
			this.degree = NonnegativeDegree.ZERO;
		}
	}

	/**
	 * 
	 * @param string
	 * @return
	 */
	public static AngleDegree parse(String string) {
		NonnegativeDegree d;

		if(string == null) {
			throw new NullPointerException();
		} else if(string.length() == 0) {
			throw new NumberFormatException();
		}

		switch(string.charAt(0)) {
		case '+':
			d = NonnegativeDegree.parse(string.substring(1));
			return new AngleDegree(d.remainder(DEGREE360));
		case '-':
			d = NonnegativeDegree.parse(string.substring(1));
			return new AngleDegree(
					DEGREE360.subtract(d.remainder(DEGREE360)));
		default:
			d = NonnegativeDegree.parse(string);
			return new AngleDegree(d.remainder(DEGREE360));
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public AngleDegree add(AngleDegree x) {
		return new AngleDegree(
				degree.add(x.degree).remainder(DEGREE360));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public AngleDegree subtract(AngleDegree x) {
		if(degree.compareTo(x.degree) >= 0) {
			return new AngleDegree(degree.subtract(x.degree));
		} else {
			return new AngleDegree(DEGREE360.subtract(
					x.degree.subtract(degree)));
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public AngleDegree multiply(int x) {
		long a;
		int c = degree.toCentiSecond();

		a = (x < 0) ? MAX360 - c : c;
		a = (a * Math.abs(x)) % MAX360;
		return new AngleDegree(new NonnegativeDegree((int)a));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public AngleDegree multiply(double x) {
		double r;

		r = degree.doubleValue() * x;
		r = Math.IEEEremainder(r, 1296000);
		r = (r < 0) ? 1296000 + r : r;
		return new AngleDegree(new NonnegativeDegree(r));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public AngleDegree divide(int x) {
		long a;

		a = (degree.toCentiSecond() / Math.abs(x)) % MAX360;
		a = (x < 0) ? MAX360 - a : a;
		return new AngleDegree(new NonnegativeDegree((int)a));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public AngleDegree divide(double x) {
		double r;

		r = degree.doubleValue() / x;
		r = Math.IEEEremainder(r, 1296000);
		r = (r < 0) ? 1296000 + r : r;
		return new AngleDegree(new NonnegativeDegree(r));
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
		if(o instanceof AngleDegree) {
			return degree.equals(((AngleDegree)o).degree);
		}
		return false;
	}

	/*(non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return degree.toString();
	}

}
