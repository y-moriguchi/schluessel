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
 * @author MORIGUCHI, Yuichiro 2012/09/22
 */
public class Degree extends Number implements Comparable<Degree> {

	/**
	 * 
	 */
	public static Degree ZERO = new Degree(
			Signum2.POSITIVE, NonnegativeDegree.ZERO);

	//
	private Signum2 signum;
	private NonnegativeDegree number;

	//
	Degree(int x) {
		this.signum = (x < 0) ? Signum2.NEGATIVE : Signum2.POSITIVE;
		this.number = new NonnegativeDegree(Math.abs(x));
	}

	/**
	 * 
	 * @param signum
	 * @param abs
	 */
	public Degree(Signum2 signum, NonnegativeDegree abs) {
		this.signum = signum;
		this.number = abs;
	}

	/**
	 * 
	 * @param signum
	 * @param abs
	 */
	public Degree(int signum, NonnegativeDegree abs) {
		this.signum = Signum2.toSignum(signum);
		this.number = abs;
	}

	/**
	 * 
	 * @param degree
	 * @param minutes
	 * @param seconds
	 * @param undersec
	 */
	public Degree(int degree, int minutes, int seconds,
			int undersec) {
		if(degree != 0) {
			this.signum = (degree < 0) ?
					Signum2.NEGATIVE : Signum2.POSITIVE;
			this.number = new NonnegativeDegree(
					Math.abs(degree), minutes, seconds, undersec);
		} else if(minutes != 0) {
			this.signum = (minutes < 0) ?
					Signum2.NEGATIVE : Signum2.POSITIVE;
			this.number = new NonnegativeDegree(
					0, Math.abs(minutes), seconds, undersec);
		} else if(seconds != 0) {
			this.signum = (seconds < 0) ?
					Signum2.NEGATIVE : Signum2.POSITIVE;
			this.number = new NonnegativeDegree(
					0, 0, Math.abs(seconds), undersec);
		} else if(undersec != 0) {
			this.signum = (undersec < 0) ?
					Signum2.NEGATIVE : Signum2.POSITIVE;
			this.number = new NonnegativeDegree(
					0, 0, 0, Math.abs(undersec));
		} else {
			this.signum = Signum2.POSITIVE;
			this.number = NonnegativeDegree.ZERO;
		}
	}

	/**
	 * 
	 * @param string
	 * @return
	 */
	public static Degree parse(String string) {
		NonnegativeDegree d;
		Signum2 s;

		if(string == null) {
			throw new NullPointerException();
		} else if(string.length() == 0) {
			throw new NumberFormatException();
		}

		switch(string.charAt(0)) {
		case '+':
			s = Signum2.POSITIVE;
			d = NonnegativeDegree.parse(string.substring(1));
			break;
		case '-':
			s = Signum2.NEGATIVE;
			d = NonnegativeDegree.parse(string.substring(1));
			break;
		default:
			s = Signum2.POSITIVE;
			d = NonnegativeDegree.parse(string);
			break;
		}
		return new Degree(s, d);
	}

	/**
	 * 
	 * @param x
	 * @param carry
	 * @return
	 */
	public Degree add(Degree x, int[] carry) {
		NonnegativeDegree z;

		if(signum.equals(x.signum)) {
			z = number.add(x.number, carry);
			if(carry != null && carry.length > 0) {
				carry[0] = signum.multiply(carry[0]);
			}
			return new Degree(signum, z);
		} else if(number.compareTo(x.number) > 0) {
			z = number.subtract(x.number);
			if(carry != null && carry.length > 0) {
				carry[0] = 0;
			}
			return new Degree(signum, z);
		} else if(number.compareTo(x.number) < 0) {
			z = x.number.subtract(number);
			if(carry != null && carry.length > 0) {
				carry[0] = 0;
			}
			return new Degree(x.signum, z);
		} else {
			return ZERO;
		}
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Degree add(Degree x) {
		return add(x, null);
	}

	/**
	 * 
	 * @param x
	 * @param carry
	 * @return
	 */
	public Degree subtract(Degree x, int[] carry) {
		return add(x.negate(), carry);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Degree subtract(Degree x) {
		return subtract(x, null);
	}

	/**
	 * 
	 * @return
	 */
	public Degree negate() {
		return new Degree(signum.negate(), number);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Degree multiply(Degree x, int[] carry) {
		return new Degree(signum.multiply(x.signum),
				number.multiply(x.number, carry));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Degree multiply(Degree x) {
		return multiply(x, null);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Degree divide(Degree x, int[] carry) {
		return new Degree(signum.multiply(x.signum),
				number.divide(x.number, carry));
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Degree divide(Degree x) {
		return divide(x, null);
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public Degree remainder(Degree x) {
		return new Degree(signum, number.remainder(x.number));
	}

	/**
	 * 
	 * @return
	 */
	public Degree invert() {
		return new Degree(signum, number.invert());
	}

	/**
	 * 
	 * @return
	 */
	public Degree abs() {
		return new Degree(Signum2.POSITIVE, number);
	}

	/**
	 * 
	 * @return
	 */
	public int signum() {
		return (number.signum() == 0) ? 0 : signum.signum();
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#intValue()
	 */
	@Override
	public int intValue() {
		return signum.multiply(number.intValue());
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#longValue()
	 */
	@Override
	public long longValue() {
		return signum.multiply(number.longValue());
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#floatValue()
	 */
	@Override
	public float floatValue() {
		return (float)doubleValue();
	}

	/* (non-Javadoc)
	 * @see java.lang.Number#doubleValue()
	 */
	@Override
	public double doubleValue() {
		return signum.multiply(number.doubleValue());
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	@Override
	public int compareTo(Degree o) {
		int c = signum.compareTo(o.signum);

		return (c != 0) ? c : number.compareTo(o.number);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return signum.multiply(number.hashCode());
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		Degree d;

		if(!(o instanceof Degree)) {
			return false;
		} else if((d = (Degree)o).signum() == 0 && signum() == 0) {
			return true;
		} else if(!signum.equals(d.signum)) {
			return false;
		} else {
			return number.equals(d.number);
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer b = new StringBuffer();

		if(signum.signum() < 0)  b.append("-");
		b.append(number.toString());
		return b.toString();
	}

}
