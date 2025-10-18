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
package net.morilib.math.constants;

import java.util.Iterator;

import net.morilib.lang.number.Integer2;
import net.morilib.lang.number.Rational;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/26
 */
public class AlternatingSeriesNumber extends Number {

	/**
	 * 
	 */
	protected Rational scale;

	/**
	 * 
	 */
	protected Rational shift;

	/**
	 * 
	 */
	protected Iterable<Rational> iterable;

	//
	private transient double pitmp  = Double.NaN;
	private transient long   pitmpl = 0;
	private transient Integer2 pitmpb = null;

	/**
	 * 
	 * @param scale
	 */
	public AlternatingSeriesNumber(Iterable<Rational> iterable,
			Rational scale, Rational shift) {
		if(scale.isZero()) {
			throw new IllegalArgumentException();
		}
		this.iterable = iterable;
		this.scale    = scale;
		this.shift    = shift;
	}

	/**
	 * 
	 * @param scale
	 */
	public AlternatingSeriesNumber(long scale) {
		if(scale == 0) {
			throw new IllegalArgumentException();
		}
		this.scale = Rational.valueOf(scale);
	}

	//
	Rational _next(Iterator<Rational> itr) {
		return itr.next().multiply(scale).add(shift);
	}

	@Override
	public double doubleValue() {
		Iterator<Rational> itr;

		if(Double.isNaN(pitmp)) {
			synchronized(this) {
				itr = iterable.iterator();
				for(double t = pitmp; t != pitmp;) {
					pitmp = t;
					t = _next(itr).doubleValue();
				}
			}
		}
		return pitmp;
	}

	@Override
	public float floatValue() {
		return (float)doubleValue();
	}

	@Override
	public int intValue() {
		return (int)pitmp;
	}

	@Override
	public long longValue() {
		Iterator<Rational> itr;

		if(pitmpl == 0) {
			synchronized(this) {
				itr = iterable.iterator();
				for(long t = pitmpl; t != pitmpl; t = pitmpl) {
					t = _next(itr).castLong();
				}
			}
		}
		return pitmpl;
	}

	/**
	 * 
	 * @return
	 */
	public Integer2 toInteger2() {
		Iterator<Rational> itr;
		Integer2 t;

		if(pitmpb == null) {
			synchronized(this) {
				itr = iterable.iterator();
				for(t = pitmpb; !t.equals(pitmpb); t = pitmpb) {
					t = _next(itr).castInteger2();
				}
			}
		}
		return pitmpb;
	}

	/**
	 * @return the scale
	 */
	public Rational getScale() {
		return scale;
	}

	/**
	 * @return the shift
	 */
	public Rational getShift() {
		return shift;
	}

	/**
	 * 
	 * @return
	 */
	public int signum() {
		return compareTo(Rational.ZERO);
	}

	/**
	 * 
	 * @return
	 */
	public AlternatingSeriesNumber negate() {
		return new AlternatingSeriesNumber(iterable,
				scale.negate(), shift.negate());
	}

	/**
	 * 
	 * @param o
	 * @return
	 */
	public int compareTo(Rational n) {
		Iterator<Rational> itr;

		itr = iterable.iterator();
		while(true) {
			if(_next(itr).compareTo(n) < 0) {
				return -1;
			} else if(_next(itr).compareTo(n) > 0) {
				return 1;
			}
		}
	}

	/**
	 * 
	 * @param o
	 * @return
	 */
	public int compareTo(AlternatingSeriesNumber n) {
		Iterator<Rational> itr, jtr;
		Rational i1, i2, ia, ib, j1, j2, ja, jb;

		itr = iterable.iterator();
		jtr = n.iterable.iterator();
		while(true) {
			i1 = _next(itr);    i2 = _next(itr);
			j1 = n._next(jtr);  j2 = n._next(jtr);
			ia = i1.compareTo(i2) > 0 ? i1 : i2;
			ib = i1.compareTo(i2) > 0 ? i2 : i1;
			ja = j1.compareTo(j2) > 0 ? j1 : j2;
			jb = j1.compareTo(j2) > 0 ? j2 : j1;
			if(ib.compareTo(ja) > 0) {
				return 1;
			} else if(ia.compareTo(jb) < 0) {
				return -1;
			}
		}
	}

}
